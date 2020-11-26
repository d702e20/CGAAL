use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::thread;

use crossbeam_channel::{Receiver, Select};

use crate::com::{Broker, ChannelBroker};
use crate::common::{Edges, HyperEdge, Message, NegationEdge, VertexAssignment, WorkerId};

// Based on the algorithm described in "Extended Dependency Graphs and Efficient Distributed Fixed-Point Computation" by A.E. Dalsgaard et al., 2017

pub trait Vertex: Hash + Eq + PartialEq + Clone + Display + Debug {}

pub trait ExtendedDependencyGraph<V: Vertex> {
    /// Return out going edges from `vertex`.
    /// This will be cached on each worker.
    fn succ(&self, vertex: &V) -> HashSet<Edges<V>>;
}

pub fn distributed_certain_zero<
    G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
    V: Vertex + Send + Sync + 'static,
>(
    edg: G,
    v0: V,
    worker_count: u64,
) -> VertexAssignment {
    // NOTE: 'static lifetime doesn't mean the full duration of the program execution
    let (broker, mut msg_rxs, mut term_rxs) = ChannelBroker::new(worker_count);
    // TODO make `Broker` responsible for concurrency, and remove the `Arc` wrapper
    let broker = Arc::new(broker);
    // Channel used for returning the final assigment of `v0` to the calling thread
    let (tx, rx) = crossbeam_channel::bounded(worker_count as usize);

    for i in (0..worker_count).rev() {
        let msg_rx = msg_rxs.pop().unwrap();
        let term_rx = term_rxs.pop().unwrap();
        let mut worker = Worker::new(
            i,
            worker_count,
            v0.clone(),
            msg_rx,
            term_rx,
            broker.clone(),
            edg.clone(),
        );
        let tx = tx.clone();
        thread::spawn(move || {
            let result = worker.run();
            tx.send(result)
                .expect("Failed to submit final assignment of v0");
        });
    }

    rx.recv().unwrap()
}

#[derive(Debug)]
struct Worker<B: Broker<V> + Debug, G: ExtendedDependencyGraph<V>, V: Vertex> {
    id: WorkerId,
    /// Number of workers working on solving the query. This is used as part of the static allocation scheme, see `crate::Worker::vertex_owner`.
    worker_count: u64,
    v0: V,
    assignment: HashMap<V, VertexAssignment>,
    depends: HashMap<V, HashSet<Edges<V>>>,
    /// Map of workers that need to be sent a message once the final assignment of a vertex is known.
    interests: HashMap<V, HashSet<WorkerId>>,
    msg_rx: Receiver<Message<V>>,
    term_rx: Receiver<VertexAssignment>,
    broker: Arc<B>,
    /// The logic of handling which edges have been deleted from a vertex is delegated to Worker instead of having to be duplicated in every implementation of ExtendedDependencyGraph.
    /// The first time succ is called on a vertex the call goes to the ExtendedDependencyGraph implementation, and the result is saved in successors.
    /// In all subsequent calls the vertex edges will be taken from the HashMap. This allows for modification of the output of the succ function.
    successors: HashMap<V, HashSet<Edges<V>>>,
    edg: G,
}

impl<B: Broker<V> + Debug, G: ExtendedDependencyGraph<V> + Send + Sync + Debug, V: Vertex>
    Worker<B, G, V>
{
    /// Determines which worker instance is responsible for computing the value of the vertex.
    /// Vertices are allocated to workers using a static allocation scheme. Dynamic addition and removal of workers isn't supported with this method.
    fn vertex_owner(&self, vertex: &V) -> WorkerId {
        // Static allocation of vertices to workers
        let mut hasher = DefaultHasher::new();
        vertex.hash::<DefaultHasher>(&mut hasher);
        hasher.finish() % self.worker_count
    }

    /// Determines if `self` is responsible for computing the value of `vertex`
    #[inline]
    fn is_owner(&self, vertex: &V) -> bool {
        self.vertex_owner(vertex) == self.id
    }

    pub fn new(
        id: WorkerId,
        worker_count: u64,
        v0: V,
        msg_rx: Receiver<Message<V>>,
        term_rx: Receiver<VertexAssignment>,
        broker: Arc<B>,
        edg: G,
    ) -> Self {
        // Initialize fields
        let assignment = HashMap::new();
        let interests = HashMap::<V, HashSet<WorkerId>>::new();
        let depends = HashMap::<V, HashSet<Edges<V>>>::new();
        let successors = HashMap::<V, HashSet<Edges<V>>>::new();

        Self {
            id,
            worker_count,
            v0,
            assignment,
            depends,
            interests,
            msg_rx,
            term_rx,
            broker,
            successors,
            edg,
        }
    }

    // TODO move msg_rx and term_rx argument from Worker::new to Worker::run
    pub fn run(&mut self) -> VertexAssignment {
        // Alg 1, Line 2
        if self.is_owner(&self.v0.clone()) {
            self.explore(&self.v0.clone());
        }

        let term_rx = self.term_rx.clone();
        let msg_rx = self.msg_rx.clone();

        // Use crossbeam-channel::Select to listen to both the term_rx and msg_rx channel
        let mut select = Select::new();
        let oper_term = select.recv(&term_rx);
        let oper_msg = select.recv(&msg_rx);

        // TODO terminate if all queues empty and all workers are idle
        loop {
            let oper = select.select();
            match oper.index() {
                // Termination signal indicating result have been found
                i if i == oper_term => {
                    // Alg 1, Line 10
                    match oper.recv(&term_rx) {
                        Ok(assignment) => {
                            // Alg 1, Line 11-12
                            return match assignment {
                                VertexAssignment::UNDECIDED => VertexAssignment::FALSE,
                                VertexAssignment::FALSE => VertexAssignment::FALSE,
                                VertexAssignment::TRUE => VertexAssignment::TRUE,
                            };
                        }
                        Err(err) => {
                            panic!("Receiving from termination channel failed with: {}", err)
                        }
                    }
                }
                // Received more work
                i if i == oper_msg => {
                    match oper.recv(&msg_rx) {
                        // Alg 1, Line 5-9
                        Ok(msg) => match msg {
                            // Alg 1, Line 6
                            Message::HYPER(edge) => self.process_hyper_edge(edge),
                            // Alg 1, Line 7
                            Message::NEGATION(edge) => self.process_negation_edge(edge),
                            // Alg 1, Line 8
                            Message::REQUEST { vertex, worker_id } => {
                                self.process_request(&vertex, worker_id)
                            }
                            // Alg 1, Line 9
                            Message::ANSWER { vertex, assignment } => {
                                self.process_answer(&vertex, assignment)
                            }
                        },
                        Err(err) => panic!("Receiving from message channel failed with: {}", err),
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    /// Adds worker to C^i(vertex)
    fn mark_interest(&mut self, vertex: &V, worker: WorkerId) {
        if let Some(set) = self.interests.get_mut(vertex) {
            set.insert(worker);
        } else {
            let mut set = HashSet::new();
            set.insert(worker);
            self.interests.insert(vertex.clone(), set);
        }
    }

    fn explore(&mut self, vertex: &V) {
        // Line 2
        self.assignment
            .insert(vertex.clone(), VertexAssignment::UNDECIDED);

        // Line 3
        if self.is_owner(vertex) {
            let successors = self.succ(vertex); // Line 4
            if successors.is_empty() {
                // Line 4
                self.final_assign(vertex, VertexAssignment::FALSE);
            } else {
                // Line 5
                for edge in successors {
                    match edge {
                        Edges::HYPER(edge) => {
                            self.broker.send(self.id, Message::HYPER(edge.clone()))
                        }
                        Edges::NEGATION(edge) => {
                            self.broker.send(self.id, Message::NEGATION(edge.clone()))
                        }
                    }
                }
            }
        } else {
            // Line 7
            self.broker.send(
                self.vertex_owner(vertex),
                Message::REQUEST {
                    vertex: vertex.clone(),
                    worker_id: self.id,
                },
            )
        }
    }

    fn process_hyper_edge(&mut self, edge: HyperEdge<V>) {
        // Line 3, condition (in case of targets is empty, the default value is true)
        let all_final = edge.targets.iter().all(|target| {
            self.assignment
                .get(target)
                .map_or(false, |f| matches!(f, VertexAssignment::TRUE))
        });

        // Line 3
        if all_final {
            self.final_assign(&edge.source, VertexAssignment::TRUE);
            return;
        }

        // Line 4, condition
        let any_target = edge.targets.iter().any(|target| {
            self.assignment
                .get(target)
                .map_or(false, |f| matches!(f, VertexAssignment::FALSE))
        });

        // Line 4
        if any_target {
            self.delete_edge(Edges::HYPER(edge));
            return;
        }

        // Line 5-8
        for target in &edge.targets {
            // Line 5 condition
            match self.assignment.get(&target) {
                Some(VertexAssignment::UNDECIDED) => {
                    // UNDECIDED
                    // Line 7
                    self.add_depend(target, Edges::HYPER(edge.clone()));
                }
                None => {
                    // UNEXPLORED
                    // Line 7
                    self.add_depend(target, Edges::HYPER(edge.clone()));
                    // Line 8
                    self.explore(target);
                }
                _ => {}
            }
        }
    }

    // Mark `dependency` as a prerequisite for finding the final assignment of `vertex`
    fn add_depend(&mut self, vertex: &V, dependency: Edges<V>) {
        if let Some(dependencies) = self.depends.get_mut(vertex) {
            dependencies.insert(dependency);
        } else {
            let mut dependencies = HashSet::new();
            dependencies.insert(dependency);
            self.depends.insert(vertex.clone(), dependencies);
        }
    }

    // Remove `dependency` as a prerequisite for finding the final assignment of `vertex`
    fn remove_depend(&mut self, vertex: &V, dependency: Edges<V>) {
        if let Some(dependencies) = self.depends.get_mut(vertex) {
            dependencies.remove(&dependency);
        }
    }

    fn process_negation_edge(&mut self, edge: NegationEdge<V>) {
        match self.assignment.get(&edge.target) {
            // UNEXPLORED
            None => {
                // UNEXPLORED
                // Line 6
                self.add_depend(&edge.target, Edges::NEGATION(edge.clone()));
                self.broker.send(self.id, Message::NEGATION(edge.clone()));
                self.explore(&edge.target);
            }
            Some(assignment) => match assignment {
                VertexAssignment::UNDECIDED => {
                    self.final_assign(&edge.source, VertexAssignment::TRUE)
                }
                VertexAssignment::FALSE => self.final_assign(&edge.source, VertexAssignment::TRUE),
                VertexAssignment::TRUE => self.delete_edge(Edges::NEGATION(edge)),
            },
        }
    }

    // Another worker has requested the final assignment of a `vertex`
    fn process_request(&mut self, vertex: &V, requester: WorkerId) {
        if let Some(assigned) = self.assignment.get(&vertex) {
            // Final assignment of `vertex` is already known, reply immediately
            match assigned {
                VertexAssignment::FALSE => {
                    return self.broker.send(
                        requester,
                        Message::ANSWER {
                            vertex: vertex.clone(),
                            assignment: VertexAssignment::FALSE,
                        },
                    );
                }
                VertexAssignment::TRUE => {
                    return self.broker.send(
                        requester,
                        Message::ANSWER {
                            vertex: vertex.clone(),
                            assignment: VertexAssignment::TRUE,
                        },
                    );
                }
                VertexAssignment::UNDECIDED => {} // fallthrough
            }
        }
        // Final assignment of `vertex` is not yet known
        self.mark_interest(vertex, requester);
        if self.assignment.get(&vertex).is_none() {
            // UNEXPLORED
            self.explore(&vertex);
        }
    }

    fn process_answer(&mut self, vertex: &V, assigned: VertexAssignment) {
        self.final_assign(vertex, assigned)
    }

    fn final_assign(&mut self, vertex: &V, assignment: VertexAssignment) {
        // Line 2
        if *vertex == self.v0 {
            self.broker.terminate(assignment);
            return;
        }
        // Line 3
        self.assignment.insert(vertex.clone(), assignment);
        // Line 4
        if let Some(interested) = self.interests.get(&vertex) {
            interested.iter().for_each(|worker_id| {
                self.broker.send(
                    *worker_id,
                    Message::ANSWER {
                        vertex: vertex.clone(),
                        assignment,
                    },
                )
            })
        }

        // Line 5
        if let Some(depends) = self.depends.get(&vertex) {
            depends.iter().for_each(|edge| match edge {
                Edges::HYPER(edge) => self.broker.send(self.id, Message::<V>::HYPER(edge.clone())),
                Edges::NEGATION(edge) => self
                    .broker
                    .send(self.id, Message::<V>::NEGATION(edge.clone())),
            });
        }
    }

    /// Helper function for deleting edges from a vertex.
    fn delete_edge(&mut self, edge: Edges<V>) {
        // Get v
        let source = match edge {
            Edges::HYPER(ref edge) => edge.source.clone(),
            Edges::NEGATION(ref edge) => edge.source.clone(),
        };

        // Initializes the successors hashmap for key source
        if let Some(successors) = self.successors.get_mut(&source) {
            successors.remove(&edge);
        } else {
            let mut successors = self.edg.succ(&source);
            successors.remove(&edge);
            self.successors.insert(source.clone(), successors);
        }

        match self.successors.get(&source) {
            None => panic!("successors should have been filled, or at least have a empty vector"),
            Some(successors) => {
                // Line 3
                if successors.is_empty() {
                    self.final_assign(&source, VertexAssignment::FALSE);
                }
            }
        }

        match edge {
            // Line 4-6
            Edges::HYPER(ref edge) => {
                for target in &edge.targets {
                    self.remove_depend(target, Edges::HYPER(edge.clone()))
                }
            }
            // Line 7-8
            Edges::NEGATION(ref edge) => {
                self.remove_depend(&edge.target, Edges::NEGATION(edge.clone()))
            }
        }
    }

    /// Wraps the ExtendedDependencyGraph::succ(v) with caching allowing edges to be deleted.
    /// See documentation for the `successors` field.
    fn succ(&mut self, vertex: &V) -> HashSet<Edges<V>> {
        if let Some(successors) = self.successors.get(vertex) {
            // List of successors is already allocated for the vertex
            successors.clone()
        } else {
            // Setup the successors list the first time it is requested
            let successors = self.edg.succ(vertex);
            self.successors.insert(vertex.clone(), successors.clone());
            successors
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::hash_map::RandomState;
    use std::collections::HashSet;
    use std::fmt::Display;

    use serde::export::Formatter;

    use crate::atl::dependencygraph::{ATLDependencyGraph, ATLVertex};
    use crate::atl::formula::Phi;
    use crate::atl::gamestructure::EagerGameStructure;
    use crate::common::{Edges, HyperEdge, NegationEdge, VertexAssignment};
    use crate::edg::{distributed_certain_zero, ExtendedDependencyGraph, Vertex};
    use pom::set::Set;
    use std::hint::black_box;
    use std::sync::Arc;
    use test::Bencher;

    #[derive(Hash, Clone, Eq, PartialEq, Debug)]
    struct ExampleEDG {}

    #[test]
    #[ignore]
    fn test_with_edg() {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum ExampleEDGVertices {
            A,
            B,
            C,
            D,
            E,
            F,
            N,
            T,
            G,
        }
        impl Display for ExampleEDGVertices {
            fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
                unimplemented!()
            }
        }
        impl Vertex for ExampleEDGVertices {}

        impl ExtendedDependencyGraph<ExampleEDGVertices> for ExampleEDG {
            fn succ(
                &self,
                vertex: &ExampleEDGVertices,
            ) -> HashSet<Edges<ExampleEDGVertices>, RandomState> {
                // A -> B
                // A -> E
                // B -> C
                // C -> F
                // C -> T
                // D -> E
                // D -> C
                // E -> (D, F)
                // F -> Ø
                // N ..> A
                // T -> G
                // G

                match vertex {
                    ExampleEDGVertices::A => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::A,
                            targets: vec![ExampleEDGVertices::B],
                        }));

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::A,
                            targets: vec![ExampleEDGVertices::E],
                        }));

                        successors
                    }
                    ExampleEDGVertices::B => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::B,
                            targets: vec![ExampleEDGVertices::C],
                        }));

                        successors
                    }
                    ExampleEDGVertices::C => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::C,
                            targets: vec![ExampleEDGVertices::F],
                        }));

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::C,
                            targets: vec![ExampleEDGVertices::T],
                        }));

                        successors
                    }
                    ExampleEDGVertices::D => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::D,
                            targets: vec![ExampleEDGVertices::E],
                        }));

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::D,
                            targets: vec![ExampleEDGVertices::C],
                        }));

                        successors
                    }
                    ExampleEDGVertices::E => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::E,
                            targets: vec![ExampleEDGVertices::D, ExampleEDGVertices::F],
                        }));

                        successors
                    }
                    ExampleEDGVertices::F => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::F,
                            targets: vec![],
                        }));

                        successors
                    }
                    ExampleEDGVertices::N => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::NEGATION(NegationEdge {
                            source: ExampleEDGVertices::N,
                            target: ExampleEDGVertices::A,
                        }));

                        successors
                    }
                    ExampleEDGVertices::T => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::T,
                            targets: vec![ExampleEDGVertices::G],
                        }));

                        successors
                    }
                    ExampleEDGVertices::G => {
                        let successors = HashSet::new();
                        successors
                    }
                }
            }
        }
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::A, 1),
            VertexAssignment::TRUE,
            "Vertex A"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::B, 1),
            VertexAssignment::TRUE,
            "Vertex B"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::C, 1),
            VertexAssignment::TRUE,
            "Vertex C"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::D, 1),
            VertexAssignment::TRUE,
            "Vertex D"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::E, 1),
            VertexAssignment::TRUE,
            "Vertex E"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::F, 1),
            VertexAssignment::TRUE,
            "Vertex F"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::T, 1),
            VertexAssignment::FALSE,
            "Vertex T"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::G, 1),
            VertexAssignment::FALSE,
            "Vertex G"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::N, 1),
            VertexAssignment::FALSE,
            "Vertex N"
        );
    }

    #[test]
    #[ignore]
    fn test_small_dg_all_true_except_for_c() {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum ExampleEDGVertices {
            A,
            B,
            C,
            D,
        }
        impl Display for ExampleEDGVertices {
            fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
                unimplemented!()
            }
        }
        impl Vertex for ExampleEDGVertices {}

        impl ExtendedDependencyGraph<ExampleEDGVertices> for ExampleEDG {
            fn succ(
                &self,
                vertex: &ExampleEDGVertices,
            ) -> HashSet<Edges<ExampleEDGVertices>, RandomState> {
                // A -> B
                // A -> C
                // B -> D
                // C
                // D -> Ø
                match vertex {
                    ExampleEDGVertices::A => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::A,
                            targets: vec![ExampleEDGVertices::B],
                        }));

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::A,
                            targets: vec![ExampleEDGVertices::C],
                        }));

                        successors
                    }
                    ExampleEDGVertices::B => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::B,
                            targets: vec![ExampleEDGVertices::D],
                        }));

                        successors
                    }
                    ExampleEDGVertices::C => {
                        let successors = HashSet::new();

                        successors
                    }
                    ExampleEDGVertices::D => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::D,
                            targets: vec![],
                        }));

                        successors
                    }
                }
            }
        }
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::A, 1),
            VertexAssignment::TRUE,
            "Vertex A"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::B, 1),
            VertexAssignment::TRUE,
            "Vertex B"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::C, 1),
            VertexAssignment::FALSE,
            "Vertex C"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::D, 1),
            VertexAssignment::TRUE,
            "Vertex D"
        );
    }

    #[test]
    #[ignore]
    fn test_small_dg_all_false_except_for_d() {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum ExampleEDGVertices {
            A,
            B,
            C,
            D,
        }
        impl Display for ExampleEDGVertices {
            fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
                unimplemented!()
            }
        }
        impl Vertex for ExampleEDGVertices {}

        impl ExtendedDependencyGraph<ExampleEDGVertices> for ExampleEDG {
            fn succ(
                &self,
                vertex: &ExampleEDGVertices,
            ) -> HashSet<Edges<ExampleEDGVertices>, RandomState> {
                // A -> B
                // B -> C
                // C -> B
                // D -> Ø

                match vertex {
                    ExampleEDGVertices::A => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::A,
                            targets: vec![ExampleEDGVertices::B],
                        }));

                        successors
                    }
                    ExampleEDGVertices::B => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::B,
                            targets: vec![ExampleEDGVertices::C],
                        }));

                        successors
                    }
                    ExampleEDGVertices::C => {
                        let mut successors = HashSet::new();
                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::C,
                            targets: vec![ExampleEDGVertices::B],
                        }));

                        successors
                    }
                    ExampleEDGVertices::D => {
                        let mut successors = HashSet::new();
                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::D,
                            targets: vec![],
                        }));

                        successors
                    }
                }
            }
        }
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::A, 1),
            VertexAssignment::FALSE,
            "Vertex A"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::B, 1),
            VertexAssignment::FALSE,
            "Vertex B"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::C, 1),
            VertexAssignment::FALSE,
            "Vertex C"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::D, 1),
            VertexAssignment::TRUE,
            "Vertex D"
        );
    }
    #[test]
    #[ignore]
    fn test_a_node_with_no_succsessors() {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum ExampleEDGVertices {
            A,
        }
        impl Display for ExampleEDGVertices {
            fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
                unimplemented!()
            }
        }
        impl Vertex for ExampleEDGVertices {}

        impl ExtendedDependencyGraph<ExampleEDGVertices> for ExampleEDG {
            fn succ(
                &self,
                vertex: &ExampleEDGVertices,
            ) -> HashSet<Edges<ExampleEDGVertices>, RandomState> {
                match vertex {
                    ExampleEDGVertices::A => {
                        let successors = HashSet::new();

                        successors
                    }
                }
            }
        }
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::A, 1),
            VertexAssignment::FALSE,
            "Vertex A"
        );
    }
    #[test]
    #[ignore]
    fn test_termination_condtion() {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum ExampleEDGVertices {
            A,
            B,
            C,
        }
        impl Display for ExampleEDGVertices {
            fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
                unimplemented!()
            }
        }
        impl Vertex for ExampleEDGVertices {}

        impl ExtendedDependencyGraph<ExampleEDGVertices> for ExampleEDG {
            fn succ(
                &self,
                vertex: &ExampleEDGVertices,
            ) -> HashSet<Edges<ExampleEDGVertices>, RandomState> {
                match vertex {
                    ExampleEDGVertices::A => {
                        let mut successors = HashSet::new();
                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::A,
                            targets: vec![ExampleEDGVertices::B, ExampleEDGVertices::C],
                        }));

                        successors
                    }
                    ExampleEDGVertices::B => {
                        let successors = HashSet::new();

                        successors
                    }
                    ExampleEDGVertices::C => {
                        let successors = HashSet::new();

                        successors
                    }
                }
            }
        }
        eprintln!("Hello");
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::A, 1),
            VertexAssignment::FALSE,
            "Vertex A"
        );
        //assert_eq!(distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::B, 1), VertexAssignment::FALSE, "Vertex B");
        //assert_eq!(distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::C, 1), VertexAssignment::FALSE, "Vertex C");
    }

    #[test]
    #[ignore]
    fn test_loop_di_loops() {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum ExampleEDGVertices {
            A,
            B,
            C,
            D,
        }
        impl Display for ExampleEDGVertices {
            fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
                unimplemented!()
            }
        }
        impl Vertex for ExampleEDGVertices {}

        impl ExtendedDependencyGraph<ExampleEDGVertices> for ExampleEDG {
            // A -> B
            // B -> (A, C)
            // C -> D
            // D -> Ø

            fn succ(
                &self,
                vertex: &ExampleEDGVertices,
            ) -> HashSet<Edges<ExampleEDGVertices>, RandomState> {
                match vertex {
                    ExampleEDGVertices::A => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::A,
                            targets: vec![ExampleEDGVertices::B],
                        }));

                        successors
                    }
                    ExampleEDGVertices::B => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::B,
                            targets: vec![ExampleEDGVertices::A, ExampleEDGVertices::C],
                        }));

                        successors
                    }
                    ExampleEDGVertices::C => {
                        let mut successors = HashSet::new();
                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::C,
                            targets: vec![ExampleEDGVertices::D],
                        }));

                        successors
                    }
                    ExampleEDGVertices::D => {
                        let mut successors = HashSet::new();
                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::D,
                            targets: vec![],
                        }));

                        successors
                    }
                }
            }
        }
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::A, 1),
            VertexAssignment::FALSE,
            "Vertex A"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::B, 1),
            VertexAssignment::FALSE,
            "Vertex B"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::C, 1),
            VertexAssignment::TRUE,
            "Vertex C"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::D, 1),
            VertexAssignment::TRUE,
            "Vertex D"
        );
    }

    #[test]
    #[ignore]
    fn test_negation_edges() {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum ExampleEDGVertices {
            A,
            B,
        }
        impl Display for ExampleEDGVertices {
            fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
                unimplemented!()
            }
        }
        impl Vertex for ExampleEDGVertices {}

        impl ExtendedDependencyGraph<ExampleEDGVertices> for ExampleEDG {
            // A ..> B
            // B -> Ø

            fn succ(
                &self,
                vertex: &ExampleEDGVertices,
            ) -> HashSet<Edges<ExampleEDGVertices>, RandomState> {
                match vertex {
                    ExampleEDGVertices::A => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::NEGATION(NegationEdge {
                            source: ExampleEDGVertices::A,
                            target: ExampleEDGVertices::B,
                        }));

                        successors
                    }
                    ExampleEDGVertices::B => {
                        let mut successors = HashSet::new();

                        successors.insert(Edges::HYPER(HyperEdge {
                            source: ExampleEDGVertices::B,
                            targets: vec![],
                        }));

                        successors
                    }
                }
            }
        }
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::A, 1),
            VertexAssignment::FALSE,
            "Vertex A"
        );
        assert_eq!(
            distributed_certain_zero(ExampleEDG {}, ExampleEDGVertices::B, 1),
            VertexAssignment::TRUE,
            "Vertex B"
        );
    }

    #[bench]
    fn bench_mexican_standoff(b: &mut Bencher) {
        b.iter(|| {
            let json = String::from_utf8_lossy(include_bytes!("../mexican-standoff.json"));
            let game_structure: EagerGameStructure = serde_json::from_str(json.to_str()).unwrap();
            let formula = Arc::new(Phi::Next {
                players: vec![0],
                formula: Arc::new(Phi::Proposition(0)),
            });
            let graph = ATLDependencyGraph { game_structure };

            black_box(super::distributed_certain_zero)(
                graph,
                ATLVertex::FULL { state: 0, formula },
                1,
            );
        })
    }
}
