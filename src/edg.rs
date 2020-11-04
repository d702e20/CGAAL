use crate::com::{Broker, ChannelBroker};
use crate::common::{Edges, HyperEdge, Message, NegationEdge, VertexAssignment, WorkerId};
use crossbeam_channel::{Receiver, Select};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
#[cfg(feature = "graph-printer")]
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::thread;

const WORKER_COUNT: u64 = 4;

#[cfg(feature = "graph-printer")]
pub trait Vertex: Hash + Eq + PartialEq + Clone + Display + Debug {}

#[cfg(not(feature = "graph-printer"))]
pub trait Vertex: Hash + Eq + PartialEq + Clone + Debug {}

pub trait ExtendedDependencyGraph<V: Vertex> {
    /// Return out going edges from `vertex`.
    /// This will be cached on each worker.
    fn succ(&self, vertex: &V) -> HashSet<Edges<V>>;
}

pub fn distributed_certain_zero<
    G: ExtendedDependencyGraph<V> + Send + Sync + Clone + 'static,
    V: Vertex + Send + Sync + 'static,
>(
    edg: G,
    v0: V,
    worker_count: u64,
) -> VertexAssignment {
    // NOTE: 'static lifetime doesn't mean the full duration of the program execution
    let (broker, mut msg_rxs, mut term_rxs) = ChannelBroker::new(worker_count);
    let broker = Arc::new(broker);
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
    worker_count: u64,
    v0: V,
    assignment: HashMap<V, VertexAssignment>,
    depends: HashMap<V, HashSet<Edges<V>>>,
    interests: HashMap<V, HashSet<WorkerId>>,
    msg_rx: Receiver<Message<V>>,
    term_rx: Receiver<VertexAssignment>,
    broker: Arc<B>,
    successors: HashMap<V, HashSet<Edges<V>>>,
    edg: G,
}

impl<B: Broker<V> + Debug, G: ExtendedDependencyGraph<V> + Send + Sync + Debug, V: Vertex>
    Worker<B, G, V>
{
    fn vertex_owner(&self, vertex: &V) -> WorkerId {
        // Static allocation of vertices to workers
        let mut hasher = DefaultHasher::new();
        vertex.hash::<DefaultHasher>(&mut hasher);
        hasher.finish() % self.worker_count
    }

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

        // Use crossbeam-channel::Select to listen both the term_rx and msg_rx channel
        let mut select = Select::new();
        let oper_term = select.recv(&term_rx);
        let oper_msg = select.recv(&msg_rx);

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
                for edge in successors {
                    // Line 5
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
                .map_or(false, |f| matches!(f, VertexAssignment::TRUE))
        });

        // Line 4
        if any_target {
            self.delete_edge(Edges::HYPER(edge));
            return;
        }

        // Line 5-8
        for target in &edge.targets {
            // Condition from line 5
            if let Some(f) = self.assignment.get(&target) {
                match f {
                    VertexAssignment::UNDECIDED => {}
                    _ => continue,
                }
            }

            // Line 7-8, target = u
            self.add_depend(target, Edges::HYPER(edge.clone()));
            if let None = self.assignment.get(target) {
                self.explore(target)
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
        // Line 3
        match self.assignment.get(&edge.target) {
            // UNEXPLORED
            None => {
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
                    )
                }
                VertexAssignment::TRUE => {
                    return self.broker.send(
                        requester,
                        Message::ANSWER {
                            vertex: vertex.clone(),
                            assignment: VertexAssignment::TRUE,
                        },
                    )
                }
                VertexAssignment::UNDECIDED => {} // fallthrough
            }
        }
        // Final assignment of `vertex` is not yet known
        self.mark_interest(vertex, requester);
        self.explore(&vertex);
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
        // Line 2
        self.assignment.insert(vertex.clone(), assignment);
        // Line 3
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

        // Line 4
        if let Some(depends) = self.depends.get(&vertex) {
            depends.iter().for_each(|edge| match edge {
                Edges::HYPER(edge) => self.broker.send(self.id, Message::<V>::HYPER(edge.clone())),
                Edges::NEGATION(edge) => self
                    .broker
                    .send(self.id, Message::<V>::NEGATION(edge.clone())),
            });
        }
    }

    fn delete_edge(&mut self, edge: Edges<V>) {
        // Get v
        let source = match edge {
            Edges::HYPER(ref edge) => edge.source.clone(),
            Edges::NEGATION(ref edge) => edge.source.clone(),
        };

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
                if successors.is_empty() {
                    self.final_assign(&source, VertexAssignment::FALSE);
                }
            }
        }

        match edge {
            Edges::HYPER(ref edge) => {
                for target in &edge.targets {
                    self.remove_depend(target, Edges::HYPER(edge.clone()))
                }
            }
            Edges::NEGATION(ref edge) => {
                self.remove_depend(&edge.target, Edges::NEGATION(edge.clone()))
            }
        }
    }

    /// Wraps the ExtendedDependencyGraph::succ(v) with caching allowing edges to be deleted
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
