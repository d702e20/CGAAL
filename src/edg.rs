use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::thread;

use crossbeam_channel::{Receiver, Select, TrySendError};

use crate::com::{Broker, ChannelBroker};
use crate::common::{Edges, HyperEdge, Message, NegationEdge, VertexAssignment, WorkerId};
use crate::distterm::{ControllerWeight, Weight};
use std::cmp::max;
use tracing::{span, trace, Level};

// Based on the algorithm described in "Extended Dependency Graphs and Efficient Distributed Fixed-Point Computation" by A.E. Dalsgaard et al., 2017

pub trait Vertex: Hash + Eq + PartialEq + Clone + Display + Debug {}

pub trait ExtendedDependencyGraph<V: Vertex> {
    /// Return out going edges from `vertex`.
    /// This will be cached on each worker.
    fn succ(&self, vertex: &V) -> HashSet<Edges<V>>;
}

#[instrument]
pub fn distributed_certain_zero<
    G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
    V: Vertex + Send + Sync + 'static,
>(
    edg: G,
    v0: V,
    worker_count: u64,
) -> VertexAssignment {
    trace!(?v0, worker_count, "starting distributed_certain_zero");

    // NOTE: 'static lifetime doesn't mean the full duration of the program execution
    let (broker, mut msg_rxs, mut hyper_rxs, mut negation_rxs, mut term_rxs, weight_rx) =
        ChannelBroker::new(worker_count);
    // TODO make `Broker` responsible for concurrency, and remove the `Arc` wrapper
    let broker = Arc::new(broker);
    // Channel used for returning the final assigment of `v0` to the calling thread
    let (early_tx, early_rx) = crossbeam_channel::bounded(worker_count as usize);
    let mut controller_weight = ControllerWeight::new();

    for i in (0..worker_count).rev() {
        let msg_rx = msg_rxs.pop().unwrap();
        let hyper_rx = hyper_rxs.pop().unwrap();
        let negation_rx = negation_rxs.pop().unwrap();
        let term_rx = term_rxs.pop().unwrap();
        let mut worker = Worker::new(
            i,
            worker_count,
            v0.clone(),
            msg_rx,
            hyper_rx,
            negation_rx,
            term_rx,
            broker.clone(),
            edg.clone(),
        );
        let tx = early_tx.clone();
        thread::spawn(move || {
            trace!("worker thread start");
            let result = worker.run();
            match tx.try_send(result) {
                Ok(_) => {}
                Err(err) => match err {
                    TrySendError::Full(_) => panic!(
                        "Failed to submit final assignment of v0 because the channel is full: {}",
                        err
                    ),
                    TrySendError::Disconnected(_) => {}
                },
            }
        });
    }

    let mut sel = Select::new();
    let early_index = sel.recv(&early_rx);
    let weight_index = sel.recv(&weight_rx);

    loop {
        // Wait until a receive operation becomes ready and try executing it.
        let oper = sel.select();
        match oper.index() {
            i if i == early_index => {
                let assignment = oper
                    .recv(&early_rx)
                    .expect("Error receiving final assigment from early termination");
                trace!(v0_assignment = ?assignment, "early termination");
                return assignment;
            }
            i if i == weight_index => {
                let weight = oper.recv(&weight_rx).expect("Error receiving weight");
                trace!(?weight, "controller received weight");
                controller_weight.receive_weight(weight);
                if controller_weight.is_full() {
                    trace!("all weight received");
                    // Send term to all workers
                    let assignment = VertexAssignment::FALSE;
                    broker.terminate(assignment);
                    return assignment;
                }
            }
            _ => unreachable!(),
        }
    }
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
    distances: HashMap<V, u32>,
    msg_rx: Receiver<Message<V>>,
    hyper_rx: Receiver<(HyperEdge<V>, Weight)>,
    negation_rx: Receiver<(NegationEdge<V>, Weight)>,
    unsafe_edges: Vec<Vec<(NegationEdge<V>, Weight)>>,
    term_rx: Receiver<VertexAssignment>,
    broker: Arc<B>,
    /// The logic of handling which edges have been deleted from a vertex is delegated to Worker instead of having to be duplicated in every implementation of ExtendedDependencyGraph.
    /// The first time succ is called on a vertex the call goes to the ExtendedDependencyGraph implementation, and the result is saved in successors.
    /// In all subsequent calls the vertex edges will be taken from the HashMap. This allows for modification of the output of the succ function.
    successors: HashMap<V, HashSet<Edges<V>>>,
    edg: G,
    counter: u32,
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
        let hash = hasher.finish();
        trace!(hash);
        hash % self.worker_count
    }

    /// Determines if `self` is responsible for computing the value of `vertex`
    #[inline]
    fn is_owner(&self, vertex: &V) -> bool {
        self.vertex_owner(vertex) == self.id
    }

    #[allow(clippy::too_many_arguments)]
    pub fn new(
        id: WorkerId,
        worker_count: u64,
        v0: V,
        msg_rx: Receiver<Message<V>>,
        hyper_rx: Receiver<(HyperEdge<V>, Weight)>,
        negation_rx: Receiver<(NegationEdge<V>, Weight)>,
        term_rx: Receiver<VertexAssignment>,
        broker: Arc<B>,
        edg: G,
    ) -> Self {
        // Initialize fields
        let assignment = HashMap::new();
        let interests = HashMap::<V, HashSet<WorkerId>>::new();
        let depends = HashMap::<V, HashSet<Edges<V>>>::new();
        let successors = HashMap::<V, HashSet<Edges<V>>>::new();
        let distances = HashMap::<V, u32>::new();
        let unsafe_edges = Vec::<Vec<(NegationEdge<V>, Weight)>>::new();
        let counter = 0;

        trace!(
            worker_id = id,
            worker_count,
            ?v0,
            "new distributed_certain_zero worker"
        );

        Self {
            id,
            worker_count,
            v0,
            assignment,
            depends,
            interests,
            msg_rx,
            hyper_rx,
            negation_rx,
            unsafe_edges,
            distances,
            term_rx,
            broker,
            successors,
            edg,
            counter,
        }
    }

    // TODO move msg_rx and term_rx argument from Worker::new to Worker::run
    pub fn run(&mut self) -> VertexAssignment {
        let span = span!(Level::DEBUG, "worker run", worker_id = self.id);
        let _enter = span.enter();
        trace!("worker start");

        // Alg 1, Line 2
        if self.is_owner(&self.v0.clone()) {
            trace!(worker_id = self.id, "exploring v0");
            let init_weight = Weight::new_full();
            self.explore(&self.v0.clone(), init_weight);
        }

        let term_rx = self.term_rx.clone();
        let hyper_rx = self.hyper_rx.clone();
        let negation_rx = self.negation_rx.clone();
        let msg_rx = self.msg_rx.clone();

        loop {
            // Termination signal indicating result have been found
            if !term_rx.is_empty() {
                self.counter = 0;
                match term_rx.recv() {
                    Ok(assignment) => {
                        // Alg 1, Line 11-12
                        trace!(?assignment, "worker received termination");
                        return match assignment {
                            VertexAssignment::UNDECIDED => VertexAssignment::FALSE,
                            VertexAssignment::FALSE => VertexAssignment::FALSE,
                            VertexAssignment::TRUE => VertexAssignment::TRUE,
                        };
                    }
                    Err(err) => panic!("Receiving from termination channel failed with: {}", err),
                }
            } else if !msg_rx.is_empty() {
                let _guard = span!(Level::TRACE, "worker receive message", worker_id = self.id);
                self.counter = 0;
                match msg_rx.recv() {
                    // Alg 1, Line 5-9
                    Ok(msg) => match msg {
                        // Alg 1, Line 8
                        Message::REQUEST {
                            vertex,
                            distance,
                            worker_id,
                            weight,
                        } => self.process_request(&vertex, worker_id, weight, distance),
                        // Alg 1, Line 9
                        Message::ANSWER {
                            vertex,
                            assignment,
                            weight,
                        } => self.process_answer(&vertex, assignment, weight),
                    },
                    Err(err) => panic!("Receiving from message channel failed with: {}", err),
                }
            } else if !hyper_rx.is_empty() {
                self.counter = 0;
                match hyper_rx.recv() {
                    // Alg 1, Line 6
                    Ok((edge, weight)) => {
                        let _guard = span!(
                            Level::TRACE,
                            "worker receive hyper-edge",
                            worker_id = self.id,
                            ?edge,
                            ?weight
                        );
                        self.process_hyper_edge(edge, weight)
                    }
                    Err(err) => panic!(
                        "Receiving from hyper edge waiting channel failed with: {}",
                        err
                    ),
                }
            } else if !negation_rx.is_empty() {
                match negation_rx.recv() {
                    Ok((edge, weight)) => {
                        let _guard = span!(
                            Level::TRACE,
                            "worker receive negation-edge",
                            worker_id = self.id,
                            ?edge,
                            ?weight
                        );
                        match self.assignment.get(&edge.target) {
                            None => self.process_negation_edge(edge.clone(), weight),
                            // Alg 1, Line 7
                            Some(assignment) => match assignment {
                                VertexAssignment::FALSE => {
                                    self.counter = 0;
                                    self.process_negation_edge(edge.clone(), weight)
                                }
                                VertexAssignment::TRUE => {
                                    self.counter = 0;
                                    self.process_negation_edge(edge.clone(), weight)
                                }
                                // In case of undecided the edge is only processed after 10 iterations
                                VertexAssignment::UNDECIDED => {
                                    if self.counter == 9 {
                                        self.counter = 0;
                                        self.process_negation_edge(edge.clone(), weight)
                                    } else {
                                        trace!(?edge, ?weight, "queueing negation");
                                        self.broker.queue_negation(self.id, edge.clone(), weight);
                                        self.counter += 1;
                                    }
                                }
                            },
                        }
                    }
                    Err(err) => panic!(
                        "Receiving from negation edge waiting channel failed with: {}",
                        err
                    ),
                }
            } else {
                let _guard = span!(Level::TRACE, "worker release negation", worker_id = self.id);
                // if the negation channel is empty, unsafe negation edges are released
                self.release_negations(self.unsafe_edges.len());
            }
        }
    }

    /// Releasing the edges from the unsafe queue to the safe negation channel
    fn release_negations(&mut self, dist: usize) {
        trace!(distance = dist, "release negation");

        while self.unsafe_edges.len() >= dist && !self.unsafe_edges.is_empty() {
            if let Some(edges) = self.unsafe_edges.last_mut() {
                // Queue all edges in the negation channel that have the given distance
                while !edges.is_empty() {
                    let (edge, weight) = edges.pop().unwrap();
                    trace!(?edge, ?weight, "release negation edge");
                    self.broker.queue_negation(self.id, edge, weight);
                }
            }
            self.unsafe_edges.pop();
        }
    }
    /// Adds worker to C^i(vertex)
    fn mark_interest(&mut self, vertex: &V, worker: WorkerId) {
        if let Some(set) = self.interests.get_mut(vertex) {
            trace!(is_initialized = true, ?vertex, "mark vertex interest");
            set.insert(worker);
        } else {
            trace!(is_initialized = false, ?vertex, "mark vertex interest");
            let mut set = HashSet::new();
            set.insert(worker);
            self.interests.insert(vertex.clone(), set);
        }
    }

    fn explore(&mut self, vertex: &V, weight: Weight) {
        trace!(?vertex, ?weight, "exploring vertex");
        let mut weight = weight;
        // Line 2
        self.assignment
            .insert(vertex.clone(), VertexAssignment::UNDECIDED);

        // Line 3
        if self.is_owner(vertex) {
            let successors = self.succ(vertex); // Line 4
            if successors.is_empty() {
                // Line 4
                self.final_assign(vertex, VertexAssignment::FALSE, weight);
            } else {
                // Line 5
                for (i, edge) in successors.iter().enumerate() {
                    let split_weight = if i < successors.len() - 1 {
                        let (weight_for_task, remaining_weight) = weight
                            .split()
                            .unwrap_or_else(|_| panic!("Worker {} ran out of weight", self.id));
                        weight = remaining_weight;
                        weight_for_task
                    } else {
                        weight.clone()
                    };
                    match edge {
                        Edges::HYPER(edge) => {
                            self.broker.queue_hyper(self.id, edge.clone(), split_weight)
                        }
                        Edges::NEGATION(edge) => self.queue_negation(edge.clone(), split_weight),
                    }
                }
            }
        } else {
            // Line 7
            self.broker.send(
                self.vertex_owner(vertex),
                Message::REQUEST {
                    vertex: vertex.clone(),
                    distance: *self.distances.get(vertex).unwrap_or(&0),
                    worker_id: self.id,
                    weight,
                },
            )
        }
    }

    fn process_hyper_edge(&mut self, edge: HyperEdge<V>, weight: Weight) {
        trace!(?edge, ?weight, "processing hyper-edge");
        let mut weight = weight;
        // Line 3, condition (in case of targets is empty, the default value is true)
        let all_final = edge.targets.iter().all(|target| {
            self.assignment
                .get(target)
                .map_or(false, |f| matches!(f, VertexAssignment::TRUE))
        });

        // Line 3
        if all_final {
            self.final_assign(&edge.source, VertexAssignment::TRUE, weight);
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
            self.delete_edge(Edges::HYPER(edge), weight);
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
                    let (weight_for_task, remaining_weight) = weight
                        .split()
                        .unwrap_or_else(|_| panic!("Worker {} ran out of weight", self.id));
                    weight = remaining_weight;
                    // Line 7
                    self.add_depend(target, Edges::HYPER(edge.clone()));
                    // Line 8
                    self.explore(target, weight_for_task);
                }
                _ => {}
            }
        }
        // We don't know how many instances of `None` exists without iterating through the loop, so there will always be some left over weight
        self.broker.return_weight(weight);
    }

    // Mark `dependency` as a prerequisite for finding the final assignment of `vertex`
    fn add_depend(&mut self, vertex: &V, dependency: Edges<V>) {
        // Update the distance
        let default = 0;
        let tdist = *self.distances.get(vertex).unwrap_or(&default);
        match dependency.clone() {
            Edges::NEGATION(edge) => {
                let sdist = self.distances.get(&edge.source).unwrap_or(&default) + 1;
                self.distances.insert(vertex.clone(), max(sdist, tdist));
            }
            Edges::HYPER(edge) => {
                let sdist = *self.distances.get(&edge.source).unwrap_or(&default);
                self.distances.insert(vertex.clone(), max(sdist, tdist));
            }
        }

        // Mark `dependency` as a prerequisite for finding the final assignment of `vertex`
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

    fn process_negation_edge(&mut self, edge: NegationEdge<V>, weight: Weight) {
        match self.assignment.get(&edge.target) {
            // UNEXPLORED
            None => {
                // UNEXPLORED
                // Line 6
                trace!(
                    ?edge,
                    ?weight,
                    assignment = "UNEXPLORED",
                    "processing negation edge"
                );
                self.add_depend(&edge.target, Edges::NEGATION(edge.clone()));
                let (weight1, weight2) = weight
                    .split()
                    .unwrap_or_else(|_| panic!("Worker {} ran out of weight", self.id));
                self.queue_negation(edge.clone(), weight1);
                self.explore(&edge.target, weight2);
            }
            Some(assignment) => match assignment {
                VertexAssignment::UNDECIDED => {
                    trace!(?edge, ?weight, assignment = ?VertexAssignment::UNDECIDED, "processing negation edge");
                    self.final_assign(&edge.source, VertexAssignment::TRUE, weight)
                }
                VertexAssignment::FALSE => {
                    trace!(?edge, ?weight, assignment = ?VertexAssignment::FALSE, "processing negation edge");
                    self.final_assign(&edge.source, VertexAssignment::TRUE, weight)
                }
                VertexAssignment::TRUE => {
                    trace!(?edge, ?weight, assignment = ?VertexAssignment::TRUE, "processing negation edge");
                    self.delete_edge(Edges::NEGATION(edge), weight)
                }
            },
        }
    }

    /// Queueing unsafe negation, which will be queued to negation channel whenever
    /// release negation is called
    fn queue_negation(&mut self, edge: NegationEdge<V>, weight: Weight) {
        let len = self.unsafe_edges.len();
        let mut dist: usize = 0;
        if let Some(n) = self.distances.get(&edge.source) {
            dist = *n as usize;
        }

        if len <= dist {
            for _ in len..(dist + 1) {
                self.unsafe_edges.push(Vec::new());
            }
        }

        self.unsafe_edges
            .get_mut(dist as usize)
            .unwrap()
            .push((edge, weight));
    }

    // Another worker has requested the final assignment of a `vertex`
    fn process_request(&mut self, vertex: &V, requester: WorkerId, weight: Weight, distance: u32) {
        let mut weight = weight;
        trace!(
            ?vertex,
            ?requester,
            ?weight,
            distance,
            "got request for vertex assignment"
        );
        if let Some(assigned) = self.assignment.get(&vertex) {
            // Final assignment of `vertex` is already known, reply immediately
            match assigned {
                VertexAssignment::FALSE => {
                    return self.broker.send(
                        requester,
                        Message::ANSWER {
                            vertex: vertex.clone(),
                            assignment: VertexAssignment::FALSE,
                            weight,
                        },
                    );
                }
                VertexAssignment::TRUE => {
                    return self.broker.send(
                        requester,
                        Message::ANSWER {
                            vertex: vertex.clone(),
                            assignment: VertexAssignment::TRUE,
                            weight,
                        },
                    );
                }
                _ => {
                    // update distance
                    let dist = *self.distances.get(vertex).unwrap_or(&0);
                    self.distances.insert(vertex.clone(), max(dist, distance));

                    self.mark_interest(vertex, requester);

                    if self.depends.contains_key(vertex) {
                        if let Some(assignment) = self.assignment.get(vertex) {
                            if let VertexAssignment::UNDECIDED = assignment {
                                let (weight_for_task, remaining_weight) =
                                    weight.split().unwrap_or_else(|_| {
                                        panic!("Worker {} ran out of weight", self.id)
                                    });
                                weight = remaining_weight;
                                self.explore(vertex, weight_for_task)
                            }
                        }
                    }
                }
            }
        }
        // Final assignment of `vertex` is not yet known
        self.mark_interest(vertex, requester);
        if self.assignment.get(&vertex).is_none() {
            // UNEXPLORED
            self.explore(&vertex, weight);
        } else {
            self.broker.return_weight(weight);
        }
    }

    fn process_answer(&mut self, vertex: &V, assigned: VertexAssignment, weight: Weight) {
        trace!(?vertex, ?assigned, ?weight, "received final assignment");
        self.final_assign(vertex, assigned, weight);
    }

    fn final_assign(&mut self, vertex: &V, assignment: VertexAssignment, weight: Weight) {
        let mut weight = weight;
        // Line 2
        if *vertex == self.v0 {
            self.broker.terminate(assignment);
            // Don't bother returning the weight, this is early termination
            return;
        }
        // Line 3
        let prev_assignment = self.assignment.insert(vertex.clone(), assignment);
        let changed_assignment = prev_assignment != Some(assignment);
        debug!(
            ?prev_assignment,
            new_assignment = ?assignment,
            ?vertex,
            final_again =
                !(prev_assignment == Some(VertexAssignment::UNDECIDED) || prev_assignment == None),
            changed_assignment,
            "final assigned"
        );

        if changed_assignment {
            // Line 4
            if let Some(interested) = self.interests.get(&vertex) {
                for worker_id in interested {
                    let (new_weight, split_weight) = weight
                        .split()
                        .unwrap_or_else(|_| panic!("Ran out of weight on worker {}", self.id));
                    weight = new_weight;
                    self.broker.send(
                        *worker_id,
                        Message::ANSWER {
                            vertex: vertex.clone(),
                            assignment,
                            weight: split_weight,
                        },
                    )
                }
            }

            // Line 5
            if let Some(depends) = self.depends.get(&vertex) {
                for edge in depends.clone() {
                    let (new_weight, split_weight) = weight
                        .split()
                        .unwrap_or_else(|_| panic!("Ran out of weight on worker {}", self.id));
                    weight = new_weight;
                    trace!(
                        ?edge,
                        ?weight,
                        "requeueing edg because edge have received final assignment"
                    );
                    match edge {
                        Edges::HYPER(edge) => {
                            self.broker.queue_hyper(self.id, edge.clone(), split_weight)
                        }
                        Edges::NEGATION(edge) => self.queue_negation(edge.clone(), split_weight),
                    }
                }
            }
        }

        trace!(?weight, "returning remaining weight to controller");
        self.broker.return_weight(weight);
    }

    /// Helper function for deleting edges from a vertex.
    fn delete_edge(&mut self, edge: Edges<V>, weight: Weight) {
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
            trace!(?successors, remove_edge = ?edge, "initializing successors hashmap in edg::delete_edge");
            successors.remove(&edge);
            self.successors.insert(source.clone(), successors);
        }

        match self.successors.get(&source) {
            None => panic!("successors should have been filled, or at least have a empty vector"),
            Some(successors) => {
                // Line 3
                if successors.is_empty() {
                    trace!(
                        ?source,
                        assignment = ?VertexAssignment::FALSE,
                        ?weight,
                        "no more successors, final assignment is FALSE"
                    );
                    self.final_assign(&source, VertexAssignment::FALSE, weight);
                } else {
                    self.broker.return_weight(weight);
                }
            }
        }

        match edge {
            // Line 4-6
            Edges::HYPER(ref edge) => {
                debug!(source = ?edge, targets = ?edge.targets, "remove hyper-edge as dependency");
                for target in &edge.targets {
                    self.remove_depend(target, Edges::HYPER(edge.clone()))
                }
            }
            // Line 7-8
            Edges::NEGATION(ref edge) => {
                debug!(source = ?edge, target = ?edge.target, "remove negation-edge as dependency");
                self.remove_depend(&edge.target, Edges::NEGATION(edge.clone()))
            }
        }
    }

    /// Wraps the ExtendedDependencyGraph::succ(v) with caching allowing edges to be deleted.
    /// See documentation for the `successors` field.
    fn succ(&mut self, vertex: &V) -> HashSet<Edges<V>> {
        if let Some(successors) = self.successors.get(vertex) {
            debug!(?vertex, ?successors, known_vertex = true, "edg::succ");
            // List of successors is already allocated for the vertex
            successors.clone()
        } else {
            // Setup the successors list the first time it is requested
            let successors = self.edg.succ(vertex);
            self.successors.insert(vertex.clone(), successors.clone());
            debug!(
                ?vertex,
                ?successors,
                known_vertex = false,
                "loaded successors from EDG"
            );
            successors
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::hash_map::RandomState;
    use std::collections::HashSet;
    use std::fmt::Display;
    use test_env_log::test;

    use core::fmt::Formatter;

    use crate::common::{Edges, HyperEdge, NegationEdge, VertexAssignment};
    use crate::edg::{distributed_certain_zero, ExtendedDependencyGraph, Vertex};

    #[test]
    fn test_dcz_empty_hyper_edge() {
        simple_edg![
            A => -> {};
        ];
        edg_assert!(A, TRUE);
    }

    #[test]
    fn test_dcz_no_successors() {
        simple_edg![
            A => ;
        ];
        edg_assert!(A, FALSE);
    }

    #[test]
    fn test_dcz_general_01() {
        simple_edg![
            A => -> {B, C} -> {D};
            B => ;
            C => .> D;
            D => -> {};
        ];
        edg_assert!(A, TRUE);
        edg_assert!(B, FALSE);
        edg_assert!(C, FALSE);
        edg_assert!(D, TRUE);
    }

    #[test]
    fn test_dcz_general_02() {
        simple_edg![
            A => -> {B, C};
            B => .> E;
            C => -> {};
            D => -> {} -> {C};
            E => .> D;
        ];
        edg_assert!(A, TRUE);
        edg_assert!(B, TRUE);
        edg_assert!(C, TRUE);
        edg_assert!(D, TRUE);
        edg_assert!(E, FALSE);
    }

    #[test]
    #[ignore]
    fn test_dcz_general_03() {
        simple_edg![
            A => -> {B} -> {E};
            B => -> {C};
            C => -> {F} -> {H};
            D => -> {E} -> {C};
            E => -> {D, F};
            F => -> {};
            G => .> A;
            H => -> {I};
            I => ;
        ];
        edg_assert!(A, TRUE);
        edg_assert!(B, TRUE);
        edg_assert!(C, TRUE);
        edg_assert!(D, TRUE);
        edg_assert!(E, TRUE);
        edg_assert!(F, TRUE);
        edg_assert!(G, FALSE);
        edg_assert!(H, FALSE);
        edg_assert!(I, FALSE);
    }

    #[test]
    fn test_dcz_general_04() {
        simple_edg![
            A => -> {B} -> {C};
            B => -> {D};
            C => ;
            D => -> {};
        ];
        edg_assert!(A, TRUE);
        edg_assert!(B, TRUE);
        edg_assert!(C, FALSE);
        edg_assert!(D, TRUE);
    }

    #[test]
    fn test_dcz_general_05() {
        simple_edg![
            A => -> {B};
            B => -> {C};
            C => -> {B};
        ];
        edg_assert!(A, FALSE);
        edg_assert!(B, FALSE);
        edg_assert!(C, FALSE);
    }

    #[test]
    fn test_dcz_general_06() {
        simple_edg![
            A => -> {B} -> {C};
            B => ;
            C => ;
        ];
        edg_assert!(A, FALSE);
        edg_assert!(B, FALSE);
        edg_assert!(C, FALSE);
    }

    #[test]
    fn test_dcz_general_07() {
        simple_edg![
            A => -> {B};
            B => -> {A, C};
            C => -> {D};
            D => -> {};
        ];
        edg_assert!(A, FALSE);
        edg_assert!(B, FALSE);
        edg_assert!(C, TRUE);
        edg_assert!(D, TRUE);
    }

    #[test]
    fn test_dcz_general_08() {
        simple_edg![
            A => -> {B, C};
            B => -> {C} -> {D};
            C => -> {B};
            D => -> {C} -> {};
        ];
        edg_assert!(A, TRUE);
        edg_assert!(B, TRUE);
        edg_assert!(C, TRUE);
        edg_assert!(D, TRUE);
    }

    #[test]
    #[ignore]
    fn test_dcz_negation_01() {
        simple_edg![
            A => .> B;
            B => -> {};
        ];
        edg_assert!(A, FALSE);
        edg_assert!(B, TRUE);
    }

    #[test]
    #[ignore]
    fn test_dcz_negation_02() {
        simple_edg![
            A => .> B;
            B => -> {C};
            C => -> {B} .> D;
            D => -> {E};
            E => -> {D};
        ];
        edg_assert!(A, FALSE);
        edg_assert!(B, TRUE);
        edg_assert!(C, TRUE);
        edg_assert!(D, FALSE);
        edg_assert!(E, FALSE);
    }

    #[test]
    fn test_dcz_negation_03() {
        simple_edg![
            A => .> B .> C;
            B => .> D;
            C => -> {D};
            D => ;
        ];
        edg_assert!(A, TRUE);
        edg_assert!(B, TRUE);
        edg_assert!(C, FALSE);
        edg_assert!(D, FALSE);
    }

    #[test]
    fn test_dcz_negation_04() {
        simple_edg![
            A => .> B;
            B => -> {B};
        ];
        edg_assert!(A, TRUE);
        edg_assert!(B, FALSE);
    }
}
