use crossbeam_channel::{Receiver, TryRecvError};
use std::collections::{HashMap, HashSet};
use std::thread;
use crate::common::{Edges, WorkerId, Message, HyperEdge, VertexAssignment, NegationEdge};
use crate::com::{ChannelBroker, Broker};
use std::sync::Arc;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

const WORKER_COUNT: u64 = 4;

pub trait ExtendedDependencyGraph<V: Hash + Eq + PartialEq + Clone> {
    fn succ(&self, vert: &V) -> HashSet<Edges<V>>;
}

pub fn distributed_certain_zero<G: ExtendedDependencyGraph<V> + Send + Sync + Clone + 'static, V: Hash + Eq + PartialEq + Clone + Send + Sync + 'static>(edg: G, v0: V) {
    let (broker, mut msg_rxs, mut term_rxs) = ChannelBroker::new(WORKER_COUNT);
    let broker = Arc::new(broker);

    for i in (0..WORKER_COUNT).rev() {
        let msg_rx = msg_rxs.pop().unwrap();
        let term_rx = term_rxs.pop().unwrap();
        let mut worker = Worker::new(
            i,
            v0.clone(),
            msg_rx,
            term_rx,
            broker.clone(),
            edg.clone(),
        );
        thread::spawn(move || worker.run());
    }
}

struct Worker<B: Broker<V>, G: ExtendedDependencyGraph<V>, V: Hash + Eq + PartialEq + Clone> {
    id: WorkerId,
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

fn vertex_owner<V: Hash + Eq + PartialEq + Clone>(vertex: V) -> WorkerId {
    let mut hasher = DefaultHasher::new();
    vertex.hash::<DefaultHasher>(&mut hasher);
    hasher.finish() % WORKER_COUNT
}

impl<B: Broker<V>, G: ExtendedDependencyGraph<V> + Send + Sync, V: Hash + Eq + PartialEq + Clone> Worker<B, G, V> {
    #[inline]
    fn is_owner(&self, vertex: &V) -> bool {
        vertex_owner(vertex) == self.id
    }

    pub fn new(
        id: WorkerId,
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
        if self.is_owner(&self.v0.clone()) { // Alg 1, Line 2
            self.explore(&self.v0.clone());
        }

        let term_rx = self.term_rx.clone();
        let msg_rx = self.msg_rx.clone();

        let mut select = Select::new();
        let oper_term = select.recv(&term_rx);
        let oper_msg = select.recv(&msg_rx);

        loop {
            let oper = select.select();
            match oper.index() {
                i if i == oper_term => {
                    match oper.recv(&term_rx) { // Alg 1, Line 10
                        Ok(assignment) => return match assignment { // Alg 1, Line 11-12
                            VertexAssignment::UNDECIDED => VertexAssignment::FALSE,
                            VertexAssignment::FALSE => VertexAssignment::FALSE,
                            VertexAssignment::TRUE => VertexAssignment::TRUE,
                        },
                        Err(err) => panic!("Receiving from termination channel failed with: {}", err),
                    }
                },
                i if i == oper_msg => {
                    match oper.recv(&msg_rx) { // Alg 1, Line 5-9
                        Ok(msg) => match msg {
                            Message::HYPER(edge) => self.process_hyper_edge(edge), // Alg 1, Line 6
                            Message::NEGATION(edge) => self.process_negation_edge(edge), // Alg 1, Line 7
                            Message::REQUEST { vertex, worker_id } => self.process_request(&vertex, worker_id), // Alg 1, Line 8
                            Message::ANSWER { vertex, assignment } => self.process_answer(&vertex, assignment) // Alg 1, Line 9
                        }
                        Err(err) => panic!("Receiving from message channel failed with: {}", err),
                    }
                },
                _ => unreachable!(),
            }
        }
    }

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
        self.assignment.insert(vertex.clone(), VertexAssignment::UNDECIDED);

        if self.is_owner(vertex) {
            let successors = self.succ(vertex);
            if successors.is_empty() {
                self.final_assign(vertex, VertexAssignment::FALSE);
            } else {
                for edge in successors {
                    match edge {
                        Edges::HYPER(edge) => self.broker.send(self.id, Message::HYPER(edge.clone())),
                        Edges::NEGATION(edge) => self.broker.send(self.id, Message::NEGATION(edge.clone())),
                    }
                }
            }
        } else {
            self.broker.send(
                vertex_owner(vertex),
                Message::REQUEST { vertex: vertex.clone(), worker_id: self.id }
            )
        }
    }

    fn process_hyper_edge(&mut self, edge: HyperEdge<V>) {
        let all_final = edge.targets
            .iter()
            .all(|target| {
                if let Some(f) = self.assignment.get(target) {
                    matches!(f, VertexAssignment::TRUE)
                } else {
                    false
                }
            });

        if all_final {
            self.final_assign(&edge.source, VertexAssignment::TRUE);
            return;
        }

        let any_target = edge.targets
            .iter()
            .any(|target| {
                if let Some(f) = self.assignment.get(target) {
                    matches!(f, VertexAssignment::FALSE)
                } else {
                    false
                }
            });

        if any_target {
            self.delete_edge(Edges::HYPER(edge));
            return;
        }

        for target in &edge.targets { // Line 5-8
            match self.assignment.get(&target) { // Condition from line 5
                None => {},
                Some(f) => match f {
                    VertexAssignment::UNDECIDED => {},
                    _ => continue,
                }
            }

            // Line 8, target = u
            self.add_depend(target, Edges::HYPER(edge.clone()));
            match self.assignment.get(target) {
                None => self.explore(target),
                Some(_) => {}
            }
        }
    }

    fn add_depend(&mut self, vertex: &V, depended: Edges<V>) {
        if let Some(dependens) = self.depends.get_mut(vertex) {
            dependens.insert(depended);
        } else {
            let mut dependens = HashSet::new();
            dependens.insert(depended);
            self.depends.insert(vertex.clone(), dependens);
        }
    }

    fn remove_depend(&mut self, vertex: &V, depended: Edges<V>) {
        if let Some(dependens) = self.depends.get_mut(vertex) {
            dependens.remove(&depended);
        }
    }

    fn process_negation_edge(&mut self, edge: NegationEdge<V>) {
        match self.assignment.get(&edge.target) { // Line 3
            None => {
                self.add_depend(&edge.target, Edges::NEGATION(edge.clone()));
                self.broker.send(self.id, Message::NEGATION(edge.clone()));
                self.explore(&edge.target);
            },
            Some(assignment) => match assignment {
                VertexAssignment::UNDECIDED => self.final_assign(&edge.source, VertexAssignment::TRUE),
                VertexAssignment::FALSE => self.final_assign(&edge.source, VertexAssignment::TRUE),
                VertexAssignment::TRUE => self.delete_edge(Edges::NEGATION(edge)),
            },
        }
    }

    fn process_request(&mut self, vertex: &V, requester: WorkerId) {
        if let Some(assigned) = self.assignment.get(&vertex) {
            match assigned {
                VertexAssignment::FALSE => self.broker.send(requester, Message::ANSWER { vertex: vertex.clone(), assignment: VertexAssignment::FALSE }),
                VertexAssignment::TRUE => self.broker.send(requester, Message::ANSWER { vertex: vertex.clone(), assignment: VertexAssignment::TRUE }),
                _ => {}
            }
        } else {
            self.mark_interest(vertex, requester);
            self.explore(&vertex);
        }
    }

    fn process_answer(&mut self, vertex: &V, assigned: VertexAssignment) {
        self.final_assign(vertex, assigned)
    }

    fn final_assign(&mut self, vertex: &V, assignment: VertexAssignment) {
        if *vertex == self.v0 { // Line 2
            self.broker.terminate(assignment);
            return
        }
        self.assignment.insert(vertex.clone(), assignment); // Line 2
        match self.interests.get(&vertex) { // Line 3
            None => {}
            Some(interested) => {
                interested.iter()
                    .for_each(|worker_id| {
                        self.broker.send(
                            *worker_id,
                            Message::ANSWER {
                                vertex: vertex.clone(),
                                assignment,
                            }
                        )
                    })
            }
        }

        if let Some(depends) = self.depends.get(&vertex) { // Line 4
            depends.iter()
                .for_each(|edge| {
                    match edge {
                        Edges::HYPER(edge) => self.broker.send(self.id, Message::<V>::HYPER(edge.clone())),
                        Edges::NEGATION(edge) => self.broker.send(self.id, Message::<V>::NEGATION(edge.clone())),
                    }
                });
        }
    }

    fn delete_edge(&mut self, edge: Edges<V>) {
        let source = match edge { // Get v
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

    fn succ(&mut self, vertex: &V) -> HashSet<Edges<V>> {
        if let Some(successors) = self.successors.get(vertex) {
            successors.clone()
        } else {
            let successors = self.edg.succ(vertex);
            self.successors.insert(vertex.clone(), successors.clone());
            successors
        }
    }
}