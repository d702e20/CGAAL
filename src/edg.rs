use crossbeam_channel::{Receiver, TryRecvError};
use std::collections::{HashMap, HashSet};
use std::thread;
use crate::common::{Edges, WorkerId, Vertex, Message, HyperEdge, VertexAssignment, NegationEdge};
use crate::com::{ChannelBroker, Broker};
use std::sync::Arc;

const WORKER_COUNT: i32 = 4;

pub trait ExtendedDependencyGraph {
    fn succ(&self, vert: Vertex) -> HashSet<Edges>;
}

// TODO replace static life with a lifetime matching the duration of the computation
pub fn distributed_certain_zero<G: ExtendedDependencyGraph + Send + Sync + 'static>(edg: G, v0: Vertex) {
    let (broker, mut msg_rxs, mut term_rxs) = ChannelBroker::new(WORKER_COUNT);
    let broker = Arc::new(broker);
    let edg = Arc::new(edg);

    for i in WORKER_COUNT..0 {
        let worker_id = i.clone();
        let msg_rx = msg_rxs.pop().unwrap();
        let term_rx = term_rxs.pop().unwrap();
        let broker = broker.clone();
        let edg = edg.clone();
        thread::spawn(move || {
            let mut worker = Worker::new(
                worker_id,
                v0,
                msg_rx,
                term_rx,
                broker,
                edg,
            );
            worker.run(v0);
        });
    }
}

type Assignment = HashMap<Vertex, VertexAssignment>;

struct Worker<B: Broker, G: ExtendedDependencyGraph> {
    id: WorkerId,
    v0: Vertex,
    assignment: Assignment,
    depends: HashMap<Vertex, HashSet<Edges>>,
    interests: HashMap<Vertex, HashSet<WorkerId>>,
    msg_rx: Receiver<Message>,
    term_rx: Receiver<()>,
    broker: Arc<B>,
    successors: HashMap<Vertex, HashSet<Edges>>,
    edg: Arc<G>,
}

fn vertex_owner(vertex: Vertex) -> WorkerId {
    vertex % WORKER_COUNT
}

impl<B: Broker, G: ExtendedDependencyGraph + Send + Sync> Worker<B, G> {
    #[inline]
    fn is_owner(&self, vertex: Vertex) -> bool {
        vertex_owner(vertex) == self.id
    }

    pub fn new(
        id: WorkerId,
        v0: Vertex,
        msg_rx: Receiver<Message>,
        term_rx: Receiver<()>,
        broker: Arc<B>,
        edg: Arc<G>,
    ) -> Self {
        let assignment: Assignment = HashMap::new();
        let interests = HashMap::<Vertex, HashSet<WorkerId>>::new();
        let depends = HashMap::<Vertex, HashSet<Edges>>::new();
        let successors = HashMap::<Vertex, HashSet<Edges>>::new();

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

    pub fn run(&mut self, v0: Vertex) -> VertexAssignment {
        if self.is_owner(v0) {
            self.explore(v0);
        }

        loop {
            // TODO use select! from crossbeam and remove Message::TERMINATE
            // Check for termination signal
            match self.term_rx.try_recv() {
                Ok(_) => break,
                Err(err) => match err {
                    TryRecvError::Empty => {} // Continue operating
                    TryRecvError::Disconnected => {
                        panic!("Termination channel disconnected unexpectedly")
                    }
                },
            }

            // Receive work
            let msg = self.msg_rx
                .recv()
                .expect("Message channel disconnected unexpectedly");
            match msg {
                Message::HYPER(edge) => self.process_hyper_edge(edge),
                Message::NEGATION(edge) => self.process_negation_edge(edge),
                Message::REQUEST { vertex, worker_id } => self.process_request(vertex, worker_id),
                Message::ANSWER { vertex, assignment } => self.process_answer(vertex, assignment),
                Message::TERMINATE => break,
            }
        }

        return match self.assignment.get(&v0) {
            None => panic!("v0 never received an assignment"),
            Some(assignment) => match assignment {
                VertexAssignment::UNDECIDED => VertexAssignment::FALSE,
                VertexAssignment::FALSE => VertexAssignment::FALSE,
                VertexAssignment::TRUE => VertexAssignment::TRUE,
            }
        }
    }

    fn mark_interest(&mut self, vertex: Vertex, worker: WorkerId) {
        if let Some(set) = self.interests.get_mut(&vertex) {
            set.insert(worker);
        } else {
            let mut set = HashSet::new();
            set.insert(worker);
            self.interests.insert(vertex, set);
        }
    }

    fn explore(&mut self, vertex: Vertex) {
        self.assignment.insert(vertex, VertexAssignment::UNDECIDED);

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
                Message::REQUEST { vertex, worker_id: self.id }
            )
        }
    }

    fn process_hyper_edge(&mut self, edge: HyperEdge) {
        let all_final = edge.targets
            .iter()
            .all(|target| {
                if let Some(f) = self.assignment.get(target) {
                    match f {
                        VertexAssignment::TRUE => true,
                        _ => false,
                    }
                } else {
                    false
                }
            });

        if all_final {
            self.final_assign(edge.source, VertexAssignment::TRUE);
            return;
        }

        let any_target = edge.targets
            .iter()
            .any(|target| {
                if let Some(f) = self.assignment.get(target) {
                    match f {
                        VertexAssignment::FALSE => true,
                        _ => false,
                    }
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
            self.add_depend(*target, Edges::HYPER(edge.clone()));
            match self.assignment.get(&target) {
                None => self.explore(target.clone()),
                Some(_) => {}
            }
        }
    }

    fn add_depend(&mut self, vertex: Vertex, depended: Edges) {
        if let Some(dependens) = self.depends.get_mut(&vertex) {
            dependens.insert(depended);
        } else {
            let mut dependens = HashSet::new();
            dependens.insert(depended);
            self.depends.insert(vertex, dependens);
        }
    }

    fn remove_depend(&mut self, vertex: Vertex, depended: Edges) {
        if let Some(dependens) = self.depends.get_mut(&vertex) {
            dependens.remove(&depended);
        }
    }

    fn process_negation_edge(&mut self, edge: NegationEdge) {
        match self.assignment.get(&edge.target) { // Line 3
            None => {
                self.add_depend(edge.target, Edges::NEGATION(edge.clone()));
                self.broker.send(self.id, Message::NEGATION(edge.clone()));
                self.explore(edge.target);
            },
            Some(assignment) => match assignment {
                VertexAssignment::UNDECIDED => self.final_assign(edge.source, VertexAssignment::TRUE),
                VertexAssignment::FALSE => self.final_assign(edge.source, VertexAssignment::TRUE),
                VertexAssignment::TRUE => self.delete_edge(Edges::NEGATION(edge)),
            },
        }
    }

    fn process_request(&mut self, vertex: Vertex, requester: WorkerId) {
        if let Some(assigned) = self.assignment.get(&vertex) {
            match assigned {
                VertexAssignment::FALSE => self.broker.send(requester, Message::ANSWER { vertex, assignment: VertexAssignment::FALSE }),
                VertexAssignment::TRUE => self.broker.send(requester, Message::ANSWER { vertex, assignment: VertexAssignment::TRUE }),
                _ => {}
            }
        } else {
            self.mark_interest(vertex, requester);
            self.explore(vertex);
        }
    }

    fn process_answer(&mut self, vertex: Vertex, assigned: VertexAssignment) {
        self.final_assign(vertex, assigned)
    }

    fn final_assign(&mut self, vertex: Vertex, assignment: VertexAssignment) {
        if vertex == self.v0 { // Line 2
            todo!("Announce result, and terminate all workers")
        }
        self.assignment.insert(vertex, assignment); // Line 2
        match self.interests.get(&vertex) { // Line 3
            None => {}
            Some(interested) => {
                interested.iter()
                    .for_each(|worker_id| {
                        self.broker.send(
                            *worker_id,
                            Message::ANSWER {
                                vertex,
                                assignment: assignment.clone()
                            }
                        )
                    })
            }
        }

        if let Some(depends) = self.depends.get(&vertex) { // Line 4
            depends.iter()
                .for_each(|edge| {
                    match edge {
                        Edges::HYPER(edge) => self.broker.send(self.id, Message::HYPER(edge.clone())),
                        Edges::NEGATION(edge) => self.broker.send(self.id, Message::NEGATION(edge.clone())),
                    }
                });
        }
    }

    fn delete_edge(&mut self, edge: Edges) {
        let source = match edge { // Get v
            Edges::HYPER(ref edge) => edge.source,
            Edges::NEGATION(ref edge) => edge.source,
        };

        if let Some(successors) = self.successors.get_mut(&source) {
            successors.remove(&edge);
        } else {
            let mut successors = self.edg.succ(source);
            successors.remove(&edge);
            self.successors.insert(source, successors);
        }

        match self.successors.get(&source) {
            None => panic!("successors should have been filled, or at least have a empty vector"),
            Some(successors) => {
                if successors.is_empty() {
                    self.final_assign(source, VertexAssignment::FALSE);
                }
            }
        }

        match edge {
            Edges::HYPER(ref edge) => {
                for target in &edge.targets {
                    self.remove_depend(*target, Edges::HYPER(edge.clone()))
                }
            }
            Edges::NEGATION(ref edge) => {
                self.remove_depend(edge.target, Edges::NEGATION(edge.clone()))
            }
        }
    }

    fn succ(&mut self, vertex: Vertex) -> HashSet<Edges> {
        if let Some(successors) = self.successors.get(&vertex) {
            successors.clone()
        } else {
            let successors = self.edg.succ(vertex);
            self.successors.insert(vertex, successors.clone());
            successors
        }
    }
}