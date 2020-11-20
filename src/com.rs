use crate::common::{HyperEdge, Message, NegationEdge, VertexAssignment, WorkerId};
use crossbeam_channel::{unbounded, Receiver, Sender};
use std::hash::Hash;

/// Broker implement the function of W_E, W_N, M_R and M_A
pub trait Broker<V: Hash + Eq + PartialEq + Clone> {
    /// Send message to worker with id `to`
    fn send(&self, to: WorkerId, msg: Message<V>);

    /// Send hyper edge for queuing to worker with id `to`
    fn queue_hyper(&self, to: WorkerId, edge: HyperEdge<V>);

    /// Send negation edge for queuing to worker with id `to`
    fn queue_negation(&self, to: WorkerId, edge: NegationEdge<V>);

    /// Signal to all workers to terminate because a result has been found
    fn terminate(&self, assignment: VertexAssignment);
}

/// Implements Broker using channels from crossbeam_channel
#[derive(Debug)]
pub struct ChannelBroker<V: Hash + Eq + PartialEq + Clone> {
    workers: Vec<Sender<Message<V>>>,
    hyper_chans: Vec<Sender<HyperEdge<V>>>,
    negation_chans: Vec<Sender<NegationEdge<V>>>,
    term_chans: Vec<Sender<VertexAssignment>>,
}

impl<V: Hash + Eq + PartialEq + Clone> Broker<V> for ChannelBroker<V> {
    fn send(&self, to: WorkerId, msg: Message<V>) {
        self.workers
            .get(to as usize)
            .expect("receiver id out of bounds")
            .send(msg)
            .expect(&*format!("Send to worker {} failed", to));
    }

    fn queue_hyper(&self, to: WorkerId, edge: HyperEdge<V>) {
        self.hyper_chans
            .get(to as usize)
            .expect("receiver id out of bounds")
            .send(edge)
            .expect(&*format!("Send to worker {} failed", to));
    }

    fn queue_negation(&self, to: WorkerId, edge: NegationEdge<V>) {
        self.negation_chans
            .get(to as usize)
            .expect("receiver id out of bounds")
            .send(edge)
            .expect(&*format!("Send to worker {} failed", to));
    }

    fn terminate(&self, assignment: VertexAssignment) {
        for i in 0..self.workers.len() {
            self.term_chans
                .get(i as usize)
                .expect("receiver id out of bounds")
                .send(assignment)
                .expect(&*format!(
                    "Failed to send termination signal to worker {}",
                    i
                ));
        }
    }
}

type WorkQueue<V> = Receiver<Message<V>>;
type HyperQueue<V> = Receiver<HyperEdge<V>>;
type NegationQueue<V> = Receiver<NegationEdge<V>>;
type TermQueue = Receiver<VertexAssignment>;

impl<V: Hash + Eq + PartialEq + Clone> ChannelBroker<V> {
    pub fn new(
        worker_count: u64,
    ) -> (
        Self,
        Vec<WorkQueue<V>>,
        Vec<HyperQueue<V>>,
        Vec<NegationQueue<V>>,
        Vec<TermQueue>,
    ) {
        // Create a message channel foreach worker
        let mut msg_senders = Vec::with_capacity(worker_count as usize);
        let mut msg_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            msg_senders.push(sender);
            msg_receivers.push(receiver);
        }

        // Create a hyper edges waiting channel foreach worker
        let mut hyper_senders = Vec::with_capacity(worker_count as usize);
        let mut hyper_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            hyper_senders.push(sender);
            hyper_receivers.push(receiver);
        }

        // Create a message channel foreach worker
        let mut negation_senders = Vec::with_capacity(worker_count as usize);
        let mut negation_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            negation_senders.push(sender);
            negation_receivers.push(receiver);
        }

        // Create a termination channel foreach worker
        // These are used to signal early termination because the final assignment of `v0` has been discovered
        let mut term_senders = Vec::with_capacity(worker_count as usize);
        let mut term_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            term_senders.push(sender);
            term_receivers.push(receiver);
        }

        (
            Self {
                workers: msg_senders,
                hyper_chans: hyper_senders,
                negation_chans: negation_senders,
                term_chans: term_senders,
            },
            msg_receivers,
            hyper_receivers,
            negation_receivers,
            term_receivers,
        )
    }
}
