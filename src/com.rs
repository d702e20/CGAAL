use crate::common::{HyperEdge, Message, NegationEdge, VertexAssignment, WorkerId};
use crossbeam_channel::{unbounded, Receiver, Sender};
use std::hash::Hash;

/// Broker implement the function of W_E, W_N, M_R and M_A
pub trait Broker<V: Hash + Eq + PartialEq + Clone> {
    /// Send message to worker with id `to`
    fn send(&self, to: WorkerId, msg: Message<V>);

    /// Send hyper edge for queuing to worker with id `to`
    fn queue_hyper(&self, to: WorkerId, edge: HyperEdge<V>);

    /// Send safe negation edge for queuing to worker with id `to`
    fn queue_negation(&self, to: WorkerId, edge: NegationEdge<V>);

    /// Signal to all workers to terminate because a result has been found
    fn terminate(&self, assignment: VertexAssignment);
}

/// Implements Broker using channels from crossbeam_channel
#[derive(Debug)]
pub struct ChannelBroker<V: Hash + Eq + PartialEq + Clone> {
    workers: Vec<Sender<Message<V>>>,
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
        self.send(to, Message::HYPER(edge));
    }

    fn queue_negation(&self, to: WorkerId, edge: NegationEdge<V>) {
        self.send(to, Message::NEGATION(edge));
    }

    fn terminate(&self, assignment: VertexAssignment) {
        for i in 0u64..self.workers.len() as u64 {
            self.send(i, Message::TERMINATE(assignment))
        }
    }
}

type MsgQueueList<V> = Vec<Receiver<Message<V>>>;

impl<V: Hash + Eq + PartialEq + Clone> ChannelBroker<V> {
    pub fn new(worker_count: u64) -> (Self, MsgQueueList<V>) {
        // Create a message channel foreach worker
        let mut msg_senders = Vec::with_capacity(worker_count as usize);
        let mut msg_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            msg_senders.push(sender);
            msg_receivers.push(receiver);
        }

        (
            Self {
                workers: msg_senders,
            },
            msg_receivers,
        )
    }
}
