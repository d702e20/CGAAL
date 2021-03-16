use crate::common::{HyperEdge, Message, NegationEdge, VertexAssignment, WorkerId};
use crossbeam_channel::{unbounded, Receiver, Sender};
use std::hash::Hash;

/// Broker implement the function of W_E, W_N, M_R and M_A
pub trait Broker<V: Hash + Eq + PartialEq + Clone> {
    /// Send message to worker with id `to`
    fn send(&self, to: WorkerId, msg: Message<V>);

    /// Send result to the main thread
    fn return_result(&self, assignment: VertexAssignment);

    /// Signal all workers to release the given depth
    fn release(&self, depth: usize);

    /// Signal to all workers to terminate
    fn terminate(&self);
}

/// Implements Broker using channels from crossbeam_channel
#[derive(Debug)]
pub struct ChannelBroker<V: Hash + Eq + PartialEq + Clone> {
    workers: Vec<Sender<Message<V>>>,
    result: Sender<VertexAssignment>,
}

impl<V: Hash + Eq + PartialEq + Clone> Broker<V> for ChannelBroker<V> {
    fn send(&self, to: WorkerId, msg: Message<V>) {
        self.workers
            .get(to as usize)
            .expect("receiver id out of bounds")
            .send(msg)
            .expect(&*format!("Send to worker {} failed", to));
    }

    fn return_result(&self, assignment: VertexAssignment) {
        self.result
            .send(assignment)
            .expect("Failed to send result to main thread")
    }

    fn release(&self, depth: usize) {
        for i in 0..self.workers.len() {
            self.send(i as u64, Message::RELEASE(depth))
        }
    }

    fn terminate(&self) {
        for i in 0..self.workers.len() {
            self.send(i as u64, Message::TERMINATE)
        }
    }
}

type MsgQueueList<V> = Vec<Receiver<Message<V>>>;

impl<V: Hash + Eq + PartialEq + Clone> ChannelBroker<V> {
    pub fn new(worker_count: u64) -> (Self, MsgQueueList<V>, Receiver<VertexAssignment>) {
        // Create a message channel foreach worker
        let mut msg_senders = Vec::with_capacity(worker_count as usize);
        let mut msg_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            msg_senders.push(sender);
            msg_receivers.push(receiver);
        }

        let (result_tx, result_rx) = unbounded();

        (
            Self {
                workers: msg_senders,
                result: result_tx,
            },
            msg_receivers,
            result_rx,
        )
    }
}
