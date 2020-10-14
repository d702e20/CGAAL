use crate::common::{WorkerId, Message, VertexAssignment};
use crossbeam_channel::{Sender, Receiver, unbounded};
use std::hash::Hash;

pub trait Broker<V: Hash + Eq + PartialEq + Clone> {
    fn send(&self, to: WorkerId, msg: Message<V>);

    /// Signal to all workers a terminate because a result have been
    fn terminate(&self, assignment: VertexAssignment);
}

pub struct ChannelBroker<V: Hash + Eq + PartialEq + Clone> {
    workers: Vec<Sender<Message<V>>>,
    term_chans: Vec<Sender<VertexAssignment>>
}

impl<V: Hash + Eq + PartialEq + Clone> Broker<V> for ChannelBroker<V> {
    fn send(&self, to: WorkerId, msg: Message<V>) {
        self.workers
            .get(to as usize)
            .expect("receiver id out of bounds")
            .send(msg)
            .expect(&*format!("Send to worker {} failed", to));
    }

    fn terminate(&self, assignment: VertexAssignment) {
        for i in 0..self.workers.len() {
            self.term_chans
                .get(i as usize)
                .expect("receiver id out of bounds")
                .send(assignment)
                .expect(&*format!("Failed to send termination signal to worker {}", i));
        }
    }
}

impl<V: Hash + Eq + PartialEq + Clone> ChannelBroker<V> {
    pub fn new(worker_count: u64) -> (Self, Vec<Receiver<Message<V>>>, Vec<Receiver<VertexAssignment>>) {
        let mut msg_senders = Vec::with_capacity(worker_count as usize);
        let mut msg_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            msg_senders.push(sender);
            msg_receivers.push(receiver);
        }

        let mut term_senders = Vec::with_capacity(worker_count as usize);
        let mut term_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            term_senders.push(sender);
            term_receivers.push(receiver);
        }

        (Self { workers: msg_senders, term_chans: term_senders }, msg_receivers, term_receivers)
    }
}