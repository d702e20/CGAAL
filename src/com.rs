use crate::common::{Message, VertexAssignment, WorkerId};
use crossbeam_channel::{unbounded, Receiver, Sender};
use std::hash::Hash;

/// Broker implement the function of W_E, W_N, M_R and M_A
pub trait Broker<V: Hash + Eq + PartialEq + Clone> {
    /// Send message to worker with id `to`
    fn send(&self, to: WorkerId, msg: Message<V>);

    /// Signal to all workers to terminate because a result has been found
    fn terminate(&self, assignment: VertexAssignment);
}

/// Implements Broker using channels from crossbeam_channel
#[derive(Debug)]
pub struct ChannelBroker<V: Hash + Eq + PartialEq + Clone> {
    workers: Vec<Sender<Message<V>>>,
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

impl<V: Hash + Eq + PartialEq + Clone> ChannelBroker<V> {
    pub fn new(
        worker_count: u64,
    ) -> (
        Self,
        Vec<Receiver<Message<V>>>,
        Vec<Receiver<VertexAssignment>>,
    ) {
        // Create a message channel foreach worker
        let mut msg_senders = Vec::with_capacity(worker_count as usize);
        let mut msg_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            msg_senders.push(sender);
            msg_receivers.push(receiver);
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
                term_chans: term_senders,
            },
            msg_receivers,
            term_receivers,
        )
    }
}
