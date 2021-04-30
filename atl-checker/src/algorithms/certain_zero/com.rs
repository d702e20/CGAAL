use crate::algorithms::certain_zero::common::{Message, VertexAssignment, WorkerId};
use crossbeam_channel::{unbounded, Receiver, Sender, TryRecvError};
use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;

/// Broker implement the function of W_E, W_N, M_R and M_A
pub trait Broker<V: Hash + Eq + PartialEq + Clone> {
    /// Send message to worker with id `to`
    fn send(&self, to: WorkerId, msg: Message<V>);

    /// Send result to the main thread
    fn return_result(&self, assignment: VertexAssignment);

    /// Send assignment table to the main thread
    fn return_assignments(&self, assignments: HashMap<V, VertexAssignment>);

    /// Signal all workers to release the given depth
    fn release(&self, depth: usize);

    /// Signal to all workers to terminate
    fn terminate(&self);

    /// Attempts to retrieve a message destined for the current worker.
    ///
    /// Returns `Ok(Some(V))` when a message available, and `Ok(None)` when no message is currently available.
    /// Returns `Err` in case of unrecoverable error.
    fn receive(&self) -> Result<Option<Message<V>>, Box<dyn Error>>;
}

pub trait BrokerManager<V: Hash + Eq + PartialEq + Clone> {
    fn receive_result(&self) -> Result<VertexAssignment, Box<dyn Error>>;

    fn receive_assignment(&self) -> Result<HashMap<V, VertexAssignment>, Box<dyn Error>>;
}

pub struct ChannelBrokerManager<V: Hash + Eq + PartialEq + Clone> {
    result: Receiver<VertexAssignment>,
    assignment: Receiver<HashMap<V, VertexAssignment>>,
}

impl<V: Hash + Eq + PartialEq + Clone> BrokerManager<V> for ChannelBrokerManager<V> {
    fn receive_result(&self) -> Result<VertexAssignment, Box<dyn Error>> {
        match self.result.recv() {
            Ok(msg) => Ok(msg),
            Err(err) => Err(Box::new(err)),
        }
    }

    fn receive_assignment(&self) -> Result<HashMap<V, VertexAssignment>, Box<dyn Error>> {
        match self.assignment.recv() {
            Ok(msg) => Ok(msg),
            Err(err) => Err(Box::new(err)),
        }
    }
}

/// Implements Broker using channels from crossbeam_channel
#[derive(Debug)]
pub struct ChannelBroker<V: Hash + Eq + PartialEq + Clone> {
    workers: Vec<Sender<Message<V>>>,
    result: Sender<VertexAssignment>,
    assignment: Sender<HashMap<V, VertexAssignment>>,
    receiver: Receiver<Message<V>>,
}

impl<V: Hash + Eq + PartialEq + Clone> Broker<V> for ChannelBroker<V> {
    fn send(&self, to: WorkerId, msg: Message<V>) {
        debug!("send");
        self.workers
            .get(to as usize)
            .expect("receiver id out of bounds")
            .send(msg)
            .expect(&*format!("Send to worker {} failed", to));
    }

    fn return_result(&self, assignment: VertexAssignment) {
        self.result
            .send(assignment)
            .expect("Failed to send result to main thread");
        self.terminate();
    }

    fn return_assignments(&self, assignments: HashMap<V, VertexAssignment>) {
        self.assignment
            .send(assignments)
            .expect("Failed to send the assignments table to main thread");
    }

    fn release(&self, depth: usize) {
        for i in 0..self.workers.len() {
            self.send(i as u64, Message::RELEASE(depth))
        }
    }

    fn terminate(&self) {
        for to in 0..self.workers.len() {
            let _err = self
                .workers
                .get(to as usize)
                .expect("receiver id out of bounds")
                .send(Message::TERMINATE);
        }
    }

    fn receive(&self) -> Result<Option<Message<V>>, Box<dyn Error>> {
        match self.receiver.try_recv() {
            Ok(msg) => Ok(Some(msg)),
            Err(err) => match err {
                TryRecvError::Empty => {
                    debug!("nothing to receive");
                    Ok(None)
                }
                TryRecvError::Disconnected => Err(Box::new(err)),
            },
        }
    }
}

impl<V: Hash + Eq + PartialEq + Clone> ChannelBroker<V> {
    pub fn new(worker_count: u64) -> (Vec<Self>, ChannelBrokerManager<V>) {
        // Create a message channel foreach worker
        let mut msg_senders = Vec::with_capacity(worker_count as usize);
        let mut msg_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = unbounded();
            msg_senders.push(sender);
            msg_receivers.push(receiver);
        }

        let (result_tx, result_rx) = unbounded();
        let (assignment_tx, assignment_rx) = unbounded();

        let brokers = msg_receivers
            .drain(..)
            .map(|receiver| Self {
                workers: msg_senders.clone(),
                result: result_tx.clone(),
                assignment: assignment_tx.clone(),
                receiver,
            })
            .collect();

        let broker_manager = ChannelBrokerManager {
            result: result_rx,
            assignment: assignment_rx,
        };

        (brokers, broker_manager)
    }
}
