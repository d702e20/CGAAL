use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::hash::Hash;
use crossbeam_channel::{Receiver, Sender, TryRecvError};

#[derive(Clone, Debug)]
pub enum GMessage<V: Hash + Eq + PartialEq + Clone> {
    Assignment {
        assignment: HashMap<V,bool>,
    },
    Terminate,
}

pub trait GBroker<V: Hash + Eq + PartialEq + Clone> {

    fn return_changes(&self, assignments: HashSet<(V, bool)>);

    fn receive(&self) -> Result<Option<GMessage<V>>, Box<dyn Error>>;

    fn get_task(&self) -> Result<Option<V>, Box<dyn Error>>;
}

pub trait GBrokerManager<V: Hash + Eq + PartialEq + Clone> {
    fn send_assignments(&self, assignments: HashMap<V,bool>);
    fn queue_task(&self, task: V);
    fn receive_changes(&self) -> Result<HashSet<(V, bool)>, Box<dyn Error>>;
    fn terminate(&self);

}

pub struct GChannelBrokerManager<V: Hash + Eq + PartialEq + Clone> {
    workers: Vec<Sender<GMessage<V>>>,
    changes: Receiver<HashSet<(V, bool)>>,
    task_queue: Sender<V>,

}

impl<V: Hash + Eq + PartialEq + Clone> GBrokerManager<V> for GChannelBrokerManager<V>{
    fn send_assignments(&self, assignment: HashMap<V,bool>){
        let msg = GMessage::Assignment {assignment};
        for worker in &self.workers{
            worker.send(msg.clone()).expect("Sending assignments to workers failed");
        }
    }
    fn queue_task(&self, task: V){
        self.task_queue.send(task).expect("Sending tasks failed");
    }

    fn receive_changes(&self) -> Result<HashSet<(V, bool)>, Box<dyn Error>> {
        match self.changes.recv() {
            Ok(msg) => Ok(msg),
            Err(err) => Err(Box::new(err)),
        }
    }

    fn terminate(&self) {
        for to in 0..self.workers.len() {
            let _err = self
                .workers
                .get(to as usize)
                .expect("receiver id out of bounds")
                .send(GMessage::Terminate);
        }
    }
}

#[derive(Debug)]
pub struct GChannelBroker<V: Hash + Eq + PartialEq + Clone> {
    changes: Sender<HashSet<(V, bool)>>,
    receiver: Receiver<GMessage<V>>,
    task_queue: Receiver<V>,
}

impl<V: Hash + Eq + PartialEq + Clone> GBroker<V> for GChannelBroker<V> {
    fn return_changes(&self, changes: HashSet<(V, bool)>) {
        self.changes
            .send(changes)
            .expect("Failed to send the assignments table to main thread");
    }

    fn receive(&self) -> Result<Option<GMessage<V>>, Box<dyn Error>> {
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

    fn get_task(&self) -> Result<Option<V>, Box<dyn Error>> {
        match self.task_queue.try_recv() {
            Ok(vertex) => Ok(Some(vertex)),
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

impl<V: Hash + Eq + PartialEq + Clone> GChannelBroker<V> {
    pub fn new(worker_count: u64) -> (Vec<Self>, GChannelBrokerManager<V>) {
        // Create a message channel foreach worker
        let mut msg_senders = Vec::with_capacity(worker_count as usize);
        let mut msg_receivers = Vec::with_capacity(worker_count as usize);

        for _ in 0..worker_count {
            let (sender, receiver) = crossbeam_channel::unbounded();
            msg_senders.push(sender);
            msg_receivers.push(receiver);
        }
        let (task_tx,task_rx) = crossbeam_channel::unbounded();
        let (changes_tx, changes_rx) = crossbeam_channel::unbounded();

        let brokers = msg_receivers
            .drain(..)
            .map(|receiver| Self {
                changes: changes_tx.clone(),
                receiver,
                task_queue: task_rx.clone(),
            })
            .collect();

        let broker_manager = GChannelBrokerManager {
            workers: msg_senders,
            changes: changes_rx,
            task_queue: task_tx,
        };

        (brokers, broker_manager)
    }
}