use crossbeam_channel::{Receiver, Sender, TryRecvError};
use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;

#[derive(Clone, Debug)]
pub enum GMessage<V: Hash + Eq + PartialEq + Clone> {
    Updates {
        updates: HashMap<V, bool>,
        iteration: usize,
    },
    Result {
        task: V,
        value: bool,
    },
    Terminate,
}

pub trait GBroker<V: Hash + Eq + PartialEq + Clone> {
    fn send_terminate(&self);
    fn send_result(&self, task: V, value: bool);
    fn receive(&self) -> Result<Option<GMessage<V>>, Box<dyn Error>>;
    fn get_task(&self) -> Result<Option<(V, usize)>, Box<dyn Error>>;
}

pub trait GBrokerManager<V: Hash + Eq + PartialEq + Clone> {
    fn send_updates(&self, assignments: HashMap<V, bool>, iterations: usize);
    fn queue_task(&self, task: V, iteration: usize);
    fn receive(&self) -> Result<Option<GMessage<V>>, Box<dyn Error>>;
    fn terminate(&self);
}

pub struct GChannelBrokerManager<V: Hash + Eq + PartialEq + Clone> {
    workers: Vec<Sender<GMessage<V>>>,
    updates: Receiver<GMessage<V>>,
    task_queue_producer: Sender<(V, usize)>,
}

impl<V: Hash + Eq + PartialEq + Clone> GBrokerManager<V> for GChannelBrokerManager<V> {
    fn send_updates(&self, updates: HashMap<V, bool>, iterations: usize) {
        let msg = GMessage::Updates {
            updates,
            iteration: iterations,
        };
        for worker in &self.workers {
            worker
                .send(msg.clone())
                .expect("Sending assignments to workers failed");
        }
    }
    fn queue_task(&self, task: V, iteration: usize) {
        self.task_queue_producer
            .send((task, iteration))
            .expect("Sending tasks failed");
    }

    fn receive(&self) -> Result<Option<GMessage<V>>, Box<dyn Error>> {
        match self.updates.try_recv() {
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

    fn terminate(&self) {
        for to in 0..self.workers.len() {
            let _err = self
                .workers
                .get(to)
                .expect("receiver id out of bounds")
                .send(GMessage::Terminate);
        }
    }
}

#[derive(Debug)]
pub struct GChannelBroker<V: Hash + Eq + PartialEq + Clone> {
    updates: Sender<GMessage<V>>,
    receiver: Receiver<GMessage<V>>,
    task_queue_consumer: Receiver<(V, usize)>,
}

impl<V: Hash + Eq + PartialEq + Clone> GBroker<V> for GChannelBroker<V> {
    fn send_terminate(&self) {
        self.updates
            .send(GMessage::Terminate)
            .expect("Failed to send terminate message");
    }

    fn send_result(&self, task: V, value: bool) {
        self.updates
            .send(GMessage::Result { task, value })
            .expect("Failed to send the result");
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

    fn get_task(&self) -> Result<Option<(V, usize)>, Box<dyn Error>> {
        match self.task_queue_consumer.try_recv() {
            Ok(t) => Ok(Some(t)),
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
        let (task_tx, task_rx) = crossbeam_channel::unbounded();
        let (changes_tx, changes_rx) = crossbeam_channel::unbounded();

        let brokers = msg_receivers
            .drain(..)
            .map(|receiver| Self {
                updates: changes_tx.clone(),
                receiver,
                task_queue_consumer: task_rx.clone(),
            })
            .collect();

        let broker_manager = GChannelBrokerManager {
            workers: msg_senders,
            updates: changes_rx,
            task_queue_producer: task_tx,
        };

        (brokers, broker_manager)
    }
}
