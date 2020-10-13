use crate::common::{WorkerId, Message};
use crossbeam_channel::{Sender, Receiver, unbounded};

pub trait Broker {
    fn send(&self, to: WorkerId, msg: Message);

    fn broadcast(&self, msg: Message);

    fn terminate(&self);
}

pub struct ChannelBroker {
    workers: Vec<Sender<Message>>,
    term_chans: Vec<Sender<()>>
}

impl Broker for ChannelBroker {
    fn send(&self, to: WorkerId, msg: Message) {
        self.workers
            .get(to as usize)
            .expect("receiver id out of bounds")
            .send(msg)
            .expect(&*format!("Send to worker {} failed", to));
    }

    fn broadcast(&self, msg: Message) {
        for i in 0..self.workers.len() {
            self.send(i as i32, msg.clone());
        }
    }

    fn terminate(&self) {
        for i in 0..self.workers.len() {
            self.send(i as i32, Message::TERMINATE);
            self.term_chans
                .get(i as usize)
                .expect("receiver id out of bounds")
                .send(())
                .expect(&*format!("Failed to send termination signal to worker {}", i));
        }
    }
}

impl ChannelBroker {
    pub fn new(worker_count: i32) -> (Self, Vec<Receiver<Message>>, Vec<Receiver<()>>) {
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