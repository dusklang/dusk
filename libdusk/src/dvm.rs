use std::collections::HashMap;
use std::sync::mpsc;
use std::thread::{self, ThreadId};
use std::sync::Mutex;
use std::thread_local;

use lazy_static::lazy_static;

use crate::driver::{DriverRef, DRIVER};

struct Response;

enum MessageKind {
    Connect(mpsc::Sender<Response>),
}

struct Message {
    kind: MessageKind,
    id: ThreadId,
}

#[derive(Debug)]
pub enum Error {
    CoordinatorAlreadyCreated,
    NoCoordinatorCreated,
    UnableToConnect,
    UnableToSendMessage,
}

type Result<T> = std::result::Result<T, Error>;

struct DvmServer {
    sender: mpsc::Sender<Message>,
    receiver: mpsc::Receiver<Response>,
    id: ThreadId,
}

pub struct DvmServerCoordinator {
    receiver: mpsc::Receiver<Message>,
    senders: HashMap<ThreadId, mpsc::Sender<Response>>,
}

impl DvmServerCoordinator {
    pub fn new() -> Result<DvmServerCoordinator> {
        let mut shared_sender = SHARED_SENDER.lock().unwrap();
        if shared_sender.is_some() {
            return Err(Error::CoordinatorAlreadyCreated);
        }
        let (sender, receiver) = mpsc::channel();
        *shared_sender = Some(sender);

        Ok(DvmServerCoordinator { receiver, senders: Default::default() })
    }

    pub fn run(self) -> ! {
        loop {

        }
    }
}

impl DvmServer {
    pub fn new() -> Result<DvmServer> {
        let sender = SHARED_SENDER.lock().unwrap();
        let Some(sender) = &*sender else {
            return Err(Error::NoCoordinatorCreated);
        };
        let sender = sender.clone();
        let id = thread::current().id();
        let (response_sender, receiver) = mpsc::channel();
        sender.send(Message { kind: MessageKind::Connect(response_sender), id })
            .map_err(|_| Error::UnableToConnect)?;
        Ok(DvmServer { sender, receiver, id })
    }

    pub fn send_message(&self, message: MessageKind) -> Result<()> {
        self.sender.send(Message { kind: message, id: self.id }).map_err(|_| Error::UnableToSendMessage)?;
        Ok(())
    }
}
lazy_static! {
    static ref SHARED_SENDER: Mutex<Option<mpsc::Sender<Message>>> = Mutex::new(None);
}
