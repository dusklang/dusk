use std::collections::HashMap;
use std::sync::mpsc;
use std::thread::{self, ThreadId};
use std::sync::{Mutex, LazyLock};
use std::thread_local;

use crate::driver::{DriverRef, DRIVER};
use crate::interpreter::{Value, Result as EvalResult};
use crate::mir::FunctionRef;
use crate::ty::Type;

pub struct Response(pub EvalResult<Value>);

pub struct Call {
    pub func_ref: FunctionRef,
    pub arguments: Vec<Value>,
    pub generic_arguments: Vec<Type>,
}

pub enum MessageKind {
    Connect(mpsc::Sender<Response>),
    Call(Call),
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
    UnableToRecvResponse,
}

type Result<T> = std::result::Result<T, Error>;

pub struct DvmServerCoordinator {
    receiver: mpsc::Receiver<Message>,
    senders: HashMap<ThreadId, mpsc::Sender<Response>>,
}

pub fn launch_coordinator_thread() {
    let coordinator = DvmServerCoordinator::new().unwrap();
    thread::spawn(|| {
        coordinator.run()
    });
}

pub fn send_message(message: MessageKind) -> Result<Response> {
    SERVER.with(|server| {
        server.send_message(message)
    })
}

impl DvmServerCoordinator {
    fn new() -> Result<DvmServerCoordinator> {
        let mut shared_sender = SHARED_SENDER.lock().unwrap();
        if shared_sender.is_some() {
            return Err(Error::CoordinatorAlreadyCreated);
        }
        let (sender, receiver) = mpsc::channel();
        *shared_sender = Some(sender);

        Ok(DvmServerCoordinator { receiver, senders: Default::default() })
    }

    fn run(mut self) -> ! {
        let (call_tx, call_rx) = mpsc::channel::<Call>();
        let (call_response_tx, call_response_rx) = mpsc::channel::<EvalResult<Value>>();
        thread::spawn(move || {
            let mut driver = DriverRef::new(&DRIVER);
            while let Ok(call) = call_rx.recv() {
                let res = driver.call_direct(call.func_ref, call.arguments, call.generic_arguments);
                driver.unlock(); // prevent deadlock
                _ = call_response_tx.send(res);
            }
        });
        let mut driver = DriverRef::new(&DRIVER);
        loop {
            driver.unlock();
            let Ok(message) = self.receiver.recv() else { continue; };
            match message.kind {
                MessageKind::Call(call) => {
                    call_tx.send(call).unwrap();

                    self.wait_for_response(&mut driver, &call_response_rx, message.id);
                },
                MessageKind::Connect(_) => self.handle_message(&mut driver, message),
            }
        }
    }

    fn wait_for_response(&mut self, driver: &mut DriverRef, call_response_rx: &mpsc::Receiver<EvalResult<Value>>, id: ThreadId) {
        loop {
            if let Ok(response) = call_response_rx.try_recv() {
                self.senders[&id].send(Response(response)).unwrap();
                return;
            }
    
            let Ok(message) = self.receiver.try_recv() else { continue; };
            self.handle_message(driver, message);
        }
    }

    fn handle_message(&mut self, _driver: &mut DriverRef, message: Message) {
        match message.kind {
            MessageKind::Call(_) => unimplemented!("unable to accept another call while the previous one is still in-flight"),
            MessageKind::Connect(new_sender) => {
                self.senders.insert(message.id, new_sender);
            }
        }
    }
}

pub struct DvmServer {
    sender: mpsc::Sender<Message>,
    receiver: mpsc::Receiver<Response>,
    id: ThreadId,
}

impl Default for DvmServer {
    fn default() -> Self {
        DvmServer::new().unwrap()
    }
}


impl DvmServer {
    fn new() -> Result<DvmServer> {
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

    fn send_message(&self, message: MessageKind) -> Result<Response> {
        self.sender.send(Message { kind: message, id: self.id }).map_err(|_| Error::UnableToSendMessage)?;
        let response = self.receiver.recv().map_err(|_| Error::UnableToRecvResponse)?;
        Ok(Response(response.0))
    }
}

thread_local! {
    static SERVER: DvmServer = DvmServer::new().unwrap();
}

static SHARED_SENDER: LazyLock<Mutex<Option<mpsc::Sender<Message>>>> = LazyLock::new(|| Mutex::new(None));