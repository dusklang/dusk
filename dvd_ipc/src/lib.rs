//! This crate contains code for IPC between the Dusk compiler and the Dusk Visual Debugger (DVD).
//! 
//! The basic flow of execution when using DVD is:
//! - Compiler sends a [message](Message) to DVD and blocks on a [response](Response). (TODO: consider optimizing this simple protocol if it's too slow)
//! - DVD processes the information in the message, and eventually sends a [response](Response) (possibly in response to user input).
//! - Compiler continues until the next message needs to be sent.

#[cfg(feature = "enabled")]
use serde::{Serialize, Deserialize};

use dusk_dire::hir::{ExprId, DeclId, ItemId};
use dusk_dire::tir::CompId;

/// Messages *from* the compiler *to* the debugger.
#[cfg_attr(feature = "enabled", derive(Debug, Serialize, Deserialize))]
pub enum Message {
    /// Sent before compilation begins
    WillBegin,
    /// Sent after adding a new expression. Includes source text, if exists.
    DidAddExpr {
        id: ExprId,
        item_id: ItemId,
        text: Option<String>,
    },
    /// Sent after adding a new declaration. Includes source text, if exists.
    DidAddDecl {
        id: DeclId,
        item_id: ItemId,
        text: Option<String>,
    },

    /// Sent before initializing TIR
    WillInitializeTir,
    /// Sent after adding decl to TIR
    DidInitializeTirForDecl {
        id: DeclId,
        is_mut: bool,
        param_tys: Vec<ExprId>,
        // TODO: Generic params
    },
    /// Sent after adding a type 1 dependency from `depender` on `dependee`
    DidAddTirType1Dependency {
        depender: ItemId,
        dependee: ItemId,
    },
    /// Sent after adding a new component to the TIR graph
    DidAddTirComponent {
        id: CompId,
    },
    /// Sent after adding an item to a TIR component
    DidAddItemToTirComponent {
        component: CompId,
        item: ItemId,
    },
    /// Sent after adding a meta-dependency from `depender` on `dependee`
    DidAddTirMetaDependency {
        depender: ItemId,
        dependee: ItemId,
    },
    /// Sent after initializing TIR
    DidInitializeTir,

    /// Sent before building another set of units and meta-units of TIR
    WillBuildMoreTir,

    /// Sent before adding all type 2-4 dependencies for this set
    WillAddTirDependencies,
    /// Sent after adding a type 2 dependency from `depender` on `dependee`
    DidAddTirType2Dependency {
        depender: ItemId,
        dependee: ItemId,
    },
    /// Sent after adding a type 2 dependency from `depender` on `dependee`
    DidAddTirType3Dependency {
        depender: ItemId,
        dependee: ItemId,
    },
    /// Sent after adding a type 2 dependency from `depender` on `dependee`
    DidAddTirType4Dependency {
        depender: ItemId,
        dependee: ItemId,
    },
    /// Sent after adding all type 2-4 dependencies for this set
    DidAddTirDependencies,

    /// Sent after building another set of units and meta-units of TIR
    DidBuildMoreTir,

    /// Sent before typechecking a set of TIR
    WillTypeCheckSet,

    /// Sent after typechecking a set of TIR
    DidTypeCheckSet,

    /// Sent just before exiting
    WillExit,
}

/// Responses *from* the debugger *to* the compiler.
#[cfg_attr(feature = "enabled", derive(Serialize, Deserialize))]
pub enum Response {
    /// Continue compiling as normal
    Continue,
    /// Quit compiling
    Quit
}

// Exports dependent on the presence of the "enabled" feature
pub use dependent_exports::*;

#[cfg(feature = "enabled")]
mod dependent_exports {
    use std::io::{Write, Read};
    use std::process::{Command, Stdio};
    use std::sync::Mutex;
    
    use serde::Serialize;
    use serde::de::DeserializeOwned;
    use interprocess::local_socket::{LocalSocketListener, LocalSocketStream};
    use lazy_static::lazy_static;

    use crate::{Message, Response};

    trait DvdCoordinatorTrait: Send {
        fn send(&mut self, message: &mut dyn FnMut() -> crate::Message);
    }
    
    struct MockCoordinator;
    impl DvdCoordinatorTrait for MockCoordinator {
        fn send(&mut self, _message: &mut dyn FnMut() -> Message) {}
    }
    
    pub fn send_value(w: &mut impl Write, value: impl Serialize) {
        let payload = serde_json::to_vec(&value).unwrap();
        w.write_all(&(payload.len() as u32).to_ne_bytes()).unwrap();
        w.write_all(&payload).unwrap();
    }
    
    pub fn receive_value<V: DeserializeOwned>(r: &mut impl Read) -> V {
        let mut len = [0u8; 4];
        r.read_exact(&mut len).unwrap();
        let len = u32::from_ne_bytes(len);
        let mut payload = Vec::new();
        payload.resize(len as usize, 0u8);
        r.read_exact(&mut payload).unwrap();
        serde_json::from_slice(&payload).unwrap()
    }
    
    struct DvdCoordinator {
        conn: LocalSocketStream,
    }
    impl DvdCoordinatorTrait for DvdCoordinator {
        fn send(&mut self, message: &mut dyn FnMut() -> Message) {
            send_value(&mut self.conn, message());
            let response: Response = receive_value(&mut self.conn);
            match response {
                Response::Continue => {},
                Response::Quit => {
                    println!("Told to quit");
                    std::process::exit(0);
                }
            }
        }
    }
    
    #[inline]
    pub fn send(mut message: impl FnMut() -> Message) {
        COORDINATOR.lock().unwrap().send(&mut message);
    }
    
    pub fn connect() {
        let listener = LocalSocketListener::bind("DUSK_VISUAL_DEBUGGER").unwrap();
        Command::new("cargo")
            .arg("run")
            .arg("--bin")
            .arg("dvd")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .stdin(Stdio::null())
            .spawn()
            .unwrap();
        println!("Waiting for connection to DVD...");
        let conn = listener.incoming().next().unwrap().unwrap();
        println!("Got connection.");
        *COORDINATOR.lock().unwrap() = Box::new(DvdCoordinator { conn });
    }
    
    lazy_static! {
        // TODO: don't use a mutex, since this will only be accessed from one thread probably
        static ref COORDINATOR: Mutex<Box<dyn DvdCoordinatorTrait>> = Mutex::new(Box::new(MockCoordinator));
    }
}


#[cfg(not(feature = "enabled"))]
mod dependent_exports {
    use std::io::{Write, Read};

    use crate::Message;
    
    #[inline]
    pub fn send_value<V>(_w: &mut impl Write, _value: V) {}
    
    #[inline]
    pub fn receive_value<V>(_r: &mut impl Read) -> V {
        unimplemented!("can't call dvd_ipc::receive_value() without the \"enabled\" feature")
    }

    #[inline]
    pub fn send(_message: impl FnMut() -> Message) {}
    
    #[inline]
    pub fn connect() {}
}