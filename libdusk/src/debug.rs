//! This module contains code for communicating with the Dusk Visual Debugger (DVD).
//! 
//! The basic flow of execution when using DVD is:
//! - Compiler sends a [message](Message) to DVD and blocks on a [response](Response). (TODO: consider optimizing this simple protocol if it's too slow)
//! - DVD processes the information in the message, and eventually sends a [response](Response) (possibly in response to user input).
//! - Compiler continues until the next message needs to be sent.

use std::sync::Mutex;
use std::process::{Command, Stdio};
use std::io::Write;

use dire::hir::{ExprId, DeclId, ItemId};
use crate::tir::CompId;
use lazy_static::lazy_static;
use interprocess::local_socket::{LocalSocketListener, LocalSocketStream};

/// Messages *from* the compiler *to* the debugger.
#[derive(Debug)]
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
}

/// Responses *from* the debugger *to* the compiler.
pub enum Response {
    /// Continue compiling as normal
    Continue,
    /// Quit compiling
    Quit
}

trait DvdCoordinatorTrait: Send {
    fn send(&mut self, message: &mut dyn FnMut() -> Message);
}

struct MockCoordinator;
impl DvdCoordinatorTrait for MockCoordinator {
    fn send(&mut self, _message: &mut dyn FnMut() -> Message) {}
}

struct DvdCoordinator {
    conn: LocalSocketStream,
}
impl DvdCoordinator {
    fn send(&mut self, bytes: &[u8]) {
        self.conn.write_all(&(bytes.len() as u32).to_ne_bytes()).unwrap();
        self.conn.write_all(bytes).unwrap();
    }
}
impl DvdCoordinatorTrait for DvdCoordinator {
    fn send(&mut self, message: &mut dyn FnMut() -> Message) {
        let message = format!("{:?}", message());
        println!("Sending message: {}", message);
        self.send(message.as_bytes());
    }
}

#[inline]
pub fn send(mut message: impl FnMut() -> Message) {
    COORDINATOR.lock().unwrap().send(&mut message);
}

pub fn connect() {
    let listener = LocalSocketListener::bind("DUSK_VISUAL_DEBUGGER").unwrap();
    Command::new("abs")
        .arg("run")
        .current_dir("dvd")
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