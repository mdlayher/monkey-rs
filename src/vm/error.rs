//! Error types for the `vm` module.

use std::{io, result};

use crate::{
    code::Opcode,
    object::{self, Object},
};

extern crate thiserror;
use thiserror::Error;

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug, Error)]
pub enum Error {
    #[error("runtime error: {0}")]
    Runtime(ErrorKind),
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
}

/// Describes runtime errors which may occur during `Vm::run`.
#[derive(Debug, Error, PartialEq)]
pub enum ErrorKind {
    #[error(r#"operator "{0}" is not supported for argument(s): {1:?}"#)]
    OperatorUnsupported(Opcode, Vec<Object>),
    #[error("function call expects {want} arguments, but got {got}")]
    WrongNumberArguments { want: usize, got: usize },
    #[error("duplicate key {0} in data structure")]
    DuplicateKey(object::Hashable),
    #[error("cannot call object {0:?} as function")]
    BadFunctionCall(Object),
    #[error("cannot dereference non-pointer object {0:?}")]
    BadPointerDereference(Object),
    #[error("object error: {0}")]
    Object(#[from] object::Error),
}
