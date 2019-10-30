//! Error types for the `vm` module.

use std::{error, fmt, io, result};

use crate::{
    code::Opcode,
    object::{self, Object},
};

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    Runtime(ErrorKind),
    Io(io::Error),
}

impl Error {
    /// Produces an `Error::Runtime(ErrorKind::BadArguments)` error with the
    /// input parameters.
    pub fn bad_arguments(kind: BadArgumentKind, op: Opcode, args: &[Object]) -> Self {
        // Sanity checks to ensure kind and number of arguments always match up.
        let n = match kind {
            BadArgumentKind::UnaryOperatorUnsupported => 1,
            BadArgumentKind::BinaryOperatorUnsupported => 2,
        };

        assert_eq!(
            args.len(),
            n,
            "unexpected number of arguments for {:?} error",
            kind
        );

        Self::Runtime(ErrorKind::BadArguments {
            kind,
            op,
            // Make a copy of args so they can return to the caller without
            // referencing our owned stack.
            args: args.to_vec(),
        })
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Runtime(kind) => write!(f, "runtime error: {}", kind),
            Error::Io(err) => err.fmt(f),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        match self {
            Error::Io(err) => Some(err),
            _ => None,
        }
    }
}

/// Describes runtime errors which may occur during `Vm::run`.
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    BadArguments {
        kind: BadArgumentKind,
        op: Opcode,
        args: Vec<Object>,
    },
    DuplicateKey(object::Hashable),
    BadFunctionCall(Object),
    BadPointerDereference(Object),
    Object(object::Error),
}

/// Describes a particular type of bad arguments error.
#[derive(Debug, PartialEq)]
pub enum BadArgumentKind {
    UnaryOperatorUnsupported,
    BinaryOperatorUnsupported,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::BadArguments { kind, op, args } => match kind {
                BadArgumentKind::UnaryOperatorUnsupported => write!(
                    f,
                    "unary operator {} not supported on argument: \"{}{}\"",
                    op, op, args[0],
                ),
                BadArgumentKind::BinaryOperatorUnsupported => write!(
                    f,
                    "binary operator {} not supported on arguments: \"{} {} {}\"",
                    op, args[0], op, args[1],
                ),
            },
            ErrorKind::DuplicateKey(k) => write!(f, "duplicate key {} in data structure", k),
            ErrorKind::BadFunctionCall(o) => write!(f, "cannot call object {:?} as a function", o),
            ErrorKind::BadPointerDereference(o) => {
                write!(f, "cannot dereference non-pointer object {:?}", o)
            }
            ErrorKind::Object(err) => write!(f, "object error: {}", err),
        }
    }
}
