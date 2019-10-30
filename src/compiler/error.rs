//! Error types for the `compiler` module.

use std::{error, fmt, result};

use crate::code;

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    Compile(ErrorKind),
    Code(code::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Compile(k) => write!(f, "compile error: {}", k),
            Error::Code(c) => write!(f, "code error: {}", c),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        match self {
            Error::Compile(_) => None,
            Error::Code(c) => Some(c),
        }
    }
}

/// Describes compilation errors which may occur.
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    UndefinedIdentifier(String),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::UndefinedIdentifier(id) => write!(f, "undefined identifier: \"{}\"", id,),
        }
    }
}
