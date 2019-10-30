//! A compiler for the Monkey programming language from
//! <https://compilerbook.com/>.

pub mod compile;
pub use compile::{Bytecode, Compiler};

pub mod error;
pub use error::{Error, ErrorKind, Result};

mod symbol;
