//! A virtual machine for the Monkey programming language from
//! <https://compilerbook.com/>.

pub mod error;
pub use error::{Error, ErrorKind, Result};

mod frame;

pub mod machine;
pub use machine::Vm;

mod op;
