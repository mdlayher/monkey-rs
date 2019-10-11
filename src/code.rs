//! Bytecode representation for the Monkey programming language from
//! <https://compilerbook.com/>.

extern crate byteorder;

use std::error;
use std::fmt;
use std::io;
use std::result;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

// Note: Opcode is a small enough type that clippy recommends moving/copying
// it rathe than borrowing it.

/// An opcode for the Monkey virtual machine.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Opcode {
    Constant = 0,
}

impl From<u8> for Opcode {
    /// Convert from a u8 to an Opcode.
    fn from(v: u8) -> Self {
        match v {
            0 => Opcode::Constant,
            _ => panic!("unhandled u8 to Opcode conversion: {}", v),
        }
    }
}

// TODO(mdlayher): probably make the API accept a byte slice owned by the caller.

/// Parses bytecode for one instruction into an `Opcode` and operands.
pub fn parse(buf: &[u8]) -> Result<(Opcode, Vec<usize>)> {
    let mut c = io::Cursor::new(buf);

    let op = Opcode::from(c.read_u8().map_err(Error::Io)?);
    let def = lookup(op);

    let mut operands = vec![];
    for w in def.operand_widths {
        match w {
            Width::Two => operands.push(c.read_u16::<BigEndian>().map_err(Error::Io)? as usize),
        }
    }

    Ok((op, operands))
}

/// Produces bytecode for one instruction from an input `Opcode` and its
/// operands.
pub fn make(op: Opcode, operands: &[usize]) -> Result<Vec<u8>> {
    let def = lookup(op);

    // Ensure the correct number of operands were passed for this opcode.
    if operands.len() != def.operand_widths.len() {
        return Err(Error::Internal {
            op: def.name.to_string(),
            kind: ErrorKind::BadNumberOperands {
                want: def.operand_widths.len(),
                got: operands.len(),
            },
        });
    }

    // Allocate enough space for the opcode and all of its operands.
    let mut buf = Vec::with_capacity(
        1 + def
            .operand_widths
            .iter()
            .map(|w| *w as usize)
            .sum::<usize>(),
    );
    buf.push(op as u8);

    // Write the instruction into the vector.
    for (oper, width) in operands.iter().zip(def.operand_widths.iter()) {
        match width {
            Width::Two => buf
                .write_u16::<BigEndian>(*oper as u16)
                .map_err(Error::Io)?,
        };
    }

    Ok(buf)
}

/// The number of bytes required to hold an operand.
#[derive(Clone, Copy, Debug)]
enum Width {
    Two = 2,
}

/// The definition of an `Opcode` with attached operands and their sizes
/// in bytes.
#[derive(Debug)]
struct Definition<'a> {
    name: &'a str,
    operand_widths: Vec<Width>,
}

/// Look up the `Definition` for a given `Opcode`.
fn lookup<'a>(op: Opcode) -> Definition<'a> {
    match op {
        Opcode::Constant => Definition {
            name: "Constant",
            operand_widths: vec![Width::Two],
        },
    }
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    Internal { op: String, kind: ErrorKind },
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Internal { op, kind } => write!(f, "internal error: opcode {}: {}", op, kind),
            Error::Io(err) => err.fmt(f),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        None
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    BadNumberOperands { want: usize, got: usize },
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::BadNumberOperands { want, got } => {
                write!(f, "want {} operand(s), but got {}", want, got)
            }
        }
    }
}
