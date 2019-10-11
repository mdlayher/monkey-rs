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
    // Base operations.
    Constant = 0x00,
    Pop = 0x01,
    // Math operations.
    Add = 0x10,
    Sub = 0x11,
    Mul = 0x12,
    Div = 0x13,
    Mod = 0x14,
}

impl From<u8> for Opcode {
    /// Convert from a u8 to an Opcode.
    fn from(v: u8) -> Self {
        match v {
            0x00 => Opcode::Constant,
            0x01 => Opcode::Pop,
            0x10 => Opcode::Add,
            0x11 => Opcode::Sub,
            0x12 => Opcode::Mul,
            0x13 => Opcode::Div,
            0x14 => Opcode::Mod,
            _ => panic!("unhandled u8 to Opcode conversion: {}", v),
        }
    }
}

// TODO(mdlayher): probably make the API accept a byte slice owned by the caller.

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

/// A stream of bytecode instructions.
#[derive(Debug, PartialEq)]
pub struct Instructions {
    pub stream: Vec<(Opcode, Vec<usize>)>,
}

impl Instructions {
    /// Parses a bytecode stream to produce `Instructions`.
    pub fn parse(buf: &[u8]) -> Result<Self> {
        let mut c = io::Cursor::new(buf);
        let mut ins = Instructions { stream: vec![] };

        // Keep reading until we hit the end of the stream.
        while c.position() < buf.len() as u64 {
            let op = Opcode::from(c.read_u8().map_err(Error::Io)?);
            let def = lookup(op);

            let mut operands = Vec::with_capacity(def.operand_widths.len());
            for w in def.operand_widths {
                match w {
                    Width::Two => {
                        operands.push(c.read_u16::<BigEndian>().map_err(Error::Io)? as usize)
                    }
                }
            }

            ins.stream.push((op, operands));
        }

        Ok(ins)
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, (op, operands)) in self.stream.iter().enumerate() {
            // TODO(mdlayher): add proper offsets and formatting.
            writeln!(f, "{:04} {:?} {:?}", i, op, operands)?;
        }
        Ok(())
    }
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
        Opcode::Pop => Definition {
            name: "Pop",
            operand_widths: vec![],
        },
        Opcode::Add => Definition {
            name: "Add",
            operand_widths: vec![],
        },
        Opcode::Sub => Definition {
            name: "Sub",
            operand_widths: vec![],
        },
        Opcode::Mul => Definition {
            name: "Mul",
            operand_widths: vec![],
        },
        Opcode::Div => Definition {
            name: "Div",
            operand_widths: vec![],
        },
        Opcode::Mod => Definition {
            name: "Mod",
            operand_widths: vec![],
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
        match self {
            Error::Io(err) => Some(err),
            _ => None,
        }
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
