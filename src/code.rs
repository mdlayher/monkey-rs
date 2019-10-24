//! Bytecode representation for the Monkey programming language from
//! <https://compilerbook.com/>.

extern crate byteorder;

use std::{error, fmt, io, result};

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

// Note: Opcode is a small enough type that clippy recommends moving/copying
// it rathe than borrowing it.

/// An opcode for the Monkey virtual machine.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Opcode {
    Control(ControlOpcode),
    Unary(UnaryOpcode),
    Binary(BinaryOpcode),
    Composite(CompositeOpcode),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ControlOpcode {
    Constant = 0x00,
    Pop = 0x01,
    True = 0x02,
    False = 0x03,
    JumpNotTrue = 0x04,
    Jump = 0x05,
    Null = 0x06,
    GetGlobal = 0x07,
    SetGlobal = 0x08,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOpcode {
    Negate = 0x10,
    Not = 0x11,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOpcode {
    Add = 0x20,
    Sub = 0x21,
    Mul = 0x22,
    Div = 0x23,
    Mod = 0x24,
    Equal = 0x25,
    NotEqual = 0x26,
    GreaterThan = 0x27,
    Index = 0x28,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CompositeOpcode {
    Array = 0x30,
    Hash = 0x31,
    Set = 0x32,
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Control(c) => c.fmt(f),
            Self::Unary(u) => u.fmt(f),
            Self::Binary(b) => b.fmt(f),
            Self::Composite(c) => c.fmt(f),
        }
    }
}

impl From<u8> for Opcode {
    /// Convert from a u8 to an Opcode.
    fn from(v: u8) -> Self {
        match v {
            0x00..=0x0f => Self::Control(ControlOpcode::from(v)),
            0x10..=0x1f => Self::Unary(UnaryOpcode::from(v)),
            0x20..=0x2f => Self::Binary(BinaryOpcode::from(v)),
            0x30..=0x3f => Self::Composite(CompositeOpcode::from(v)),
            _ => panic!("unhandled u8 to Opcode conversion: {}", v),
        }
    }
}

impl fmt::Display for ControlOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Constant => write!(f, "CONST"),
            Self::Pop => write!(f, "POP"),
            Self::True => write!(f, "TRUE"),
            Self::False => write!(f, "FALSE"),
            Self::JumpNotTrue => write!(f, "JUMP NOT TRUE"),
            Self::Jump => write!(f, "JUMP"),
            Self::Null => write!(f, "NULL"),
            Self::GetGlobal => write!(f, "GET GLOBAL"),
            Self::SetGlobal => write!(f, "SET GLOBAL"),
        }
    }
}

impl From<u8> for ControlOpcode {
    /// Convert from a u8 to an Opcode.
    fn from(v: u8) -> Self {
        match v {
            0x00 => ControlOpcode::Constant,
            0x01 => ControlOpcode::Pop,
            0x02 => ControlOpcode::True,
            0x03 => ControlOpcode::False,
            0x04 => ControlOpcode::JumpNotTrue,
            0x05 => ControlOpcode::Jump,
            0x06 => ControlOpcode::Null,
            0x07 => ControlOpcode::GetGlobal,
            0x08 => ControlOpcode::SetGlobal,
            _ => panic!("unhandled u8 to ControlOpcode conversion: {}", v),
        }
    }
}

impl fmt::Display for UnaryOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Negate => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

impl From<u8> for UnaryOpcode {
    /// Convert from a u8 to an Opcode.
    fn from(v: u8) -> Self {
        match v {
            0x10 => UnaryOpcode::Negate,
            0x11 => UnaryOpcode::Not,
            _ => panic!("unhandled u8 to UnaryOpcode conversion: {}", v),
        }
    }
}

impl fmt::Display for BinaryOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::GreaterThan => write!(f, ">"),
            Self::Index => write!(f, "[]"),
        }
    }
}

impl From<u8> for BinaryOpcode {
    /// Convert from a u8 to an Opcode.
    fn from(v: u8) -> Self {
        match v {
            0x20 => BinaryOpcode::Add,
            0x21 => BinaryOpcode::Sub,
            0x22 => BinaryOpcode::Mul,
            0x23 => BinaryOpcode::Div,
            0x24 => BinaryOpcode::Mod,
            0x25 => BinaryOpcode::Equal,
            0x26 => BinaryOpcode::NotEqual,
            0x27 => BinaryOpcode::GreaterThan,
            0x28 => BinaryOpcode::Index,
            _ => panic!("unhandled u8 to BinaryOpcode conversion: {}", v),
        }
    }
}

impl fmt::Display for CompositeOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Array => write!(f, "[]"),
            Self::Hash => write!(f, "{{}}"),
            Self::Set => write!(f, "set{{}}"),
        }
    }
}

impl From<u8> for CompositeOpcode {
    /// Convert from a u8 to an Opcode.
    fn from(v: u8) -> Self {
        match v {
            0x30 => CompositeOpcode::Array,
            0x31 => CompositeOpcode::Hash,
            0x32 => CompositeOpcode::Set,
            _ => panic!("unhandled u8 to CompositeOpcode conversion: {}", v),
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
            op,
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

    let byte = match op {
        Opcode::Control(c) => c as u8,
        Opcode::Unary(u) => u as u8,
        Opcode::Binary(b) => b as u8,
        Opcode::Composite(c) => c as u8,
    };
    buf.push(byte);

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
    // A stream of instructions with their byte index, opcode, and operands.
    pub stream: Vec<(usize, Opcode, Vec<usize>)>,
}

impl Instructions {
    /// Parses a bytecode stream to produce `Instructions`.
    pub fn parse(buf: &[u8]) -> Result<Self> {
        let mut c = io::Cursor::new(buf);
        let mut ins = Instructions { stream: vec![] };

        // Keep reading until we hit the end of the stream.
        let mut idx = 0;
        while c.position() < buf.len() as u64 {
            // Track the position of each operation in the stream.
            let op_idx = idx;
            let op = Opcode::from(c.read_u8().map_err(Error::Io)?);
            idx += 1;

            let def = lookup(op);

            let mut operands = Vec::with_capacity(def.operand_widths.len());
            for w in def.operand_widths {
                idx += w as usize;
                match w {
                    Width::Two => {
                        operands.push(c.read_u16::<BigEndian>().map_err(Error::Io)? as usize)
                    }
                }
            }

            ins.stream.push((op_idx, op, operands));
        }

        Ok(ins)
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, op, operands) in &self.stream {
            writeln!(f, "{:04} {:?} {:?}", idx, op, operands)?;
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
        Opcode::Control(c) => match c {
            ControlOpcode::Constant => Definition {
                name: "Constant",
                operand_widths: vec![Width::Two],
            },
            ControlOpcode::Pop => Definition {
                name: "Pop",
                operand_widths: vec![],
            },
            ControlOpcode::True => Definition {
                name: "True",
                operand_widths: vec![],
            },
            ControlOpcode::False => Definition {
                name: "False",
                operand_widths: vec![],
            },
            ControlOpcode::JumpNotTrue => Definition {
                name: "JumpNotTrue",
                operand_widths: vec![Width::Two],
            },
            ControlOpcode::Jump => Definition {
                name: "Jump",
                operand_widths: vec![Width::Two],
            },
            ControlOpcode::Null => Definition {
                name: "Null",
                operand_widths: vec![],
            },
            ControlOpcode::GetGlobal => Definition {
                name: "GetGlobal",
                operand_widths: vec![Width::Two],
            },
            ControlOpcode::SetGlobal => Definition {
                name: "SetGlobal",
                operand_widths: vec![Width::Two],
            },
        },
        Opcode::Unary(u) => match u {
            UnaryOpcode::Negate => Definition {
                name: "Negate",
                operand_widths: vec![],
            },
            UnaryOpcode::Not => Definition {
                name: "Not",
                operand_widths: vec![],
            },
        },
        Opcode::Binary(b) => match b {
            BinaryOpcode::Add => Definition {
                name: "Add",
                operand_widths: vec![],
            },
            BinaryOpcode::Sub => Definition {
                name: "Sub",
                operand_widths: vec![],
            },
            BinaryOpcode::Mul => Definition {
                name: "Mul",
                operand_widths: vec![],
            },
            BinaryOpcode::Div => Definition {
                name: "Div",
                operand_widths: vec![],
            },
            BinaryOpcode::Mod => Definition {
                name: "Mod",
                operand_widths: vec![],
            },
            BinaryOpcode::Equal => Definition {
                name: "Equal",
                operand_widths: vec![],
            },
            BinaryOpcode::NotEqual => Definition {
                name: "NotEqual",
                operand_widths: vec![],
            },
            BinaryOpcode::GreaterThan => Definition {
                name: "GreaterThan",
                operand_widths: vec![],
            },
            BinaryOpcode::Index => Definition {
                name: "Index",
                operand_widths: vec![],
            },
        },
        Opcode::Composite(c) => match c {
            CompositeOpcode::Array => Definition {
                name: "Array",
                operand_widths: vec![Width::Two],
            },
            CompositeOpcode::Hash => Definition {
                name: "Hash",
                operand_widths: vec![Width::Two],
            },
            CompositeOpcode::Set => Definition {
                name: "Set",
                operand_widths: vec![Width::Two],
            },
        },
    }
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    Internal { op: Opcode, kind: ErrorKind },
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Internal { op, kind } => write!(f, "internal error: opcode {:?}: {}", op, kind),
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
