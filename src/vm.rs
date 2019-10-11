//! A virtual machine for the Monkey programming language from
//! <https://compilerbook.com/>.

extern crate byteorder;

use std::{error, fmt, io, result};

use crate::{code, compiler, object};

use byteorder::{BigEndian, ReadBytesExt};

pub struct Vm<'a> {
    stack: &'a mut Vec<object::Object>,
    sp: usize,
}

pub fn new_stack() -> Vec<object::Object> {
    // TODO(mdlayher): growable stacks.
    vec![object::Object::Null; 64]
}

impl<'a> Vm<'a> {
    pub fn new(stack: &'a mut Vec<object::Object>) -> Self {
        Vm { stack, sp: 0 }
    }

    pub fn last_popped(&self) -> &object::Object {
        &self.stack[self.sp]
    }

    pub fn dump_stack(&self) -> Vec<object::Object> {
        self.stack.iter().take(self.sp).cloned().collect()
    }

    pub fn run(&mut self, bc: &compiler::Bytecode) -> Result<()> {
        let len = bc.instructions.len() as u64;
        let mut c = io::Cursor::new(&bc.instructions);

        while c.position() < len {
            let op = code::Opcode::from(c.read_u8().map_err(Error::Io)?);

            match op {
                code::Opcode::Constant => {
                    let idx = c.read_u16::<BigEndian>().map_err(Error::Io)?;
                    self.push(bc.constants[idx as usize].clone())?;
                }
                // TODO(mdlayher): split opcodes by category so the compiler
                // can enforce which ones are binary ops and etc?
                code::Opcode::Add
                | code::Opcode::Sub
                | code::Opcode::Mul
                | code::Opcode::Div
                | code::Opcode::Mod => {
                    self.binary_op(op)?;
                }
                code::Opcode::Pop => {
                    self.pop_n(1);
                }
            };
        }

        Ok(())
    }

    fn binary_op(&mut self, op: code::Opcode) -> Result<()> {
        let args = self.pop_n(2);
        match args {
            Some(args) => match (&args[0], &args[1]) {
                (object::Object::Integer(r), object::Object::Integer(l)) => {
                    // Integer operation.
                    let out = match op {
                        code::Opcode::Add => l + r,
                        code::Opcode::Sub => l - r,
                        code::Opcode::Mul => l * r,
                        code::Opcode::Div => l / r,
                        code::Opcode::Mod => l % r,
                        _ => panic!("unhandled binary op: {:?}", op),
                    };

                    self.push(object::Object::Integer(out))?;
                }
                (_, _) => return Err(Error::Internal(ErrorKind::BadArguments)),
            },
            None => return Err(Error::Internal(ErrorKind::StackEmpty)),
        };

        Ok(())
    }

    fn push(&mut self, obj: object::Object) -> Result<()> {
        if self.sp >= self.stack.len() {
            return Err(Error::Internal(ErrorKind::StackOverflow));
        }

        self.stack[self.sp] = obj;
        self.sp += 1;
        Ok(())
    }

    fn pop_n(&mut self, n: usize) -> Option<Vec<object::Object>> {
        if self.sp == 0 {
            // Nothing on the stack, nothing to do.
            return None;
        }

        let mut out = Vec::with_capacity(n);
        for _ in 0..n {
            if self.sp == 0 {
                // Ran out of elements on the stack.
                return None;
            }

            // Clone this element and send it back to the caller. Because we
            // reuse the stack space for output, cloning is a safer option than
            // using references.
            out.push(self.stack[self.sp - 1].clone());
            self.sp -= 1;
        }

        // Must always return the requested number of elements.
        assert!(out.len() == n);

        Some(out)
    }
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    Internal(ErrorKind),
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Internal(kind) => write!(f, "internal VM error: {}", kind),
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
    BadArguments, // TODO(mdlayher): more debug information.
    StackEmpty,
    StackOverflow,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::BadArguments => write!(f, "bad arguments for operation"),
            ErrorKind::StackEmpty => write!(f, "stack empty while performing operation"),
            ErrorKind::StackOverflow => write!(f, "stack overflow"),
        }
    }
}
