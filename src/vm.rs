//! A virtual machine for the Monkey programming language from
//! <https://compilerbook.com/>.

extern crate byteorder;

use std::{error, fmt, io, result};

use crate::{
    code::{BinaryOpcode, ControlOpcode, Opcode, UnaryOpcode},
    compiler,
    object::{self, Object},
};

use byteorder::{BigEndian, ReadBytesExt};

pub struct Vm<'a> {
    stack: &'a mut Vec<Object>,
    sp: usize,
}

pub fn new_stack() -> Vec<Object> {
    // TODO(mdlayher): growable stacks.
    vec![Object::Null; 64]
}

impl<'a> Vm<'a> {
    pub fn new(stack: &'a mut Vec<Object>) -> Self {
        Vm { stack, sp: 0 }
    }

    pub fn last_popped(&self) -> &Object {
        &self.stack[self.sp]
    }

    pub fn dump_stack(&self) -> Vec<Object> {
        self.stack.iter().take(self.sp).cloned().collect()
    }

    pub fn run(&mut self, bc: &compiler::Bytecode) -> Result<()> {
        let len = bc.instructions.len() as u64;
        let mut c = io::Cursor::new(&bc.instructions);

        while c.position() < len {
            let op = Opcode::from(c.read_u8().map_err(Error::Io)?);

            match op {
                Opcode::Control(ctrl) => match ctrl {
                    ControlOpcode::Constant => {
                        let idx = c.read_u16::<BigEndian>().map_err(Error::Io)?;
                        self.push(bc.constants[idx as usize].clone());
                    }
                    ControlOpcode::Pop => {
                        self.pop_n(1);
                    }
                    ControlOpcode::True => {
                        self.push(object::TRUE);
                    }
                    ControlOpcode::False => {
                        self.push(object::FALSE);
                    }
                    _ => unimplemented!(),
                },
                Opcode::Unary(u) => self.unary_op(u)?,
                Opcode::Binary(b) => self.binary_op(b)?,
            };
        }

        Ok(())
    }

    fn unary_op(&mut self, op: UnaryOpcode) -> Result<()> {
        let args = self
            .pop_n(1)
            .expect("stack did not have enough elements for unary operation");

        let out = match (op, &args[0]) {
            (UnaryOpcode::Not, _) => match args[0] {
                Object::Boolean(b) => Object::Boolean(!b),
                // According to the compiler book, all non-boolean false
                // values are considered truthy and should return false.
                _ => object::FALSE,
            },
            (UnaryOpcode::Negate, Object::Integer(i)) => Object::Integer(-i),
            // Invalid combination.
            (_, _) => return Err(Error::Internal(ErrorKind::BadArguments)),
        };

        self.push(out);
        Ok(())
    }

    fn binary_op(&mut self, op: BinaryOpcode) -> Result<()> {
        let args = self
            .pop_n(2)
            .expect("stack did not have enough elements for binary operation");

        match (&args[0], &args[1]) {
            // Integer operations.
            (Object::Integer(r), Object::Integer(l)) => {
                let out = match op {
                    BinaryOpcode::Add => Object::Integer(l + r),
                    BinaryOpcode::Sub => Object::Integer(l - r),
                    BinaryOpcode::Mul => Object::Integer(l * r),
                    BinaryOpcode::Div => Object::Integer(l / r),
                    BinaryOpcode::Mod => Object::Integer(l % r),
                    BinaryOpcode::Equal => Object::Boolean(l == r),
                    BinaryOpcode::NotEqual => Object::Boolean(l != r),
                    BinaryOpcode::GreaterThan => Object::Boolean(l > r),
                };

                self.push(out);
                Ok(())
            }
            // Boolean operations.
            (Object::Boolean(r), Object::Boolean(l)) => {
                let out = match op {
                    BinaryOpcode::Equal => l == r,
                    BinaryOpcode::NotEqual => l != r,
                    _ => panic!("unhandled boolean binary op: {:?}", op),
                };

                self.push(Object::Boolean(out));
                Ok(())
            }
            // Invalid combination.
            (_, _) => Err(Error::Internal(ErrorKind::BadArguments)),
        }
    }

    fn push(&mut self, obj: Object) {
        if self.sp >= self.stack.len() {
            // Grow the stack to push this element.
            self.stack.push(obj);
        } else {
            // Use an existing slot on the stack.
            self.stack[self.sp] = obj;
        }

        self.sp += 1;
    }

    fn pop_n(&mut self, n: usize) -> Option<Vec<Object>> {
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
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::BadArguments => write!(f, "bad arguments for operation"),
        }
    }
}
