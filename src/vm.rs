//! A virtual machine for the Monkey programming language from
//! <https://compilerbook.com/>.

extern crate byteorder;

use std::{
    error, fmt,
    io::{self, Seek},
    result,
};

use crate::{
    code::{BinaryOpcode, ControlOpcode, Opcode, UnaryOpcode},
    compiler,
    object::{self, Object},
};

use byteorder::{BigEndian, ReadBytesExt};

/// A virtual machine that can execute monkey bytecode.
pub struct Vm<'a> {
    stack: &'a mut Vec<Object>,
    sp: usize,
}

/// Creates a stack of a suitable size for use with `Vm::new`.
pub fn new_stack() -> Vec<Object> {
    vec![Object::Null; 64]
}

impl<'a> Vm<'a> {
    /// Creates a new `Vm` that uses the input stack.
    pub fn new(stack: &'a mut Vec<Object>) -> Self {
        Vm { stack, sp: 0 }
    }

    /// Returns the last `Object` popped off the stack after calling `run`.
    pub fn last_popped(&self) -> &Object {
        &self.stack[self.sp]
    }

    /// Creates a copy of the in-use portion of the stack.
    pub fn dump_stack(&self) -> Vec<Object> {
        self.stack.iter().take(self.sp).cloned().collect()
    }

    /// Runs the virtual machine with the input `compiler::Bytecode`.
    pub fn run(&mut self, bc: &compiler::Bytecode) -> Result<()> {
        let len = bc.instructions.len() as u64;
        let mut c = io::Cursor::new(&bc.instructions);

        while c.position() < len {
            let op = Opcode::from(c.read_u8().map_err(Error::Io)?);

            match op {
                Opcode::Control(ctrl) => self.control_op(&mut c, ctrl, &bc.constants)?,
                Opcode::Unary(u) => self.unary_op(u)?,
                Opcode::Binary(b) => self.binary_op(b)?,
            };
        }

        Ok(())
    }

    /// Executes a control operation.
    fn control_op(
        &mut self,
        c: &mut io::Cursor<&Vec<u8>>,
        op: ControlOpcode,
        consts: &[Object],
    ) -> Result<()> {
        match op {
            ControlOpcode::Constant => {
                let idx = c.read_u16::<BigEndian>().map_err(Error::Io)?;
                self.push(consts[idx as usize].clone());
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
            ControlOpcode::Jump => {
                // Immediately jump to the specified index.
                let idx = c.read_u16::<BigEndian>().map_err(Error::Io)?;
                c.seek(io::SeekFrom::Start(u64::from(idx)))
                    .map_err(Error::Io)?;
            }
            ControlOpcode::JumpNotTrue => {
                // Conditionally jump to the specified index if the
                // top value on the stack is not truthy.
                let idx = c.read_u16::<BigEndian>().map_err(Error::Io)?;

                let (start, end) = self.pop_n(1);
                let args = &self.stack[start..end];

                let truthy = match args[0] {
                    Object::Boolean(b) => b,
                    Object::Null => false,
                    _ => true,
                };
                if !truthy {
                    c.seek(io::SeekFrom::Start(u64::from(idx)))
                        .map_err(Error::Io)?;
                }
            }
            ControlOpcode::Null => {
                self.push(Object::Null);
            }
        };

        Ok(())
    }

    /// Executes a unary operation against one object.
    fn unary_op(&mut self, op: UnaryOpcode) -> Result<()> {
        let (start, end) = self.pop_n(1);
        let args = &self.stack[start..end];

        let out = match (op, &args[0]) {
            (UnaryOpcode::Not, _) => match args[0] {
                // Invert booleans directly.
                Object::Boolean(b) => Object::Boolean(!b),
                // !null is truthy.
                Object::Null => object::TRUE,
                // Anything else is considered truthy and should return false.
                _ => object::FALSE,
            },
            (UnaryOpcode::Negate, Object::Integer(i)) => Object::Integer(-i),
            // Invalid combination.
            (_, _) => {
                return Err(Error::bad_arguments(
                    BadArgumentKind::UnaryOperatorUnsupported,
                    Opcode::Unary(op),
                    &args,
                ));
            }
        };

        self.push(out);
        Ok(())
    }

    /// Executes a binary operation against two objects.
    fn binary_op(&mut self, op: BinaryOpcode) -> Result<()> {
        let (start, end) = self.pop_n(2);
        let args = &self.stack[start..end];

        match (&args[0], &args[1]) {
            // Integer operations.
            (Object::Integer(l), Object::Integer(r)) => {
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
            (Object::Boolean(l), Object::Boolean(r)) => {
                let out = match op {
                    BinaryOpcode::Equal => l == r,
                    BinaryOpcode::NotEqual => l != r,
                    _ => {
                        // No other binary operators are supported on booleans.
                        return Err(Error::bad_arguments(
                            BadArgumentKind::BinaryOperatorUnsupported,
                            Opcode::Binary(op),
                            args,
                        ));
                    }
                };

                self.push(Object::Boolean(out));
                Ok(())
            }
            // Float and Float/Integer operations. Integers are automatically
            // coerced into floats for operations, and all of these operations
            // will produce a float result.
            (Object::Float(l), Object::Float(r)) => {
                let obj = self.binary_float_op(op, args, *l, *r)?;
                self.push(obj);
                Ok(())
            }
            (Object::Integer(l), Object::Float(r)) => {
                let obj = self.binary_float_op(op, args, *l as f64, *r)?;
                self.push(obj);
                Ok(())
            }
            (Object::Float(l), Object::Integer(r)) => {
                let obj = self.binary_float_op(op, args, *l, *r as f64)?;
                self.push(obj);
                Ok(())
            }
            // Invalid combination.
            (_, _) => Err(Error::bad_arguments(
                BadArgumentKind::BinaryOperatorUnsupported,
                Opcode::Binary(op),
                args,
            )),
        }
    }

    /// Executes a binary float operation.
    // Does not take &mut self and push directly to stack to simplify borrowing.
    fn binary_float_op(&self, op: BinaryOpcode, args: &[Object], l: f64, r: f64) -> Result<Object> {
        let out = match op {
            BinaryOpcode::Add => Object::Float(l + r),
            BinaryOpcode::Sub => Object::Float(l - r),
            BinaryOpcode::Mul => Object::Float(l * r),
            BinaryOpcode::Div => Object::Float(l / r),
            BinaryOpcode::Mod => Object::Float(l % r),
            BinaryOpcode::GreaterThan => Object::Boolean(l > r),
            BinaryOpcode::Equal | BinaryOpcode::NotEqual => {
                // == and != are not supported with float arguments.
                return Err(Error::bad_arguments(
                    BadArgumentKind::BinaryOperatorUnsupported,
                    Opcode::Binary(op),
                    args,
                ));
            }
        };

        Ok(out)
    }

    /// Pushes an `Object` onto the stack, growing the stack if needed.
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

    /// Provides start and end indices for a slice of the stack that holds
    /// `n` elements.
    fn pop_n(&mut self, n: usize) -> (usize, usize) {
        if self.sp == 0 {
            // Nothing on the stack.
            panic!("stack is empty, cannot pop {} elements", n);
        }

        let start = self.sp.checked_sub(n).expect("stack pointer underflow");
        let end = self.sp;

        assert_eq!(
            end - start,
            n,
            "must return indices for {} elements from stack",
            n
        );

        self.sp -= n;

        (start, end)
    }
}

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
    fn bad_arguments(kind: BadArgumentKind, op: Opcode, args: &[Object]) -> Self {
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
        }
    }
}
