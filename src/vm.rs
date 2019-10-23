//! A virtual machine for the Monkey programming language from
//! <https://compilerbook.com/>.

extern crate byteorder;

use std::{
    error, fmt,
    io::{self, Seek},
    result,
};

use crate::{
    code::{BinaryOpcode, CompositeOpcode, ControlOpcode, Opcode, UnaryOpcode},
    compiler,
    object::{self, Object},
};

use byteorder::{BigEndian, ReadBytesExt};

/// A virtual machine that can execute monkey bytecode.
pub struct Vm<'a> {
    stack: &'a mut Vec<Object>,
    sp: usize,
    globals: Vec<Object>,
}

/// Creates a stack of a suitable size for use with `Vm::new`.
pub fn new_stack() -> Vec<Object> {
    vec![Object::Null; 64]
}

impl<'a> Vm<'a> {
    /// Creates a new `Vm` that uses the input stack.
    pub fn new(stack: &'a mut Vec<Object>) -> Self {
        Vm {
            stack,
            sp: 0,
            // TODO: grow globals vector as needed.
            globals: vec![Object::Null; 64],
        }
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
    pub fn run(&mut self, bc: compiler::Bytecode) -> Result<()> {
        let mut ctx = RunContext::new(bc);
        while ctx.run() {
            let op = Opcode::from(ctx.read_u8()?);

            match op {
                Opcode::Control(ctrl) => self.control_op(&mut ctx, ctrl)?,
                Opcode::Unary(u) => self.unary_op(u)?,
                Opcode::Binary(b) => self.binary_op(b)?,
                Opcode::Composite(com) => self.composite_op(&mut ctx, com)?,
            };
        }

        Ok(())
    }

    /// Executes a control operation.
    fn control_op(&mut self, ctx: &mut RunContext, op: ControlOpcode) -> Result<()> {
        match op {
            ControlOpcode::Constant => {
                let idx = ctx.read_u16()?;
                self.push(ctx.consts[idx as usize].clone());
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
                let idx = ctx.read_u16()?;
                ctx.seek(io::SeekFrom::Start(u64::from(idx)))?;
            }
            ControlOpcode::JumpNotTrue => {
                // Conditionally jump to the specified index if the
                // top value on the stack is not truthy.
                let idx = ctx.read_u16()?;

                let (start, end) = self.pop_n(1);
                let args = &self.stack[start..end];

                let truthy = match args[0] {
                    Object::Boolean(b) => b,
                    Object::Null => false,
                    _ => true,
                };
                if !truthy {
                    ctx.seek(io::SeekFrom::Start(u64::from(idx)))?;
                }
            }
            ControlOpcode::Null => {
                self.push(Object::Null);
            }
            ControlOpcode::GetGlobal => {
                // Read a global from the operand's specified index and push
                // its value onto the stack.
                let idx = ctx.read_u16()?;
                self.push(self.globals[idx as usize].clone());
            }
            ControlOpcode::SetGlobal => {
                // Just need the index of the single element.
                let (start, _) = self.pop_n(1);

                // Register a new global with the operand's specified index.
                let idx = ctx.read_u16()?;
                self.globals[idx as usize] = self.stack[start].clone();
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
            // String operations.
            (Object::String(l), Object::String(r)) => {
                // Only concatentation is supported on strings.
                if op == BinaryOpcode::Add {
                    let out = Object::String(l.clone() + r);
                    self.push(out);
                    Ok(())
                } else {
                    Err(Error::bad_arguments(
                        BadArgumentKind::BinaryOperatorUnsupported,
                        Opcode::Binary(op),
                        args,
                    ))
                }
            }
            // Invalid combination.
            (_, _) => Err(Error::bad_arguments(
                BadArgumentKind::BinaryOperatorUnsupported,
                Opcode::Binary(op),
                args,
            )),
        }
    }

    /// Executes a composite operation against multiple objects.
    fn composite_op(&mut self, ctx: &mut RunContext, op: CompositeOpcode) -> Result<()> {
        // Read the appropriate number of elements from the stack as indicated
        // by the composite opcode's operand.
        let n = match op {
            CompositeOpcode::Array => ctx.read_u16()?,
        };

        let (start, end) = self.pop_n(n as usize);
        let args = &self.stack[start..end];

        // Copy each argument into elements to return as an array object.
        let mut elements = Vec::with_capacity(args.len());
        for a in args {
            // The stack owns these elements so we must clone them to add
            // array elements.
            elements.push(a.clone())
        }

        self.push(Object::Array(object::Array { elements }));
        Ok(())
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
        if self.sp == 0 && n > 0 {
            // Tried to pop elements, but nothing is on the stack.
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

/// Contains context for a running `Vm` bytecode program.
struct RunContext {
    ins_len: u64,
    c: io::Cursor<Vec<u8>>,
    consts: Vec<Object>,
}

impl RunContext {
    /// Produces a `RunContext` from input `compiler::Bytecode`.
    fn new(bc: compiler::Bytecode) -> Self {
        RunContext {
            ins_len: bc.instructions.len() as u64,
            c: io::Cursor::new(bc.instructions),
            consts: bc.constants,
        }
    }

    /// Determines if more instructions can be read and executed.
    fn run(&self) -> bool {
        self.c.position() < self.ins_len
    }

    /// Seeks to the specified location in the instructions stream.
    fn seek(&mut self, pos: io::SeekFrom) -> Result<u64> {
        self.c.seek(pos).map_err(Error::Io)
    }

    /// Reads a `u8` value from the instructions stream.
    fn read_u8(&mut self) -> Result<u8> {
        self.c.read_u8().map_err(Error::Io)
    }

    /// Reads a `u16` value from the instructions stream.
    fn read_u16(&mut self) -> Result<u16> {
        self.c.read_u16::<BigEndian>().map_err(Error::Io)
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
