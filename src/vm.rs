//! A virtual machine for the Monkey programming language from
//! <https://compilerbook.com/>.

extern crate byteorder;

use std::{
    collections::{BTreeMap, BTreeSet},
    convert::TryFrom,
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
pub struct Vm {
    stack: Vec<Object>,
    sp: usize,
    heap: Vec<Object>,
    globals: Vec<Object>,
    consts: Vec<Object>,
    frames: FrameStack,
}

impl Vm {
    /// Creates a new `Vm` that uses the input stack.
    pub fn new(bc: compiler::Bytecode) -> Self {
        Self::with_stack_size(bc, 16)
    }

    /// Creates a new `Vm` with the specified initial stack size.
    pub fn with_stack_size(bc: compiler::Bytecode, n: usize) -> Self {
        // Create a main function and associated frame with a frame pointer
        // value of 0.
        let main = Frame::new(
            object::CompiledFunction {
                instructions: bc.instructions,
                // main has no local bindings.
                num_locals: 0,
            },
            // Stack/frame pointer starts at 0 for main.
            0,
        );

        Vm {
            stack: vec![Object::Null; n],
            sp: 0,
            // TODO: grow globals vector as needed.
            globals: vec![Object::Null; 64],
            consts: bc.constants,
            heap: vec![],
            frames: FrameStack(vec![main]),
        }
    }

    /// Returns the last `Object` popped off the stack after calling `run`.
    pub fn last_popped(&self) -> &Object {
        &self.stack[self.sp]
    }

    /// Creates a copy of the stack.
    pub fn dump_stack(&self) -> Vec<Object> {
        self.stack.to_vec()
    }

    /// Creates a copy of the heap.
    pub fn dump_heap(&self) -> Vec<Object> {
        self.heap.to_vec()
    }

    /// Runs the virtual machine.
    pub fn run(&mut self) -> Result<()> {
        // Run until there are no more instructions.
        while self.frames.run() {
            let op = Opcode::from(self.frames.current_mut().read_u8()?);

            match op {
                Opcode::Control(ctrl) => self.control_op(ctrl)?,
                Opcode::Unary(u) => self.unary_op(u)?,
                Opcode::Binary(b) => self.binary_op(b)?,
                Opcode::Composite(com) => self.composite_op(com)?,
            };
        }

        Ok(())
    }

    /// Executes a control operation.
    fn control_op(&mut self, op: ControlOpcode) -> Result<()> {
        match op {
            ControlOpcode::Constant => {
                let idx = self.frames.current_mut().read_u16()?;
                self.push(self.consts[idx as usize].clone());
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
                let ctx = self.frames.current_mut();
                let idx = ctx.read_u16()?;
                ctx.seek(io::SeekFrom::Start(u64::from(idx)))?;
            }
            ControlOpcode::JumpNotTrue => {
                // Conditionally jump to the specified index if the
                // top value on the stack is not truthy.
                let idx = self.frames.current_mut().read_u16()?;

                let (start, end) = self.pop_n(1);
                let args = &self.stack[start..end];

                let truthy = match args[0] {
                    Object::Boolean(b) => b,
                    Object::Null => false,
                    _ => true,
                };
                if !truthy {
                    self.frames
                        .current_mut()
                        .seek(io::SeekFrom::Start(u64::from(idx)))?;
                }
            }
            ControlOpcode::Null => {
                self.push(Object::Null);
            }
            ControlOpcode::GetGlobal => {
                // Read a global from the operand's specified index and push
                // its value onto the stack.
                let idx = self.frames.current_mut().read_u16()?;
                self.push(self.globals[idx as usize].clone());
            }
            ControlOpcode::SetGlobal => {
                // Just need the index of the single element.
                let (start, _) = self.pop_n(1);

                // Register a new global with the operand's specified index.
                let ctx = self.frames.current_mut();
                let idx = ctx.read_u16()?;
                self.globals[idx as usize] = self.stack[start].clone();
            }
            ControlOpcode::Call => {
                // Just need the index of the single element.
                let (start, _) = self.pop_n(1);
                let obj = &self.stack[start];

                let func = match obj {
                    Object::CompiledFunction(f) => f,
                    _ => return Err(Error::Runtime(ErrorKind::BadFunctionCall(obj.clone()))),
                };

                // Push a new frame and set its frame pointer to the current
                // stack pointer.
                let frame = Frame::new(func.clone(), self.sp);
                self.sp = frame.fp + frame.num_locals;
                self.frames.push(frame);
            }
            ControlOpcode::ReturnValue => {
                let (i, _) = self.pop_n(1);
                let ret = self.stack[i].clone();

                let f = self.frames.pop();
                self.sp = f.fp;

                // NB: the book specifies an extra pop_n(1) here and in Return,
                // but it seems that our VM is already popping the function off
                // the stack. Perhaps this is a slight error in the compiler,
                // but it seems to work.
                self.push(ret);
            }
            ControlOpcode::Return => {
                let f = self.frames.pop();
                self.sp = f.fp;

                // NB: see above note in ReturnValue.
                self.push(Object::Null);
            }
            ControlOpcode::SetPointer => {
                // Just need the index of the single element.
                let (i, _) = self.pop_n(1);

                // Overwrite the element at the specified index in the heap.
                let idx = self.frames.current_mut().read_u16()?;
                dbg!(&self.heap);
                self.heap[idx as usize] = self.stack[i].clone();
            }
            ControlOpcode::SetLocal => {
                let f = self.frames.current_mut();
                let i = f.fp + f.read_u8()? as usize;

                let (start, _) = self.pop_n(1);

                self.stack[i] = self.stack[start].clone();
            }
            ControlOpcode::GetLocal => {
                let f = self.frames.current_mut();
                let i = f.fp + f.read_u8()? as usize;

                self.push(self.stack[i].clone());
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
            (UnaryOpcode::Address, v) => {
                self.heap.push(v.clone());
                Object::Pointer(self.heap.len() - 1)
            }
            (UnaryOpcode::Dereference, Object::Pointer(p)) => match self.heap.get(*p) {
                Some(v) => v.clone(),
                None => Object::Null,
            },
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
                    BinaryOpcode::Index => {
                        // Indexing not supported on integers.
                        return Err(Error::bad_arguments(
                            BadArgumentKind::BinaryOperatorUnsupported,
                            Opcode::Binary(op),
                            args,
                        ));
                    }
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
                let obj = Self::binary_float_op(op, args, *l, *r)?;
                self.push(obj);
                Ok(())
            }
            (Object::Integer(l), Object::Float(r)) => {
                let obj = Self::binary_float_op(op, args, *l as f64, *r)?;
                self.push(obj);
                Ok(())
            }
            (Object::Float(l), Object::Integer(r)) => {
                let obj = Self::binary_float_op(op, args, *l, *r as f64)?;
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
            // Pointer arithmetic with integer operations.
            (Object::Integer(l), Object::Pointer(r)) => {
                let obj = Self::pointer_arithmetic_op(op, args, *l, *r as i64)?;
                self.push(obj);
                Ok(())
            }
            (Object::Pointer(l), Object::Integer(r)) => {
                let obj = Self::pointer_arithmetic_op(op, args, *l as i64, *r)?;
                self.push(obj);
                Ok(())
            }
            // Set operations.
            (Object::Set(l), Object::Set(r)) => {
                let obj = Self::binary_set_op(op, args, l, r)?;
                self.push(obj);
                Ok(())
            }
            // Composite literal indexing operations.
            (Object::Array(_), _) | (Object::Hash(_), _) | (Object::Set(_), _) => {
                // Only indexing is supported on these composite literals.
                if op == BinaryOpcode::Index {
                    let out = Self::composite_index_op(args)?;
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
    fn composite_op(&mut self, op: CompositeOpcode) -> Result<()> {
        // Read the appropriate number of elements from the stack as indicated
        // by the composite opcode's operand.
        let n = self.frames.current_mut().read_u16()? as usize;

        let (start, end) = self.pop_n(n);
        let args = &self.stack[start..end];

        let out = match op {
            CompositeOpcode::Array => Object::Array(object::Array {
                // Clone args off the stack for the output object.
                elements: args.to_vec(),
            }),
            CompositeOpcode::Hash => Self::build_hash(args)?,
            CompositeOpcode::Set => {
                let mut set = BTreeSet::new();

                for a in args {
                    // Only accept object::Hashable objects as keys.
                    let arg = object::Hashable::try_from(a)
                        .map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

                    // Do not allow duplicate keys in the set.
                    if set.contains(&arg) {
                        return Err(Error::Runtime(ErrorKind::DuplicateKey(arg)));
                    }
                    set.insert(arg);
                }

                Object::Set(object::Set { set })
            }
        };

        self.push(out);
        Ok(())
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

    /// Executes a binary float operation.
    fn binary_float_op(op: BinaryOpcode, args: &[Object], l: f64, r: f64) -> Result<Object> {
        let out = match op {
            BinaryOpcode::Add => Object::Float(l + r),
            BinaryOpcode::Sub => Object::Float(l - r),
            BinaryOpcode::Mul => Object::Float(l * r),
            BinaryOpcode::Div => Object::Float(l / r),
            BinaryOpcode::Mod => Object::Float(l % r),
            BinaryOpcode::GreaterThan => Object::Boolean(l > r),
            BinaryOpcode::Equal | BinaryOpcode::NotEqual | BinaryOpcode::Index => {
                // Operator not supported with float arguments.
                return Err(Error::bad_arguments(
                    BadArgumentKind::BinaryOperatorUnsupported,
                    Opcode::Binary(op),
                    args,
                ));
            }
        };

        Ok(out)
    }

    /// Builds a Hash from stack objects.
    fn build_hash(args: &[Object]) -> Result<Object> {
        if args.is_empty() {
            // No arguments, empty hash.
            return Ok(Object::Hash(object::Hash::default()));
        }

        // The parser and compiler should make this assertion hold.
        assert!(
            args.len() % 2 == 0,
            "hash must contain an even number of objects"
        );

        // Iterate two objects at a time for each key/value pair.
        let mut pairs = BTreeMap::new();

        let mut i = 0;
        while i < args.len() {
            // Only accept object::Hashable objects as keys.
            let k = object::Hashable::try_from(&args[i])
                .map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

            // Do not allow duplicate hash keys.
            if pairs.get(&k).is_some() {
                return Err(Error::Runtime(ErrorKind::DuplicateKey(k)));
            }

            let v = args[i + 1].clone();
            i += 2;

            pairs.insert(k, v);
        }

        Ok(Object::Hash(object::Hash { pairs }))
    }

    /// Executes a binary set operation.
    fn binary_set_op(
        op: BinaryOpcode,
        args: &[Object],
        l: &object::Set,
        r: &object::Set,
    ) -> Result<Object> {
        // Check for subset.
        if op == BinaryOpcode::GreaterThan {
            return Ok(if r.set.is_subset(&l.set) {
                object::TRUE
            } else {
                object::FALSE
            });
        }

        // TODO(mdlayher): work out the operator situation here, but this
        // is good enough for now.
        let set: BTreeSet<_> = match op {
            BinaryOpcode::Add => l.set.union(&r.set).cloned().collect(),
            BinaryOpcode::Sub => l.set.difference(&r.set).cloned().collect(),
            BinaryOpcode::Mul => l.set.intersection(&r.set).cloned().collect(),
            BinaryOpcode::Div => l.set.symmetric_difference(&r.set).cloned().collect(),
            _ => {
                return Err(Error::bad_arguments(
                    BadArgumentKind::BinaryOperatorUnsupported,
                    Opcode::Binary(op),
                    args,
                ));
            }
        };

        Ok(Object::Set(object::Set { set }))
    }

    /// Executes a pointer arithmetic operation.
    fn pointer_arithmetic_op(op: BinaryOpcode, args: &[Object], l: i64, r: i64) -> Result<Object> {
        let out = match op {
            BinaryOpcode::Add => l + r,
            BinaryOpcode::Sub => l - r,
            _ => {
                // Reign in the pointer arithmetic madness!
                return Err(Error::bad_arguments(
                    BadArgumentKind::BinaryOperatorUnsupported,
                    Opcode::Binary(op),
                    args,
                ));
            }
        };

        Ok(Object::Pointer(out as usize))
    }

    /// Executes a composite indexing operation.
    fn composite_index_op(args: &[Object]) -> Result<Object> {
        assert_eq!(
            args.len(),
            2,
            "expected exactly 2 arguments for composite indexing operation"
        );

        match (&args[0], &args[1]) {
            // Array with numeric index.
            (Object::Array(a), Object::Integer(i)) => {
                // Is the element in bounds? If not, return null.
                if *i >= 0 && (*i as usize) < a.elements.len() {
                    Ok(a.elements[*i as usize].clone())
                } else {
                    Ok(Object::Null)
                }
            }
            // Hash with some type of index.
            (object::Object::Hash(h), k) => {
                let k = object::Hashable::try_from(k)
                    .map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

                // Does the element exist? If not, return null.
                if let Some(v) = h.pairs.get(&k) {
                    Ok(v.clone())
                } else {
                    Ok(Object::Null)
                }
            }
            // Set with some type of index.
            (object::Object::Set(s), k) => {
                let k = object::Hashable::try_from(k)
                    .map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

                // Does the element exist in the set?
                if s.set.contains(&k) {
                    Ok(object::TRUE)
                } else {
                    Ok(object::FALSE)
                }
            }
            _ => Err(Error::bad_arguments(
                BadArgumentKind::BinaryOperatorUnsupported,
                Opcode::Binary(BinaryOpcode::Index),
                args,
            )),
        }
    }
}

/// Stores a stack of stack frames that can track function execution.
struct FrameStack(Vec<Frame>);

impl FrameStack {
    /// Produces a `&Frame` of the current frame on the stack.
    fn current(&self) -> &Frame {
        self.0.last().expect("must not be none")
    }

    /// Produces a `&mut Frame` of the current frame on the stack.
    fn current_mut(&mut self) -> &mut Frame {
        self.0.last_mut().expect("must not be none")
    }

    /// Pushes a new `Frame` onto the stack.
    fn push(&mut self, f: Frame) {
        self.0.push(f);
    }

    /// Pops a `Frame` off the stack.
    fn pop(&mut self) -> Frame {
        self.0.pop().expect("must not be none")
    }

    /// Determines if more instructions can be read and executed.
    fn run(&self) -> bool {
        let c = self.current();
        c.c.position() < c.ins_len
    }
}

/// Stores a stack frame for a running `Vm` bytecode program.
struct Frame {
    ins_len: u64,
    c: io::Cursor<Vec<u8>>,
    num_locals: usize,
    fp: usize,
}

impl Frame {
    /// Produces a `Frame` from an input `object::CompiledFunction`.
    fn new(func: object::CompiledFunction, fp: usize) -> Self {
        Frame {
            ins_len: func.instructions.len() as u64,
            c: io::Cursor::new(func.instructions),
            num_locals: func.num_locals,
            fp,
        }
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
    DuplicateKey(object::Hashable),
    BadFunctionCall(Object),
    BadPointerDereference(Object),
    Object(object::Error),
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
            ErrorKind::DuplicateKey(k) => write!(f, "duplicate key {} in data structure", k),
            ErrorKind::BadFunctionCall(o) => write!(f, "cannot call object {:?} as a function", o),
            ErrorKind::BadPointerDereference(o) => {
                write!(f, "cannot dereference non-pointer object {:?}", o)
            }
            ErrorKind::Object(err) => write!(f, "object error: {}", err),
        }
    }
}
