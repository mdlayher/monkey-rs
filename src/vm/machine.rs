//! Virtual machine structures for use within the `vm` module.

use std::io;

use super::{
    error::{Error, ErrorKind, Result},
    frame::{Frame, FrameStack},
    op,
};

use crate::{
    code::{BinaryOpcode, CompositeOpcode, ControlOpcode, Opcode, UnaryOpcode},
    compiler,
    object::{self, Object},
};

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
            object::Closure {
                func: object::CompiledFunction {
                    instructions: bc.instructions,
                    // main has no local bindings and no parameters.
                    num_locals: 0,
                    num_parameters: 0,
                },
                // main has no free variables.
                free: vec![],
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
                self.pop();
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

                let truthy = match *self.pop() {
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
                // Register a new global with the operand's specified index.
                let ctx = self.frames.current_mut();
                let idx = ctx.read_u16()?;
                self.globals[idx as usize] = self.pop().clone();
            }
            ControlOpcode::Call => {
                let num_args = self.frames.current_mut().read_u8()? as usize;

                // Take the compiled closure or built-in at the correct offset
                // after its arguments and handle it.
                let obj = self.stack[self.sp - 1 - num_args].clone();
                match obj {
                    Object::Builtin(b) => self.call_builtin(b, num_args)?,
                    Object::Closure(c) => self.call_closure(c, num_args)?,
                    _ => return Err(Error::Runtime(ErrorKind::BadFunctionCall(obj))),
                };
            }
            ControlOpcode::ReturnValue => {
                let ret = self.pop().clone();

                let f = self.frames.pop();
                self.sp = f.fp - 1;
                self.push(ret);
            }
            ControlOpcode::Return => {
                let f = self.frames.pop();
                self.sp = f.fp - 1;
                self.push(Object::Null);
            }
            ControlOpcode::SetPointer => {
                // Overwrite the element at the specified index in the heap.
                let idx = self.frames.current_mut().read_u16()?;
                self.heap[idx as usize] = self.pop().clone();
            }
            ControlOpcode::SetLocal => {
                let f = self.frames.current_mut();
                let i = f.fp + f.read_u8()? as usize;

                self.stack[i] = self.pop().clone();
            }
            ControlOpcode::GetLocal => {
                let f = self.frames.current_mut();
                let i = f.fp + f.read_u8()? as usize;

                self.push(self.stack[i].clone());
            }
            ControlOpcode::GetBuiltin => {
                let i = self.frames.current_mut().read_u8()?;
                self.push(Object::Builtin(object::Builtin::from(i)));
            }
            ControlOpcode::Closure => {
                let ctx = self.frames.current_mut();
                let idx = ctx.read_u16()?;
                let num_free = ctx.read_u8()? as usize;

                let obj = &self.consts[idx as usize];
                let func = match obj {
                    Object::CompiledFunction(f) => f.clone(),
                    _ => panic!("not a compiled function object: {:?}", obj),
                };

                let free = self.stack[self.sp - num_free..self.sp].to_vec();
                self.sp -= num_free;

                self.push(Object::Closure(object::Closure { func, free }));
            }
            ControlOpcode::GetFree => {
                let ctx = self.frames.current_mut();
                let idx = ctx.read_u8()?;

                let obj = ctx.free[idx as usize].clone();
                self.push(obj);
            }
        };

        Ok(())
    }

    /// Executes a unary operation against one object.
    fn unary_op(&mut self, op: UnaryOpcode) -> Result<()> {
        let obj = self.pop().clone();

        let out = match (op, &obj) {
            (UnaryOpcode::Not, _) => match &obj {
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
                return Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                    Opcode::Unary(op),
                    vec![obj],
                )));
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
                        return Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                            Opcode::Binary(op),
                            args.to_vec(),
                        )));
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
                        return Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                            Opcode::Binary(op),
                            args.to_vec(),
                        )));
                    }
                };

                self.push(Object::Boolean(out));
                Ok(())
            }
            // Float and Float/Integer operations. Integers are automatically
            // coerced into floats for operations, and all of these operations
            // will produce a float result.
            (Object::Float(l), Object::Float(r)) => {
                let obj = op::binary_float(op, args, *l, *r)?;
                self.push(obj);
                Ok(())
            }
            (Object::Integer(l), Object::Float(r)) => {
                let obj = op::binary_float(op, args, *l as f64, *r)?;
                self.push(obj);
                Ok(())
            }
            (Object::Float(l), Object::Integer(r)) => {
                let obj = op::binary_float(op, args, *l, *r as f64)?;
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
                    Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                        Opcode::Binary(op),
                        args.to_vec(),
                    )))
                }
            }
            // Pointer arithmetic with integer operations.
            (Object::Integer(l), Object::Pointer(r)) => {
                let obj = op::pointer_arithmetic(op, args, *l, *r as i64)?;
                self.push(obj);
                Ok(())
            }
            (Object::Pointer(l), Object::Integer(r)) => {
                let obj = op::pointer_arithmetic(op, args, *l as i64, *r)?;
                self.push(obj);
                Ok(())
            }
            // Set operations.
            (Object::Set(l), Object::Set(r)) => {
                let obj = op::binary_set(op, args, l, r)?;
                self.push(obj);
                Ok(())
            }
            // Composite literal indexing operations.
            (Object::Array(_), _) | (Object::Hash(_), _) | (Object::Set(_), _) => {
                // Only indexing is supported on these composite literals.
                if op == BinaryOpcode::Index {
                    let out = op::composite_index(args)?;
                    self.push(out);
                    Ok(())
                } else {
                    Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                        Opcode::Binary(op),
                        args.to_vec(),
                    )))
                }
            }
            // Invalid combination.
            (_, _) => Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                Opcode::Binary(op),
                args.to_vec(),
            ))),
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
            CompositeOpcode::Hash => op::build_hash(args)?,
            CompositeOpcode::Set => op::build_set(args)?,
        };

        self.push(out);
        Ok(())
    }

    /// Executes a call operation on a built-in function.
    fn call_builtin(&mut self, b: object::Builtin, num_args: usize) -> Result<()> {
        // Gather the stack arguments and apply the built-in accordingly.
        let args = &self.stack[self.sp - num_args..self.sp];
        let obj = b
            .apply(args)
            .map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

        // Adjust the stack in preparation for returning the output.
        self.sp = self.sp - 1 - num_args;
        self.push(obj);

        Ok(())
    }

    /// Executes a call operation on a user-defined function or closure.
    fn call_closure(&mut self, cl: object::Closure, num_args: usize) -> Result<()> {
        // Ensure the user passed the right number of parameters.
        if num_args != cl.func.num_parameters {
            return Err(Error::Runtime(ErrorKind::WrongNumberArguments {
                want: cl.func.num_parameters,
                got: num_args,
            }));
        }

        // Push a new frame and set its frame pointer to the current
        // stack pointer.
        let frame = Frame::new(cl, self.sp - num_args);
        self.sp = frame.fp + frame.num_locals;
        self.frames.push(frame);

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

    /// Pops off and returns a reference to the top element of the stack.
    fn pop(&mut self) -> &Object {
        let (i, _) = self.pop_n(1);
        &self.stack[i]
    }
}
