//! Frame structures for use within the `vm` module.

use std::io::{self, Seek};

use super::error::{Error, Result};

use crate::object;

extern crate byteorder;
use byteorder::{BigEndian, ReadBytesExt};

/// Stores a stack of stack frames that can track function execution.
pub struct FrameStack(pub Vec<Frame>);

impl FrameStack {
    /// Produces a `&Frame` of the current frame on the stack.
    fn current(&self) -> &Frame {
        self.0.last().expect("must not be none")
    }

    /// Produces a `&mut Frame` of the current frame on the stack.
    pub fn current_mut(&mut self) -> &mut Frame {
        self.0.last_mut().expect("must not be none")
    }

    /// Pushes a new `Frame` onto the stack.
    pub fn push(&mut self, f: Frame) {
        self.0.push(f);
    }

    /// Pops a `Frame` off the stack.
    pub fn pop(&mut self) -> Frame {
        self.0.pop().expect("must not be none")
    }

    /// Determines if more instructions can be read and executed.
    pub fn run(&self) -> bool {
        let c = self.current();
        c.c.position() < c.ins_len
    }
}

/// Stores a stack frame for a running `Vm` bytecode program.
pub struct Frame {
    cl: object::Closure,
    ins_len: u64,
    c: io::Cursor<Vec<u8>>,
    pub num_locals: usize,
    pub fp: usize,
}

impl Frame {
    /// Produces a `Frame` from an input `object::Closure`.
    pub fn new(cl: object::Closure, fp: usize) -> Self {
        // TODO: work this out.
        let clos = cl.clone();
        Frame {
            cl: clos,
            ins_len: cl.func.instructions.len() as u64,
            c: io::Cursor::new(cl.func.instructions),
            num_locals: cl.func.num_locals,
            fp,
        }
    }

    /// Seeks to the specified location in the instructions stream.
    pub fn seek(&mut self, pos: io::SeekFrom) -> Result<u64> {
        self.c.seek(pos).map_err(Error::Io)
    }

    /// Reads a `u8` value from the instructions stream.
    pub fn read_u8(&mut self) -> Result<u8> {
        self.c.read_u8().map_err(Error::Io)
    }

    /// Reads a `u16` value from the instructions stream.
    pub fn read_u16(&mut self) -> Result<u16> {
        self.c.read_u16::<BigEndian>().map_err(Error::Io)
    }
}
