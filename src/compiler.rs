//! A compiler for the Monkey programming language from
//! <https://compilerbook.com/>.

use std::{error, fmt, mem, result};

use crate::{
    ast,
    code::{self, BinaryOpcode, ControlOpcode, Opcode, UnaryOpcode},
    object::Object,
    token::Token,
};

#[derive(Default)]
pub struct Compiler {
    instructions: Vec<u8>,
    constants: Vec<Object>,
    last: Option<Emitted>,
    previous: Option<Emitted>,
}

#[derive(Debug)]
struct Emitted {
    op: Opcode,
    pos: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler::default()
    }

    pub fn compile(&mut self, node: ast::Node) -> Result<()> {
        match node {
            ast::Node::Program(p) => {
                for s in p.statements {
                    self.compile(ast::Node::Statement(s))?;
                }
            }
            ast::Node::Expression(e) => match e {
                ast::Expression::Boolean(b) => {
                    let op = if b {
                        ControlOpcode::True
                    } else {
                        ControlOpcode::False
                    };

                    self.emit(Opcode::Control(op), vec![])?;
                }
                ast::Expression::Integer(i) => {
                    let oper = vec![self.add_constant(Object::Integer(i.value))];
                    self.emit(Opcode::Control(ControlOpcode::Constant), oper)?;
                }
                ast::Expression::Infix(i) => self.compile_infix_expression(i)?,
                ast::Expression::Prefix(p) => {
                    self.compile(ast::Node::Expression(*p.right))?;

                    let op = match p.operator {
                        Token::Minus => UnaryOpcode::Negate,
                        Token::Bang => UnaryOpcode::Not,
                        _ => panic!("unhandled prefix operator: {:?}", p.operator),
                    };

                    self.emit(Opcode::Unary(op), vec![])?;
                }
                ast::Expression::If(i) => self.compile_if_expression(i)?,
                _ => panic!("unhandled expression type"),
            },
            ast::Node::Statement(s) => match s {
                ast::Statement::Expression(e) => {
                    self.compile(ast::Node::Expression(e))?;
                    self.emit(Opcode::Control(ControlOpcode::Pop), vec![])?;
                }
                ast::Statement::Block(b) => {
                    for s in b.statements {
                        self.compile(ast::Node::Statement(s))?;
                    }
                }
                _ => panic!("unhandled statement type"),
            },
        };

        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn compile_infix_expression(&mut self, e: ast::InfixExpression) -> Result<()> {
        // Reorder less-than expressions to greater-than by compiling RHS and
        // then LHS to simplify bytecode.
        if e.operator == Token::LessThan {
            self.compile(ast::Node::Expression(*e.right))?;
            self.compile(ast::Node::Expression(*e.left))?;

            self.emit(Opcode::Binary(BinaryOpcode::GreaterThan), vec![])?;
            return Ok(());
        }

        // Evaluate all other expressions from LHS to RHS.
        self.compile(ast::Node::Expression(*e.left))?;
        self.compile(ast::Node::Expression(*e.right))?;

        let op = match e.operator {
            Token::Plus => BinaryOpcode::Add,
            Token::Minus => BinaryOpcode::Sub,
            Token::Asterisk => BinaryOpcode::Mul,
            Token::Slash => BinaryOpcode::Div,
            Token::Percent => BinaryOpcode::Mod,
            Token::Equal => BinaryOpcode::Equal,
            Token::NotEqual => BinaryOpcode::NotEqual,
            Token::GreaterThan => BinaryOpcode::GreaterThan,
            _ => panic!("unhandled infix operator: {:?}", e.operator),
        };

        self.emit(Opcode::Binary(op), vec![])?;
        Ok(())
    }

    fn compile_if_expression(&mut self, e: ast::IfExpression) -> Result<()> {
        self.compile(ast::Node::Expression(*e.condition))?;

        // Emit a jump with a placeholder operand, but track its
        // position so we can replace the operand at a later time
        // once we've emitted more instructions.
        let jump_not_true_pos =
            self.emit(Opcode::Control(ControlOpcode::JumpNotTrue), vec![9999])?;

        self.compile(ast::Node::Statement(ast::Statement::Block(e.consequence)))?;

        // Remove a duplicate pop that occurs in the middle of
        // the conditional.
        self.try_remove_last(Opcode::Control(ControlOpcode::Pop));

        if let Some(a) = e.alternative {
            // We have an alternative, set up a jump over it that
            // the consequence above will hit and skip this block.
            let jump_pos = self.emit(Opcode::Control(ControlOpcode::Jump), vec![9999])?;

            // Rewrite the jump with the correct instruction pointer.
            self.change_operand(jump_not_true_pos, self.instructions.len())?;

            self.compile(ast::Node::Statement(ast::Statement::Block(a)))?;

            // Remove a duplicate pop that occurs in the middle of
            // the conditional.
            self.try_remove_last(Opcode::Control(ControlOpcode::Pop));

            self.change_operand(jump_pos, self.instructions.len())?;
        } else {
            // Rewrite the jump with the correct instruction pointer.
            self.change_operand(jump_not_true_pos, self.instructions.len())?;
        }

        Ok(())
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: Opcode, operands: Vec<usize>) -> Result<usize> {
        let ins = code::make(op, &operands).map_err(Error::Code)?;

        // Track the last emitted instruction and its position for later
        // modification if necessary.
        let pos = self.add_instruction(&ins);
        self.set_last(op, pos);

        Ok(pos)
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend(ins);
        pos
    }

    fn set_last(&mut self, op: Opcode, pos: usize) {
        let last = Some(Emitted { op, pos });

        // Store value of last into previous and then overwrite last.
        mem::swap(&mut self.last, &mut self.previous);
        self.last = last;
    }

    fn try_remove_last(&mut self, op: Opcode) -> bool {
        match &self.last {
            None => return false,
            Some(l) => {
                if l.op != op {
                    return false;
                }
            }
        }

        // Trim the last instruction from the instructions stream.
        let end = (&self.last).as_ref().expect("last must not be none").pos;
        self.instructions = self.instructions.drain(..end).collect();

        // Store value of previous into last.
        mem::swap(&mut self.last, &mut self.previous);
        true
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) -> Result<()> {
        let op = Opcode::from(self.instructions[op_pos]);
        let ins = code::make(op, &[operand]).map_err(Error::Code)?;

        self.replace_instruction(op_pos, &ins);
        Ok(())
    }

    fn replace_instruction(&mut self, pos: usize, ins: &[u8]) {
        for (i, b) in ins.iter().enumerate() {
            self.instructions[pos + i] = *b
        }
    }
}

pub struct Bytecode {
    pub instructions: Vec<u8>,
    pub constants: Vec<Object>,
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    Code(code::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Code(c) => write!(f, "code error: {}", c),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        match self {
            Error::Code(c) => Some(c),
        }
    }
}
