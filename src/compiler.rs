//! A compiler for the Monkey programming language from
//! <https://compilerbook.com/>.

use std::error;
use std::fmt;
use std::result;

use crate::{ast, code, object, token};

#[derive(Default)]
pub struct Compiler {
    instructions: Vec<u8>,
    constants: Vec<object::Object>,
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
                        code::Opcode::True
                    } else {
                        code::Opcode::False
                    };

                    self.emit(op, vec![])?;
                }
                ast::Expression::Integer(i) => {
                    let oper = vec![self.add_constant(object::Object::Integer(i.value))];
                    self.emit(code::Opcode::Constant, oper)?;
                }
                ast::Expression::Infix(i) => self.compile_infix_expression(i)?,
                ast::Expression::Prefix(p) => {
                    self.compile(ast::Node::Expression(*p.right))?;

                    let op = match p.operator {
                        token::Token::Minus => code::Opcode::Negate,
                        token::Token::Bang => code::Opcode::Not,
                        _ => panic!("unhandled prefix operator: {:?}", p.operator),
                    };

                    self.emit(op, vec![])?;
                }
                _ => panic!("unhandled expression type"),
            },
            ast::Node::Statement(s) => match s {
                ast::Statement::Expression(e) => {
                    self.compile(ast::Node::Expression(e))?;
                    self.emit(code::Opcode::Pop, vec![])?;
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
        if e.operator == token::Token::LessThan {
            self.compile(ast::Node::Expression(*e.right))?;
            self.compile(ast::Node::Expression(*e.left))?;

            self.emit(code::Opcode::GreaterThan, vec![])?;
            return Ok(());
        }

        // Evaluate all other expressions from LHS to RHS.
        self.compile(ast::Node::Expression(*e.left))?;
        self.compile(ast::Node::Expression(*e.right))?;

        let op = match e.operator {
            token::Token::Plus => code::Opcode::Add,
            token::Token::Minus => code::Opcode::Sub,
            token::Token::Asterisk => code::Opcode::Mul,
            token::Token::Slash => code::Opcode::Div,
            token::Token::Percent => code::Opcode::Mod,
            token::Token::Equal => code::Opcode::Equal,
            token::Token::NotEqual => code::Opcode::NotEqual,
            token::Token::GreaterThan => code::Opcode::GreaterThan,
            _ => panic!("unhandled infix operator: {:?}", e.operator),
        };

        self.emit(op, vec![])?;
        Ok(())
    }

    fn add_constant(&mut self, obj: object::Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: code::Opcode, operands: Vec<usize>) -> Result<usize> {
        let ins = code::make(op, &operands).map_err(Error::Code)?;
        Ok(self.add_instruction(&ins))
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend(ins);
        pos
    }
}

pub struct Bytecode {
    pub instructions: Vec<u8>,
    pub constants: Vec<object::Object>,
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
