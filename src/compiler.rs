//! A compiler for the Monkey programming language from
//! <https://compilerbook.com/>.

use std::collections::HashMap;
use std::{error, fmt, mem, result};

use crate::{
    ast,
    code::{self, BinaryOpcode, CompositeOpcode, ControlOpcode, Opcode, UnaryOpcode},
    object::{self, Object},
    token::Token,
};

#[derive(Default)]
pub struct Compiler {
    constants: Vec<Object>,
    symbols: SymbolTable,
    scopes: Vec<CompilationScope>,
}

#[derive(Default)]
struct CompilationScope {
    instructions: Vec<u8>,
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
        // Initialize and enter the "main" scope.
        let mut c = Compiler::default();
        c.enter_scope();
        c
    }

    pub fn compile(&mut self, node: ast::Node) -> Result<()> {
        match node {
            ast::Node::Program(p) => {
                for s in p.statements {
                    self.compile(ast::Node::Statement(s))?;
                }
            }
            ast::Node::Expression(e) => match e {
                ast::Expression::Array(a) => {
                    // Compile each of the expressions in an array, and then
                    // emit an Array opcode with the number of elements
                    // belonging to that array as an operand.
                    for e in &a.elements {
                        // TODO(mdlayher): remove cloning?
                        self.compile(ast::Node::Expression(e.clone()))?;
                    }

                    self.emit(
                        Opcode::Composite(CompositeOpcode::Array),
                        vec![a.elements.len()],
                    )?;
                }
                ast::Expression::Boolean(b) => {
                    let op = if b {
                        ControlOpcode::True
                    } else {
                        ControlOpcode::False
                    };

                    self.emit(Opcode::Control(op), vec![])?;
                }
                ast::Expression::Float(f) => {
                    let oper = vec![self.add_constant(Object::Float(f.into()))];
                    self.emit(Opcode::Control(ControlOpcode::Constant), oper)?;
                }
                ast::Expression::Function(f) => {
                    // Enter a new scope and compile the body of the function.
                    // Once we leave the scope, place the function's instructions
                    // as a compiled function constant to be executed at a
                    // later time.
                    self.enter_scope();
                    self.compile(ast::Node::Statement(ast::Statement::Block(f.body)))?;
                    let instructions = self.leave_scope();

                    let oper = vec![self.add_constant(Object::CompiledFunction(
                        object::CompiledFunction { instructions },
                    ))];
                    self.emit(Opcode::Control(ControlOpcode::Constant), oper)?;
                }
                ast::Expression::Hash(h) => {
                    // Compile each key/value pair in order.
                    for (k, v) in &h.pairs {
                        self.compile(ast::Node::Expression(k.clone()))?;
                        self.compile(ast::Node::Expression(v.clone()))?;
                    }

                    self.emit(
                        Opcode::Composite(CompositeOpcode::Hash),
                        // Each pair is 2 elements.
                        vec![h.pairs.len() * 2],
                    )?;
                }
                ast::Expression::Identifier(id) => {
                    // Attempt to resolve the identifier or return an error if
                    // it is undefined.
                    let s = self
                        .symbols
                        .resolve(&id)
                        .ok_or(Error::Compile(ErrorKind::UndefinedIdentifier(id)))?;

                    // End the borrow of s by just taking the index we need.
                    let index = s.index;
                    self.emit(Opcode::Control(ControlOpcode::GetGlobal), vec![index])?;
                }
                ast::Expression::If(i) => self.compile_if_expression(i)?,
                ast::Expression::Index(i) => {
                    // Compile left and index expressions and emit the index
                    // opcode to be interpreted by the VM.
                    self.compile(ast::Node::Expression(*i.left))?;
                    self.compile(ast::Node::Expression(*i.index))?;

                    self.emit(Opcode::Binary(BinaryOpcode::Index), vec![])?;
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
                ast::Expression::Set(s) => {
                    for i in &s.set {
                        self.compile(ast::Node::Expression(i.clone()))?;
                    }

                    self.emit(Opcode::Composite(CompositeOpcode::Set), vec![s.set.len()])?;
                }
                ast::Expression::String(s) => {
                    let oper = vec![self.add_constant(Object::String(s))];
                    self.emit(Opcode::Control(ControlOpcode::Constant), oper)?;
                }
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
                ast::Statement::Let(l) => {
                    self.compile(ast::Node::Expression(l.value))?;

                    // Define this identifier with an index.
                    let idx = self.symbols.define(l.name);
                    self.emit(Opcode::Control(ControlOpcode::SetGlobal), vec![idx])?;
                }
                ast::Statement::Return(r) => {
                    self.compile(ast::Node::Expression(r.value))?;
                    self.emit(Opcode::Control(ControlOpcode::ReturnValue), vec![])?;
                }
            },
        };

        Ok(())
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.scope().instructions.clone(),
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
        self.try_remove_last(Opcode::Control(ControlOpcode::Pop));

        // Emit a jump with a placeholder that enables the null branch if there
        // is no alternative.
        let jump_pos = self.emit(Opcode::Control(ControlOpcode::Jump), vec![9999])?;

        // Rewrite the jump with the correct instruction pointer.
        self.change_operand(jump_not_true_pos, self.scope().instructions.len())?;

        if let Some(a) = e.alternative {
            // We have an alternative, compile it.
            self.compile(ast::Node::Statement(ast::Statement::Block(a)))?;
            self.try_remove_last(Opcode::Control(ControlOpcode::Pop));
        } else {
            // There is no alternative, emit a null.
            self.emit(Opcode::Control(ControlOpcode::Null), vec![])?;
        }

        self.change_operand(jump_pos, self.scope().instructions.len())?;

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
        let scope = self.scope_mut();
        let pos = scope.instructions.len();
        scope.instructions.extend(ins);
        pos
    }

    fn set_last(&mut self, op: Opcode, pos: usize) {
        let last = Some(Emitted { op, pos });

        // Store value of last into previous and then overwrite last.
        let scope = self.scope_mut();
        mem::swap(&mut scope.last, &mut scope.previous);
        scope.last = last;
    }

    fn try_remove_last(&mut self, op: Opcode) -> bool {
        let scope = self.scope_mut();
        match &scope.last {
            None => return false,
            Some(l) => {
                if l.op != op {
                    return false;
                }
            }
        }

        // Trim the last instruction from the instructions stream.
        let end = scope.last.as_ref().expect("last must not be none").pos;
        scope.instructions = scope.instructions.drain(..end).collect();

        // Store value of previous into last.
        mem::swap(&mut scope.last, &mut scope.previous);
        true
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) -> Result<()> {
        let op = Opcode::from(self.scope().instructions[op_pos]);
        let ins = code::make(op, &[operand]).map_err(Error::Code)?;

        self.replace_instruction(op_pos, &ins);
        Ok(())
    }

    fn replace_instruction(&mut self, pos: usize, ins: &[u8]) {
        let scope = self.scope_mut();
        for (i, b) in ins.iter().enumerate() {
            scope.instructions[pos + i] = *b
        }
    }

    fn scope(&self) -> &CompilationScope {
        &self.scopes[self.scopes.len() - 1]
    }

    fn scope_mut(&mut self) -> &mut CompilationScope {
        let l = self.scopes.len() - 1;
        &mut self.scopes[l]
    }

    fn enter_scope(&mut self) {
        self.scopes.push(CompilationScope::default());
    }

    fn leave_scope(&mut self) -> Vec<u8> {
        let scope = self
            .scopes
            .pop()
            .expect("left scope without entering one previously");

        scope.instructions
    }
}

pub struct Bytecode {
    pub instructions: Vec<u8>,
    pub constants: Vec<Object>,
}

/// A table that can be used to define and resolve `Symbols`.
#[derive(Debug, Default)]
pub struct SymbolTable {
    pub store: HashMap<String, Symbol>,
    pub num_definitions: usize,
}

impl SymbolTable {
    /// Defines a new `Symbol` by name.
    pub fn define(&mut self, name: String) -> usize {
        let index = self.num_definitions;
        self.store.insert(
            name,
            Symbol {
                scope: Scope::Global,
                index,
            },
        );

        self.num_definitions += 1;
        index
    }

    /// Resolves a `Symbol` by its name and returns whether or not it
    /// was defined.
    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

/// A symbol definition.
#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    //pub name: String,
    pub scope: Scope,
    pub index: usize,
}

/// The scope of a symbol.
#[derive(Clone, Debug, PartialEq)]
pub enum Scope {
    Global,
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug)]
pub enum Error {
    Compile(ErrorKind),
    Code(code::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Compile(k) => write!(f, "compile error: {}", k),
            Error::Code(c) => write!(f, "code error: {}", c),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        match self {
            Error::Compile(_) => None,
            Error::Code(c) => Some(c),
        }
    }
}

/// Describes compilation errors which may occur.
#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    UndefinedIdentifier(String),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::UndefinedIdentifier(id) => write!(f, "undefined identifier: \"{}\"", id,),
        }
    }
}
