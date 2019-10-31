//! A compiler for the Monkey programming language from
//! <https://compilerbook.com/>.

use std::{cell::RefCell, fmt, mem, rc::Rc};

use super::{
    error::{Error, ErrorKind, Result},
    symbol::*,
};

use crate::{
    ast,
    code::{self, BinaryOpcode::*, CompositeOpcode::*, ControlOpcode::*, Opcode, UnaryOpcode::*},
    object::{self, Object},
    token::Token,
};

#[derive(Default)]
pub struct Compiler {
    constants: Vec<Object>,
    scopes: Vec<CompilationScope>,

    // The current and optional outer symbol tables, used when entering and
    // leaving scopes.
    symbols: Rc<RefCell<SymbolTable>>,
    symbols_outer: Rc<RefCell<SymbolTable>>,
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
        // Initialize the compiler and enter the "main" scope. Do not call
        // the enter_scope method to avoid creating an immediate outer
        // symbol table.
        let c = Self {
            scopes: vec![CompilationScope::default()],
            ..Self::default()
        };

        // Define builtins so we can emit the appropriate opcodes for
        // handling them.
        {
            let mut table = c.symbols.borrow_mut();
            for b in object::builtins() {
                table.define_builtin(b);
            }
        }

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

                    self.emit(Opcode::Composite(Array), vec![a.elements.len()])?;
                }
                ast::Expression::Boolean(b) => {
                    let op = if b { True } else { False };

                    self.emit(Opcode::Control(op), vec![])?;
                }
                ast::Expression::Call(c) => {
                    // Compile the function itself.
                    self.compile(ast::Node::Expression(*c.function))?;

                    // Then compile each of its arguments and emit the Call
                    // opcode with the number of arguments.
                    let n = c.arguments.len();
                    for a in c.arguments {
                        self.compile(ast::Node::Expression(a))?;
                    }

                    self.emit(Opcode::Control(Call), vec![n])?;
                }
                ast::Expression::Float(f) => {
                    let oper = vec![self.add_constant(Object::Float(f.into()))];
                    self.emit(Opcode::Control(Constant), oper)?;
                }
                ast::Expression::Function(f) => {
                    // Enter a new scope and compile the arguments and body of
                    // the function. Once we leave the scope, place the
                    // function's instructions as a compiled function constant
                    // to be executed at a later time.
                    self.enter_scope();

                    // Shorten the scope of the symbol table borrow and define
                    // symbols for the parameters.
                    {
                        let mut table = self.symbols.borrow_mut();
                        for p in &f.parameters {
                            table.define(p.to_string());
                        }
                    }

                    self.compile(ast::Node::Statement(ast::Statement::Block(f.body)))?;

                    // Allow the return value to be passed to the calling scope.
                    if self.is_last(Opcode::Control(Pop)) {
                        self.replace_last_with_return()?;
                    }

                    // If there was no return value, make an implicit return.
                    if !self.is_last(Opcode::Control(ReturnValue)) {
                        self.emit(Opcode::Control(Return), vec![])?;
                    }

                    // Retrieve the number of local definitions to pass along
                    // as part of the function's metadata.
                    let num_locals = self.symbols.borrow().num_definitions;
                    let instructions = self.leave_scope();

                    let oper = vec![self.add_constant(Object::CompiledFunction(
                        object::CompiledFunction {
                            instructions,
                            num_locals,
                            num_parameters: f.parameters.len(),
                        },
                    ))];
                    self.emit(Opcode::Control(Constant), oper)?;
                }
                ast::Expression::Hash(h) => {
                    // Compile each key/value pair in order.
                    for (k, v) in &h.pairs {
                        self.compile(ast::Node::Expression(k.clone()))?;
                        self.compile(ast::Node::Expression(v.clone()))?;
                    }

                    self.emit(
                        Opcode::Composite(Hash),
                        // Each pair is 2 elements.
                        vec![h.pairs.len() * 2],
                    )?;
                }
                ast::Expression::Identifier(id) => {
                    // Attempt to resolve the identifier or return an error if
                    // it is undefined.
                    let s = self
                        .symbols
                        .borrow()
                        .resolve(&id)
                        .ok_or(Error::Compile(ErrorKind::UndefinedIdentifier(id)))?;

                    let op = match s.scope {
                        Scope::Builtin => GetBuiltin,
                        Scope::Global => GetGlobal,
                        Scope::Local => GetLocal,
                    };

                    self.emit(Opcode::Control(op), vec![s.index])?;
                }
                ast::Expression::If(i) => self.compile_if_expression(i)?,
                ast::Expression::Index(i) => {
                    // Compile left and index expressions and emit the index
                    // opcode to be interpreted by the VM.
                    self.compile(ast::Node::Expression(*i.left))?;
                    self.compile(ast::Node::Expression(*i.index))?;

                    self.emit(Opcode::Binary(Index), vec![])?;
                }
                ast::Expression::Integer(i) => {
                    let oper = vec![self.add_constant(Object::Integer(i.value))];
                    self.emit(Opcode::Control(Constant), oper)?;
                }
                ast::Expression::Infix(i) => self.compile_infix_expression(i)?,
                ast::Expression::Prefix(p) => {
                    self.compile(ast::Node::Expression(*p.right))?;

                    let op = match p.operator {
                        Token::Minus => Negate,
                        Token::Bang => Not,
                        Token::Ampersand => Address,
                        Token::Asterisk => Dereference,
                        _ => panic!("unhandled prefix operator: {:?}", p.operator),
                    };

                    self.emit(Opcode::Unary(op), vec![])?;
                }
                ast::Expression::Set(s) => {
                    for i in &s.set {
                        self.compile(ast::Node::Expression(i.clone()))?;
                    }

                    self.emit(Opcode::Composite(Set), vec![s.set.len()])?;
                }
                ast::Expression::String(s) => {
                    let oper = vec![self.add_constant(Object::String(s))];
                    self.emit(Opcode::Control(Constant), oper)?;
                }
            },
            ast::Node::Statement(s) => match s {
                ast::Statement::Expression(e) => {
                    self.compile(ast::Node::Expression(e))?;
                    self.emit(Opcode::Control(Pop), vec![])?;
                }
                ast::Statement::Block(b) => {
                    for s in b.statements {
                        self.compile(ast::Node::Statement(s))?;
                    }
                }
                ast::Statement::Let(l) => {
                    self.compile(ast::Node::Expression(l.value))?;

                    // Define this identifier and emit it with the appropriate
                    // scope.
                    let s = self.symbols.borrow_mut().define(l.name);
                    let op = match s.scope {
                        Scope::Global => SetGlobal,
                        Scope::Local => SetLocal,
                        Scope::Builtin => panic!("cannot define an object with builtin scope"),
                    };

                    self.emit(Opcode::Control(op), vec![s.index])?;
                }
                ast::Statement::LetDereference(l) => {
                    self.compile(ast::Node::Expression(l.value))?;

                    // Resolve this identifier's index so the VM can dereference
                    // one or more pointers.
                    //
                    // BUG(mdlayher): we can't properly handle multiple
                    // pointers at this point.
                    let idx = self
                        .symbols
                        .borrow()
                        .resolve(&l.name)
                        .ok_or(Error::Compile(ErrorKind::UndefinedIdentifier(l.name)))?
                        .index;

                    self.emit(Opcode::Control(SetPointer), vec![idx])?;
                }
                ast::Statement::Return(r) => {
                    self.compile(ast::Node::Expression(r.value))?;
                    self.emit(Opcode::Control(ReturnValue), vec![])?;
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

            self.emit(Opcode::Binary(GreaterThan), vec![])?;
            return Ok(());
        }

        // Evaluate all other expressions from LHS to RHS.
        self.compile(ast::Node::Expression(*e.left))?;
        self.compile(ast::Node::Expression(*e.right))?;

        let op = match e.operator {
            Token::Plus => Add,
            Token::Minus => Sub,
            Token::Asterisk => Mul,
            Token::Slash => Div,
            Token::Percent => Mod,
            Token::Equal => Equal,
            Token::NotEqual => NotEqual,
            Token::GreaterThan => GreaterThan,
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
        let jump_not_true_pos = self.emit(Opcode::Control(JumpNotTrue), vec![9999])?;

        self.compile(ast::Node::Statement(ast::Statement::Block(e.consequence)))?;
        self.try_remove_last(Opcode::Control(Pop));

        // Emit a jump with a placeholder that enables the null branch if there
        // is no alternative.
        let jump_pos = self.emit(Opcode::Control(Jump), vec![9999])?;

        // Rewrite the jump with the correct instruction pointer.
        self.change_operand(jump_not_true_pos, self.scope().instructions.len())?;

        if let Some(a) = e.alternative {
            // We have an alternative, compile it.
            self.compile(ast::Node::Statement(ast::Statement::Block(a)))?;
            self.try_remove_last(Opcode::Control(Pop));
        } else {
            // There is no alternative, emit a null.
            self.emit(Opcode::Control(Null), vec![])?;
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

    fn is_last(&self, op: Opcode) -> bool {
        match &self.scope().last {
            Some(e) => e.op == op,
            None => false,
        }
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

    fn replace_last_with_return(&mut self) -> Result<()> {
        let pos = self
            .scope()
            .last
            .as_ref()
            .expect("last must not be none")
            .pos;

        let op = Opcode::Control(ReturnValue);

        let ins = code::make(op, &[]).map_err(Error::Code)?;
        self.replace_instruction(pos, &ins);

        self.scope_mut()
            .last
            .as_mut()
            .expect("last must not be none")
            .op = op;

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

        // Keep track of the outer symbol table so we can swap it back in place
        // when we leave this scope.
        self.symbols_outer = Rc::clone(&self.symbols);

        // Create an enclosed symbol table which will reference the current
        // one for symbols outside its own scope.
        let s = SymbolTable::new_enclosed(Rc::clone(&self.symbols));
        self.symbols = Rc::new(RefCell::new(s));
    }

    fn leave_scope(&mut self) -> Vec<u8> {
        // Restore the previous symbol table from the outer scope.
        mem::swap(&mut self.symbols, &mut self.symbols_outer);

        let scope = self
            .scopes
            .pop()
            .expect("left scope without entering one previously");

        scope.instructions
    }
}

#[derive(Clone)]
pub struct Bytecode {
    pub instructions: Vec<u8>,
    pub constants: Vec<Object>,
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "constants:\n")?;
        for (i, con) in self.constants.iter().enumerate() {
            writeln!(f, "{:02}: {}", i, con)?;
        }

        writeln!(f, "bytecode:\n")?;
        write!(
            f,
            "{}",
            code::Instructions::parse(&self.instructions).expect("instructions must parse")
        )
    }
}
