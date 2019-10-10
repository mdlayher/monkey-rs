//! An abstract syntax tree for the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::token;
use std::hash::Hash;
use std::hash::Hasher;

use std::collections::HashMap;
use std::fmt;

/// Any AST node in a Monkey program.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Program(p) => p.fmt(f),
            Node::Statement(s) => s.fmt(f),
            Node::Expression(e) => e.fmt(f),
        }
    }
}

/// The top level structure of a Monkey program.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Program {
    /// The statements that make up the `Program`.
    pub statements: Vec<Statement>,
}

impl Program {
    /// Creates a new `Program` for use with a `parser::Parser`.
    pub fn new() -> Self {
        Program { statements: vec![] }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            stmt.fmt(f)?;
        }

        Ok(())
    }
}

/// Possible statement types in Monkey.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Return(ReturnStatement),
    Block(BlockStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Expression(stmt) => stmt.fmt(f),
            Statement::Let(stmt) => stmt.fmt(f),
            Statement::Return(stmt) => stmt.fmt(f),
            Statement::Block(stmt) => stmt.fmt(f),
        }
    }
}

/// A statement that binds an expression to an identifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

/// A statement that returns a value.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
}

/// A statement produced by a block.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            stmt.fmt(f)?;
        }

        Ok(())
    }
}

/// A computed expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(token::Integer),
    Float(token::Float),
    Boolean(bool),
    String(String),
    Array(ArrayLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Index(IndexExpression),
    Hash(HashLiteral),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(id) => id.fmt(f),
            Expression::Integer(int) => int.fmt(f),
            Expression::Float(fl) => fl.fmt(f),
            Expression::Boolean(b) => b.fmt(f),
            Expression::Prefix(p) => p.fmt(f),
            Expression::String(s) => write!(f, r#""{}""#, s),
            Expression::Array(a) => a.fmt(f),
            Expression::Infix(i) => i.fmt(f),
            Expression::If(i) => i.fmt(f),
            Expression::Function(fl) => fl.fmt(f),
            Expression::Call(c) => c.fmt(f),
            Expression::Index(i) => i.fmt(f),
            Expression::Hash(h) => h.fmt(f),
        }
    }
}

/// An array of objects.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|e| format!("{}", e))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

/// A prefix expression such as negation or logical not.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PrefixExpression {
    pub operator: token::Token,
    pub right: Box<Expression>,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

/// An infix expression such as a mathematical computation.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: token::Token,
    pub right: Box<Expression>,
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

/// An expression comprised of an if/else block.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} {{ {} }}", self.condition, self.consequence)?;

        if let Some(alt) = &self.alternative {
            write!(f, " else {{ {} }}", alt)?;
        }

        Ok(())
    }
}

/// A function literal.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionLiteral {
    pub parameters: Vec<String>,
    pub body: BlockStatement,
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn({}) {{ {} }}", self.parameters.join(", "), self.body)
    }
}

/// A function call expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut args = vec![];
        for arg in &self.arguments {
            args.push(format!("{}", arg));
        }

        write!(f, "{}({})", self.function, args.join(", "))
    }
}

/// An array index expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

/// A hash literal expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HashLiteral {
    pub pairs: HashMap<Expression, Expression>,
}

// This is a bit of a hack, but we need all Expressions to implement Hasher,
// although we will never hash a HashLiteral itself.
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("HashLiteral cannot be hashed");
    }
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut pairs = vec![];
        for pair in &self.pairs {
            pairs.push(format!(r#"{}: {}"#, pair.0, pair.1));
        }

        // Sort for deterministic output.
        pairs.sort();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}
