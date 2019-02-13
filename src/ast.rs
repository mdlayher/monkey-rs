//! An abstract syntax tree for the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::token;

use std::fmt;

/// The top level structure of a Monkey program.
#[derive(Debug, Default)]
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
#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Let(LetStatement),
    Return(ReturnStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Expression(stmt) => stmt.fmt(f),
            Statement::Let(stmt) => write!(f, "let {} = {};", stmt.name, stmt.value),
            Statement::Return(stmt) => write!(f, "return {};", stmt.value),
        }
    }
}

/// A statement that binds an expression to an identifier.
#[derive(Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

/// A statement that returns a value.
#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

/// A computed expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    // TODO(mdlayher): remove!
    Todo,

    Identifier(String),
    Integer(token::Integer),
    Boolean(bool),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Todo => write!(f, "TODO"),
            Expression::Identifier(id) => id.fmt(f),
            Expression::Integer(int) => int.fmt(f),
            Expression::Boolean(b) => b.fmt(f),
            Expression::Prefix(p) => p.fmt(f),
            Expression::Infix(i) => i.fmt(f),
        }
    }
}

/// A prefix expression such as negation or logical not.
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
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
