//! An abstract syntax tree for the Monkey programming language from
//! <https://interpreterbook.com/>.

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
        match *self {
            Statement::Expression(ref stmt) => stmt.fmt(f),
            Statement::Let(ref stmt) => write!(f, "let {} = {};", stmt.name, stmt.value),
            Statement::Return(ref stmt) => write!(f, "return {};", stmt.value),
        }
    }
}

/// A statement that binds an expression to an identifier.
#[derive(Debug)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

/// A statement that returns a value.
#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

/// A computed expression.
#[derive(Debug)]
pub enum Expression {
    // TODO(mdlayher): remove!
    Todo,

    Identifier(Identifier),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Todo => write!(f, "TODO"),
            Expression::Identifier(ref id) => id.fmt(f),
        }
    }
}

/// A programmer-created identifier.
#[derive(Debug)]
pub struct Identifier {
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
