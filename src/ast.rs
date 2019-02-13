//! An abstract syntax tree for the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::token;

use std::fmt;

/// The top level structure of a Monkey program.
#[derive(Clone, Debug, Default, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
}

/// A statement produced by a block.
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    // TODO(mdlayher): remove!
    Todo,

    Identifier(String),
    Integer(token::Integer),
    Boolean(bool),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
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
            Expression::If(i) => i.fmt(f),
            Expression::Function(fl) => fl.fmt(f),
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

/// An expression comprised of an if/else block.
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionLiteral {
    pub parameters: Vec<String>,
    pub body: BlockStatement,
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn({}) {{ {} }}", self.parameters.join(", "), self.body)
    }
}
