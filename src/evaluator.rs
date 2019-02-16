//! An AST evaluator for the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::ast;
use crate::object::Object;
use crate::token::Token;

use std::error;
use std::fmt;
use std::result;

/// Evaluates an `ast::Node` and produces an `object::Object`.
pub fn eval(node: ast::Node) -> Result<Object> {
    // TODO(mdlayher): clean up error handling via err_node.
    let err_node = node.clone();
    match node {
        ast::Node::Program(prog) => eval_statements(&prog.statements),
        ast::Node::Statement(stmt) => match stmt {
            ast::Statement::Block(block) => eval_statements(&block.statements),
            ast::Statement::Expression(expr) => eval(ast::Node::Expression(expr)),
            _ => Err(Error::Evaluation(
                err_node,
                "unhandled statement type".to_string(),
            )),
        },
        ast::Node::Expression(expr) => match expr {
            ast::Expression::Integer(i) => Ok(Object::Integer(i.value)),
            ast::Expression::Boolean(b) => Ok(Object::Boolean(b)),
            ast::Expression::Prefix(p) => eval_prefix_expression(p, err_node),
            ast::Expression::Infix(i) => eval_infix_expression(i, err_node),
            ast::Expression::If(stmt) => eval_if_expression(stmt),
            _ => Err(Error::Evaluation(
                err_node,
                "unhandled expression type".to_string(),
            )),
        },
    }
}

/// Evaluates statements and returns the result of the last statement.
fn eval_statements(stmts: &[ast::Statement]) -> Result<Object> {
    let mut result = Object::Null;

    for stmt in stmts {
        result = eval(ast::Node::Statement(stmt.clone()))?;
    }

    Ok(result)
}

/// Evaluates a prefix expression to produce an Object.
fn eval_prefix_expression(expr: ast::PrefixExpression, err_node: ast::Node) -> Result<Object> {
    // Evaluate the right side before applying the prefix operator.
    let right = eval(ast::Node::Expression(*expr.right))?;

    match expr.operator {
        // Logical negation.
        Token::Bang => match right {
            // Negate the input boolean.
            Object::Boolean(b) => Ok(Object::Boolean(!b)),
            // !null == true.
            Object::Null => Ok(Object::Boolean(true)),
            // 5 == true, so !5 == false.
            _ => Ok(Object::Boolean(false)),
        },
        // Negative numbers.
        Token::Minus => match right {
            Object::Integer(i) => Ok(Object::Integer(-i)),

            _ => Err(Error::Evaluation(
                err_node,
                "cannot negate non-integer value".to_string(),
            )),
        },

        _ => Err(Error::Evaluation(
            err_node,
            "unhandled prefix operator".to_string(),
        )),
    }
}

/// Evaluates an infix expression to produce an Object.
fn eval_infix_expression(expr: ast::InfixExpression, err_node: ast::Node) -> Result<Object> {
    let left = eval(ast::Node::Expression(*expr.left))?;
    let right = eval(ast::Node::Expression(*expr.right))?;

    // Left and right types must match.
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => match expr.operator {
            Token::Plus => Ok(Object::Integer(l + r)),
            Token::Minus => Ok(Object::Integer(l - r)),
            Token::Asterisk => Ok(Object::Integer(l * r)),
            Token::Slash => Ok(Object::Integer(l / r)),
            Token::Percent => Ok(Object::Integer(l % r)),

            Token::LessThan => Ok(Object::Boolean(l < r)),
            Token::GreaterThan => Ok(Object::Boolean(l > r)),
            Token::Equal => Ok(Object::Boolean(l == r)),
            Token::NotEqual => Ok(Object::Boolean(l != r)),

            _ => Err(Error::Evaluation(
                err_node,
                "unhandled integer infix operator".to_string(),
            )),
        },

        (Object::Boolean(l), Object::Boolean(r)) => match expr.operator {
            Token::Equal => Ok(Object::Boolean(l == r)),
            Token::NotEqual => Ok(Object::Boolean(l != r)),

            _ => Err(Error::Evaluation(
                err_node,
                "unhandled boolean infix operator".to_string(),
            )),
        },

        _ => Err(Error::Evaluation(
            err_node,
            "unhandled or mismatched infix expression types".to_string(),
        )),
    }
}

/// Evaluates an if/else expression to produce an Object.
fn eval_if_expression(expr: ast::IfExpression) -> Result<Object> {
    let condition = eval(ast::Node::Expression(*expr.condition))?;

    if is_truthy(&condition) {
        eval(ast::Node::Statement(ast::Statement::Block(
            expr.consequence,
        )))
    } else if let Some(alt) = expr.alternative {
        eval(ast::Node::Statement(ast::Statement::Block(alt)))
    } else {
        Ok(Object::Null)
    }
}

/// Determines if an object is truthy in Monkey.
fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Boolean(false) | Object::Null => false,
        Object::Boolean(true) | _ => true,
    }
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug, PartialEq)]
pub enum Error {
    Evaluation(ast::Node, String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Evaluation(node, err) => write!(f, "evaluating node {}: {}", node, err),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
