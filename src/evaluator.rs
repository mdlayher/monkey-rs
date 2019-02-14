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
        ast::Node::Program(prog) => {
            // TODO(mdlayher): evaluate more statements in the program.
            eval(ast::Node::Statement(prog.statements[0].clone()))
        }
        ast::Node::Statement(stmt) => match stmt {
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
            _ => Err(Error::Evaluation(
                err_node,
                "unhandled expression type".to_string(),
            )),
        },
    }
}

/// Evaluates a prefix expression to produce an Object.
fn eval_prefix_expression(p: ast::PrefixExpression, err_node: ast::Node) -> Result<Object> {
    // Evaluate the right side before applying the prefix operator.
    let right = eval(ast::Node::Expression(*p.right))?;

    match p.operator {
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
