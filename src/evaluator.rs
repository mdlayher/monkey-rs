//! An AST evaluator for the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::ast;
use crate::object::Object;

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
            _ => Err(Error::Evaluation(
                err_node,
                "unhandled expression type".to_string(),
            )),
        },
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
