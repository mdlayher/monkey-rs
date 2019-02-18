//! An AST evaluator for the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::ast;
use crate::object::{self, Object};
use crate::token::Token;

use std::error;
use std::fmt;
use std::result;

/// Evaluates an `ast::Node` and produces an `object::Object`.
pub fn eval(node: ast::Node, env: &mut object::Environment) -> Result<Object> {
    // TODO(mdlayher): clean up error handling via err_node.
    let err_node = node.clone();
    match node {
        ast::Node::Program(prog) => eval_program(prog, env),
        ast::Node::Statement(stmt) => match stmt {
            ast::Statement::Block(block) => eval_block_statement(block, env),
            ast::Statement::Expression(expr) => eval(ast::Node::Expression(expr), env),
            ast::Statement::Let(stmt) => {
                let obj = eval(ast::Node::Expression(stmt.value), env)?;

                // eval succeeded; capture this binding in our environment.
                env.set(stmt.name, &obj);
                Ok(obj)
            }
            ast::Statement::Return(ret) => Ok(Object::ReturnValue(Box::new(eval(
                ast::Node::Expression(ret.value),
                env,
            )?))),
        },
        ast::Node::Expression(expr) => match expr {
            ast::Expression::Integer(i) => Ok(Object::Integer(i.value)),
            ast::Expression::Boolean(b) => Ok(Object::Boolean(b)),
            ast::Expression::Prefix(p) => eval_prefix_expression(p, env, err_node),
            ast::Expression::Infix(i) => eval_infix_expression(i, env, err_node),
            ast::Expression::If(stmt) => eval_if_expression(stmt, env),
            ast::Expression::Identifier(id) => eval_identifier(id, env),

            _ => Err(Error::Evaluation(
                err_node,
                "unhandled expression type".to_string(),
            )),
        },
    }
}

/// Evaluates a program and returns the result.
fn eval_program(prog: ast::Program, env: &mut object::Environment) -> Result<Object> {
    let mut result = Object::Null;

    for stmt in prog.statements {
        result = eval(ast::Node::Statement(stmt.clone()), env)?;

        // Handle early return statements if applicable, unwrapping the inner
        // value and terminating the program.
        if let Object::ReturnValue(value) = result {
            return Ok(*value);
        }
    }

    Ok(result)
}

/// Evaluates a block statement and returns the result.
fn eval_block_statement(
    block: ast::BlockStatement,
    env: &mut object::Environment,
) -> Result<Object> {
    let mut result = Object::Null;

    for stmt in block.statements {
        result = eval(ast::Node::Statement(stmt.clone()), env)?;

        // Handle early return statements if applicable, but do not unwrap the
        // inner value so that only this block statement terminates, and not
        // the entire program.
        if let Object::ReturnValue(_) = result {
            return Ok(result);
        }
    }

    Ok(result)
}

/// Evaluates a prefix expression to produce an Object.
fn eval_prefix_expression(
    expr: ast::PrefixExpression,
    env: &mut object::Environment,
    err_node: ast::Node,
) -> Result<Object> {
    // Evaluate the right side before applying the prefix operator.
    let right = eval(ast::Node::Expression(*expr.right), env)?;

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
fn eval_infix_expression(
    expr: ast::InfixExpression,
    env: &mut object::Environment,
    err_node: ast::Node,
) -> Result<Object> {
    let left = eval(ast::Node::Expression(*expr.left), env)?;
    let right = eval(ast::Node::Expression(*expr.right), env)?;

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
fn eval_if_expression(expr: ast::IfExpression, env: &mut object::Environment) -> Result<Object> {
    let condition = eval(ast::Node::Expression(*expr.condition), env)?;

    if is_truthy(&condition) {
        eval(
            ast::Node::Statement(ast::Statement::Block(expr.consequence)),
            env,
        )
    } else if let Some(alt) = expr.alternative {
        eval(ast::Node::Statement(ast::Statement::Block(alt)), env)
    } else {
        Ok(Object::Null)
    }
}

/// Evaluates an object bound to an identifier and returns the result.
fn eval_identifier(id: String, env: &mut object::Environment) -> Result<Object> {
    Ok(env
        .get(&id)
        .ok_or_else(|| Error::UnknownIdentifier(id))?
        .clone())
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
    UnknownIdentifier(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Evaluation(node, err) => write!(f, "evaluating node {}: {}", node, err),
            Error::UnknownIdentifier(id) => write!(f, "unknown identifier: {}", id),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
