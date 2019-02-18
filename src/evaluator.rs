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
            ast::Expression::Float(f) => Ok(Object::Float(f)),
            ast::Expression::String(_s) => panic!("unhandled!"),
            ast::Expression::Prefix(p) => eval_prefix_expression(p, env, err_node),
            ast::Expression::Infix(i) => eval_infix_expression(i, env, err_node),
            ast::Expression::If(stmt) => eval_if_expression(stmt, env),
            ast::Expression::Identifier(id) => eval_identifier(id, env),
            ast::Expression::Function(func) => Ok(Object::Function(object::Function {
                parameters: func.parameters,
                body: func.body,

                // TODO(mdlayher): lifetimes get pretty ugly here if we don't
                // clone this.
                env: env.clone(),
            })),
            ast::Expression::Call(call) => {
                let function = if let Object::Function(func) =
                    eval(ast::Node::Expression(*call.function), env)?
                {
                    func
                } else {
                    return Err(Error::Evaluation(
                        err_node,
                        "can only apply functions with function object".to_string(),
                    ));
                };

                let args = eval_expressions(call.arguments, env)?;

                apply_function(function, args)
            }
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
            Object::Float(f) => Ok(Object::Float(-f)),

            _ => Err(Error::Evaluation(
                err_node,
                "cannot negate non-numeric value".to_string(),
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
            Token::Plus | Token::Minus | Token::Asterisk | Token::Slash | Token::Percent => Ok(
                Object::Integer(eval_infix_op(expr.operator, l as f64, r as f64) as i64),
            ),
            Token::LessThan => Ok(Object::Boolean(l < r)),
            Token::GreaterThan => Ok(Object::Boolean(l > r)),
            Token::Equal => Ok(Object::Boolean(l == r)),
            Token::NotEqual => Ok(Object::Boolean(l != r)),

            _ => Err(Error::Evaluation(
                err_node,
                "unhandled integer infix operator".to_string(),
            )),
        },

        (Object::Float(l), Object::Float(r)) => match expr.operator {
            Token::Plus | Token::Minus | Token::Asterisk | Token::Slash | Token::Percent => {
                Ok(Object::Float(eval_infix_op(expr.operator, l, r)))
            }

            _ => Err(Error::Evaluation(
                err_node,
                "unhandled float infix operator".to_string(),
            )),
        },

        // TODO(mdlayher): this duplication is a little gross.
        (Object::Integer(l), Object::Float(r)) => match expr.operator {
            Token::Plus | Token::Minus | Token::Asterisk | Token::Slash | Token::Percent => Ok(
                Object::Float(eval_infix_op(expr.operator, l as f64, r as f64)),
            ),

            _ => Err(Error::Evaluation(
                err_node,
                "unhandled integer/float infix operator".to_string(),
            )),
        },

        (Object::Float(l), Object::Integer(r)) => match expr.operator {
            Token::Plus | Token::Minus | Token::Asterisk | Token::Slash | Token::Percent => Ok(
                Object::Float(eval_infix_op(expr.operator, l as f64, r as f64)),
            ),

            _ => Err(Error::Evaluation(
                err_node,
                "unhandled float/integer infix operator".to_string(),
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

// Evaluates `l (op) r` and returns the result for simple mathematical operations.
fn eval_infix_op(op: Token, l: f64, r: f64) -> f64 {
    match op {
        Token::Plus => l + r,
        Token::Minus => l - r,
        Token::Asterisk => l * r,
        Token::Slash => l / r,
        Token::Percent => l % r,

        _ => panic!("eval_infix_op called with unsupported operator"),
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

/// Evaluates several expressions and produces objects for each of them.
fn eval_expressions(
    expressions: Vec<ast::Expression>,
    env: &mut object::Environment,
) -> Result<Vec<Object>> {
    let mut results = vec![];

    for expr in expressions {
        results.push(eval(ast::Node::Expression(expr), env)?);
    }

    Ok(results)
}

/// Applies a function with arguments to produce a result object.
fn apply_function(function: object::Function, args: Vec<Object>) -> Result<Object> {
    // Bind function arguments in an enclosed environment.
    let mut extended_env = extend_function_env(&function, args);
    let evaluated = eval(
        ast::Node::Statement(ast::Statement::Block(function.body)),
        &mut extended_env,
    )?;

    // If the function had an early return, stop evaluation.
    if let Object::ReturnValue(ret) = evaluated {
        Ok(*ret)
    } else {
        Ok(evaluated)
    }
}

// Extends a function's environment to bind its arguments.
fn extend_function_env(func: &object::Function, args: Vec<Object>) -> object::Environment {
    let mut env = object::Environment::new_enclosed(func.env.clone());

    for (i, param) in func.parameters.iter().enumerate() {
        env.set(param.to_string(), &args[i]);
    }

    env
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
