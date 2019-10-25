//! An AST evaluator for the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::ast;
use crate::object::{self, Object};
use crate::token::Token;

use std::collections::BTreeMap;
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
            ast::Expression::Float(f) => Ok(Object::Float(f.into())),
            ast::Expression::String(s) => Ok(Object::String(s)),
            ast::Expression::Array(a) => Ok(Object::Array(object::Array {
                elements: eval_expressions(a.elements, env)?,
            })),
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
                let func = eval(ast::Node::Expression(*call.function), env)?;
                let args = eval_expressions(call.arguments, env)?;

                let function = match func {
                    Object::Function(f) => f,
                    // Built-ins use their own execution logic.
                    Object::Builtin(b) => return b.apply(&args).map_err(Error::Object),
                    _ => {
                        return Err(Error::Evaluation(
                            err_node,
                            "can only apply functions with function or builtin object".to_string(),
                        ));
                    }
                };

                apply_function(function, &args, err_node)
            }
            ast::Expression::Index(i) => match (
                eval(ast::Node::Expression(*i.left), env)?,
                eval(ast::Node::Expression(*i.index), env)?,
            ) {
                // Array with numeric index.
                (object::Object::Array(a), object::Object::Integer(i)) => {
                    // Is the element in bounds? If not, return null.
                    if i >= 0 && (i as usize) < a.elements.len() {
                        Ok(a.elements[i as usize].clone())
                    } else {
                        Ok(Object::Null)
                    }
                }

                // Hash with some type of index.
                (object::Object::Hash(h), k) => {
                    let key = match k {
                        // TODO(mdlayher): deduplicate this conversion.
                        object::Object::Boolean(b) => object::Hashable::Boolean(b),
                        object::Object::Integer(i) => object::Hashable::Integer(i),
                        object::Object::String(s) => object::Hashable::String(s),
                        _ => {
                            return Err(Error::Evaluation(
                                err_node,
                                "only strings, integers, and booleans can be used as hash keys"
                                    .to_string(),
                            ));
                        }
                    };

                    // Does the element exist? If not, return null.
                    if let Some(v) = h.pairs.get(&key) {
                        Ok(v.clone())
                    } else {
                        Ok(Object::Null)
                    }
                }

                // Unhandled combination.
                _ => Err(Error::Evaluation(
                    err_node,
                    "index operator not supported on data structure of this type".to_string(),
                )),
            },
            ast::Expression::Hash(h) => {
                let mut pairs = BTreeMap::new();

                for (k, v) in h.pairs {
                    let key = match eval(ast::Node::Expression(k), env)? {
                        // TODO(mdlayher): deduplicate this conversion.
                        object::Object::Boolean(b) => object::Hashable::Boolean(b),
                        object::Object::Integer(i) => object::Hashable::Integer(i),
                        object::Object::String(s) => object::Hashable::String(s),
                        _ => {
                            return Err(Error::Evaluation(
                                err_node,
                                "only strings, integers, and booleans can be used as hash keys"
                                    .to_string(),
                            ));
                        }
                    };

                    pairs.insert(key, eval(ast::Node::Expression(v), env)?);
                }

                Ok(Object::Hash(object::Hash { pairs }))
            }
            // The evaluator is deprecated and new types will be handled
            // exclusively by the compiler.
            _ => unimplemented!(),
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

        (Object::String(l), Object::String(r)) => match expr.operator {
            Token::Plus => Ok(Object::String(l + &r)),

            _ => Err(Error::Evaluation(
                err_node,
                "unhandled string infix operator".to_string(),
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
    match object::Builtin::lookup(&id) {
        // Found a built-in.
        Some(b) => Ok(Object::Builtin(b)),

        // Didn't find a built-in, look for user-defined identifiers.
        None => Ok(env
            .get(&id)
            .ok_or_else(|| Error::UnknownIdentifier(id))?
            .clone()),
    }
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
fn apply_function(
    function: object::Function,
    args: &[Object],
    err_node: ast::Node,
) -> Result<Object> {
    // Bind function arguments in an enclosed environment.
    let mut extended_env = extend_function_env(&function, &args, err_node)?;
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
fn extend_function_env(
    func: &object::Function,
    args: &[Object],
    err_node: ast::Node,
) -> Result<object::Environment> {
    if func.parameters.len() != args.len() {
        return Err(Error::Evaluation(
            err_node,
            format!(
                "expected {} parameters to call function, but got {}",
                func.parameters.len(),
                args.len()
            ),
        ));
    }

    let mut env = object::Environment::new_enclosed(func.env.clone());

    for (i, param) in func.parameters.iter().enumerate() {
        env.set(param.to_string(), &args[i]);
    }

    Ok(env)
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
    Object(object::Error),
    UnknownIdentifier(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Evaluation(node, err) => write!(f, "evaluating node {}: {}", node, err),
            Error::Object(err) => err.fmt(f),
            Error::UnknownIdentifier(id) => write!(f, "unknown identifier: {}", id),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        None
    }
}
