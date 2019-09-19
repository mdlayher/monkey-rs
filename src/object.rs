//! Objects produced when evaluating the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::ast;

use std::collections::HashMap;
use std::error;
use std::fmt;
use std::result;

/// Objects produced when evaluating Monkey source code, along with their
/// associated data if applicable.
#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    ReturnValue(Box<Object>),
    Function(Function),
    Builtin(Builtin),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(i) => i.fmt(f),
            Object::Float(fl) => fl.fmt(f),
            Object::Boolean(b) => b.fmt(f),
            Object::String(s) => s.fmt(f),
            Object::ReturnValue(r) => write!(f, "return({})", r),
            Object::Function(func) => func.fmt(f),
            Object::Builtin(b) => b.fmt(f),
        }
    }
}

/// An execution environment used when evaluating Monkey source code.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    /// Creates a new `Environment`.
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    /// Creates an enclosed `Environment` for use within a function call.
    pub fn new_enclosed(outer: Self) -> Self {
        let mut env = Self::new();
        env.outer = Some(Box::new(outer));
        env
    }

    /// Retrieves the object associated with an identifier name, or returns
    /// `None` if no object is associated with `name`.
    pub fn get(&self, name: &str) -> Option<&Object> {
        match (self.store.get(name), &self.outer) {
            // We found a binding in this environment; no need to consult the
            // outer environment.
            (Some(obj), _) => Some(obj),
            // We did not find a binding; try the outer environment.
            (None, Some(outer)) => outer.get(name),
            // We found no binding and there is no outer environment.
            (None, _) => None,
        }
    }

    /// Binds an object in the environment with the identifier `name`.
    pub fn set(&mut self, name: String, obj: &Object) -> Object {
        self.store.insert(name, obj.clone());
        obj.clone()
    }
}

/// The object representation of a Monkey function.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub parameters: Vec<String>,
    pub body: ast::BlockStatement,
    pub env: Environment,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params = self.parameters.join(", ");

        write!(f, "fn({}) {{\n{}\n}}", params, self.body)
    }
}

/// The object representation of a built-in Monkey function.
#[derive(Clone, Debug, PartialEq)]
pub enum Builtin {
    Len,
}

impl Builtin {
    /// Constructs a built-in using its name.
    pub fn lookup(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Builtin::Len),

            _ => None,
        }
    }

    /// Applies the appropriate built-in function on `args` to produce an
    /// `Object`.
    pub fn apply(&self, args: &[Object]) -> Result<Object> {
        match self {
            Builtin::Len => builtin_len(&args),
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::Len => write!(f, "len"),
        }
    }
}

fn builtin_len(args: &[Object]) -> Result<Object> {
    if args.len() != 1 {
        return Err(Error::Builtin(
            Builtin::Len,
            format!("expected 1 argument, but got {}", args.len()),
        ));
    }

    let string = if let Object::String(s) = &args[0] {
        s
    } else {
        return Err(Error::Builtin(
            Builtin::Len,
            format!("argument {} is not a string", &args[0]),
        ));
    };

    Ok(Object::Integer(string.len() as i64))
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug, PartialEq)]
pub enum Error {
    Builtin(Builtin, String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Builtin(b, err) => write!(f, "built-in {}: {}", b, err),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        None
    }
}
