//! Objects produced when evaluating the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::ast;
use std::collections::HashMap;
use std::fmt;

/// Objects produced when evaluating Monkey source code, along with their
/// associated data if applicable.
#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Function(Function),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(i) => i.fmt(f),
            Object::Boolean(b) => b.fmt(f),
            Object::ReturnValue(r) => write!(f, "return({})", r),
            Object::Function(func) => func.fmt(f),
        }
    }
}

/// An execution environment used when evaluating Monkey source code.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    /// Creates a new `Environment`.
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    /// Retrieves the object associated with an identifier name, or returns
    /// `None` if no object is associated with `name`.
    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
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
