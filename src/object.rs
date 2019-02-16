//! Objects produced when evaluating the Monkey programming language from
//! <https://interpreterbook.com/>.

use std::fmt;

/// Objects produced when evaluating Monkey source code, along with their
/// associated data if applicable.
#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(i) => i.fmt(f),
            Object::Boolean(b) => b.fmt(f),
            Object::ReturnValue(r) => write!(f, "return({})", r),
        }
    }
}
