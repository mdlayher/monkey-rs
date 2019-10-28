//! Objects produced when evaluating the Monkey programming language from
//! <https://interpreterbook.com/>.

use crate::ast;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::TryFrom;
use std::error;
use std::fmt;
use std::result;

/// Boolean object constants for true and false for easy use.
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

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
    Array(Array),
    Hash(Hash),
    Set(Set),
    CompiledFunction(CompiledFunction),
    Pointer(usize),
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
            Object::Array(a) => a.fmt(f),
            Object::Hash(h) => h.fmt(f),
            Object::Set(s) => s.fmt(f),
            Object::CompiledFunction(func) => func.fmt(f),
            Object::Pointer(p) => write!(f, "{:#06x}", p),
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
    First,
    Last,
    Len,
    Push,
    Puts,
    Rest,
}

impl Builtin {
    /// Constructs a built-in using its name.
    pub fn lookup(name: &str) -> Option<Self> {
        match name {
            "first" => Some(Builtin::First),
            "last" => Some(Builtin::Last),
            "len" => Some(Builtin::Len),
            "push" => Some(Builtin::Push),
            "puts" => Some(Builtin::Puts),
            "rest" => Some(Builtin::Rest),

            _ => None,
        }
    }

    /// Applies the appropriate built-in function on `args` to produce an
    /// `Object`.
    pub fn apply(&self, args: &[Object]) -> Result<Object> {
        match self {
            Builtin::First => builtin_first(&args),
            Builtin::Last => builtin_last(&args),
            Builtin::Len => builtin_len(&args),
            Builtin::Push => builtin_push(&args),
            Builtin::Puts => builtin_puts(&args),
            Builtin::Rest => builtin_rest(&args),
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::First => write!(f, "first"),
            Builtin::Last => write!(f, "last"),
            Builtin::Len => write!(f, "len"),
            Builtin::Push => write!(f, "push"),
            Builtin::Puts => write!(f, "puts"),
            Builtin::Rest => write!(f, "rest"),
        }
    }
}

fn builtin_first(args: &[Object]) -> Result<Object> {
    if args.len() != 1 {
        return Err(Error::Builtin(
            Builtin::First,
            format!("expected 1 argument, but got {}", args.len()),
        ));
    }

    if let Object::Array(a) = &args[0] {
        if !a.elements.is_empty() {
            Ok(a.elements.first().unwrap().clone())
        } else {
            Ok(Object::Null)
        }
    } else {
        Err(Error::Builtin(
            Builtin::First,
            format!("argument {} is not an array", &args[0]),
        ))
    }
}

fn builtin_last(args: &[Object]) -> Result<Object> {
    if args.len() != 1 {
        return Err(Error::Builtin(
            Builtin::Last,
            format!("expected 1 argument, but got {}", args.len()),
        ));
    }

    if let Object::Array(a) = &args[0] {
        if !a.elements.is_empty() {
            Ok(a.elements.last().unwrap().clone())
        } else {
            Ok(Object::Null)
        }
    } else {
        Err(Error::Builtin(
            Builtin::Last,
            format!("argument {} is not an array", &args[0]),
        ))
    }
}

fn builtin_len(args: &[Object]) -> Result<Object> {
    if args.len() != 1 {
        return Err(Error::Builtin(
            Builtin::Len,
            format!("expected 1 argument, but got {}", args.len()),
        ));
    }

    match &args[0] {
        Object::Array(a) => Ok(Object::Integer(a.elements.len() as i64)),
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        _ => Err(Error::Builtin(
            Builtin::Len,
            format!("argument {} cannot be used", &args[0]),
        )),
    }
}

fn builtin_push(args: &[Object]) -> Result<Object> {
    if args.len() != 2 {
        return Err(Error::Builtin(
            Builtin::Push,
            format!("expected 2 argument, but got {}", args.len()),
        ));
    }

    let arr = if let Object::Array(a) = &args[0] {
        a
    } else {
        return Err(Error::Builtin(
            Builtin::Push,
            format!("first argument {} is not an array", &args[0]),
        ));
    };

    let mut out = arr.elements.clone();
    out.push(args[1].clone());

    Ok(Object::Array(Array { elements: out }))
}

fn builtin_puts(args: &[Object]) -> Result<Object> {
    for a in args {
        println!("{}", a);
    }
    Ok(Object::Null)
}

fn builtin_rest(args: &[Object]) -> Result<Object> {
    if args.len() != 1 {
        return Err(Error::Builtin(
            Builtin::Rest,
            format!("expected 1 argument, but got {}", args.len()),
        ));
    }

    if let Object::Array(a) = &args[0] {
        if !a.elements.is_empty() {
            Ok(Object::Array(Array {
                elements: a.elements.iter().skip(1).cloned().collect(),
            }))
        } else {
            Ok(Object::Null)
        }
    } else {
        Err(Error::Builtin(
            Builtin::Rest,
            format!("argument {} is not an array", &args[0]),
        ))
    }
}

/// The object representation of a Monkey array.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|e| format!("{}", e))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

/// Objects which may be used as `Hash` keys.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Hashable {
    Boolean(bool),
    Integer(i64),
    String(String),
}

impl fmt::Display for Hashable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Hashable::Integer(i) => i.fmt(f),
            Hashable::Boolean(b) => b.fmt(f),
            Hashable::String(s) => s.fmt(f),
        }
    }
}

impl TryFrom<&Object> for Hashable {
    type Error = Error;

    /// Attempt conversion from an Object to a Hashable enum.
    fn try_from(obj: &Object) -> Result<Self> {
        match obj {
            Object::Integer(i) => Ok(Self::Integer(*i)),
            Object::Boolean(b) => Ok(Self::Boolean(*b)),
            Object::String(s) => Ok(Self::String(s.to_string())),
            _ => Err(Error::NotHashable(obj.clone())),
        }
    }
}

/// The object representation of a Monkey hash.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Hash {
    pub pairs: BTreeMap<Hashable, Object>,
}

impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut pairs = vec![];
        for pair in &self.pairs {
            pairs.push(format!(r#"{}: {}"#, pair.0, pair.1));
        }

        // Sort for deterministic output.
        pairs.sort();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

/// The object representation of a Monkey set.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Set {
    pub set: BTreeSet<Hashable>,
}

impl fmt::Display for Set {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut items = vec![];
        for e in &self.set {
            items.push(format!("{}", e));
        }

        write!(f, "set{{{}}}", items.join(", "))
    }
}

/// The object representation of a compiled Monkey function.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CompiledFunction {
    pub instructions: Vec<u8>,
}

impl fmt::Display for CompiledFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "compiled_fn({:?})", self.instructions)
    }
}

/// A Result type specialized use with for an Error.
pub type Result<T> = result::Result<T, Error>;

/// Specifies the different classes of errors which may occur.
#[derive(Debug, PartialEq)]
pub enum Error {
    Builtin(Builtin, String),
    NotHashable(Object),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Builtin(b, err) => write!(f, "built-in {}: {}", b, err),
            Error::NotHashable(obj) => write!(f, "object {:?} cannot be a hash key", obj),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        None
    }
}
