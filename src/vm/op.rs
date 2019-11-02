//! Operations which manipulate and produce `Object` values for the `Vm`.

use std::{
    collections::{BTreeMap, BTreeSet},
    convert::TryFrom,
};

use crate::{
    code::{BinaryOpcode, Opcode},
    object::{self, Hash, Hashable, Object, Set},
};

use super::error::*;

/// Executes a binary float operation.
pub fn binary_float(op: BinaryOpcode, args: &[Object], l: f64, r: f64) -> Result<Object> {
    let out = match op {
        BinaryOpcode::Add => Object::Float(l + r),
        BinaryOpcode::Sub => Object::Float(l - r),
        BinaryOpcode::Mul => Object::Float(l * r),
        BinaryOpcode::Div => Object::Float(l / r),
        BinaryOpcode::Mod => Object::Float(l % r),
        BinaryOpcode::GreaterThan => Object::Boolean(l > r),
        BinaryOpcode::Equal | BinaryOpcode::NotEqual | BinaryOpcode::Index => {
            // Operator not supported with float arguments.
            return Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                Opcode::Binary(op),
                args.to_vec(),
            )));
        }
    };

    Ok(out)
}

/// Executes a binary set operation.
pub fn binary_set(op: BinaryOpcode, args: &[Object], l: &Set, r: &Set) -> Result<Object> {
    // Check for subset.
    if op == BinaryOpcode::GreaterThan {
        return Ok(if r.set.is_subset(&l.set) {
            object::TRUE
        } else {
            object::FALSE
        });
    }

    // TODO(mdlayher): work out the operator situation here, but this
    // is good enough for now.
    let set: BTreeSet<_> = match op {
        BinaryOpcode::Add => l.set.union(&r.set).cloned().collect(),
        BinaryOpcode::Sub => l.set.difference(&r.set).cloned().collect(),
        BinaryOpcode::Mul => l.set.intersection(&r.set).cloned().collect(),
        BinaryOpcode::Div => l.set.symmetric_difference(&r.set).cloned().collect(),
        _ => {
            return Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                Opcode::Binary(op),
                args.to_vec(),
            )));
        }
    };

    Ok(Object::Set(object::Set { set }))
}

/// Executes a pointer arithmetic operation.
pub fn pointer_arithmetic(op: BinaryOpcode, args: &[Object], l: i64, r: i64) -> Result<Object> {
    let out = match op {
        BinaryOpcode::Add => l + r,
        BinaryOpcode::Sub => l - r,
        _ => {
            // Reign in the pointer arithmetic madness!
            return Err(Error::Runtime(ErrorKind::OperatorUnsupported(
                Opcode::Binary(op),
                args.to_vec(),
            )));
        }
    };

    Ok(Object::Pointer(out as usize))
}

/// Executes a composite indexing operation.
pub fn composite_index(args: &[Object]) -> Result<Object> {
    assert_eq!(
        args.len(),
        2,
        "expected exactly 2 arguments for composite indexing operation"
    );

    match (&args[0], &args[1]) {
        // Array with numeric index.
        (Object::Array(a), Object::Integer(i)) => {
            // Is the element in bounds? If not, return null.
            if *i >= 0 && (*i as usize) < a.elements.len() {
                Ok(a.elements[*i as usize].clone())
            } else {
                Ok(Object::Null)
            }
        }
        // Hash with some type of index.
        (object::Object::Hash(h), k) => {
            let k =
                object::Hashable::try_from(k).map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

            // Does the element exist? If not, return null.
            if let Some(v) = h.pairs.get(&k) {
                Ok(v.clone())
            } else {
                Ok(Object::Null)
            }
        }
        // Set with some type of index.
        (object::Object::Set(s), k) => {
            let k =
                object::Hashable::try_from(k).map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

            // Does the element exist in the set?
            if s.set.contains(&k) {
                Ok(object::TRUE)
            } else {
                Ok(object::FALSE)
            }
        }
        _ => Err(Error::Runtime(ErrorKind::OperatorUnsupported(
            Opcode::Binary(BinaryOpcode::Index),
            args.to_vec(),
        ))),
    }
}

/// Builds a Hash from stack objects.
pub fn build_hash(args: &[Object]) -> Result<Object> {
    if args.is_empty() {
        // No arguments, empty hash.
        return Ok(Object::Hash(Hash::default()));
    }

    // The parser and compiler should make this assertion hold.
    assert!(
        args.len() % 2 == 0,
        "hash must contain an even number of objects"
    );

    // Iterate two objects at a time for each key/value pair.
    let mut pairs = BTreeMap::new();

    let mut i = 0;
    while i < args.len() {
        // Only accept object::Hashable objects as keys.
        let k = Hashable::try_from(&args[i]).map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

        // Do not allow duplicate hash keys.
        if pairs.get(&k).is_some() {
            return Err(Error::Runtime(ErrorKind::DuplicateKey(k)));
        }

        let v = args[i + 1].clone();
        i += 2;

        pairs.insert(k, v);
    }

    Ok(Object::Hash(Hash { pairs }))
}

/// Builds a Set from stack objects.
pub fn build_set(args: &[Object]) -> Result<Object> {
    let mut set = BTreeSet::new();

    for a in args {
        // Only accept object::Hashable objects as keys.
        let arg = Hashable::try_from(a).map_err(|e| Error::Runtime(ErrorKind::Object(e)))?;

        // Do not allow duplicate keys in the set.
        if set.contains(&arg) {
            return Err(Error::Runtime(ErrorKind::DuplicateKey(arg)));
        }
        set.insert(arg);
    }

    Ok(Object::Set(Set { set }))
}
