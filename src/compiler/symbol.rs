//! Symbol types for the `compiler` module.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// A table that can be used to define and resolve `Symbols`.
#[derive(Debug, Default)]
pub struct SymbolTable {
    pub num_definitions: usize,
    store: HashMap<String, Symbol>,
    outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    /// Creates a new `SymbolTable` that can also reference symbols defined by
    /// the `outer` table.
    pub fn new_enclosed(outer: Rc<RefCell<Self>>) -> Self {
        Self {
            outer: Some(outer),
            ..Self::default()
        }
    }

    /// Defines a new `Symbol` by name.
    pub fn define(&mut self, name: String) -> Symbol {
        let scope = match self.outer {
            Some(_) => Scope::Local,
            None => Scope::Global,
        };

        let s = Symbol {
            scope,
            index: self.num_definitions,
        };
        self.num_definitions += 1;

        self.store.insert(name, s.clone());
        s
    }

    /// Resolves a `Symbol` by its name and returns whether or not it
    /// was defined.
    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        match (self.store.get(name), &self.outer) {
            // We found a binding in this symbol table.
            (Some(s), _) => Some(s.clone()),
            // We did not find a binding; try the outer symbol table.
            (None, Some(outer)) => outer.borrow().resolve(name),
            // We found no binding and there is no outer symbol table.
            (None, _) => None,
        }
    }
}

/// A symbol definition.
#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub scope: Scope,
    pub index: usize,
}

/// The scope of a symbol.
#[derive(Clone, Debug, PartialEq)]
pub enum Scope {
    Global,
    Local,
}
