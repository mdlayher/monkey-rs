//! Symbol types for the `compiler` module.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object;

/// A table that can be used to define and resolve `Symbols`.
#[derive(Debug, Default)]
pub struct SymbolTable {
    pub num_definitions: usize,
    pub free: Vec<Symbol>,
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

    /// Defines a built-in symbol.
    pub fn define_builtin(&mut self, b: object::Builtin) {
        let index = b.clone() as usize;
        let name = format!("{}", b);

        self.store.insert(
            name.clone(),
            Symbol {
                name,
                scope: Scope::Builtin,
                index,
            },
        );
    }

    /// Defines a free symbol.
    pub fn define_free(&mut self, orig: Symbol) -> Symbol {
        self.free.push(orig.clone());

        let s = Symbol {
            name: orig.name.clone(),
            scope: Scope::Free,
            index: self.free.len() - 1,
        };

        self.store.insert(orig.name, s.clone());
        s
    }

    /// Defines a new `Symbol` by name.
    pub fn define(&mut self, name: String) -> Symbol {
        let scope = match self.outer {
            Some(_) => Scope::Local,
            None => Scope::Global,
        };

        let s = Symbol {
            name: name.clone(),
            scope,
            index: self.num_definitions,
        };
        self.num_definitions += 1;

        self.store.insert(name, s.clone());
        s
    }

    /// Resolves a `Symbol` by its name and returns whether or not it
    /// was defined.
    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        if let Some(s) = self.store.get(name) {
            return Some(s.clone());
        }

        // Ensure self.outer exists before trying to resolve through it.
        let outer = &self.outer.as_ref()?;

        let (obj, free) = match outer.borrow_mut().resolve(name) {
            Some(s) => match s.scope {
                Scope::Builtin | Scope::Global => (Some(s), false),
                _ => (Some(s), true),
            },
            None => (None, false),
        };

        match (obj, free) {
            (Some(s), true) => Some(self.define_free(s)),
            (Some(s), false) => Some(s),
            (None, _) => None,
        }
    }
}

/// A symbol definition.
#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: Scope,
    pub index: usize,
}

/// The scope of a symbol.
#[derive(Clone, Debug, PartialEq)]
pub enum Scope {
    Builtin,
    Global,
    Local,
    Free,
}

#[cfg(test)]
mod tests {
    use super::{Scope, Symbol, SymbolTable};
    use crate::object;
    use std::{cell::RefCell, rc::Rc};

    #[test]
    fn symbol_table_global_ok() {
        let mut st = SymbolTable::default();
        st.define("a".to_string());

        let tests = vec![
            (
                "b",
                Symbol {
                    name: "b".to_string(),
                    scope: Scope::Global,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    name: "c".to_string(),
                    scope: Scope::Global,
                    index: 2,
                },
            ),
        ];

        for (name, symbol) in &tests {
            let defined = st.define((*name).to_string());
            let resolved = st.resolve(name).expect("a symbol should be defined");

            assert_eq!(defined, resolved, "defined and resolved symbol mismatch");
            assert_eq!(resolved, *symbol, "test case symbol mismatch");

            st.resolve(&"a").expect("a should always be defined");
        }
    }

    #[test]
    fn symbol_table_nesting_ok() {
        let global = Rc::new(RefCell::new(SymbolTable::default()));
        global.borrow_mut().define("a".to_string());

        global.borrow_mut().define_builtin(object::Builtin::First);

        let local = Rc::new(RefCell::new(SymbolTable::new_enclosed(global)));
        local.borrow_mut().define("b".to_string());

        let mut nested = SymbolTable::new_enclosed(local);
        nested.define("c".to_string());

        let tests = vec![
            (
                "b",
                Symbol {
                    name: "b".to_string(),
                    scope: Scope::Local,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    name: "c".to_string(),
                    scope: Scope::Local,
                    index: 2,
                },
            ),
        ];

        for (name, symbol) in &tests {
            let defined = nested.define((*name).to_string());
            let resolved = nested.resolve(name).expect("a symbol should be defined");

            assert_eq!(defined, resolved, "defined and resolved symbol mismatch");
            assert_eq!(resolved, *symbol, "test case symbol mismatch");

            let gsym = nested.resolve(&"a").expect("a should always be defined");
            assert_eq!(0, gsym.index);
            assert_eq!(Scope::Global, gsym.scope);

            let bsym = nested
                .resolve(&"first")
                .expect("first should always be defined");
            assert_eq!(0, bsym.index);
            assert_eq!(Scope::Builtin, bsym.scope);
        }
    }
}
