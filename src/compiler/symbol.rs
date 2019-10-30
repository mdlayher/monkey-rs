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

#[cfg(test)]
mod tests {
    use super::{Scope, Symbol, SymbolTable};
    use std::{cell::RefCell, rc::Rc};

    #[test]
    fn symbol_table_global_ok() {
        let mut st = SymbolTable::default();
        st.define("a".to_string());

        let tests = vec![
            (
                "b",
                Symbol {
                    scope: Scope::Global,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    scope: Scope::Global,
                    index: 2,
                },
            ),
        ];

        for (name, symbol) in &tests {
            let defined = st.define(name.to_string());
            let resolved = st.resolve(name).expect("a symbol should be defined");

            assert_eq!(defined, resolved, "defined and resolved symbol mismatch");
            assert_eq!(resolved, *symbol, "test case symbol mismatch");

            st.resolve(&"a").expect("a should always be defined");
        }
    }

    #[test]
    fn symbol_table_local_ok() {
        let global = Rc::new(RefCell::new(SymbolTable::default()));
        global.borrow_mut().define("a".to_string());

        let local = Rc::new(RefCell::new(SymbolTable::new_enclosed(global)));
        local.borrow_mut().define("b".to_string());

        let mut nested = SymbolTable::new_enclosed(local);
        nested.define("c".to_string());

        let tests = vec![
            (
                "b",
                Symbol {
                    scope: Scope::Local,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    scope: Scope::Local,
                    index: 2,
                },
            ),
        ];

        for (name, symbol) in &tests {
            let defined = nested.define(name.to_string());
            let resolved = nested.resolve(name).expect("a symbol should be defined");

            assert_eq!(defined, resolved, "defined and resolved symbol mismatch");
            assert_eq!(resolved, *symbol, "test case symbol mismatch");

            let gsym = nested.resolve(&"a").expect("a should always be defined");
            assert_eq!(0, gsym.index);
            assert_eq!(Scope::Global, gsym.scope);
        }
    }
}
