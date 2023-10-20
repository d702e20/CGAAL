use std::cell::RefCell;
use std::fmt::Display;
use std::ops::Deref;

use crate::parsing::ast::Decl;

/// An index of a [Symbol] in a [SymbolTable].
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct SymbIdx(pub usize);

impl Deref for SymbIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for SymbIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// An entry in a [SymbolTable].
#[derive(Clone, Debug)]
struct Symbol {
    name: String,
    decl_rc: RefCell<Decl>,
}

/// A [SymbolTable] keeps track of registered symbols and their declarations.
/// Every declaration must have a unique name, aka a unique owned identifier.
#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
}

impl Symbol {
    pub fn new(decl: Decl) -> Self {
        Symbol {
            name: decl.ident.to_string(),
            decl_rc: decl.into(),
        }
    }
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            symbols: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    /// Creates and inserts a symbol for the given declaration under the given owned name.
    /// Returns the index of the inserted symbol.
    /// If the name is already associated with a different symbol, an error is returned instead.
    pub fn insert(&mut self, decl: Decl) -> Result<SymbIdx, String> {
        if self.exists(&decl.ident.to_string()) {
            return Err(format!("The name '{}' is already declared", decl.ident));
        }
        self.symbols.push(Symbol::new(decl));
        Ok(SymbIdx(self.symbols.len() - 1))
    }

    /// Returns the declaration associated with the given index.
    pub fn get(&self, idx: SymbIdx) -> &RefCell<Decl> {
        &self.symbols[idx.0].decl_rc
    }

    /// Returns if any known declaration has the given name
    pub fn exists(&self, name: &str) -> bool {
        self.symbols.iter().any(|s| s.name == name)
    }

    /// Returns the declaration associated with the given identifier.
    pub fn get_by_name(&self, name: &str) -> Option<&RefCell<Decl>> {
        for symb in &self.symbols {
            if &symb.name == name {
                return Some(&symb.decl_rc);
            }
        }
        None
    }

    /// Returns the [SymbIdx] of the declaration with the given name.
    pub fn get_index_of_name(&self, name: &str) -> Option<SymbIdx> {
        self.symbols
            .iter()
            .enumerate()
            .find_map(|(i, s)| (&s.name == name).then_some(SymbIdx(i)))
    }

    pub fn iter(&self) -> impl Iterator<Item = &RefCell<Decl>> {
        self.symbols.iter().map(|s| &s.decl_rc)
    }

    /// Consume the symbol table to construct a simple declaration list with the same ordering.
    pub fn solidify(mut self) -> Vec<Decl> {
        self.symbols
            .drain(..)
            .map(|symb| symb.decl_rc.into_inner())
            .collect()
    }
}
