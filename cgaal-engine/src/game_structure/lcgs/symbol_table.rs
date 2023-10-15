use std::cell::RefCell;
use std::fmt::Display;
use std::ops::Deref;

use crate::parsing::ast::{Decl, OwnedIdent};

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

/// A [SymbolTable] keeps track of registered symbols and their declarations.
/// Every declaration must have a unique name, aka a unique owned identifier.
#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    symbols: Vec<RefCell<Decl>>,
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
        for symbol in &self.symbols {
            if &symbol.borrow().ident == &decl.ident {
                return Err(format!("The name '{}' is already declared", decl.ident));
            }
        }
        self.symbols.push(decl.into());
        Ok(SymbIdx(self.symbols.len() - 1))
    }

    /// Returns the declaration associated with the given index.
    pub fn get(&self, idx: SymbIdx) -> &RefCell<Decl> {
        &self.symbols[idx.0]
    }

    /// Returns the declaration associated with the given identifier.
    pub fn get_by_name(&self, oi: &OwnedIdent) -> Option<&RefCell<Decl>> {
        for symb in self.symbols.iter() {
            if &symb.borrow().ident == oi {
                return Some(symb);
            }
        }
        None
    }

    pub fn iter(&self) -> std::slice::Iter<RefCell<Decl>> {
        self.symbols.iter()
    }

    /// Consume the symbol table to construct a simple declaration table.
    pub fn solidify(mut self) -> Vec<Decl> {
        self.symbols
            .drain(..)
            .map(|symb| symb.into_inner())
            .collect()
    }
}

impl<'a> IntoIterator for &'a SymbolTable {
    type Item = &'a RefCell<Decl>;
    type IntoIter = std::slice::Iter<'a, RefCell<Decl>>;

    fn into_iter(self) -> Self::IntoIter {
        self.symbols.iter()
    }
}
