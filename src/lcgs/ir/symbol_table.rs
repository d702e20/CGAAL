use std::collections::HashMap;

use crate::lcgs::ast::Decl;
use std::fmt::{Display, Formatter};
use std::cell::RefCell;

/// An identifier for a symbol with a given owner.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SymbolIdentifier {
    pub owner: Owner,
    pub name: String,
}

impl Display for SymbolIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", match &self.owner {
            Owner::Player(pname) => pname,
            Owner::Global => ":global",
        }, &self.name)
    }
}

/// A `Symbol` is an identifier and information about it.
#[derive(Debug)]
pub struct Symbol {
    pub identifier: SymbolIdentifier,
    pub declaration: Decl,
}

/// A `SymbolTable` keeps track of registered symbols and their properties.
/// In this language symbols always belongs to either the global scope or a
/// player. Hence, keys are `SymbolIdentifier`s consisting of both an
/// owner and a name.
pub struct SymbolTable {
    symbols: HashMap<SymbolIdentifier, RefCell<Symbol>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            symbols: HashMap::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    /// Creates and inserts a symbol for the given declaration for the given owner with the
    /// given name. If the name is already associated with a different symbol, the previous
    /// symbol is returned.
    pub fn insert(&mut self, owner: &Owner, name: &str, decl: Decl) -> Option<RefCell<Symbol>> {
        let symb_id = SymbolIdentifier {
            owner: owner.clone(),
            name: name.to_string(),
        };
        let symb = Symbol {
            identifier: symb_id.clone(),
            declaration: decl,
        };
        self.symbols.insert(symb_id, RefCell::new(symb))
    }

    /// Get the symbol associated with the given owner and name, if it exists.
    pub fn get(&self, owner: &Owner, name: &str) -> Option<&RefCell<Symbol>> {
        let symb_id = SymbolIdentifier {
            owner: owner.clone(),
            name: name.to_string(),
        };
        self.symbols.get(&symb_id)
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable::new()
    }
}

impl<'a> IntoIterator for &'a SymbolTable {
    type Item = (&'a SymbolIdentifier, &'a RefCell<Symbol>);
    type IntoIter = std::collections::hash_map::Iter<'a, SymbolIdentifier, RefCell<Symbol>>;

    fn into_iter(self) -> Self::IntoIter {
        self.symbols.iter()
    }
}

/// OwnedIdentifiers always belongs to a player or the global scope. This enum allows
/// us to abstract over both possibilities.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Owner {
    Player(String),
    Global,
}

impl Owner {
    #[inline]
    pub fn symbol_id(&self, name: &str) -> SymbolIdentifier {
        SymbolIdentifier {
            owner: self.clone(),
            name: name.to_string(),
        }
    }
}