use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::lcgs::ast::Decl;

/// An identifier for a symbol with a given owner.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct SymbolIdentifier {
    pub owner: Owner,
    pub name: String,
}

impl From<&str> for SymbolIdentifier {
    /// Converts a string into a symbol identifier. The string must be on the form "owner.name".
    /// If the owner is ":global", then the owner will be the global scope.
    fn from(string: &str) -> SymbolIdentifier {
        let split: Vec<&str> = string.split(".").collect();
        debug_assert!(split.len() == 2, "Invalid symbol identifier. Must consist of an owner and a name.");
        let owner = match split[0] {
            ":global" => Owner::Global,
            player => Owner::Player(player.to_string())
        };
        let name = split[1].to_string();
        SymbolIdentifier { owner, name }
    }
}

impl Display for SymbolIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", &self.owner, &self.name)
    }
}

/// A `Symbol` is an identifier and information about it.
#[derive(Debug, Clone)]
pub struct Symbol {
    pub identifier: SymbolIdentifier,
    pub declaration: RefCell<Decl>,
}

/// A `SymbolTable` keeps track of registered symbols and their properties.
/// In this language symbols always belongs to either the global scope or a
/// player. Hence, keys are `SymbolIdentifier`s consisting of both an
/// owner and a name.
#[derive(Clone, Debug)]
pub struct SymbolTable {
    symbols: HashMap<SymbolIdentifier, Symbol>,
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
    pub fn insert(&mut self, owner: &Owner, name: &str, decl: Decl) -> Option<Symbol> {
        let symb_id = SymbolIdentifier {
            owner: owner.clone(),
            name: name.to_string(),
        };
        let symb = Symbol {
            identifier: symb_id.clone(),
            declaration: RefCell::new(decl),
        };
        self.symbols.insert(symb_id, symb)
    }

    /// Get the symbol associated with the given owner and name, if it exists.
    pub fn get(&self, owner: &Owner, name: &str) -> Option<&Symbol> {
        let symb_id = SymbolIdentifier {
            owner: owner.clone(),
            name: name.to_string(),
        };
        self.symbols.get(&symb_id)
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, SymbolIdentifier, Symbol> {
        self.into_iter()
    }

    /// Consume the symbol table to construct a simple declaration table.
    pub fn solidify(mut self) -> HashMap<SymbolIdentifier, Decl> {
        self.symbols.drain().map(|(symb_id, symb)| {
            (symb_id, symb.declaration.into_inner())
        }).collect()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable::new()
    }
}

impl<'a> IntoIterator for &'a SymbolTable {
    type Item = (&'a SymbolIdentifier, &'a Symbol);
    type IntoIter = std::collections::hash_map::Iter<'a, SymbolIdentifier, Symbol>;

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

impl Display for Owner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Owner::Player(pname) => pname,
                Owner::Global => ":global",
            }
        )
    }
}
