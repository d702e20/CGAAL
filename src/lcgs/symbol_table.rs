use crate::lcgs::ast::{ConstDecl, LabelDecl, PlayerDecl, StateVarDecl, TemplateDecl, TransitionDecl, Decl};
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use pom::set::Set;

/// An identifier for a symbol with a given owner.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SymbolIdentifier {
    pub owner: Owner,
    pub name: String,
}

/// A `Symbol` is an identifier and information about it.
#[derive(Debug)]
pub struct Symbol {
    pub identifier: SymbolIdentifier,
    pub declaration: Rc<Decl>,
}

/// A `PlayerSymbolTable` contains registered symbols belonging to a player
/// (or the global scope).
#[derive(Default)]
pub struct PlayerSymbolTable {
    player: Owner,
    symbols: HashMap<String, Symbol>,
}

impl PlayerSymbolTable {
    /// Create a new symbol table for the given owner
    fn new(owner: Owner) -> PlayerSymbolTable {
        PlayerSymbolTable {
            player: owner,
            symbols: HashMap::new(),
        }
    }

    /// Insert a symbol with the given name pointing at the given declaration.
    /// If the name is new [None] is returned. If the name was already association
    /// with a symbol, the previous symbol is returned.
    pub fn insert(&mut self, name: &str, decl: Rc<Decl>) -> Option<Symbol> {
        let symb = Symbol {
            identifier: SymbolIdentifier {
                owner: self.player.clone(),
                name: name.to_string(),
            },
            declaration: decl
        };
        self.symbols.insert(name.to_string(), symb)
    }

    /// Returns the symbol associated with the given name if such exists.
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

/// A `SymbolTable` keeps track of registered symbols and their properties.
/// In this language symbols always belongs to either the global scope or a
/// player. The player's name gives access to the symbols owner by that player.
#[derive(Default)]
pub struct SymbolTable {
    player_tables: HashMap<String, PlayerSymbolTable>,
    global_table: PlayerSymbolTable,
}

impl SymbolTable {
    /// Register a new `PlayerSymbolTable` of the given name. If a `PlayerSymbolTable`
    /// is already registered for that name, an Err is returned
    pub fn add_player(&mut self, player_name: &str) -> Result<(), ()> {
        if self.player_tables.contains_key(player_name) {
            Err(())
        } else {
            self.player_tables
                .insert(player_name.into(), Default::default());
            Ok(())
        }
    }

    /// Returns the `PlayerSymbolTable` of the given player, if it exists.
    pub fn get_player_table(&self, player_name: &str) -> Option<&PlayerSymbolTable> {
        self.player_tables.get(player_name)
    }

    /// Returns the global `PlayerSymbolTable` which contains symbols in the global scope
    pub fn get_global_table(&self) -> &PlayerSymbolTable {
        &self.global_table
    }

    /// Returns the PlayerSymbolTable belonging to the given owner.
    pub fn get_table(&self, owner: &Owner) -> Option<&PlayerSymbolTable> {
        match owner {
            Owner::Player(name) => self.get_player_table(name),
            Owner::Global => self.get_global_table(),
        }
    }

    /// Creates and inserts a symbol for the given declaration for the given owner
    /// with the given name. If the owner does not exists, [None] is returned. If
    /// the name is already associated with a different symbol, the previous symbol
    /// is contained in the [Some]. If the name is new, [Some(None)] is returned.
    pub fn insert(&mut self, owner: &Owner, name: &str, decl: Rc<Decl>) -> Option<Option<Symbol>> {
        self.get_table(owner).map(|mut table|table.insert(name, decl))
    }

    /// Get the symbol association with the given owner and name. If the owner does not
    /// exists, [None] is returned. If the owner exists, but not the name, [Some(None)] is
    /// returned.
    pub fn get(&self, owner: &Owner, name: &str) -> Option<Option<&Symbol>> {
        self.get_table(owner).map(|table|table.get(name))
    }
}

/// OwnedIdentifiers always belongs to a player or the global scope. This enum allows
/// us to abstract over both possibilities.
#[derive(Clone)]
pub enum Owner {
    Player(String),
    Global,
}
