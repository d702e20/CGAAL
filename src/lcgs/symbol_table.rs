use crate::lcgs::ast::{
    ConstDecl, LabelDecl, PlayerDecl, StateVarDecl, TemplateDecl, TransitionDecl,
};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

/// A `Symbol` is a name and information about it.
pub struct Symbol {
    pub label: String,
    pub kind: SymbolKind,
}

pub enum SymbolKind {
    Const(Weak<ConstDecl>),
    Label(Weak<LabelDecl>),
    StateVar(Weak<StateVarDecl>),
    Player(Weak<PlayerDecl>),
    Module(Weak<TemplateDecl>),
    Transition(Weak<TransitionDecl>),
}

/// A `PlayerSymbolTable` contains registered symbols belonging to a player
/// (or the global scope).
#[derive(Default)]
pub struct PlayerSymbolTable {
    symbols: HashMap<String, Symbol>,
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
    pub fn new() -> SymbolTable {
        return Default::default();
    }

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
    pub fn get_player_table(&self, player_name: &str) -> Result<&PlayerSymbolTable, ()> {
        self.player_tables.get(player_name).ok_or(())
    }

    /// Returns the global `PlayerSymbolTable` which contains symbols in the global scope
    pub fn get_global_table(&self) -> &PlayerSymbolTable {
        &self.global_table
    }

    /// Returns the PlayerSymbolTable belonging to the given owner.
    pub fn get_table(&self, owner: &Owner) -> Result<&PlayerSymbolTable, ()> {
        match owner {
            Owner::Player(name) => self.get_player_table(name),
            Owner::Global => self.get_global_table(),
        }
    }
}

/// Identifiers always belongs to a player or the global scope. This enum allows
/// you to abstract over both.
pub enum Owner {
    Player(String),
    Global,
}
