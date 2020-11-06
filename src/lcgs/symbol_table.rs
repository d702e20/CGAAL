use std::collections::HashMap;

pub struct Symbol {
    pub label: String,
    pub kind: SymbolKind,
}

pub enum SymbolKind {
    TODO
}

pub struct SymbolTable {
    store: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
        }
    }
}