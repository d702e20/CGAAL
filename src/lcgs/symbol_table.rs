use crate::lcgs::ast::{
    ConstDecl, LabelDecl, PlayerDecl, StateVarDecl, TemplateDecl, TransitionDecl,
};
use std::collections::HashMap;
use std::rc::Rc;

pub struct Symbol {
    pub label: String,
    pub kind: SymbolKind,
}

pub enum SymbolKind {
    Const(Rc<ConstDecl>),
    Label(Rc<LabelDecl>),
    StateVar(Rc<StateVarDecl>),
    Player(Rc<PlayerDecl>),
    Module(Rc<TemplateDecl>),
    Transition(Rc<TransitionDecl>),
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
