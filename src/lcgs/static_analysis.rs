use crate::lcgs::ast::{LazyConcurrentGameStructure, Identifier, Expr, VariableDefinition, StateVariableDefinition, PropositionMapping, Transition, StateChange};
use crate::lcgs::visitor::Visitor;
use crate::lcgs::program::Program;
use std::collections::HashMap;
use std::fmt::Display;

pub struct Symbol {
    pub label: String,
    pub kind: SymbolKind,
}

pub enum SymbolKind {
    Player,
    StateVar,
    Var,
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

pub struct StaticAnalysis {
    symbol_table: SymbolTable,
    errors: Vec<String>,
}

impl StaticAnalysis {
    pub fn new() -> StaticAnalysis {
        StaticAnalysis {
            symbol_table: SymbolTable::new(),
            errors: vec![],
        }
    }

    pub fn run(program: &mut Program) -> Result<(), ()> {
        let mut analysis = StaticAnalysis::new();
        analysis.visit_cgs(&program.lcgs);
        Ok(())
    }
}

impl Visitor for StaticAnalysis {
    fn visit_cgs(&mut self, cgs: &LazyConcurrentGameStructure) {
        unimplemented!()
    }

    fn visit_identifier(&mut self, id: &Identifier) {
        unimplemented!()
    }

    fn visit_state_var(&mut self, state_var: &StateVariableDefinition) {
        if self.symbol_table.store.contains_key(&state_var.identifier.label) {
            self.errors.push(format!("'{}' is already declared.", state_var.identifier.label));
        } else {
            let symb = Symbol {
                label: state_var.identifier.label.clone(),
                kind: SymbolKind::StateVar,
            };
            self.symbol_table.store.insert(symb.label.clone(), symb);
        }
        unimplemented!()
    }

    fn visit_var_def(&mut self, state_var: &VariableDefinition) {
        unimplemented!()
    }

    fn visit_prop_mapping(&mut self, prop_mapping: &PropositionMapping) {
        unimplemented!()
    }

    fn visit_transition(&mut self, transition: &Transition) {
        unimplemented!()
    }

    fn visit_state_change(&mut self, state_change: &StateChange) {
        unimplemented!()
    }

    fn visit_expr(&mut self, expr: &Expr) {
        unimplemented!()
    }
}