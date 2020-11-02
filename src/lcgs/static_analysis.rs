use crate::lcgs::ast::{LazyConcurrentGameStructure, Identifier, Expr, VariableDefinition, StateVariable, PropositionMapping, Transition, StateChange};
use crate::lcgs::visitor::Visitor;
use std::collections::HashMap;

pub struct SymbolTable {
    store: HashMap<String, ()>,
}

pub struct StaticAnalysis {}

impl StaticAnalysis {
    fn new() -> StaticAnalysis {
        StaticAnalysis
    }

    pub fn run(cgs: &mut LazyConcurrentGameStructure) -> Result<(), ()> {
        let mut analysis = StaticAnalysis::new();
        analysis.visit_cgs(cgs);
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

    fn visit_state_var(&mut self, state_var: &StateVariable) {
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