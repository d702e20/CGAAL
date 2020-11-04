use crate::lcgs::ast::*;

pub trait Visitor {
    fn visit_cgs(&mut self, cgs: &LazyConcurrentGameStructure);
    fn visit_identifier(&mut self, id: &Identifier);
    fn visit_state_var(&mut self, state_var: &StateVariableDefinition);
    fn visit_var_def(&mut self, state_var: &VariableDefinition);
    fn visit_prop_mapping(&mut self, prop_mapping: &PropositionMapping);
    fn visit_transition(&mut self, transition: &Transition);
    fn visit_state_change(&mut self, state_change: &StateChange);
    fn visit_expr(&mut self, expr: &Expr);
}
