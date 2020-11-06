use crate::lcgs::ast::*;
use std::rc::Rc;

pub trait Visitor {
    fn visit_root(&mut self, root: &mut Root);
    fn visit_decl(&mut self, decl: &mut Decl);
    fn visit_identifier(&mut self, id: &mut Identifier);
    fn visit_const(&mut self, con: &mut ConstDecl);
    fn visit_label(&mut self, label: &mut LabelDecl);
    fn visit_player(&mut self, player: &mut PlayerDecl);
    fn visit_relabelling(&mut self, relabelling: &mut Relabelling);
    fn visit_relabel_case(&mut self, relabel_case: &mut RelabelCase);
    fn visit_module(&mut self, module: &mut ModuleDecl);
    fn visit_state_var(&mut self, state_var: &mut StateVarDecl);
    fn visit_transition(&mut self, transition: &mut TransitionDecl);
    fn visit_state_change(&mut self, state_change: &StateChange);
    fn visit_type_range(&mut self, type_range: &mut TypeRange);
    fn visit_expr(&mut self, expr: &Expr);
}
