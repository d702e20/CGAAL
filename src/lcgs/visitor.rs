use crate::lcgs::ast::*;
use std::rc::Rc;

pub trait Visitor {
    fn visit_root(&mut self, root: &mut Rc<Root>);
    fn visit_decl(&mut self, decl: &mut Rc<Decl>);
    fn visit_identifier(&mut self, id: &mut Rc<OwnedIdentifier>);
    fn visit_const(&mut self, con: &mut Rc<ConstDecl>);
    fn visit_label(&mut self, label: &mut Rc<LabelDecl>);
    fn visit_player(&mut self, player: &mut Rc<PlayerDecl>);
    fn visit_relabelling(&mut self, relabelling: &mut Rc<Relabelling>);
    fn visit_relabel_case(&mut self, relabel_case: &mut Rc<RelabelCase>);
    fn visit_template(&mut self, module: &mut Rc<TemplateDecl>);
    fn visit_state_var(&mut self, state_var: &mut Rc<StateVarDecl>);
    fn visit_transition(&mut self, transition: &mut Rc<TransitionDecl>);
    fn visit_state_var_change(&mut self, state_var_change: &mut Rc<StateVarChangeDecl>);
    fn visit_type_range(&mut self, type_range: &mut Rc<TypeRange>);
    fn visit_expr(&mut self, expr: &mut Rc<Expr>);
}
