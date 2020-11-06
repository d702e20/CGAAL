use crate::lcgs::ast::*;
use std::borrow::BorrowMut;
use std::ops::Deref;
use std::rc::Rc;

pub trait Visitor {
    fn visit_root(&mut self, mut root: Rc<Root>);
    fn visit_decl(&mut self, mut decl: Rc<Decl>);
    fn visit_identifier(&mut self, mut id: Rc<Identifier>);
    fn visit_const(&mut self, mut con: Rc<ConstDecl>);
    fn visit_label(&mut self, mut label: Rc<LabelDecl>);
    fn visit_player(&mut self, mut player: Rc<PlayerDecl>);
    fn visit_relabelling(&mut self, mut relabelling: Rc<Relabelling>);
    fn visit_relabel_case(&mut self, mut relabel_case: Rc<RelabelCase>);
    fn visit_template(&mut self, mut module: Rc<TemplateDecl>);
    fn visit_state_var(&mut self, mut state_var: Rc<StateVarDecl>);
    fn visit_transition(&mut self, mut transition: Rc<TransitionDecl>);
    fn visit_state_change(&mut self, mut state_change: Rc<StateChange>);
    fn visit_type_range(&mut self, mut type_range: Rc<TypeRange>);
    fn visit_expr(&mut self, mut expr: Rc<Expr>);
}
