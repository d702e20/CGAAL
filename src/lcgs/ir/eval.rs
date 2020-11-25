use crate::lcgs::ast::{BinaryOpKind, DeclKind, Expr, ExprKind, Identifier, UnaryOpKind};
use crate::lcgs::ir::intermediate::State;
use crate::lcgs::ir::symbol_table::{Owner, SymbolIdentifier, SymbolTable};

pub struct Evaluator<'a> {
    state: &'a State,
}

impl<'a> Evaluator<'a> {
    pub fn new(state: &'a State) -> Evaluator<'a> {
        Evaluator { state }
    }

    pub fn eval(&self, expr: &Expr) -> Result<i32, ()> {
        match &expr.kind {
            ExprKind::Number(n) => Ok(*n),
            ExprKind::OwnedIdent(id) => self.eval_ident(id),
            ExprKind::UnaryOp(op, e) => self.eval_unop(op, e),
            ExprKind::BinaryOp(op, e1, e2) => self.eval_binop(op, e1, e2),
            ExprKind::TernaryIf(c, e1, e2) => self.eval_if(c, e1, e2),
        }
    }

    fn eval_ident(&self, id: &Identifier) -> Result<i32, ()> {
        // At this point identifiers should be resolved and easily found in the State's value table
        let value = match id {
            Identifier::Simple { .. } | Identifier::OptionalOwner { .. } => {
                panic!("Unresolved identifier. Something went wrong in symbol checking.")
            }
            Identifier::Resolved { owner, name } => *self
                .state
                .0
                .get(&SymbolIdentifier {
                    owner: owner.clone(),
                    name: name.to_string(),
                })
                .expect("Symbol does not exist. Compiler should have failed already."),
        };
        Ok(value)
    }

    fn eval_unop(&self, op: &UnaryOpKind, e: &Expr) -> Result<i32, ()> {
        let res = self.eval(e)?;
        Ok(op.as_fun()(res))
    }

    fn eval_binop(&self, op: &BinaryOpKind, e1: &Expr, e2: &Expr) -> Result<i32, ()> {
        let res1 = self.eval(e1)?;
        let res2 = self.eval(e2)?;
        Ok(op.as_fn()(res1, res2))
    }
    fn eval_if(&self, c: &Expr, e1: &Expr, e2: &Expr) -> Result<i32, ()> {
        let c = self.eval(c)?;
        if c != 0 {
            self.eval(e1)
        } else {
            self.eval(e2)
        }
    }
}
