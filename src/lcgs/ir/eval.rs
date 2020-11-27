use crate::lcgs::ast::{BinaryOpKind, DeclKind, Expr, ExprKind, Identifier, UnaryOpKind};
use crate::lcgs::ir::symbol_table::{Owner, SymbolTable};

pub struct Evaluator<'a> {
    symbols: &'a SymbolTable,
    scope_owner: &'a Owner,
}

impl<'a> Evaluator<'a> {
    pub fn new(symbols: &'a SymbolTable, scope_owner: &'a Owner) -> Evaluator<'a> {
        Evaluator {
            symbols,
            scope_owner,
        }
    }

    pub fn eval(&self, expr: &Expr) -> Result<i32, ()> {
        match &expr.kind {
            ExprKind::Number(n) => Ok(*n),
            ExprKind::OwnedIdent(id) => self.eval_ident(id),
            ExprKind::UnaryOp(op, e) => self.eval_unop(op, e),
            ExprKind::BinaryOp(op, e1, e2) => self.eval_binop(op, e1, e2),
            ExprKind::TernaryIf(c, e1, e2) => self.eval_if(c, e1, e2),
            ExprKind::Max(exprs) => self.eval_min(exprs),
            ExprKind::Min(exprs) => self.eval_max(exprs),
        }
    }

    fn eval_ident(&self, id: &Identifier) -> Result<i32, ()> {
        // At this point identifiers should be resolved and easily found in the symbol table
        let symb = match id {
            Identifier::Simple { .. } | Identifier::OptionalOwner { .. } => {
                panic!("Unresolved identifier. Something went wrong in symbol checking.")
            }
            Identifier::Resolved { owner, name } => self
                .symbols
                .get(owner, name)
                .expect("Symbol does not exist. Compiler should have failed already."),
        };

        match &symb.declaration.borrow().kind {
            DeclKind::Const(con) => self.eval(&con.definition),
            DeclKind::Label(_) => unimplemented!(), // TODO Depends on current state
            DeclKind::StateVar(_) => unimplemented!(), // TODO Depends on current state
            DeclKind::Transition(_) => unimplemented!(), // TODO Depends on current transition
            _ => Err(()),
        }
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
    fn eval_min(&self, ls: &[Expr]) -> Result<i32, ()> {
        ls.iter().map(|p| self.eval(p)).min().unwrap()
    }
    fn eval_max(&self, ls: &[Expr]) -> Result<i32, ()> {
        ls.iter().map(|p| self.eval(p)).max().unwrap()
    }
}
