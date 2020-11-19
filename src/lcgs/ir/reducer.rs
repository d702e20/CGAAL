use crate::lcgs::ir::symbol_table::{SymbolTable, Owner};
use crate::lcgs::ast::{Expr, ExprKind, OwnedIdentifier, UnaryOpKind, BinaryOpKind};

pub struct Reducer<'a> {
    symbols: &'a SymbolTable,
    scope_owner: Owner,
}

impl<'a> Reducer<'a> {
    pub fn new(symbols: &'a SymbolTable, scope_owner: Owner) -> Reducer<'a> {
        Reducer {
            symbols,
            scope_owner,
        }
    }

    pub fn reduce(&self, expr: &Expr) -> Expr {
        match &expr.kind {
            ExprKind::Number(_) => expr.clone(),
            ExprKind::OwnedIdent(id) => self.reduce_ident(id),
            ExprKind::UnaryOp(_, _) => unimplemented!(),
            ExprKind::BinaryOp(_, _, _) => unimplemented!(),
            ExprKind::TernaryIf(_, _, _) => unimplemented!(),
        }
    }

    fn reduce_ident(&self, id: &OwnedIdentifier) -> Expr {
        unimplemented!()
    }

    fn reduce_unop(&self, op: &UnaryOpKind, expr: &Expr) -> Expr {
        let mut res = self.reduce(expr);
        if let ExprKind::Number(n) = &mut res.kind {
            match op {
                UnaryOpKind::Not => *n = -*n,
                UnaryOpKind::Negation => *n = (*n == 0) as i32,
            }
        }
        return res
    }

    fn reduce_binop(&self, op: &BinaryOpKind, e1: &Expr, e2: &Expr) -> Expr {
        // Reduce if both operands are numbers
        // TODO Some operators allow reduction even when only one operand is a number
        let res1 = self.reduce(e1);
        let res2 = self.reduce(e2);
        if let ExprKind::Number(n1) = &res1.kind {
            if let ExprKind::Number(n2) = &res2.kind {
                return Expr {
                    kind: ExprKind::Number(op.as_fn()(*n1, *n2))
                }
            }
        }
        return Expr {
            kind: ExprKind::BinaryOp(
                op.clone(),
                Box::new(res1),
                Box::new(res2),
            )
        }
    }

    fn reduce_if(&self, cond: &Expr, e1: &Expr, e2: &Expr) -> Expr {
        let cond_res = self.reduce(cond);
        if let ExprKind::Number(n) = &cond_res.kind {
            return if *n == 0 {
                self.reduce(e2)
            } else {
                self.reduce(e1)
            }
        }
        return Expr {
            kind: ExprKind::TernaryIf(
                Box::new(cond_res),
                Box::new(self.reduce(e1)),
                Box::new(self.reduce(e2)),
            )
        }
    }
}