use crate::game_structure::lcgs::ast::{BinaryOpKind, Expr, ExprKind, Identifier, UnaryOpKind};
use crate::game_structure::lcgs::ir::intermediate::State;
use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;

pub struct Evaluator<'a> {
    state: &'a State,
}

impl<'a> Evaluator<'a> {
    pub fn new(state: &'a State) -> Evaluator<'a> {
        Evaluator { state }
    }

    pub fn eval(&self, expr: &Expr) -> i32 {
        match &expr.kind {
            ExprKind::Number(n) => *n,
            ExprKind::OwnedIdent(id) => self.eval_ident(id),
            ExprKind::UnaryOp(op, e) => self.eval_unop(op, e),
            ExprKind::BinaryOp(op, e1, e2) => self.eval_binop(op, e1, e2),
            ExprKind::TernaryIf(c, e1, e2) => self.eval_if(c, e1, e2),
            ExprKind::Max(exprs) => self.eval_max(exprs),
            ExprKind::Min(exprs) => self.eval_min(exprs),
        }
    }

    fn eval_ident(&self, id: &Identifier) -> i32 {
        // At this point identifiers should be resolved and easily found in the State's value table
        match id {
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
        }
    }

    fn eval_unop(&self, op: &UnaryOpKind, e: &Expr) -> i32 {
        let res = self.eval(e);
        op.as_fn()(res)
    }

    fn eval_binop(&self, op: &BinaryOpKind, e1: &Expr, e2: &Expr) -> i32 {
        let res1 = self.eval(e1);
        let res2 = self.eval(e2);
        op.as_fn()(res1, res2)
    }
    fn eval_if(&self, c: &Expr, e1: &Expr, e2: &Expr) -> i32 {
        let c = self.eval(c);
        if c != 0 {
            self.eval(e1)
        } else {
            self.eval(e2)
        }
    }
    fn eval_min(&self, ls: &[Expr]) -> i32 {
        ls.iter().map(|p| self.eval(p)).min().unwrap()
    }
    fn eval_max(&self, ls: &[Expr]) -> i32 {
        ls.iter().map(|p| self.eval(p)).max().unwrap()
    }
}

#[cfg(test)]
mod test {
    use crate::game_structure::lcgs::ast::{Expr, ExprKind};
    use crate::game_structure::lcgs::ir::eval::Evaluator;
    use crate::game_structure::lcgs::ir::intermediate::State;
    use std::collections::HashMap;

    #[test]
    fn test_max() {
        let expr = Expr {
            kind: ExprKind::Max(vec![
                Expr {
                    kind: ExprKind::Number(1),
                },
                Expr {
                    kind: ExprKind::Number(3),
                },
                Expr {
                    kind: ExprKind::Number(2),
                },
            ]),
        };
        let state = State(HashMap::new());
        let evaluator = Evaluator::new(&state);
        assert_eq!(evaluator.eval(&expr), 3);
    }

    #[test]
    fn test_min() {
        let expr = Expr {
            kind: ExprKind::Min(vec![
                Expr {
                    kind: ExprKind::Number(1),
                },
                Expr {
                    kind: ExprKind::Number(3),
                },
                Expr {
                    kind: ExprKind::Number(2),
                },
            ]),
        };
        let state = State(HashMap::new());
        let evaluator = Evaluator::new(&state);
        assert_eq!(evaluator.eval(&expr), 1);
    }
}
