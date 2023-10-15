use crate::game_structure::lcgs::intermediate::State;
use crate::parsing::ast::{BinaryOpKind, Expr, ExprKind, UnaryOpKind};

pub struct Evaluator<'a> {
    state: &'a State,
}

impl<'a> Evaluator<'a> {
    pub fn new(state: &'a State) -> Evaluator<'a> {
        Evaluator { state }
    }

    pub fn eval(&self, expr: &Expr) -> i32 {
        match &expr.kind {
            ExprKind::Num(n) => *n,
            ExprKind::OwnedIdent(oi) => panic!("Found owned identifier '{}' during evaluation. All identifiers should have been replaced with symbols by now.", oi),
            ExprKind::Symbol(symb) => self.state.0[symb],
            ExprKind::Unary(op, e) => self.eval_unop(op, e),
            ExprKind::Binary(op, e1, e2) => self.eval_binop(op, e1, e2),
            ExprKind::TernaryIf(c, e1, e2) => self.eval_if(c, e1, e2),
            ExprKind::Max(exprs) => self.eval_max(exprs),
            ExprKind::Min(exprs) => self.eval_min(exprs),
            ExprKind::True => 1,
            ExprKind::False => 0,
            ExprKind::Paren(expr) => self.eval(expr),
            ExprKind::Coalition(_) => panic!("Attempted to evaluate coalition expressions in Evaluator."),
            ExprKind::Error => panic!("Attempted to evaluate error expression in Evaluator."),
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
    use std::collections::HashMap;
    use crate::game_structure::lcgs::eval::Evaluator;
    use crate::game_structure::lcgs::intermediate::State;
    use crate::parsing::ast::{Expr, ExprKind};
    use crate::parsing::span::NO_SPAN;

    #[test]
    fn test_max() {
        let expr = Expr::new(NO_SPAN, ExprKind::Max(vec![
            Expr::new(NO_SPAN, ExprKind::Num(1)),
            Expr::new(NO_SPAN, ExprKind::Num(3)),
            Expr::new(NO_SPAN, ExprKind::Num(2)),
        ]));
        let state = State(HashMap::new());
        let evaluator = Evaluator::new(&state);
        assert_eq!(evaluator.eval(&expr), 3);
    }

    #[test]
    fn test_min() {
        let expr = Expr::new(NO_SPAN, ExprKind::Min(vec![
            Expr::new(NO_SPAN, ExprKind::Num(1)),
            Expr::new(NO_SPAN, ExprKind::Num(3)),
            Expr::new(NO_SPAN, ExprKind::Num(2)),
        ]));
        let state = State(HashMap::new());
        let evaluator = Evaluator::new(&state);
        assert_eq!(evaluator.eval(&expr), 1);
    }
}
