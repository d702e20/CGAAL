use crate::lcgs::ir::symbol_table::{SymbolTable, Owner};
use crate::lcgs::ast::{ExprKind, Expr, OwnedIdentifier, DeclKind, UnaryOpKind, BinaryOpKind};

struct Evaluator<'a> {
    symbols: &'a SymbolTable,
    scope_owner: Owner,
}

impl<'a> Evaluator<'a> {
    pub fn new(symbols: &'a SymbolTable, scope_owner: Owner) -> Evaluator<'a> {
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
        }
    }

    fn eval_ident(&self, id: &OwnedIdentifier) -> Result<i32, ()> {
        // Owner may be omitted. If omitted, we assume it is the scope owner, unless such thing
        // does not exist, then we assume it's global. If we still can't find it, we have an error.
        let OwnedIdentifier { owner, name } = id;
        let symb = if let Some(player_name) = owner {
            let owner = Owner::Player(player_name.to_string());
            self.symbols
                .get_(&owner, &name)
                .expect("Unknown player") // TODO Use custom error
                .expect("Unknown identifier") // TODO Use custom error
        } else {
            self.symbols
                .get_(&self.scope_owner, &name)
                .unwrap()
                .or_else(|| self.symbols.get_(&Owner::Global, &name).unwrap())
                .expect("Unknown identifier, neither declared locally or globally")
            // TODO Use custom error
        };

        match &symb.declaration.kind {
            DeclKind::Const(con) => self.eval(&con.definition),
            DeclKind::Label(_) => unimplemented!(), // TODO Depends on context
            DeclKind::StateVar(_) => unimplemented!(), // TODO Depends on context
            DeclKind::Transition(_) => unimplemented!(), // TODO Depends on context
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
}
