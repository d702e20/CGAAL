use crate::lcgs::ast::{BinaryOpKind, DeclKind, Expr, ExprKind, Identifier, UnaryOpKind};
use crate::lcgs::ir::symbol_table::{Owner, SymbolIdentifier, SymbolTable};

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

    pub fn reduce(&self, expr: &Expr) -> Result<Expr, ()> {
        match &expr.kind {
            ExprKind::Number(_) => Ok(expr.clone()),
            ExprKind::OwnedIdent(id) => self.reduce_ident(id),
            ExprKind::UnaryOp(op, expr) => self.reduce_unop(op, expr),
            ExprKind::BinaryOp(op, e1, e2) => self.reduce_binop(op, e1, e2),
            ExprKind::TernaryIf(c, e1, e2) => self.reduce_if(c, e1, e2),
        }
    }

    fn reduce_ident(&self, id: &Identifier) -> Result<Expr, ()> {
        // Owner may be omitted. If omitted, we assume it is the scope owner, unless such thing
        // does not exist, then we assume it's global. If we still can't find it, we have an error.
        let symb = match id {
            // Simple identifiers are typically declaration names and should be resolved differently
            Identifier::Simple { .. } => panic!("Should not be reduced."),
            Identifier::OptionalOwner { owner, name } => {
                if let Some(player_name) = owner {
                    let owner = Owner::Player(player_name.to_string());
                    self.symbols
                        .get(&owner, &name)
                        .expect("Unknown player or identifier") // TODO Use custom error
                } else {
                    self.symbols
                        .get(&self.scope_owner, &name)
                        .or_else(|| self.symbols.get(&Owner::Global, &name))
                        .expect("Unknown identifier, neither declared locally or globally")
                    // TODO Use custom error
                }
            }
            // Already resolved. Compiler error
            Identifier::Resolved { .. } => panic!("Identifier was already resolved once.")
        };

        match &symb.declaration.kind {
            // If symbol points to a constant declaration we can inline the value
            DeclKind::Const(con) => return self.reduce(&con.definition),
            DeclKind::Label(_) => unimplemented!(), // TODO Depends on context
            DeclKind::StateVar(_) => unimplemented!(), // TODO Depends on context
            DeclKind::Transition(_) => unimplemented!(), // TODO Depends on context
            _ => return Err(()),
        }

        let SymbolIdentifier { owner, name } = &symb.identifier;
        return Ok(Expr {
            kind: ExprKind::OwnedIdent(Box::new(Identifier::Resolved {
                owner: owner.clone(),
                name: name.clone(),
            })),
        });
    }

    fn reduce_unop(&self, op: &UnaryOpKind, expr: &Expr) -> Result<Expr, ()> {
        let mut res = self.reduce(expr)?;
        if let ExprKind::Number(n) = &mut res.kind {
            match op {
                UnaryOpKind::Not => *n = -*n,
                UnaryOpKind::Negation => *n = (*n == 0) as i32,
            }
        }
        Ok(Expr {
            kind: ExprKind::UnaryOp(op.clone(), Box::new(res)),
        })
    }

    fn reduce_binop(&self, op: &BinaryOpKind, e1: &Expr, e2: &Expr) -> Result<Expr, ()> {
        // Reduce if both operands are numbers
        // TODO Some operators allow reduction even when only one operand is a number
        let res1 = self.reduce(e1)?;
        let res2 = self.reduce(e2)?;
        if let ExprKind::Number(n1) = &res1.kind {
            if let ExprKind::Number(n2) = &res2.kind {
                return Ok(Expr {
                    kind: ExprKind::Number(op.as_fn()(*n1, *n2)),
                });
            }
        }
        Ok(Expr {
            kind: ExprKind::BinaryOp(op.clone(), Box::new(res1), Box::new(res2)),
        })
    }

    fn reduce_if(&self, cond: &Expr, e1: &Expr, e2: &Expr) -> Result<Expr, ()> {
        let cond_res = self.reduce(cond)?;
        if let ExprKind::Number(n) = &cond_res.kind {
            return if *n == 0 {
                self.reduce(e2)
            } else {
                self.reduce(e1)
            };
        }
        Ok(Expr {
            kind: ExprKind::TernaryIf(
                Box::new(cond_res),
                Box::new(self.reduce(e1)?),
                Box::new(self.reduce(e2)?),
            ),
        })
    }
}
