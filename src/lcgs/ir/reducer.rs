use crate::lcgs::ast::{BinaryOpKind, DeclKind, Expr, ExprKind, Identifier, UnaryOpKind};
use crate::lcgs::ir::symbol_table::{Owner, SymbolIdentifier, SymbolTable};

#[derive(Eq, PartialEq)]
pub enum ReduceMode {
    /// In Const mode, identifiers in expressions can only refer to constants.
    Const,
    /// In LabelOrTransition mode, identifiers in expressions can only refer to constants
    /// and state variables.
    LabelOrTransition,
    /// In StateVarChange mode, identifiers in expressions can only refer constants, state
    /// variables, and transitions (actions)
    StateVarChange,
}

pub struct Reducer<'a> {
    symbols: &'a SymbolTable,
    scope_owner: Owner,
    mode: ReduceMode,
}

impl<'a> Reducer<'a> {
    pub fn new(symbols: &'a SymbolTable, scope_owner: Owner, mode: ReduceMode) -> Reducer<'a> {
        Reducer {
            symbols,
            scope_owner,
            mode,
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
                // Constants are reduced and evaluated early, so we differentiate here to
                // give better error messages.
                if self.mode == ReduceMode::Const {
                    if let Some(player_name) = owner {
                            // TODO Use custom error
                        panic!("Unknown constant. Constants can only depend on other constants declared above itself.")
                    } else {
                        self.symbols
                        .get(&Owner::Global, &name)
                            // TODO Use custom error
                        .expect("Unknown constant. Constants can only depend on constants declared above itself.")
                        .borrow()
                    }
                } else {
                    if let Some(player_name) = owner {
                        // We first ensure that the player exists in order to give a
                        // more accurate error message, if necessary
                        self.symbols
                            .get(&Owner::Global, player_name)
                            .expect("Unknown player"); // TODO Use custom error

                        // The player exists, so now we fetch the symbol
                        let owner = Owner::Player(player_name.to_string());
                        self.symbols
                            .get(&owner, &name)
                                // TODO Use custom error
                            .expect("Unknown identifier. The player does not own a declaration of that name")
                    } else {
                        self.symbols
                            .get(&self.scope_owner, &name)
                            .or_else(|| self.symbols.get(&Owner::Global, &name))
                                // TODO Use custom error
                            .expect("Unknown identifier, neither declared locally or globally")
                    }
                    .try_borrow()
                        // If the try_borrow fails, it means that the RefCell is currently being
                        // mutated by someone. We are only reducing one declaration at a time,
                        // so the declaration has an identifier referring to itself.
                    .expect("Declaration refers to itself.") // TODO Use custom error
                }
            }
            // Already resolved once ... which should never happen.
            Identifier::Resolved { .. } => panic!("Identifier was already resolved once."),
        };

        match &symb.declaration.kind {
            // If symbol points to a constant declaration we can inline the value
            DeclKind::Const(con) => return self.reduce(&con.definition),
            DeclKind::StateVar(_) => {
                if !matches!(
                    self.mode,
                    ReduceMode::LabelOrTransition | ReduceMode::StateVarChange
                ) {
                    return Err(());
                }
            }
            DeclKind::Transition(_) => {
                if !matches!(self.mode, ReduceMode::StateVarChange) {
                    return Err(());
                }
            }
            // DeclKind::Label(_) => .., // Maybe in the future
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
