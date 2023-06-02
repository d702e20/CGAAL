use crate::game_structure::lcgs::ast::{
    BinaryOpKind, DeclKind, Expr, ExprKind, Identifier, UnaryOpKind,
};
use crate::game_structure::lcgs::ir::symbol_table::{Owner, SymbolIdentifier, SymbolTable};

#[derive(Debug)]
pub struct SymbolError {
    pub msg: String,
}

/// [CheckMode]s control which declaration identifiers are allow to refer to in the [SymbolChecker].
#[derive(Eq, PartialEq)]
pub enum CheckMode {
    /// In Const mode, identifiers in expressions can only refer to constants.
    Const,
    /// In LabelOrTransition mode, identifiers in expressions can only refer to constants
    /// and state variables.
    LabelOrTransition,
    /// In StateVarUpdate mode, identifiers in expressions can only refer constants, state
    /// variables, and transitions (actions)
    StateVarUpdate,
}

impl CheckMode {
    /// Returns true if the declaration kind is allowed in this mode. See the definition of
    /// each mode for more details.
    pub fn allows(&self, decl_kind: &DeclKind) -> bool {
        match self {
            CheckMode::Const => matches!(decl_kind, DeclKind::Const(_)),
            CheckMode::LabelOrTransition => {
                matches!(decl_kind, DeclKind::Const(_) | DeclKind::StateVar(_))
            }
            CheckMode::StateVarUpdate => matches!(
                decl_kind,
                DeclKind::Const(_) | DeclKind::StateVar(_) | DeclKind::Transition(_)
            ),
        }
    }
}

/// A [SymbolChecker] will check identifiers in expressions and also optimize expressions
/// where possible. The [SymbolChecker] has multiple modes for different types of expressions.
pub struct SymbolChecker<'a> {
    symbols: &'a SymbolTable,
    scope_owner: Owner,
    mode: CheckMode,
}

impl<'a> SymbolChecker<'a> {
    pub fn new(symbols: &'a SymbolTable, scope_owner: Owner, mode: CheckMode) -> SymbolChecker<'a> {
        SymbolChecker {
            symbols,
            scope_owner,
            mode,
        }
    }

    /// Checks and evaluates an expressions. This is only used in [CheckMode::Const]
    /// where we assume the expression can be reduced to a value already during symbol checking.
    pub fn check_eval(&self, expr: &Expr) -> Result<i32, SymbolError> {
        let checked = self.check(expr)?;
        if let ExprKind::Number(n) = checked.kind {
            Ok(n)
        } else {
            panic!("Constant expression was not reduced to a single number.")
        }
    }

    /// Checks the given expressions
    pub fn check(&self, expr: &Expr) -> Result<Expr, SymbolError> {
        match &expr.kind {
            ExprKind::Number(_) => Ok(expr.clone()),
            ExprKind::OwnedIdent(id) => self.check_ident(id),
            ExprKind::UnaryOp(op, expr) => self.check_unop(op, expr),
            ExprKind::BinaryOp(op, e1, e2) => self.check_binop(op, e1, e2),
            ExprKind::TernaryIf(c, e1, e2) => self.check_if(c, e1, e2),
            ExprKind::Min(ls) => self.check_min(ls),
            ExprKind::Max(ls) => self.check_max(ls),
        }
    }

    /// Checks the given owned identifier.
    fn check_ident(&self, id: &Identifier) -> Result<Expr, SymbolError> {
        // Owner may be omitted. If omitted, we assume it is the scope owner, unless such thing
        // does not exist, then we assume it's global. If we still can't find it, we have an error.
        let symb = match id {
            // Simple identifiers are typically declaration names and should not appear in
            // expressions. They are resolved differently.
            Identifier::Simple { .. } => {
                panic!("Simple identifiers should not be resolved by SymbolChecker.")
            }
            Identifier::OptionalOwner { owner, name } => {
                // Constants are evaluated early, so we differentiate here to
                // give better error messages.
                if self.mode == CheckMode::Const {
                    if let Some(player_name) = owner {
                        // Constants are never owned by players
                        return Err(SymbolError {
                            msg: format!(
                                "Expected constant expression. Found '{}.{}', which is not constant, since it is owned by a player.",
                                player_name, name,
                            ),
                        });
                    } else {
                        self.symbols.get(&Owner::Global, name).ok_or(SymbolError {
                            msg: format!(
                                "Expected constant expression. Found unknown constant '{}'.",
                                name
                            ),
                        })?
                    }
                } else if let Some(player_name) = owner {
                    // We first ensure that the player exists in order to give a
                    // more accurate error message, if necessary
                    self.symbols
                        .get(&Owner::Global, player_name)
                        .ok_or(SymbolError {
                            msg: format!("Unknown player '{}'.", player_name),
                        })?;

                    // The player exists, so now we fetch the symbol
                    let owner = Owner::Player(player_name.to_string());
                    self.symbols
                        .get(&owner, name)
                        .ok_or(SymbolError {
                            msg: format!("Unknown identifier '{}.{}'. The player does not own a declaration of that name.", owner, name)
                        })?
                } else {
                    // Player is omitted. Assume it is scope owner. If not, then try global.
                    self.symbols
                        .get(&self.scope_owner, name)
                        .or_else(|| self.symbols.get(&Owner::Global, name))
                        .ok_or(SymbolError {
                            msg: format!(
                                "Unknown identifier '{}', neither declared in player's scope or globally",
                                name
                            ),
                        })?
                }
            }
            // Already resolved once ... which should never happen.
            Identifier::Resolved { .. } => panic!("Identifier was already resolved once."),
        };

        if let Ok(declaration) = &symb.declaration.try_borrow() {
            // Check if symbol is allowed to be referenced in this mode
            if !self.mode.allows(&declaration.kind) {
                let context = match self.mode {
                    CheckMode::Const => "a constant expression".to_string(),
                    CheckMode::LabelOrTransition => "a label or transition condition".to_string(),
                    CheckMode::StateVarUpdate => "a state-variable's update expression".to_string(),
                };
                return Err(SymbolError {
                    msg: format!(
                        "The declaration '{}' cannot be referenced in {}.",
                        symb.identifier, context
                    ),
                });
            }

            if let DeclKind::Const(con) = &declaration.kind {
                // If symbol points to a constant declaration we can inline the value
                return self.check(&con.definition);
            }

            // Identifier is okay. Return a resolved identifier where owner is specified.
            let SymbolIdentifier { owner, name } = &symb.identifier;
            Ok(Expr {
                kind: ExprKind::OwnedIdent(Box::new(Identifier::Resolved {
                    owner: owner.clone(),
                    name: name.clone(),
                })),
            })
        } else {
            // The try_borrow must have failed, which means that the
            // RefCell is currently being mutated by someone. We are only reducing
            // one declaration at a time, so the declaration must have an identifier
            // referring to the declaration itself. This is only okay, if we are
            // in CheckMode::StateVarUpdate. In such case we can return immediately.
            if self.mode == CheckMode::StateVarUpdate {
                Ok(Expr {
                    kind: ExprKind::OwnedIdent(Box::new(Identifier::Resolved {
                        owner: symb.identifier.owner.clone(),
                        name: symb.identifier.name.clone(),
                    })),
                })
            } else {
                Err(SymbolError {
                    msg: format!("The definition of '{}' refers to itself.", symb.identifier),
                })
            }
        }
    }

    /// Optimizes the given unary operator and checks the operand
    fn check_unop(&self, op: &UnaryOpKind, expr: &Expr) -> Result<Expr, SymbolError> {
        let res = self.check(expr)?;
        if let ExprKind::Number(n) = &res.kind {
            return Ok(Expr {
                kind: ExprKind::Number(op.as_fn()(*n)),
            });
        }
        Ok(Expr {
            kind: ExprKind::UnaryOp(op.clone(), Box::new(res)),
        })
    }

    /// Optimizes the given binary operator and checks the operands
    fn check_binop(&self, op: &BinaryOpKind, e1: &Expr, e2: &Expr) -> Result<Expr, SymbolError> {
        // Optimize if both operands are numbers
        // TODO Some operators allow optimizations even when only one operand is a number
        let res1 = self.check(e1)?;
        let res2 = self.check(e2)?;
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

    /// Optimizes the given ternary if and checks the operands
    fn check_if(&self, cond: &Expr, e1: &Expr, e2: &Expr) -> Result<Expr, SymbolError> {
        let cond_res = self.check(cond)?;
        if let ExprKind::Number(n) = &cond_res.kind {
            return if *n == 0 {
                self.check(e2)
            } else {
                self.check(e1)
            };
        }
        Ok(Expr {
            kind: ExprKind::TernaryIf(
                Box::new(cond_res),
                Box::new(self.check(e1)?),
                Box::new(self.check(e2)?),
            ),
        })
    }
    /// First combines all numbers, as we already know the min of that
    /// Then returns a new checked Vec of Expr to find Min of.
    #[allow(clippy::unnecessary_wraps)]
    fn check_min(&self, ls: &[Expr]) -> Result<Expr, SymbolError> {
        let checked_list: Vec<Expr> = ls.iter().map(|p| self.check(p).unwrap()).collect();
        let number: Option<i32> = checked_list
            .iter()
            .filter_map(|p| match p.kind {
                ExprKind::Number(v) => Some(v),
                _ => None,
            })
            .min();
        if let Some(x) = number {
            let mut res: Vec<Expr> = checked_list
                .into_iter()
                .filter(|p| !matches!(p.kind, ExprKind::Number(_)))
                .collect();
            if res.is_empty() {
                Ok(Expr {
                    kind: ExprKind::Number(x),
                })
            } else {
                res.push(Expr {
                    kind: ExprKind::Number(x),
                });
                Ok(Expr {
                    kind: ExprKind::Min(res),
                })
            }
        } else {
            Ok(Expr {
                kind: ExprKind::Min(checked_list),
            })
        }
    }
    /// First combines all numbers, as we already know the max of that
    /// Then returns a new checked Vec of Expr to find Max of.
    #[allow(clippy::unnecessary_wraps)]
    fn check_max(&self, ls: &[Expr]) -> Result<Expr, SymbolError> {
        let checked_list: Vec<Expr> = ls.iter().map(|p| self.check(p).unwrap()).collect();
        let number: Option<i32> = checked_list
            .iter()
            .filter_map(|p| match p.kind {
                ExprKind::Number(v) => Some(v),
                _ => None,
            })
            .max();
        if let Some(x) = number {
            let mut res: Vec<Expr> = checked_list
                .into_iter()
                .filter(|p| !matches!(p.kind, ExprKind::Number(_)))
                .collect();
            if res.is_empty() {
                Ok(Expr {
                    kind: ExprKind::Number(x),
                })
            } else {
                res.push(Expr {
                    kind: ExprKind::Number(x),
                });
                Ok(Expr {
                    kind: ExprKind::Max(res),
                })
            }
        } else {
            Ok(Expr {
                kind: ExprKind::Max(checked_list),
            })
        }
    }
}
