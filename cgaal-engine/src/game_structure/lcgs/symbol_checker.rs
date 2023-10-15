use crate::game_structure::lcgs::symbol_table::SymbolTable;
use crate::parsing::ast::{BinaryOpKind, Coalition, DeclKind, Expr, ExprKind, Ident, OwnedIdent, UnaryOpKind};
use crate::parsing::errors::SpannedError;
use crate::parsing::span::Span;

/// [CheckMode]s control which declaration identifiers are allow to refer to in the [SymbolChecker].
#[derive(Eq, PartialEq)]
pub enum CheckMode {
    /// In [ConstExpr] mode, identifiers in expressions can only refer to constants.
    ConstExpr,
    /// In [StateExpr] mode, identifiers in expressions can only refer to constants
    /// and state variables.
    StateExpr,
    /// In [UpdateExpr] mode, identifiers in expressions can only refer to constants,
    /// state variables, and actions
    UpdateExpr,
}

impl CheckMode {
    /// Returns true if the declaration kind is allowed in this mode. See the definition of
    /// each mode for more details.
    pub fn allows(&self, decl_kind: &DeclKind) -> bool {
        match self {
            CheckMode::ConstExpr => matches!(decl_kind, DeclKind::Const(_) | DeclKind::Error),
            CheckMode::StateExpr => {
                matches!(
                    decl_kind,
                    DeclKind::Const(_) | DeclKind::StateVar(_) | DeclKind::Error
                )
            }
            CheckMode::UpdateExpr => matches!(
                decl_kind,
                DeclKind::Const(_) | DeclKind::StateVar(_) | DeclKind::Action(_) | DeclKind::Error
            ),
        }
    }
}

/// A [SymbolChecker] will check identifiers in expressions and also optimize expressions
/// where possible. The [SymbolChecker] has multiple modes for different types of expressions.
pub struct SymbolChecker<'a> {
    symbols: &'a SymbolTable,
    scope_owner: &'a Option<Ident>,
    mode: CheckMode,
}

impl<'a> SymbolChecker<'a> {
    pub fn new(
        symbols: &'a SymbolTable,
        scope_owner: &'a Option<Ident>,
        mode: CheckMode,
    ) -> SymbolChecker<'a> {
        SymbolChecker {
            symbols,
            scope_owner,
            mode,
        }
    }

    /// Checks and evaluates an expressions. This is only used in [CheckMode::ConstExpr]
    /// where we assume the expression can be reduced to a value already during symbol checking.
    pub fn check_eval(&self, expr: &Expr) -> Result<i32, SpannedError> {
        let checked = self.check(expr)?;
        // FIXME: Should also accept true and false, since those are equal to 0 and 1
        if let ExprKind::Num(n) = checked.kind {
            Ok(n)
        } else {
            Err(SpannedError {
                span: expr.span,
                msg: format!("Expression could not be reduced to a constant"),
            })
        }
    }

    /// Checks the given expressions
    pub fn check(&self, expr: &Expr) -> Result<Expr, SpannedError> {
        match &expr.kind {
            ExprKind::Num(_) => Ok(expr.clone()),
            ExprKind::Symbol(_) => unreachable!("Expression has been symbol checked already`?"),
            ExprKind::OwnedIdent(id) => self.check_ident(&expr.span, id),
            ExprKind::Unary(op, e) => self.check_unop(&expr.span, op, e),
            ExprKind::Binary(op, e1, e2) => self.check_binop(&expr.span, op, e1, e2),
            ExprKind::TernaryIf(c, e1, e2) => self.check_if(&expr.span, c, e1, e2),
            ExprKind::Min(es) => self.check_min(&expr.span, es),
            ExprKind::Max(es) => self.check_max(&expr.span, es),
            ExprKind::Paren(e) => self.check(e),
            ExprKind::True => Ok(expr.clone()),
            ExprKind::False => Ok(expr.clone()),
            ExprKind::Coalition(coal) => self.check_coalition(&expr.span, coal),
            ExprKind::Error => Ok(expr.clone()),
        }
    }

    /// Checks the given owned identifier.
    fn check_ident(&self, span: &Span, oi: &OwnedIdent) -> Result<Expr, SpannedError> {
        // Owner may be omitted. If omitted, we assume owner is self.scope_owner.
        // If self.scope_owner is None too, then we assume it's a global declaration.
        // If we still can't find the declaration, we have an error.
        let res_oi = OwnedIdent::new(oi.owner.clone().or(self.scope_owner.clone()), oi.name.clone());

        // We first ensure that the player exists in order to give a
        // more accurate error message, if necessary
        if let Some(player_name) = &res_oi.owner {
            self.symbols
                .get_by_name(&OwnedIdent::new(None, player_name.clone()))
                .ok_or(SpannedError::new(
                    player_name.span,
                    format!("Unknown player '{}'.", player_name.text),
                ))?;
        }

        // FIXME: In CheckMode::ConstExpr, only constants are declared and
        //        this will give confusing error messages if anything else if referenced.
        // Find declaration
        let decl_rc = self.symbols.get_by_name(&res_oi).ok_or(SpannedError::new(
            *span,
            format!("Unknown declaration '{}'.", oi),
        ))?;

        if let Ok(decl) = &decl_rc.try_borrow() {
            // Check if declaration is allowed to be referenced in this mode
            if !self.mode.allows(&decl.kind) {
                let context = match self.mode {
                    CheckMode::ConstExpr => "a constant expression".to_string(),
                    CheckMode::StateExpr => "a label or action condition".to_string(),
                    CheckMode::UpdateExpr => "a state-variable update expression".to_string(),
                };
                return Err(SpannedError::new(
                    *span,
                    format!(
                        "The declaration '{}' cannot be referenced in {}.",
                        res_oi, context
                    ),
                ));
            }

            if let DeclKind::Const(con) = &decl.kind {
                // If symbol points to a constant declaration we can inline the value
                return self.check(&con);
            }

            // Identifier is okay. Return a resolved identifier where owner is specified.
            return Ok(Expr::new(*span, ExprKind::OwnedIdent(res_oi)));
        } else {
            // The try_borrow have failed, which means that the
            // RefCell is currently being mutated by someone. We are only reducing
            // one declaration at a time, so the declaration must have an identifier
            // referring to the declaration itself. This is only okay, if we are
            // in CheckMode::UpdateExpr. In such case we can return immediately.
            if self.mode == CheckMode::UpdateExpr {
                Ok(Expr::new(*span, ExprKind::OwnedIdent(res_oi)))
            } else {
                Err(SpannedError::new(*span, format!("The declaration '{}' refers to itself.", oi)))
            }
        }
    }

    /// Optimizes the given unary operator and checks the operand
    fn check_unop(&self, span: &Span, op: &UnaryOpKind, expr: &Expr) -> Result<Expr, SpannedError> {
        let res = self.check(expr)?;
        if let ExprKind::Num(n) = &res.kind {
            return Ok(Expr::new(*span, ExprKind::Num(op.as_fn()(*n))));
        }
        Ok(Expr::new(
            *span,
            ExprKind::Unary(op.clone(), Box::new(res)),
        ))
    }

    /// Optimizes the given binary operator and checks the operands
    fn check_binop(&self, span: &Span, op: &BinaryOpKind, e1: &Expr, e2: &Expr) -> Result<Expr, SpannedError> {
        // Optimize if both operands are numbers
        // TODO Some operators allow optimizations even when only one operand is a number
        let res1 = self.check(e1)?;
        let res2 = self.check(e2)?;
        if let ExprKind::Num(n1) = &res1.kind {
            if let ExprKind::Num(n2) = &res2.kind {
                return Ok(Expr::new(*span, ExprKind::Num(op.as_fn()(*n1, *n2))));
            }
        }
        Ok(Expr::new(
            *span,
            ExprKind::Binary(op.clone(), Box::new(res1), Box::new(res2)),
        ))
    }

    /// Optimizes the given ternary if and checks the operands
    fn check_if(&self, span: &Span, cond: &Expr, e1: &Expr, e2: &Expr) -> Result<Expr, SpannedError> {
        let cond_res = self.check(cond)?;
        if let ExprKind::Num(n) = &cond_res.kind {
            return if *n == 0 {
                // TODO: Promote span of e2 to span of if expression. This needs to be done for all expressions.
                self.check(e2)
            } else {
                self.check(e1)
            };
        }
        Ok(Expr::new(
            *span,
            ExprKind::TernaryIf(Box::new(cond_res), Box::new(self.check(e1)?), Box::new(self.check(e2)?)),
        ))
    }

    /// Checks the given minimum expression and computes minimum if possible
    fn check_min(&self, span: &Span, es: &[Expr]) -> Result<Expr, SpannedError> {
        // Combine all numbers, as we already know the minimum of those
        let checked_list: Vec<Expr> = es.iter().map(|p| self.check(p).unwrap()).collect();
        let opt_smallest: Option<i32> = checked_list
            .iter()
            .filter_map(|p| match p.kind {
                ExprKind::Num(v) => Some(v),
                _ => None,
            })
            .min();
        if let Some(smallest) = opt_smallest {
            let mut res: Vec<Expr> = checked_list
                .into_iter()
                .filter(|p| !matches!(p.kind, ExprKind::Num(_)))
                .collect();
            if res.is_empty() {
                Ok(Expr::new(*span, ExprKind::Num(smallest)))
            } else {
                res.push(Expr::new(*span, ExprKind::Num(smallest)));
                Ok(Expr::new(*span, ExprKind::Min(res)))
            }
        } else {
            Ok(Expr::new(*span, ExprKind::Min(checked_list)))
        }
    }

    /// Checks the given maximum expression and computes maximum if possible
    fn check_max(&self, span: &Span, es: &[Expr]) -> Result<Expr, SpannedError> {
        // Combine all numbers, as we already know the maximum of those
        let checked_list: Vec<Expr> = es.iter().map(|p| self.check(p).unwrap()).collect();
        let opt_biggest: Option<i32> = checked_list
            .iter()
            .filter_map(|p| match p.kind {
                ExprKind::Num(v) => Some(v),
                _ => None,
            })
            .max();
        if let Some(biggest) = opt_biggest {
            let mut res: Vec<Expr> = checked_list
                .into_iter()
                .filter(|p| !matches!(p.kind, ExprKind::Num(_)))
                .collect();
            if res.is_empty() {
                Ok(Expr::new(*span, ExprKind::Num(biggest)))
            } else {
                res.push(Expr::new(*span, ExprKind::Num(biggest)));
                Ok(Expr::new(*span, ExprKind::Max(res)))
            }
        } else {
            Ok(Expr::new(*span, ExprKind::Max(checked_list)))
        }
    }
    fn check_coalition(&self, span: &Span, coal: &Coalition) -> Result<Expr, SpannedError> {
        for id in &coal.players {
            let decl = self.symbols
                .get_by_name(&OwnedIdent::new(None, id.clone()))
                .ok_or(SpannedError::new(
                    id.span,
                    format!("Unknown player '{}'.", id.text),
                ))?;
            if let Ok(decl) = &decl.try_borrow() {
                if !decl.kind.is_player() {
                    return Err(SpannedError::new(
                        *span,
                        format!("The declaration '{}' is not a player.", id.text),
                    ));
                }
            } else {
                // This case is only possible if the coalition expression is used in
                // an expression of an LCGS declaration
                return Err(SpannedError::new(*span, format!("Coalitions can only be used in ATL queries.")));
            }
        }
        let path_expr = self.check(&coal.expr)?;
        Ok(Expr::new(*span, ExprKind::Coalition(Coalition::new(*span, coal.players.clone(), coal.kind, path_expr))))
    }
}
