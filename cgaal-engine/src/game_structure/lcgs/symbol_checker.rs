use crate::game_structure::lcgs::intermediate::Player;
use crate::game_structure::lcgs::relabeling::Relabeler;
use crate::game_structure::lcgs::symbol_table::{SymbIdx, SymbolTable};
use crate::game_structure::{PlayerIdx, PropIdx};
use crate::parsing::ast::{
    BinaryOpKind, Coalition, Decl, DeclKind, Expr, ExprKind, Ident, LcgsRoot, OwnedIdent,
    UnaryOpKind,
};
use crate::parsing::errors::{ErrorLog, SeeErrorLog, SpannedError};
use crate::parsing::span::Span;
use std::ops::DerefMut;

/// A symbol table and lists of indices by kind. The output of [symbol_check].
#[derive(Default, Debug)]
pub struct SymbolRegistry {
    pub symbols: SymbolTable,
    pub players: Vec<Player>,
    pub labels: Vec<SymbIdx>,
    pub vars: Vec<SymbIdx>,
}

/// Check every identifier in the program refers to an existing and appropriate declaration.
/// Outputs a symbol table and lists of symbol indices by kind.
/// Any errors are added to the [ErrorLog].
pub fn symbol_check(root: LcgsRoot, errors: &ErrorLog) -> Result<SymbolRegistry, SeeErrorLog> {
    let out = register_decls(root).map_err(|err| errors.log_err(err))?;
    check_and_optimize_decls(&out.symbols).map_err(|err| errors.log_err(err))?;
    Ok(out)
}

/// Registers all declarations from the root in a symbol table and creates lists of
/// symbol indices by kind.
/// Constant expressions are optimized to numbers immediately.
fn register_decls(root: LcgsRoot) -> Result<SymbolRegistry, SpannedError> {
    let mut symbols = SymbolTable::new();
    let mut labels = vec![];
    let mut vars = vec![];
    let mut players_indices = vec![];

    // Register global declarations.
    // Constants are evaluated immediately.
    // Players are put in a separate vector and handled afterwards.
    // Symbol table is given ownership of the declarations.
    for Decl {
        span,
        ident,
        index: _index,
        kind,
    } in root.decls
    {
        match kind {
            DeclKind::Const(expr) => {
                // We can evaluate constants immediately as constants can only
                // refer to other constants that are above them in the program.
                // If they don't reduce to a single number, then the SymbolChecker
                // produces an error.
                let reduced =
                    SymbolChecker::new(&symbols, &None, CheckMode::Const).check_eval(&expr)?;
                let decl = Decl::new(
                    span,
                    ident.name,
                    DeclKind::Const(Expr::new(expr.span, ExprKind::Num(reduced))),
                );
                let _ = symbols.insert(decl)?;
            }
            DeclKind::StateLabel(_, expr) => {
                // Insert in symbol table and add to labels list
                let decl = Decl::new(
                    span,
                    ident.name,
                    DeclKind::StateLabel(PropIdx(labels.len()), expr),
                );
                let index = symbols.insert(decl)?;
                labels.push(index);
            }
            DeclKind::StateVar(state_var) => {
                // Insert in symbol table and add to vars list
                let decl = Decl::new(span, ident.name, DeclKind::StateVar(state_var));
                let index = symbols.insert(decl)?;
                vars.push(index);
            }
            DeclKind::Template(inner_decls) => {
                let decl = Decl::new(span, ident.name, DeclKind::Template(inner_decls));
                let _ = symbols.insert(decl)?;
            }
            DeclKind::Player(mut player) => {
                player.index = PlayerIdx(players_indices.len());
                let decl = Decl::new(span, ident.name, DeclKind::Player(player));
                let index = symbols.insert(decl)?;
                players_indices.push(index);
            }
            _ => unreachable!("Not a global declaration. Parser must have failed."),
        }
    }

    // Register player declarations. Here we clone the declarations since multiple
    // players can use the same template
    let mut players = vec![];
    for (index, symbol_index) in players_indices.drain(..).enumerate() {
        let pdecl = symbols.get(symbol_index).borrow();
        let DeclKind::Player(pkind) = &pdecl.kind else {
            unreachable!()
        };
        let mut player = Player::new(PlayerIdx(index), symbol_index);

        let tdecl_opt = symbols.get_by_name(&pkind.template_ident.to_string());
        let Some(tdecl_rc) = tdecl_opt else {
            return Err(SpannedError::new(
                pkind.template_ident.span,
                format!("Undeclared template '{}'", pkind.template_ident),
            ));
        };
        let tdecl = tdecl_rc.borrow();
        let DeclKind::Template(inner_decls) = &tdecl.kind else {
            return Err(SpannedError::new(
                tdecl.span,
                format!("'{}' is not a template.", tdecl.ident),
            ));
        };

        let relabeler = Relabeler::new(&pkind.relabellings);

        // Go through each declaration in the template and clone it, relabel it, and set its owner
        let new_decls = inner_decls
            .iter()
            .map(|idecl| {
                let mut new_decl = relabeler.relabel_decl(idecl.clone())?;
                new_decl.ident.owner = Some(pdecl.ident.name.clone());
                Ok(new_decl)
            })
            .collect::<Result<Vec<_>, _>>()?;

        drop(pdecl);
        drop(tdecl);

        // Register the new declarations
        for new_decl in new_decls {
            let index = symbols.insert(new_decl)?;
            let mut ndecl = symbols.get(index).borrow_mut();
            match &mut ndecl.kind {
                DeclKind::StateLabel(id, _) => {
                    *id = PropIdx(labels.len());
                    labels.push(index);
                }
                DeclKind::StateVar(_) => vars.push(index),
                DeclKind::Action(_) => player.actions.push(index),
                _ => panic!("Not a declaration allowed in templates. Parser must have failed."),
            }
        }

        players.push(player);
    }

    Ok(SymbolRegistry {
        symbols,
        players,
        labels,
        vars,
    })
}

/// Checks all identifiers, making sure they refer to existing and appropriate declarations.
/// Also reduces the declarations in a [SymbolTable] to more compact versions, if possible.
fn check_and_optimize_decls(symbols: &SymbolTable) -> Result<(), SpannedError> {
    for symbol in symbols.iter() {
        // Optimize the declaration's expression(s)
        let mut decl_ref = symbol.borrow_mut();
        let Decl {
            span: _, // unchanged
            ident,
            index: _, // unchanged
            kind,
        } = decl_ref.deref_mut();
        match kind {
            DeclKind::StateLabel(_, expr) => {
                *expr = SymbolChecker::new(symbols, &ident.owner, CheckMode::State).check(expr)?;
            }
            DeclKind::StateVar(var) => {
                // Both initial value, min, and max are expected to be constant.
                // Hence, we also evaluate them now so we don't have to do that each time.
                // Note: These expr can use any constant expr from the global scope, regardless of ordering
                let checker = SymbolChecker::new(symbols, &ident.owner, CheckMode::Const);
                var.init_val = checker.check_eval(&var.init)?;
                let min = checker.check_eval(&var.range.min)?;
                let max = checker.check_eval(&var.range.max)?;
                var.range.val = min..=max;
                if !var.range.val.contains(&var.init_val) {
                    return Err(SpannedError::new(
                        var.init.span,
                        format!(
                            "Initial value {} is not in range {}..{}",
                            var.init_val, min, max
                        ),
                    ));
                }
                if ident.name.text != var.update_ident.text {
                    return Err(SpannedError::new(
                        var.update_ident.span,
                        format!("The name in the update statement does not match the name of the variable above. Expected '{}'.", ident.name)
                    ));
                }
                var.update = SymbolChecker::new(symbols, &ident.owner, CheckMode::Update)
                    .check(&var.update)?;
            }
            DeclKind::Action(cond) => {
                *cond = SymbolChecker::new(symbols, &ident.owner, CheckMode::State).check(cond)?;
            }
            // Needs no symbol check or evaluate
            DeclKind::Player(_) => {}
            DeclKind::Template(_) => {}
            DeclKind::Const(_) => {}
            DeclKind::Error => {}
        }
    }
    Ok(())
}

/// [CheckMode]s control which declaration identifiers are allow to refer to in the [SymbolChecker].
#[derive(Eq, PartialEq)]
pub enum CheckMode {
    /// In [Const] mode, identifiers in expressions can only refer to constants.
    Const,
    /// In [State] mode, identifiers in expressions can only refer to constants
    /// and state variables.
    State,
    /// In [Update] mode, identifiers in expressions can only refer to constants,
    /// state variables, and actions
    Update,
}

impl CheckMode {
    /// Returns true if the declaration kind is allowed in this mode. See the definition of
    /// each mode for more details.
    pub fn allows(&self, decl_kind: &DeclKind) -> bool {
        match self {
            CheckMode::Const => matches!(decl_kind, DeclKind::Const(_) | DeclKind::Error),
            CheckMode::State => {
                matches!(
                    decl_kind,
                    DeclKind::Const(_) | DeclKind::StateVar(_) | DeclKind::Error
                )
            }
            CheckMode::Update => matches!(
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

    /// Checks and evaluates an expressions. This is only used in [CheckMode::Const]
    /// where we assume the expression can be reduced to a value already during symbol checking.
    pub fn check_eval(&self, expr: &Expr) -> Result<i32, SpannedError> {
        let checked = self.check(expr)?;
        // FIXME: Doesn't actually evaluate the expression
        match checked.kind {
            ExprKind::Num(n) => Ok(n),
            ExprKind::True => Ok(1),
            ExprKind::False => Ok(0),
            _ => Err(SpannedError {
                span: expr.span,
                msg: "Expression could not be reduced to a constant".into(),
            }),
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
        let inferred_oi = OwnedIdent::new(
            oi.owner.clone().or(self.scope_owner.clone()),
            oi.name.clone(),
        );

        // We first ensure that the player exists in order to give a
        // more accurate error message, if necessary
        if let Some(player_name) = &inferred_oi.owner {
            self.symbols
                .get_by_name(&player_name.to_string())
                .ok_or(SpannedError::new(
                    player_name.span,
                    format!("Unknown player '{}'.", player_name.text),
                ))?;
        }

        // FIXME: In CheckMode::Const, only constants are declared and
        //        this will give confusing error messages if anything else if referenced.
        // Find declaration
        let (decl_rc, true_oi) = self
            .symbols
            .get_by_name(&inferred_oi.to_string())
            .zip(Some(&inferred_oi))
            .or_else(|| self.symbols.get_by_name(&oi.to_string()).zip(Some(oi)))
            .ok_or(SpannedError::new(
                *span,
                format!("Unknown declaration '{}'.", oi),
            ))?;

        if let Ok(decl) = &decl_rc.try_borrow() {
            // Check if declaration is allowed to be referenced in this mode
            if !self.mode.allows(&decl.kind) {
                let context = match self.mode {
                    CheckMode::Const => "a constant expression".to_string(),
                    CheckMode::State => "a label or action condition".to_string(),
                    CheckMode::Update => "a state-variable update expression".to_string(),
                };
                return Err(SpannedError::new(
                    *span,
                    format!(
                        "The declaration '{}' cannot be referenced in {}.",
                        true_oi, context
                    ),
                ));
            }

            if let DeclKind::Const(con) = &decl.kind {
                // If symbol points to a constant declaration we can inline the value
                return self.check(con);
            }

            // Identifier is okay. Return a resolved identifier
            let index = self
                .symbols
                .get_index_of_name(&true_oi.to_string())
                .unwrap();
            Ok(Expr::new(*span, ExprKind::Symbol(index)))
        } else {
            // The try_borrow have failed, which means that the
            // RefCell is currently being mutated by someone. We are only reducing
            // one declaration at a time, so the declaration must have an identifier
            // referring to the declaration itself. This is only okay, if we are
            // in CheckMode::Update. In such case we can return immediately.
            if self.mode == CheckMode::Update {
                let index = self
                    .symbols
                    .get_index_of_name(&true_oi.to_string())
                    .unwrap();
                Ok(Expr::new(*span, ExprKind::Symbol(index)))
            } else {
                Err(SpannedError::new(
                    *span,
                    format!("The declaration '{}' refers to itself.", oi),
                ))
            }
        }
    }

    /// Optimizes the given unary operator and checks the operand
    fn check_unop(&self, span: &Span, op: &UnaryOpKind, expr: &Expr) -> Result<Expr, SpannedError> {
        let res = self.check(expr)?;
        if let ExprKind::Num(n) = &res.kind {
            return Ok(Expr::new(*span, ExprKind::Num(op.as_fn()(*n))));
        }
        Ok(Expr::new(*span, ExprKind::Unary(op.clone(), Box::new(res))))
    }

    /// Optimizes the given binary operator and checks the operands
    fn check_binop(
        &self,
        span: &Span,
        op: &BinaryOpKind,
        e1: &Expr,
        e2: &Expr,
    ) -> Result<Expr, SpannedError> {
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
    fn check_if(
        &self,
        span: &Span,
        cond: &Expr,
        e1: &Expr,
        e2: &Expr,
    ) -> Result<Expr, SpannedError> {
        let cond_res = self.check(cond)?;
        if let ExprKind::Num(n) = &cond_res.kind {
            return if *n == 0 {
                self.check(e2)
            } else {
                self.check(e1)
            };
        }
        Ok(Expr::new(
            *span,
            ExprKind::TernaryIf(
                Box::new(cond_res),
                Box::new(self.check(e1)?),
                Box::new(self.check(e2)?),
            ),
        ))
    }

    /// Checks the given minimum expression and computes minimum if possible
    fn check_min(&self, span: &Span, es: &[Expr]) -> Result<Expr, SpannedError> {
        // Combine all numbers, as we already know the minimum of those
        let checked_list = es
            .iter()
            .map(|p| self.check(p))
            .collect::<Result<Vec<Expr>, SpannedError>>()?;
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
        let checked_list = es
            .iter()
            .map(|p| self.check(p))
            .collect::<Result<Vec<Expr>, SpannedError>>()?;
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
            let decl = self
                .symbols
                .get_by_name(&id.to_string())
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
                return Err(SpannedError::new(
                    *span,
                    "Coalitions can only be used in ATL queries.".into(),
                ));
            }
        }
        let path_expr = self.check(&coal.expr)?;
        Ok(Expr::new(
            *span,
            ExprKind::Coalition(Coalition::new(
                *span,
                coal.players.clone(),
                coal.kind,
                path_expr,
            )),
        ))
    }
}
