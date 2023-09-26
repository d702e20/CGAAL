use crate::atl::Phi;
use crate::game_structure::lcgs::ast::DeclKind;
use crate::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
use crate::game_structure::lcgs::ir::symbol_table::Owner;
use crate::game_structure::Player;
use crate::parsing::ast::{
    BinaryOpKind, Coalition, CoalitionKind, Expr, ExprKind, Ident, UnaryOpKind,
};
use crate::parsing::errors::ErrorLog;

/// Convert an ATL expression to a Phi formula.
/// Players and labels must be defined in the game and are compiled to their respective indexes.
/// Returns None if there were errors. See the error log for details.
pub fn convert_expr_to_phi(expr: &Expr, game: &IntermediateLcgs, errors: &ErrorLog) -> Option<Phi> {
    let Expr { span, kind } = expr;
    match kind {
        ExprKind::True => Some(Phi::True),
        ExprKind::False => Some(Phi::False),
        ExprKind::Paren(e) => convert_expr_to_phi(e, game, errors),
        ExprKind::OwnedIdent(owner, ident) => {
            if let Some(player) = owner {
                let symbol = Owner::Global.symbol_id(&player.name);
                match game.get_decl(&symbol).map(|d| &d.kind) {
                    Some(DeclKind::Player(_)) => {
                        // ok
                    }
                    Some(d) => {
                        errors.log(
                            player.span,
                            format!("Expected player, '{}' is a {}", player.name, d.kind_name()),
                        );
                        None?
                    }
                    None => {
                        errors.log(
                            player.span,
                            format!("Expected player, '{}' is not defined", player.name),
                        );
                        None?
                    }
                }
            }
            let symbol = if let Some(owner) = owner {
                Owner::Player(owner.name.clone()).symbol_id(&ident.name)
            } else {
                Owner::Global.symbol_id(&ident.name)
            };
            let decl = game.get_decl(&symbol);
            match &decl.map(|d| &d.kind) {
                Some(DeclKind::Label(l)) => Some(Phi::Proposition(l.index)),
                Some(d) => {
                    errors.log(
                        ident.span,
                        format!(
                            "Expected proposition label, '{}' is a {}",
                            ident.name,
                            d.kind_name()
                        ),
                    );
                    None
                }
                None => {
                    errors.log(
                        ident.span,
                        format!(
                            "Expected proposition label, '{}' is not defined",
                            ident.name
                        ),
                    );
                    None
                }
            }
        }
        ExprKind::Unary(op, e) => match op {
            UnaryOpKind::Not => Some(Phi::Not(convert_expr_to_phi(e, game, errors)?.into())),
            UnaryOpKind::Next | UnaryOpKind::Eventually | UnaryOpKind::Invariantly => {
                errors.log(
                    *span,
                    "Temporal operators are only allowed after a coalition".to_string(),
                );
                None
            }
            UnaryOpKind::Neg => {
                errors.log(
                    *span,
                    "Arithmetic operators is currently not supported in ATL".to_string(),
                );
                None
            }
        },
        ExprKind::Binary(op, lhs, rhs) => match op {
            BinaryOpKind::And => Some(Phi::And(
                convert_expr_to_phi(lhs, game, errors)?.into(),
                convert_expr_to_phi(rhs, game, errors)?.into(),
            )),
            BinaryOpKind::Or => Some(Phi::Or(
                convert_expr_to_phi(lhs, game, errors)?.into(),
                convert_expr_to_phi(rhs, game, errors)?.into(),
            )),
            BinaryOpKind::Until => {
                errors.log(
                    *span,
                    "Temporal operators are only allowed after a coalition".to_string(),
                );
                None
            }
            BinaryOpKind::Xor => {
                errors.log(
                    *span,
                    "Exclusive OR is currently not supported in ATL".to_string(),
                );
                None
            }
            BinaryOpKind::Implies => {
                errors.log(
                    *span,
                    "Implication is currently not supported in ATL".to_string(),
                );
                None
            }
            BinaryOpKind::Eq
            | BinaryOpKind::Neq
            | BinaryOpKind::Gt
            | BinaryOpKind::Geq
            | BinaryOpKind::Lt
            | BinaryOpKind::Leq => {
                errors.log(
                    *span,
                    "Relational operators are currently not supported in ATL".to_string(),
                );
                None
            }
            BinaryOpKind::Add | BinaryOpKind::Sub | BinaryOpKind::Mul | BinaryOpKind::Div => {
                errors.log(
                    *span,
                    "Arithmetic operators are currently not supported in ATL".to_string(),
                );
                None
            }
        },
        ExprKind::TernaryIf(_, _, _) => {
            errors.log(
                *span,
                "Ternary if expressions are currently not supported in ATL".to_string(),
            );
            None
        }
        ExprKind::Coalition(Coalition {
            players,
            kind,
            expr: path_expr,
            ..
        }) => match (kind, &path_expr.kind) {
            (CoalitionKind::Enforce, ExprKind::Unary(UnaryOpKind::Next, sub_expr)) => {
                let phi = convert_expr_to_phi(sub_expr, game, errors)?;
                Some(Phi::EnforceNext {
                    players: convert_players(players, game, errors)?,
                    formula: phi.into(),
                })
            }
            (CoalitionKind::Despite, ExprKind::Unary(UnaryOpKind::Next, sub_expr)) => {
                let phi = convert_expr_to_phi(sub_expr, game, errors)?;
                Some(Phi::DespiteNext {
                    players: convert_players(players, game, errors)?,
                    formula: phi.into(),
                })
            }
            (CoalitionKind::Enforce, ExprKind::Unary(UnaryOpKind::Eventually, sub_expr)) => {
                let phi = convert_expr_to_phi(sub_expr, game, errors)?;
                Some(Phi::EnforceEventually {
                    players: convert_players(players, game, errors)?,
                    formula: phi.into(),
                })
            }
            (CoalitionKind::Despite, ExprKind::Unary(UnaryOpKind::Eventually, sub_expr)) => {
                let phi = convert_expr_to_phi(sub_expr, game, errors)?;
                Some(Phi::DespiteEventually {
                    players: convert_players(players, game, errors)?,
                    formula: phi.into(),
                })
            }
            (CoalitionKind::Enforce, ExprKind::Unary(UnaryOpKind::Invariantly, sub_expr)) => {
                let phi = convert_expr_to_phi(sub_expr, game, errors)?;
                Some(Phi::EnforceInvariant {
                    players: convert_players(players, game, errors)?,
                    formula: phi.into(),
                })
            }
            (CoalitionKind::Despite, ExprKind::Unary(UnaryOpKind::Invariantly, sub_expr)) => {
                let phi = convert_expr_to_phi(sub_expr, game, errors)?;
                Some(Phi::DespiteInvariant {
                    players: convert_players(players, game, errors)?,
                    formula: phi.into(),
                })
            }
            (CoalitionKind::Enforce, ExprKind::Binary(BinaryOpKind::Until, lhs, rhs)) => {
                let lhs_phi = convert_expr_to_phi(lhs, game, errors)?;
                let rhs_phi = convert_expr_to_phi(rhs, game, errors)?;
                Some(Phi::EnforceUntil {
                    players: convert_players(players, game, errors)?,
                    pre: lhs_phi.into(),
                    until: rhs_phi.into(),
                })
            }
            (CoalitionKind::Despite, ExprKind::Binary(BinaryOpKind::Until, lhs, rhs)) => {
                let lhs_phi = convert_expr_to_phi(lhs, game, errors)?;
                let rhs_phi = convert_expr_to_phi(rhs, game, errors)?;
                Some(Phi::DespiteUntil {
                    players: convert_players(players, game, errors)?,
                    pre: lhs_phi.into(),
                    until: rhs_phi.into(),
                })
            }
            _ => {
                errors.log(
                    path_expr.span,
                    "Coalitions must be followed by a path formula".to_string(),
                );
                None
            }
        },
        ExprKind::Num(_) => {
            errors.log(
                *span,
                "Unexpected number. Please use true, false, or label names as propositions."
                    .to_string(),
            );
            None
        }
        ExprKind::Max(_) | ExprKind::Min(_) => {
            errors.log(
                *span,
                "Max and min expressions are currently not supported in ATL".to_string(),
            );
            None
        }
        ExprKind::Error => None,
    }
}

/// Helper function for converting a list of player names to a list of player indexes.
/// Returns None if there were errors. See the error log for details.
fn convert_players(
    players: &[Ident],
    game: &IntermediateLcgs,
    errors: &ErrorLog,
) -> Option<Vec<Player>> {
    players
        .iter()
        .map(|ident| {
            let symbol = Owner::Global.symbol_id(&ident.name);
            match game.get_decl(&symbol).map(|d| &d.kind) {
                Some(DeclKind::Player(p)) => Some(p.index),
                Some(d) => {
                    errors.log(
                        ident.span,
                        format!("Expected player, '{}' is a {}", ident.name, d.kind_name()),
                    );
                    None
                }
                None => {
                    errors.log(
                        ident.span,
                        format!("Expected player, '{}' is not defined", ident.name),
                    );
                    None
                }
            }
        })
        .collect()
}
