use crate::atl::Phi;
use crate::game_structure::lcgs::ast::DeclKind;
use crate::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
use crate::game_structure::lcgs::ir::symbol_table::Owner;
use crate::game_structure::Player;
use crate::parsing::ast::{BinaryOpKind, Coalition, CoalitionKind, Expr, ExprKind, UnaryOpKind};
use crate::parsing::errors::ErrorLog;

/// Convert an ATL expression to a Phi formula.
/// Players and labels must be defined in the game and are compiled to their respective indexes.
/// Returns None if there were errors. See the error log for details.
pub fn convert_expr_to_phi(
    expr: &Expr,
    game: &IntermediateLcgs,
    errors: &mut ErrorLog,
) -> Option<Phi> {
    let Expr { span, kind } = expr;
    match kind {
        ExprKind::True => Some(Phi::True),
        ExprKind::False => Some(Phi::False),
        ExprKind::Paren(e) => convert_expr_to_phi(e, game, errors),
        ExprKind::Ident(ident) => {
            let decl = game.get_decl(&Owner::Global.symbol_id(ident));
            match &decl.map(|d| &d.kind) {
                Some(DeclKind::Label(l)) => Some(Phi::Proposition(l.index)),
                Some(_) => {
                    errors.log(
                        *span,
                        format!("Expected proposition label, '{}' is not a label", ident),
                    );
                    None
                }
                None => {
                    errors.log(
                        *span,
                        format!("Expected proposition label, '{}' is not defined", ident),
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
            BinaryOpKind::Dot => {
                let ExprKind::Ident(owner) = &lhs.kind else {
                    errors.log(lhs.span, "Expected player name".to_string());
                    return None;
                };
                let ExprKind::Ident(prop) = &rhs.kind else {
                    errors.log(rhs.span, "Expected proposition label".to_string());
                    return None;
                };
                match game
                    .get_decl(&Owner::Global.symbol_id(owner))
                    .map(|d| &d.kind)
                {
                    Some(DeclKind::Player(_)) => {
                        let symb = Owner::Player(owner.clone()).symbol_id(prop);
                        let decl = game.get_decl(&symb);
                        match decl.map(|d| &d.kind) {
                            Some(DeclKind::Label(l)) => Some(Phi::Proposition(l.index)),
                            Some(_) => {
                                errors.log(
                                    rhs.span,
                                    format!(
                                        "Expected proposition label, '{}' is not a label",
                                        prop
                                    ),
                                );
                                None
                            }
                            None => {
                                errors.log(
                                    rhs.span,
                                    format!(
                                        "Expected proposition label, '{}' is not defined",
                                        prop
                                    ),
                                );
                                None
                            }
                        }
                    }
                    Some(_) => {
                        errors.log(
                            lhs.span,
                            format!("Expected player, '{}' is not a player", owner),
                        );
                        None
                    }
                    None => {
                        errors.log(
                            lhs.span,
                            format!("Expected player, '{}' is not defined", owner),
                        );
                        None
                    }
                }
            }
            BinaryOpKind::Until => {
                errors.log(
                    *span,
                    "Temporal operators are only allowed after a coalition".to_string(),
                );
                None
            }
        },
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
        ExprKind::Error => None,
    }
}

/// Helper function for converting a list of player names to a list of player indexes.
/// Returns None if there were errors. See the error log for details.
fn convert_players(
    players: &[Expr],
    game: &IntermediateLcgs,
    errors: &mut ErrorLog,
) -> Option<Vec<Player>> {
    players
        .iter()
        .map(|expr| match &expr.kind {
            ExprKind::Ident(name) => match game
                .get_decl(&Owner::Global.symbol_id(name))
                .map(|d| &d.kind)
            {
                Some(DeclKind::Player(p)) => Some(p.index),
                Some(_) => {
                    errors.log(
                        expr.span,
                        format!("Expected player, '{}' is not a player", name),
                    );
                    None
                }
                None => {
                    errors.log(
                        expr.span,
                        format!("Expected player, '{}' is not defined", name),
                    );
                    None
                }
            },
            _ => {
                errors.log(expr.span, "Expected player name".to_string());
                None
            }
        })
        .collect()
}
