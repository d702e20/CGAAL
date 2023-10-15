use crate::atl::Phi;
use crate::game_structure::lcgs::intermediate::IntermediateLcgs;
use crate::game_structure::PlayerIdx;
use crate::parsing::ast::{BinaryOpKind, Coalition, CoalitionKind, DeclKind, Expr, ExprKind, Ident, UnaryOpKind};
use crate::parsing::errors::ErrorLog;

pub fn convert_expr_to_phi(expr: Expr, game: &IntermediateLcgs, errors: &ErrorLog) -> Result<Phi, ()> {
    QueryBuilder::new(game, errors).build(expr)
}

pub struct QueryBuilder<'a> {
    pub game: &'a IntermediateLcgs,
    pub errors: &'a ErrorLog,
}

impl<'a> QueryBuilder<'a> {
    pub fn new(game: &'a IntermediateLcgs, errors: &'a ErrorLog) -> Self {
        QueryBuilder {
            game,
            errors,
        }
    }

    /// Convert an ATL expression to a Phi formula.
    /// Players and labels must be defined in the game and are compiled to their respective indexes.
    /// Returns Err if there were errors. See the error log for details.
    pub fn build(&self, expr: Expr) -> Result<Phi, ()> {
        let Expr { span, kind } = expr;
        match kind {
            ExprKind::True => Ok(Phi::True),
            ExprKind::False => Ok(Phi::False),
            ExprKind::Paren(e) => self.build(*e),
            ExprKind::Symbol(_) => unreachable!("ATL expressions are not symbol checked"),
            ExprKind::OwnedIdent(oi) => {
                // FIXME: Unnecessary case and checks if ATL expr were symbol checked
                if let Some(player) = &oi.owner {
                    let pdecl_opt = self.game.get_decl_by_name(&player.to_string());
                    match pdecl_opt.map(|d| &d.kind) {
                        Some(DeclKind::Player(_)) => {
                            // ok
                        }
                        Some(d) => {
                            self.errors.log(
                                player.span,
                                format!("Expected player, '{}' is a {}", player, d.kind_name()),
                            );
                            return Err(());
                        }
                        None => {
                            self.errors.log(
                                player.span,
                                format!("Expected player, '{}' is not defined", player),
                            );
                            return Err(());
                        }
                    }
                }

                let decl_opt = self.game.get_decl_by_name(&oi.to_string());
                match decl_opt.map(|d| &d.kind) {
                    Some(DeclKind::StateLabel(idx, _)) => Ok(Phi::Proposition(*idx)),
                    Some(d) => {
                        self.errors.log(
                            span,
                            format!(
                                "Expected proposition label, '{}' is a {}",
                                oi,
                                d.kind_name()
                            ),
                        );
                        Err(())
                    }
                    None => {
                        self.errors.log(
                            span,
                            format!(
                                "Expected proposition label, '{}' is not defined",
                                oi
                            ),
                        );
                        Err(())
                    }
                }
            }
            ExprKind::Unary(op, e) => match op {
                UnaryOpKind::Not => Ok(Phi::Not(self.build(*e)?.into())),
                UnaryOpKind::Next | UnaryOpKind::Eventually | UnaryOpKind::Invariantly => {
                    self.errors.log(
                        span,
                        "Temporal operators are only allowed after a coalition".to_string(),
                    );
                    Err(())
                }
                UnaryOpKind::Neg => {
                    self.errors.log(
                        span,
                        "Arithmetic operators is currently not supported in ATL".to_string(),
                    );
                    Err(())
                }
            },
            ExprKind::Binary(op, lhs, rhs) => match op {
                BinaryOpKind::And => Ok(Phi::And(
                    self.build(*lhs)?.into(),
                    self.build(*rhs)?.into(),
                )),
                BinaryOpKind::Or => Ok(Phi::Or(
                    self.build(*lhs)?.into(),
                    self.build(*rhs)?.into(),
                )),
                BinaryOpKind::Until => {
                    self.errors.log(
                        span,
                        "Temporal operators are only allowed after a coalition".to_string(),
                    );
                    Err(())
                }
                BinaryOpKind::Xor => {
                    self.errors.log(
                        span,
                        "Exclusive OR is currently not supported in ATL".to_string(),
                    );
                    Err(())
                }
                BinaryOpKind::Implies => {
                    self.errors.log(
                        span,
                        "Implication is currently not supported in ATL".to_string(),
                    );
                    Err(())
                }
                BinaryOpKind::Eq
                | BinaryOpKind::Neq
                | BinaryOpKind::Gt
                | BinaryOpKind::Geq
                | BinaryOpKind::Lt
                | BinaryOpKind::Leq => {
                    self.errors.log(
                        span,
                        "Relational operators are currently not supported in ATL".to_string(),
                    );
                    Err(())
                }
                BinaryOpKind::Add | BinaryOpKind::Sub | BinaryOpKind::Mul | BinaryOpKind::Div => {
                    self.errors.log(
                        span,
                        "Arithmetic operators are currently not supported in ATL".to_string(),
                    );
                    Err(())
                }
            },
            ExprKind::TernaryIf(_, _, _) => {
                self.errors.log(
                    span,
                    "Ternary if expressions are currently not supported in ATL".to_string(),
                );
                Err(())
            }
            ExprKind::Coalition(Coalition {
                                    players,
                                    kind,
                                    expr: path_expr,
                                    ..
                                }) => match (kind, path_expr.kind) {
                (CoalitionKind::Enforce, ExprKind::Unary(UnaryOpKind::Next, sub_expr)) => {
                    let phi = self.build(*sub_expr)?;
                    Ok(Phi::EnforceNext {
                        players: self.convert_players(players)?,
                        formula: phi.into(),
                    })
                }
                (CoalitionKind::Despite, ExprKind::Unary(UnaryOpKind::Next, sub_expr)) => {
                    let phi = self.build(*sub_expr)?;
                    Ok(Phi::DespiteNext {
                        players: self.convert_players(players)?,
                        formula: phi.into(),
                    })
                }
                (CoalitionKind::Enforce, ExprKind::Unary(UnaryOpKind::Eventually, sub_expr)) => {
                    let phi = self.build(*sub_expr)?;
                    Ok(Phi::EnforceEventually {
                        players: self.convert_players(players)?,
                        formula: phi.into(),
                    })
                }
                (CoalitionKind::Despite, ExprKind::Unary(UnaryOpKind::Eventually, sub_expr)) => {
                    let phi = self.build(*sub_expr)?;
                    Ok(Phi::DespiteEventually {
                        players: self.convert_players(players)?,
                        formula: phi.into(),
                    })
                }
                (CoalitionKind::Enforce, ExprKind::Unary(UnaryOpKind::Invariantly, sub_expr)) => {
                    let phi = self.build(*sub_expr)?;
                    Ok(Phi::EnforceInvariant {
                        players: self.convert_players(players)?,
                        formula: phi.into(),
                    })
                }
                (CoalitionKind::Despite, ExprKind::Unary(UnaryOpKind::Invariantly, sub_expr)) => {
                    let phi = self.build(*sub_expr)?;
                    Ok(Phi::DespiteInvariant {
                        players: self.convert_players(players)?,
                        formula: phi.into(),
                    })
                }
                (CoalitionKind::Enforce, ExprKind::Binary(BinaryOpKind::Until, lhs, rhs)) => {
                    let lhs_phi = self.build(*lhs)?;
                    let rhs_phi = self.build(*rhs)?;
                    Ok(Phi::EnforceUntil {
                        players: self.convert_players(players)?,
                        pre: lhs_phi.into(),
                        until: rhs_phi.into(),
                    })
                }
                (CoalitionKind::Despite, ExprKind::Binary(BinaryOpKind::Until, lhs, rhs)) => {
                    let lhs_phi = self.build(*lhs)?;
                    let rhs_phi = self.build(*rhs)?;
                    Ok(Phi::DespiteUntil {
                        players: self.convert_players(players)?,
                        pre: lhs_phi.into(),
                        until: rhs_phi.into(),
                    })
                }
                _ => {
                    self.errors.log(
                        path_expr.span,
                        "Coalitions must be followed by a path formula".to_string(),
                    );
                    Err(())
                }
            },
            ExprKind::Num(_) => {
                self.errors.log(
                    span,
                    "Unexpected number. Please use true, false, or label names as propositions."
                        .to_string(),
                );
                Err(())
            }
            ExprKind::Max(_) | ExprKind::Min(_) => {
                self.errors.log(
                    span,
                    "Max and min expressions are currently not supported in ATL".to_string(),
                );
                Err(())
            }
            ExprKind::Error => Err(()),
        }
    }

    /// Helper function for converting a list of player names to a list of player indexes.
    /// Returns None if there were errors. See the error log for details.
    fn convert_players(&self, players: Vec<Ident>) -> Result<Vec<PlayerIdx>, ()> {
        players
            .iter()
            .map(|ident| {
                let decl_opt = self.game.get_decl_by_name(&ident.to_string());
                match decl_opt.map(|d| &d.kind) {
                    Some(DeclKind::Player(p)) => Ok(p.index),
                    Some(d) => {
                        self.errors.log(
                            ident.span,
                            format!("Expected player, '{}' is a {}", ident.text, d.kind_name()),
                        );
                        Err(())
                    }
                    None => {
                        self.errors.log(
                            ident.span,
                            format!("Expected player, '{}' is not defined", ident.text),
                        );
                        Err(())
                    }
                }
            })
            .collect()
    }
}