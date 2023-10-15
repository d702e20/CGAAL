use crate::parsing::ast::{
    Coalition, Decl, DeclKind, Expr, ExprKind, Ident, OwnedIdent, RelabelCase, StateVarDecl,
};
use crate::parsing::errors::SpannedError;
use crate::parsing::span::Span;

/// A [Relabeler] applies relabeling of declarations and their parts. It performs the
/// first [RelabelCase] that applies. A [RelabelCase] consists of a `from` name and
/// a `to` expressions. Whenever `from` is found in the given template, it is
/// replaced with `to`. The semantics is slightly different when `to` is
/// a single word (Identifier without owner). If `to` is an expression, the given expression
/// will simply be inserted, whenever `from` appears. If is a word, then it will also be
/// inserted whenever `from` appears, but if an identifier is found, where `from` matches
/// either the owner or the field, then it will change that identifier instead. Examples:
/// 1) `foo + 5 [foo=10 + 2] ==> (10 + 2) + 5`
/// 2) `bar.baz [baz=yum] ==> bar.yum`
/// 3) `daf.hi > 2 [daf=dum] ==> dum.hi > 2`
///
/// And the following is of course illegal:
/// 1) `foo.bar [foo=5]`
/// 1) `baz.yum [yum=baz.hello]`
pub struct Relabeler<'a> {
    cases: &'a [RelabelCase],
}

impl<'a> Relabeler<'a> {
    pub fn new(cases: &'a [RelabelCase]) -> Relabeler<'a> {
        Relabeler { cases }
    }

    pub fn relabel_decl(&self, mut decl: Decl) -> Result<Decl, SpannedError> {
        decl.ident.name = self
            .relabel_ident_to_ident(decl.ident.name)
            .map_err(decl_ident_relabel_error)?;
        decl.kind = match decl.kind {
            DeclKind::StateLabel(idx, expr) => DeclKind::StateLabel(idx, self.relabel_expr(expr)?),
            DeclKind::StateVar(var) => DeclKind::StateVar(self.relabel_state_var(var)?),
            DeclKind::Action(expr) => DeclKind::Action(self.relabel_expr(expr)?),
            // The following declarations only appear in global scope and thus wont be renamed
            DeclKind::Const(_) => panic!("Cannot relabel a ConstDecl"),
            DeclKind::Player(_) => panic!("Cannot relabel a PlayerDecl"),
            DeclKind::Template(_) => panic!("Cannot relabel a Template"),
            DeclKind::Error => DeclKind::Error,
        };
        Ok(decl)
    }

    fn relabel_state_var(&self, mut var: StateVarDecl) -> Result<StateVarDecl, SpannedError> {
        var.update_ident = self
            .relabel_ident_to_ident(var.update_ident)
            .map_err(decl_ident_relabel_error)?;
        var.range.min = self.relabel_expr(var.range.min)?;
        var.range.max = self.relabel_expr(var.range.max)?;
        var.init = self.relabel_expr(var.init)?;
        var.update = self.relabel_expr(var.update)?;
        Ok(var)
    }

    pub fn relabel_expr(&self, mut expr: Expr) -> Result<Expr, SpannedError> {
        expr.kind = match expr.kind {
            ExprKind::Num(n) => ExprKind::Num(n),
            ExprKind::OwnedIdent(ident) => return self.relabel_owned_ident(expr.span, ident),
            ExprKind::Unary(op, expr) => ExprKind::Unary(op, self.relabel_expr(*expr)?.into()),
            ExprKind::Binary(op, lhs, rhs) => ExprKind::Binary(
                op,
                self.relabel_expr(*lhs)?.into(),
                self.relabel_expr(*rhs)?.into(),
            ),
            ExprKind::TernaryIf(cond, then_expr, else_expr) => ExprKind::TernaryIf(
                self.relabel_expr(*cond)?.into(),
                self.relabel_expr(*then_expr)?.into(),
                self.relabel_expr(*else_expr)?.into(),
            ),
            ExprKind::Min(mut exprs) => ExprKind::Min(
                exprs
                    .drain(..)
                    .map(|e| self.relabel_expr(e))
                    .collect::<Result<Vec<Expr>, SpannedError>>()?,
            ),
            ExprKind::Max(mut exprs) => ExprKind::Max(
                exprs
                    .drain(..)
                    .map(|e| self.relabel_expr(e))
                    .collect::<Result<Vec<Expr>, SpannedError>>()?,
            ),
            ExprKind::True => ExprKind::True,
            ExprKind::False => ExprKind::False,
            ExprKind::Paren(expr) => ExprKind::Paren(self.relabel_expr(*expr)?.into()),
            ExprKind::Coalition(coal) => ExprKind::Coalition(self.relabel_coalition(coal)?),
            ExprKind::Error => ExprKind::Error,
            ExprKind::Symbol(_) => panic!("Relabeling must happen before symbol checking."),
        };
        Ok(expr)
    }

    /// Relabels an owner identifier (those found in expressions). The owner is not guaranteed
    /// to be there. If it is there, we allow renaming of the owner and the name to a new name
    /// separately. If there is no explicit owner, then relabeling to an expression is also okay.
    fn relabel_owned_ident(&self, span: Span, ident: OwnedIdent) -> Result<Expr, SpannedError> {
        if let OwnedIdent {
            owner: Some(owner),
            name,
        } = ident
        {
            // There is an owner, so we relabel owner and name separately
            let new_owner = self
                .relabel_ident_to_ident(owner)
                .map_err(owned_ident_relabel_error)?;
            let new_name = self
                .relabel_ident_to_ident(name)
                .map_err(owned_ident_relabel_error)?;
            let new_oi = OwnedIdent::new(Some(new_owner), new_name);
            Ok(Expr::new(span, ExprKind::OwnedIdent(new_oi)))
        } else {
            // If we have no owner, then we allow the user to replace
            // the name with any expression
            for case in self.cases {
                if case.from.text == ident.name.text {
                    return Ok(case.to.clone());
                }
            }

            // No relabeling performed
            Ok(Expr::new(span, ExprKind::OwnedIdent(ident)))
        }
    }

    /// Relabel an [Ident] to another. An error is returned if a relabeling case applies,
    /// but the result is not an identifier.
    fn relabel_ident_to_ident(&self, mut ident: Ident) -> Result<Ident, Ident> {
        for case in self.cases {
            if case.from.text == ident.text {
                // We found a case that applies. The result must be an identifier too
                return if let ExprKind::OwnedIdent(OwnedIdent {
                    owner: Option::None,
                    name,
                }) = &case.to.kind
                {
                    ident.text = name.text.clone();
                    Ok(ident)
                } else {
                    Err(ident)
                };
            }
        }

        // No relabeling performed
        Ok(ident)
    }

    /// Relabels a coalition. If relabelled, player name must be relabeled to unowned identifiers.
    fn relabel_coalition(&self, mut coal: Coalition) -> Result<Coalition, SpannedError> {
        fn relabel_player(selth: &Relabeler, mut ident: Ident) -> Result<Ident, SpannedError> {
            for case in selth.cases {
                if case.from.text == ident.text {
                    // We found a case that applies. Since we are relabeling a player name
                    // we only allow the new name to be an unowned identifier
                    return if let ExprKind::OwnedIdent(OwnedIdent {
                        owner: Option::None,
                        name,
                    }) = &case.to.kind
                    {
                        ident.text = name.text.clone();
                        Ok(ident)
                    } else {
                        Err(SpannedError::new(
                            case.from.span,
                            format!(
                                "You cannot relabel '{}' to an expression since it is a player name.",
                                case.from.text
                            ),
                        ))
                    };
                }
            }

            // No relabeling performed
            Ok(ident)
        }

        coal.players = coal
            .players
            .drain(..)
            .map(|e| relabel_player(&self, e))
            .collect::<Result<Vec<Ident>, SpannedError>>()?;
        coal.expr = self.relabel_expr(*coal.expr)?.into();
        Ok(coal)
    }
}

/// Produce error for trying to relabel a declaration name to an expression.
fn decl_ident_relabel_error(ident: Ident) -> SpannedError {
    SpannedError::new(
        ident.span,
        format!(
            "Cannot rename '{}' to an expression since it is the name of a declaration.",
            ident.text
        ),
    )
}

/// Produce error for trying to relabel an owned identifier to an expression.
fn owned_ident_relabel_error(ident: Ident) -> SpannedError {
    SpannedError::new(
        ident.span,
        format!(
            "Cannot rename '{}' to an expression since it is part of an owned identifier.",
            ident.text
        ),
    )
}

#[cfg(test)]
mod test {
    use crate::game_structure::lcgs::ast::{
        BinaryOpKind, Decl, DeclKind, Expr, ExprKind, Identifier, LabelDecl,
    };
    use crate::game_structure::lcgs::ir::relabeling::Relabeler;
    use crate::game_structure::lcgs::parse;

    #[test]
    fn test_relabeling_expr_01() {
        // Check simple relabeling from one name to another
        let relabeling = "[test=res]";
        let relabeling = parse::relabeling().parse(relabeling.as_bytes()).unwrap();
        let expr = "5 + test";
        let expr = parse::expr().parse(expr.as_bytes()).unwrap();
        let relabeler = Relabeler::new(&relabeling);
        let new_expr = relabeler.relabel_expr(&expr).unwrap();
        assert_eq!(
            new_expr,
            Expr {
                kind: ExprKind::BinaryOp(
                    BinaryOpKind::Addition,
                    Box::new(Expr {
                        kind: ExprKind::Number(5),
                    }),
                    Box::new(Expr {
                        kind: ExprKind::OwnedIdent(Box::new(Identifier::OptionalOwner {
                            owner: None,
                            name: "res".to_string()
                        }))
                    })
                )
            }
        )
    }

    #[test]
    fn test_relabeling_expr_02() {
        // Check simple relabeling to number
        let relabeling = "[test=4]";
        let relabeling = parse::relabeling().parse(relabeling.as_bytes()).unwrap();
        let expr = "5 + test";
        let expr = parse::expr().parse(expr.as_bytes()).unwrap();
        let relabeler = Relabeler::new(&relabeling);
        let new_expr = relabeler.relabel_expr(&expr).unwrap();
        assert_eq!(
            new_expr,
            Expr {
                kind: ExprKind::BinaryOp(
                    BinaryOpKind::Addition,
                    Box::new(Expr {
                        kind: ExprKind::Number(5),
                    }),
                    Box::new(Expr {
                        kind: ExprKind::Number(4),
                    }),
                )
            }
        )
    }

    #[test]
    fn test_relabeling_expr_03() {
        // Check simple relabeling of identifier part
        let relabeling = "[bar=yum]";
        let relabeling = parse::relabeling().parse(relabeling.as_bytes()).unwrap();
        let expr = "foo.bar + bar.baz";
        let expr = parse::expr().parse(expr.as_bytes()).unwrap();
        let relabeler = Relabeler::new(&relabeling);
        let new_expr = relabeler.relabel_expr(&expr).unwrap();
        assert_eq!(
            new_expr,
            Expr {
                kind: ExprKind::BinaryOp(
                    BinaryOpKind::Addition,
                    Box::new(Expr {
                        kind: ExprKind::OwnedIdent(Box::new(Identifier::OptionalOwner {
                            owner: Some("foo".to_string()),
                            name: "yum".to_string()
                        }))
                    }),
                    Box::new(Expr {
                        kind: ExprKind::OwnedIdent(Box::new(Identifier::OptionalOwner {
                            owner: Some("yum".to_string()),
                            name: "baz".to_string()
                        }))
                    }),
                )
            }
        )
    }

    #[test]
    fn test_relabeling_label_01() {
        // Check simple relabeling of label declaration name
        let relabeling = "[foo=yum]";
        let relabeling = parse::relabeling().parse(relabeling.as_bytes()).unwrap();
        let decl = "label foo = bar.baz";
        let decl = parse::label_decl().parse(decl.as_bytes()).unwrap();
        let relabeler = Relabeler::new(&relabeling);
        let new_decl = relabeler.relabel_label(&decl).unwrap();
        assert_eq!(
            new_decl,
            Decl {
                kind: DeclKind::Label(Box::new(LabelDecl {
                    index: 0usize,
                    condition: Expr {
                        kind: ExprKind::OwnedIdent(Box::new(Identifier::OptionalOwner {
                            owner: Some("bar".to_string()),
                            name: "baz".to_string()
                        }))
                    },
                    name: Identifier::Simple {
                        name: "yum".to_string()
                    }
                }))
            }
        )
    }
}
