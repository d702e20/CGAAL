use crate::lcgs::ast::{
    BinaryOpKind, Decl, DeclKind, Expr, ExprKind, Identifier, LabelDecl, Relabeling, StateVarDecl,
    TransitionDecl, TypeRange, UnaryOpKind,
};
use std::ops::Deref;

/// A [Relabeler] applies relabeling of declarations and their parts. It performs the
/// first [RelabelCase] that applies. A [RelabelCase] consists of a `prev` name and
/// a `new` expressions. Whenever `prev` is found in the given template, it is
/// replaced with `new`. The semantics is slightly different when `new` is
/// a single word (Identifier without owner). If `new` is an expression, the given expression
/// will simply be inserted, whenever `prev` appears. If is a word, then it will also be
/// inserted whenever `prev` appears, but if an identifier is found, where `prev` matches
/// either the owner or the field, then it will change that identifier instead. Examples:
/// 1) `foo + 5 [foo=10 + 2] ==> 10 + 2 + 5`
/// 2) `bar.baz [baz=yum] ==> bar.yum`
/// 3) `daf.hi > 2 [daf=dum] ==> dum.hi > 2`
///
/// And the following is of course illegal:
/// 1) `foo.bar [foo=5]`
/// 1) `baz.yum [yum=baz.hello]`
pub struct Relabeler<'a> {
    relabeling: &'a Relabeling,
}

impl<'a> Relabeler<'a> {
    pub fn new(relabeling: &'a Relabeling) -> Relabeler<'a> {
        Relabeler { relabeling }
    }

    pub fn relabel_decl(&self, decl: &Decl) -> Result<Decl, ()> {
        match &decl.kind {
            DeclKind::Label(label) => self.relabel_label(label),
            DeclKind::StateVar(var) => self.relabel_var(var),
            DeclKind::Transition(tran) => self.relabel_transition(tran),
            // The following declarations only appear in global scope and thus wont be renamed
            DeclKind::Const(_) => panic!("Cannot relabel a ConstDecl"),
            DeclKind::Player(_) => panic!("Cannot relabel a PlayerDecl"),
            DeclKind::Template(_) => panic!("Cannot relabel a Template"),
        }
    }

    fn relabel_label(&self, label: &LabelDecl) -> Result<Decl, ()> {
        Ok(Decl {
            kind: DeclKind::Label(Box::new(LabelDecl {
                name: self.relabel_simple_ident(&label.name)?,
                condition: self.relabel_expr(&label.condition)?,
            })),
        })
    }

    fn relabel_var(&self, var: &StateVarDecl) -> Result<Decl, ()> {
        Ok(Decl {
            kind: DeclKind::StateVar(Box::new(StateVarDecl {
                name: self.relabel_simple_ident(&var.name)?,
                range: TypeRange {
                    min: self.relabel_expr(&var.range.min)?,
                    max: self.relabel_expr(&var.range.max)?,
                },
                ir_range: 0..0,
                initial_value: self.relabel_expr(&var.initial_value)?,
                ir_initial_value: 0,
                next_value: self.relabel_expr(&var.next_value)?,
            })),
        })
    }

    fn relabel_transition(&self, tran: &TransitionDecl) -> Result<Decl, ()> {
        Ok(Decl {
            kind: DeclKind::Transition(Box::new(TransitionDecl {
                name: self.relabel_simple_ident(&tran.name)?,
                condition: self.relabel_expr(&tran.condition)?,
            })),
        })
    }

    /// Relabel a simple identifier (declaration name). The returned identifier is guaranteed
    /// to be a [Identifier::Simple] too.
    fn relabel_simple_ident(&self, ident: &Identifier) -> Result<Identifier, ()> {
        let name = match ident {
            Identifier::Simple { name } => name,
            _ => unreachable!(),
        };

        // Find first relabel case that matches, if any
        for relabel in &self.relabeling.relabellings {
            if &relabel.prev == name {
                // We found a case that relabels the identifier. Since this is a simple
                // identifier, we expect the new name to be an identifier (without an owner) too.
                if let ExprKind::OwnedIdent(new_ident) = &relabel.new.kind {
                    if let Identifier::OptionalOwner {
                        owner,
                        name: new_name,
                    } = &new_ident.deref()
                    {
                        if owner.is_some() {
                            panic!("You cannot rename a declaration's name to an expression.")
                        }
                        return Ok(Identifier::Simple {
                            name: new_name.clone(),
                        });
                    }
                    unreachable!()
                } else {
                    panic!("You cannot rename a declaration's name to an expression.")
                }
            }
        }

        // No relabeling performed
        Ok(ident.clone())
    }

    pub fn relabel_expr(&self, expr: &Expr) -> Result<Expr, ()> {
        match &expr.kind {
            ExprKind::Number(_) => Ok(expr.clone()),
            ExprKind::OwnedIdent(ident) => self.relabel_owned_ident(ident),
            ExprKind::UnaryOp(op, expr) => self.relabel_unop(op, expr),
            ExprKind::BinaryOp(op, lhs, rhs) => self.relabel_binop(op, lhs, rhs),
            ExprKind::TernaryIf(cond, true_expr, false_expr) => {
                self.relabel_if(cond, true_expr, false_expr)
            }
        }
    }

    /// Relabels an owner identifier (those found in expressions). The owner is not guaranteed
    /// to be there. If it is there, we allow rename of the owner and the name separately.
    /// If there is no explicit owner, then relabeling to an expression is also okay.
    fn relabel_owned_ident(&self, ident: &Identifier) -> Result<Expr, ()> {
        if let Identifier::OptionalOwner { owner, name } = ident {
            // If we have an owner, we relabel owner and name separately
            if let Some(owner) = owner {
                let new_owner = self.relabel_ident_part(owner)?;
                let new_name = self.relabel_ident_part(name)?;
                Ok(Expr {
                    kind: ExprKind::OwnedIdent(Box::new(Identifier::OptionalOwner {
                        owner: Some(new_owner),
                        name: new_name,
                    })),
                })
            } else {
                // If we have no owner, then we allow the user user to replace the name
                // with any expression
                for relabel_case in &self.relabeling.relabellings {
                    if &relabel_case.prev == name {
                        return Ok(relabel_case.new.clone());
                    }
                }

                // No relabeling performed
                Ok(Expr {
                    kind: ExprKind::OwnedIdent(Box::new(ident.clone())),
                })
            }
        } else {
            unreachable!()
        }
    }

    /// Relabels a part of an [Identifier::OptionallyOwned]. That is, either "p1" or "foo" in
    /// "p1.foo". In this case, it is not allowed to relabel to an expression, only another name,
    /// so the resulting identifier still makes sense.
    fn relabel_ident_part(&self, name: &str) -> Result<String, ()> {
        for relabel_case in &self.relabeling.relabellings {
            if &relabel_case.prev == name {
                // We found a case that applies. Since we are relabeling a part of an identifier
                // we only allow the new name to be a valid part of an identifier, i.e. an
                // identifier with no owner
                if let ExprKind::OwnedIdent(new_ident) = &relabel_case.new.kind {
                    if let Identifier::OptionalOwner {
                        owner,
                        name: new_name,
                    } = &new_ident.deref()
                    {
                        if owner.is_some() {
                            panic!("You cannot relabel a part of an identifier to an expression.")
                        }
                        return Ok(new_name.clone());
                    }
                    unreachable!()
                }
                panic!("You cannot relabel a part of an identifier to an expression.")
            }
        }

        // No relabeling performed
        Ok(name.to_string())
    }

    fn relabel_unop(&self, op: &UnaryOpKind, expr: &Expr) -> Result<Expr, ()> {
        Ok(Expr {
            kind: ExprKind::UnaryOp(op.clone(), Box::new(self.relabel_expr(expr)?)),
        })
    }

    fn relabel_binop(&self, op: &BinaryOpKind, lhs: &Expr, rhs: &Expr) -> Result<Expr, ()> {
        Ok(Expr {
            kind: ExprKind::BinaryOp(
                op.clone(),
                Box::new(self.relabel_expr(&lhs)?),
                Box::new(self.relabel_expr(&rhs)?),
            ),
        })
    }

    fn relabel_if(&self, cond: &Expr, true_expr: &Expr, false_expr: &Expr) -> Result<Expr, ()> {
        Ok(Expr {
            kind: ExprKind::TernaryIf(
                Box::new(self.relabel_expr(&cond)?),
                Box::new(self.relabel_expr(&true_expr)?),
                Box::new(self.relabel_expr(&false_expr)?),
            ),
        })
    }
}

#[cfg(test)]
mod test {
    use crate::lcgs::ast::{BinaryOpKind, Decl, DeclKind, Expr, ExprKind, Identifier, LabelDecl};
    use crate::lcgs::ir::relabeling::Relabeler;
    use crate::lcgs::parse;

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
