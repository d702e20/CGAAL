use crate::game_structure::lcgs::ast::{BinaryOpKind, Expr, ExprKind, Identifier, UnaryOpKind};
use crate::game_structure::lcgs::ir::symbol_table::SymbolIdentifier;
use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum ComparisonOp {
    Equal,
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,
}

/// Holds extracted linear expressions from the formula in AtlVertex
/// E.g. `ax + by + cz + k = 0`.
#[derive(Clone)]
pub struct LinearConstraint {
    /// A list of variables and their coefficients
    pub terms: HashMap<SymbolIdentifier, i32>,
    pub constant: i32,
    pub comparison: ComparisonOp,
    /// The norm of the coefficients. That is, if the linear expression is `ax + by + cz + k`, then
    /// this is `sqrt(a*a + b*b + c*c)`
    pub coefficient_norm: f64,
}

pub struct LinearConstraintExtractor {
    terms: HashMap<SymbolIdentifier, i32>,
    constant: i32,
    comparison: ComparisonOp,
}

impl LinearConstraintExtractor {
    /// Returns a linear constraint of the form `ax + by + cz + k = 0` (or some other comparison
    /// operator). Terms that appear on the left-hand-side side of the comparison will be moved
    /// to the left-hand-side and the sign of their coefficients will be flipped in the process.
    /// Terms will also be combined, e.g. `x + 2 * x = 0`, will result in x's coefficient being 3.
    pub fn extract(expr: &Expr) -> Option<LinearConstraint> {
        if let ExprKind::BinaryOp(operator, lhs, rhs) = &expr.kind {
            let comparison = match operator {
                BinaryOpKind::Equality => ComparisonOp::Equal,
                BinaryOpKind::GreaterThan => ComparisonOp::Greater,
                BinaryOpKind::GreaterOrEqual => ComparisonOp::GreaterOrEq,
                BinaryOpKind::LessThan => ComparisonOp::Less,
                BinaryOpKind::LessOrEqual => ComparisonOp::LessOrEq,
                _ => return None,
            };

            println!("Determined comparison operator");
            let mut extractor = LinearConstraintExtractor {
                terms: HashMap::new(),
                constant: 0,
                comparison,
            };

            // ax + bx - cz + k == 0

            println!("Handling lhs...");
            extractor.collect_terms(lhs, 1)?;
            println!("Handling rhs...");
            extractor.collect_terms(rhs, -1)?;

            Some(extractor.into_constraint())
        } else {
            None
        }
    }

    fn collect_terms(&mut self, expr: &Expr, sign: i32) -> Option<()> {
        match &expr.kind {
            ExprKind::Number(n) => {
                println!("Const");
                self.constant += sign * n
            }
            ExprKind::OwnedIdent(ident) => {
                // A variable with no explicit coefficient (which means the coefficient is 1)
                println!("Single ident");
                if let Identifier::Resolved { owner, name } = ident.as_ref() {
                    let coefficient = self.terms.entry(owner.symbol_id(name)).or_default();
                    *coefficient += sign;
                } else {
                    panic!("Unresolved identifier")
                }
            }
            ExprKind::UnaryOp(UnaryOpKind::Negation, expr) => {
                println!("Unary negation");
                self.collect_terms(expr, -sign)?; // Flip sign
            }
            ExprKind::BinaryOp(operator, lhs, rhs) => match operator {
                BinaryOpKind::Addition => {
                    println!("Addition");
                    println!("Handling lhs");
                    self.collect_terms(lhs, sign)?;
                    println!("Handling rhs");
                    self.collect_terms(rhs, sign)?;
                }
                BinaryOpKind::Subtraction => {
                    println!("Subtraction");
                    println!("Handling lhs");
                    self.collect_terms(lhs, sign)?;
                    println!("Handling rhs");
                    self.collect_terms(rhs, -sign)?; // Note: flipped sign
                }
                BinaryOpKind::Multiplication => {
                    println!("Multiplication");
                    // One side must be a constant, the other must be a variable
                    if let (ExprKind::Number(n), ExprKind::OwnedIdent(ident))
                    | (ExprKind::OwnedIdent(ident), ExprKind::Number(n)) = (&lhs.kind, &rhs.kind)
                    {
                        if let Identifier::Resolved { owner, name } = ident.as_ref() {
                            let coefficient = self.terms.entry(owner.symbol_id(name)).or_default();
                            *coefficient += n * sign;
                        } else {
                            panic!("Unresolved identifier")
                        }
                    } else {
                        return None;
                    }
                }
                BinaryOpKind::Division => {
                    println!("Division");
                    // Variable divided with a constant is a linear expression,
                    // but constant divided by variable is not.
                    if let (ExprKind::OwnedIdent(ident), ExprKind::Number(n)) =
                        (&lhs.kind, &rhs.kind)
                    {
                        if let Identifier::Resolved { owner, name } = ident.as_ref() {
                            let coefficient = self.terms.entry(owner.symbol_id(name)).or_default();
                            *coefficient += (1 / n) * sign;
                        } else {
                            panic!("Unresolved identifier")
                        }
                    } else {
                        return None;
                    }
                }
                _ => return None,
            },
            _ => return None,
        }
        Some(())
    }

    fn into_constraint(self) -> LinearConstraint {
        let coefficient_norm = self
            .terms
            .iter()
            .map(|(_, coefficient)| (coefficient * coefficient) as f64)
            .sum::<f64>()
            .sqrt();

        LinearConstraint {
            terms: self.terms,
            constant: self.constant,
            comparison: self.comparison,
            coefficient_norm,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::algorithms::certain_zero::search_strategy::linear_constraints::{
        ComparisonOp, LinearConstraintExtractor,
    };
    use crate::game_structure::lcgs::ast::DeclKind;
    use crate::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
    use crate::game_structure::lcgs::ir::symbol_table::Owner;
    use crate::game_structure::lcgs::parse::parse_lcgs;

    #[test]
    fn simple_comparison_01() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = x < 4;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Less, lin_expr.comparison);
            assert_eq!(-4, lin_expr.constant);
            assert_eq!(Some(&1), lin_expr.terms.get(&":global.x".into()));
        }
    }

    #[test]
    fn simple_comparison_02() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 2 * x > 3;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Greater, lin_expr.comparison);
            assert_eq!(-3, lin_expr.constant);
            assert_eq!(Some(&2), lin_expr.terms.get(&":global.x".into()));
        }
    }

    #[test]
    fn simple_comparison_03() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 0 > x * 2;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Greater, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&-2), lin_expr.terms.get(&":global.x".into()));
        }
    }

    #[test]
    fn simple_comparison_04() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 0 == 5 * x;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&-5), lin_expr.terms.get(&":global.x".into()));
        }
    }

    #[test]
    fn normal_linear_constraint_01() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        z : [0..0] init 0;
        z' = 0;
        label prop = 2 * x + 3 * y == -5;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(5, lin_expr.constant);
            assert_eq!(Some(&2), lin_expr.terms.get(&":global.x".into()));
            assert_eq!(Some(&3), lin_expr.terms.get(&":global.y".into()));
        }
    }

    #[test]
    fn normal_linear_constraint_02() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        z : [0..0] init 0;
        z' = 0;
        label prop = 2 * x - 3 * y == 0;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&2), lin_expr.terms.get(&":global.x".into()));
            assert_eq!(Some(&-3), lin_expr.terms.get(&":global.y".into()));
        }
    }

    #[test]
    fn normal_linear_constraint_03() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        z : [0..0] init 0;
        z' = 0;
        label prop = - x + y - z == 0;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&-1), lin_expr.terms.get(&":global.x".into()));
            assert_eq!(Some(&1), lin_expr.terms.get(&":global.y".into()));
            assert_eq!(Some(&-1), lin_expr.terms.get(&":global.z".into()));
        }
    }

    #[test]
    fn normal_linear_constraint_04() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        z : [0..0] init 0;
        z' = 0;
        label prop = - x + y - z == 0;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&-1), lin_expr.terms.get(&":global.x".into()));
            assert_eq!(Some(&1), lin_expr.terms.get(&":global.y".into()));
            assert_eq!(Some(&-1), lin_expr.terms.get(&":global.z".into()));
        }
    }

    #[test]
    fn normal_linear_constraint_05() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 2 * x + x == 0;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&3), lin_expr.terms.get(&":global.x".into()));
        }
    }

    #[test]
    fn normal_linear_constraint_06() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 1 + 2 * x - 3 == 5;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(-7, lin_expr.constant);
            assert_eq!(Some(&2), lin_expr.terms.get(&":global.x".into()));
        }
    }

    #[test]
    fn other_linear_constraint_01() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        label prop = 2 * x == y;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&2), lin_expr.terms.get(&":global.x".into()));
            assert_eq!(Some(&-1), lin_expr.terms.get(&":global.y".into()));
        }
    }

    #[test]
    fn other_linear_constraint_02() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        z : [0..0] init 0;
        z' = 0;
        label prop = x == (y - z) + 5;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(-5, lin_expr.constant);
            assert_eq!(Some(&1), lin_expr.terms.get(&":global.x".into()));
            assert_eq!(Some(&-1), lin_expr.terms.get(&":global.y".into()));
            assert_eq!(Some(&1), lin_expr.terms.get(&":global.z".into()));
        }
    }

    #[test]
    fn other_linear_constraint_03() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        z : [0..0] init 0;
        z' = 0;
        label prop = x + x - y - x == -3 * z;
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&1), lin_expr.terms.get(&":global.x".into()));
            assert_eq!(Some(&-1), lin_expr.terms.get(&":global.y".into()));
            assert_eq!(Some(&3), lin_expr.terms.get(&":global.z".into()));
        }
    }

    #[test]
    fn other_linear_constraint_04() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        label prop = 2 * x == (-2 * y);
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(input).unwrap()).unwrap();
        let decl = lcgs.get_decl(&Owner::Global.symbol_id("prop")).unwrap();
        if let DeclKind::Label(label) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&label.condition).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0, lin_expr.constant);
            assert_eq!(Some(&2), lin_expr.terms.get(&":global.x".into()));
            assert_eq!(Some(&2), lin_expr.terms.get(&":global.y".into()));
        }
    }
}
