use crate::game_structure::lcgs::symbol_table::SymbIdx;
use crate::parsing::ast::{BinaryOpKind, Expr, ExprKind, UnaryOpKind};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

/// All comparison operators
#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum ComparisonOp {
    Equal,
    NotEqual,
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,
}

impl ComparisonOp {
    fn negated(&self) -> ComparisonOp {
        match self {
            ComparisonOp::Equal => ComparisonOp::NotEqual,
            ComparisonOp::NotEqual => ComparisonOp::Equal,
            ComparisonOp::Less => ComparisonOp::GreaterOrEq,
            ComparisonOp::LessOrEq => ComparisonOp::Greater,
            ComparisonOp::Greater => ComparisonOp::LessOrEq,
            ComparisonOp::GreaterOrEq => ComparisonOp::Less,
        }
    }
}

impl Display for ComparisonOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ComparisonOp::Equal => write!(f, "="),
            ComparisonOp::NotEqual => writeln!(f, "!="),
            ComparisonOp::Less => write!(f, "<"),
            ComparisonOp::LessOrEq => write!(f, "<="),
            ComparisonOp::Greater => write!(f, ">"),
            ComparisonOp::GreaterOrEq => write!(f, ">="),
        }
    }
}

impl From<ComparisonOp> for minilp::ComparisonOp {
    fn from(op: ComparisonOp) -> Self {
        // We are bound to lose some precision here, however, this should not matter for the
        // linear programming solutions. Except for NotEqual becoming Equal, but, we don't
        // include NotEqual's in our linear problems anyway, since they are almost always true.
        match op {
            ComparisonOp::Equal | ComparisonOp::NotEqual => minilp::ComparisonOp::Eq,
            ComparisonOp::Less | ComparisonOp::LessOrEq => minilp::ComparisonOp::Le,
            ComparisonOp::Greater | ComparisonOp::GreaterOrEq => minilp::ComparisonOp::Ge,
        }
    }
}

/// Holds extracted linear expressions from the formula in AtlVertex
/// E.g. `ax + by + cz + k = 0`.
#[derive(Clone)]
pub struct LinearConstraint {
    /// A list of variables and their coefficients
    pub terms: HashMap<SymbIdx, f64>,
    pub constant: f64,
    pub comparison: ComparisonOp,
    /// The norm of the coefficients. That is, if the linear expression is `ax + by + cz + k`, then
    /// this is `sqrt(a*a + b*b + c*c)`
    pub coefficient_norm: f64,
}

impl LinearConstraint {
    /// Return a negated version of this constraint
    pub fn negated(&self) -> Self {
        let mut clone = self.clone();
        clone.comparison = clone.comparison.negated();
        clone
    }
}

impl Display for LinearConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (coefficient, symbol) in &self.terms {
            write!(f, "{} * {} ", coefficient, symbol)?;
        }
        write!(f, "+ {} {} 0", self.constant, self.comparison)
    }
}

/// A visitor that can extract linear constraints from an Expr.
/// See [LinearConstraintExtractor::extract].
pub struct LinearConstraintExtractor {
    terms: HashMap<SymbIdx, f64>,
    constant: f64,
    comparison: ComparisonOp,
}

impl LinearConstraintExtractor {
    /// Returns a linear constraint of the form `ax + by + cz + k = 0` (or some other comparison
    /// operator). Terms that appear on the left-hand-side side of the comparison will be moved
    /// to the left-hand-side and the sign of their coefficients will be flipped in the process.
    /// Terms will also be combined, e.g. `x + 2 * x = 0`, will result in x's coefficient being 3.
    pub fn extract(expr: &Expr) -> Option<LinearConstraint> {
        // Outermost node must be a comparison
        if let ExprKind::Binary(operator, lhs, rhs) = &expr.kind {
            let comparison = match operator {
                BinaryOpKind::Eq => ComparisonOp::Equal,
                BinaryOpKind::Gt => ComparisonOp::Greater,
                BinaryOpKind::Geq => ComparisonOp::GreaterOrEq,
                BinaryOpKind::Lt => ComparisonOp::Less,
                BinaryOpKind::Leq => ComparisonOp::LessOrEq,
                _ => return None,
            };

            // Prepare extractor
            let mut extractor = LinearConstraintExtractor {
                terms: HashMap::new(),
                constant: 0.0,
                comparison,
            };

            // Visit both sides of the comparison. We want to move the stuff on the rhs to the lhs
            // so every variable found on the rhs is multiplied by -1
            extractor.collect_terms(lhs, 1.0)?;
            extractor.collect_terms(rhs, -1.0)?;

            // We found a linear constraint. Now return it
            Some(extractor.into_constraint())
        } else {
            None
        }
    }

    /// Visits an Expr and collects all valid linear terms.
    /// Negated terms are handled using the sign parameter (either 1 or -1).
    /// If this returns None, the expression is not linear.
    fn collect_terms(&mut self, expr: &Expr, sign: f64) -> Option<()> {
        match &expr.kind {
            ExprKind::Num(n) => self.constant += sign * (*n as f64),
            ExprKind::OwnedIdent(_) => panic!("Found an owned identifier during linear constraint extraction. All identifiers should have been replaced with symbols by now."),
            ExprKind::Symbol(symb) => {
                // A variable with no explicit coefficient (which means the coefficient is 1)
                let coefficient = self.terms.entry(*symb).or_default();
                *coefficient += sign;
            }
            ExprKind::Unary(UnaryOpKind::Neg, expr) => {
                // Unary negation simply flips the sign
                self.collect_terms(expr, -sign)?;
            }
            ExprKind::Binary(operator, lhs, rhs) => match operator {
                BinaryOpKind::Add => {
                    self.collect_terms(lhs, sign)?;
                    self.collect_terms(rhs, sign)?;
                }
                BinaryOpKind::Sub => {
                    self.collect_terms(lhs, sign)?;
                    self.collect_terms(rhs, -sign)?; // Note: flipped sign
                }
                BinaryOpKind::Mul => {
                    // We found a potential term
                    // One side must be a constant, the other must be a variable
                    if let (ExprKind::Num(n), ExprKind::Symbol(symb))
                    | (ExprKind::Symbol(symb), ExprKind::Num(n)) = (&lhs.kind, &rhs.kind)
                    {
                        let coefficient = self.terms.entry(*symb).or_default();
                        *coefficient += (*n as f64) * sign;
                    } else {
                        return None;
                    }
                }
                BinaryOpKind::Div => {
                    if let (ExprKind::Symbol(symb), ExprKind::Num(n)) =
                        (&lhs.kind, &rhs.kind)
                    {
                        let coefficient = self.terms.entry(*symb).or_default();
                        *coefficient += sign / (*n as f64);
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

    /// Finish the extraction by turning the extractor into a linear constraint
    fn into_constraint(self) -> LinearConstraint {
        // This value is useful for calculating distance later
        // Since it involves sqrt we don't want to do it multiple times
        let coefficient_norm = self
            .terms
            .values()
            .map(|coefficient| coefficient * coefficient)
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
    use crate::game_structure::lcgs::intermediate::IntermediateLcgs;
    use crate::parsing::ast::DeclKind;
    use crate::parsing::errors::ErrorLog;
    use crate::parsing::parse_lcgs;

    #[test]
    fn simple_comparison_01() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = x < 4;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Less, lin_expr.comparison);
            assert_eq!(-4.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            assert_eq!(Some(&1.0), lin_expr.terms.get(&x_idx));
        }
    }

    #[test]
    fn simple_comparison_02() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 2 * x > 3;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Greater, lin_expr.comparison);
            assert_eq!(-3.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            assert_eq!(Some(&2.0), lin_expr.terms.get(&x_idx));
        }
    }

    #[test]
    fn simple_comparison_03() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 0 > x * 2;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Greater, lin_expr.comparison);
            assert_eq!(0.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            assert_eq!(Some(&-2.0), lin_expr.terms.get(&x_idx));
        }
    }

    #[test]
    fn simple_comparison_04() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 0 == 5 * x;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            assert_eq!(Some(&-5.0), lin_expr.terms.get(&x_idx));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(5.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            let y_idx = lcgs.get_decl_by_name("y").unwrap().index;
            assert_eq!(Some(&2.0), lin_expr.terms.get(&x_idx));
            assert_eq!(Some(&3.0), lin_expr.terms.get(&y_idx));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            let y_idx = lcgs.get_decl_by_name("y").unwrap().index;
            assert_eq!(Some(&2.0), lin_expr.terms.get(&x_idx));
            assert_eq!(Some(&-3.0), lin_expr.terms.get(&y_idx));
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
        label prop = 2 - x + y - z == 0;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(2.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            let y_idx = lcgs.get_decl_by_name("y").unwrap().index;
            let z_idx = lcgs.get_decl_by_name("z").unwrap().index;
            assert_eq!(Some(&-1.0), lin_expr.terms.get(&x_idx));
            assert_eq!(Some(&1.0), lin_expr.terms.get(&y_idx));
            assert_eq!(Some(&-1.0), lin_expr.terms.get(&z_idx));
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
        label prop = 4 <= 1 * x + 2 * y - 3 * z;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::LessOrEq, lin_expr.comparison);
            assert_eq!(4.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            let y_idx = lcgs.get_decl_by_name("y").unwrap().index;
            let z_idx = lcgs.get_decl_by_name("z").unwrap().index;
            assert_eq!(Some(&-1.0), lin_expr.terms.get(&x_idx));
            assert_eq!(Some(&-2.0), lin_expr.terms.get(&y_idx));
            assert_eq!(Some(&3.0), lin_expr.terms.get(&z_idx));
        }
    }

    #[test]
    fn normal_linear_constraint_05() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 2 * x + x == 0;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            assert_eq!(Some(&3.0), lin_expr.terms.get(&x_idx));
        }
    }

    #[test]
    fn normal_linear_constraint_06() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 1 + 2 * x - 3 == 5;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(-7.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            assert_eq!(Some(&2.0), lin_expr.terms.get(&x_idx));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            let y_idx = lcgs.get_decl_by_name("y").unwrap().index;
            assert_eq!(Some(&2.0), lin_expr.terms.get(&x_idx));
            assert_eq!(Some(&-1.0), lin_expr.terms.get(&y_idx));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(-5.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            let y_idx = lcgs.get_decl_by_name("y").unwrap().index;
            let z_idx = lcgs.get_decl_by_name("z").unwrap().index;
            assert_eq!(Some(&1.0), lin_expr.terms.get(&x_idx));
            assert_eq!(Some(&-1.0), lin_expr.terms.get(&y_idx));
            assert_eq!(Some(&1.0), lin_expr.terms.get(&z_idx));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            let y_idx = lcgs.get_decl_by_name("y").unwrap().index;
            let z_idx = lcgs.get_decl_by_name("z").unwrap().index;
            assert_eq!(Some(&1.0), lin_expr.terms.get(&x_idx));
            assert_eq!(Some(&-1.0), lin_expr.terms.get(&y_idx));
            assert_eq!(Some(&3.0), lin_expr.terms.get(&z_idx));
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
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            let y_idx = lcgs.get_decl_by_name("y").unwrap().index;
            assert_eq!(Some(&2.0), lin_expr.terms.get(&x_idx));
            assert_eq!(Some(&2.0), lin_expr.terms.get(&y_idx));
        }
    }

    #[test]
    fn other_linear_constraint_05() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = x + 2 * x + 3 * x + 4 * x == 0;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond).unwrap();
            assert_eq!(ComparisonOp::Equal, lin_expr.comparison);
            assert_eq!(0.0, lin_expr.constant);
            let x_idx = lcgs.get_decl_by_name("x").unwrap().index;
            assert_eq!(Some(&10.0), lin_expr.terms.get(&x_idx));
        }
    }

    #[test]
    fn non_linear_constraint_01() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = x * x == 0;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond);
            assert!(lin_expr.is_none());
        }
    }

    #[test]
    fn non_linear_constraint_02() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        label prop = x * 2 * y == 0;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond);
            assert!(lin_expr.is_none());
        }
    }

    #[test]
    fn non_linear_constraint_03() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 0 < x < 10; // not what you think anyway
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond);
            assert!(lin_expr.is_none());
        }
    }

    #[test]
    fn non_linear_constraint_04() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        label prop = 100 / x < 10;
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond);
            assert!(lin_expr.is_none());
        }
    }

    #[test]
    fn non_linear_constraint_05() {
        let input = "
        x : [0..0] init 0;
        x' = 0;
        y : [0..0] init 0;
        y' = 0;
        label prop = 2 * x + 4 * y < x + min(x, y);
        ";
        let errors = ErrorLog::new();
        let root = parse_lcgs(input, &errors).unwrap();
        let lcgs = IntermediateLcgs::create(root, &errors).unwrap();
        let decl = lcgs.get_decl_by_name("prop").unwrap();
        if let DeclKind::StateLabel(_, cond) = &decl.kind {
            let lin_expr = LinearConstraintExtractor::extract(&cond);
            assert!(lin_expr.is_none());
        }
    }
}
