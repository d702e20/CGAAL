use std::cmp::min;
use std::ops;

use priority_queue::PriorityQueue;

use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::atl::Phi;
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::Edge;
use crate::game_structure::lcgs::ast::{BinaryOpKind, DeclKind, Expr, ExprKind, UnaryOpKind};
use crate::game_structure::lcgs::ir::eval::Evaluator;
use crate::game_structure::lcgs::ir::intermediate::{IntermediateLcgs, State};
use crate::game_structure::Proposition;

/// A [BiTruthDist] is an abstract metric describing how close a boolean expression is
/// to changing truth-value of a constituent clause.
/// Hence, lower values indicate a less stable expression.
#[derive(Default, Debug, Copy, Clone)]
struct BiTruthDist {
    /** Distance to be true. None represents infinity */
    dtt: Option<i32>,
    /** Negative distance to be false. None represents -infinity */
    dff: Option<i32>,
}

impl BiTruthDist {
    #[allow(unused)]
    fn new(dtt: Option<i32>, dff: Option<i32>) -> Self {
        BiTruthDist { dtt, dff }
    }

    /** BiTruthDist of a tautology, something that is always true */
    fn tautology() -> Self {
        BiTruthDist {
            dtt: Some(0),
            dff: None,
        }
    }

    /** BiTruthDist of a contradiction, something that is always false */
    fn contradiction() -> Self {
        BiTruthDist {
            dtt: None,
            dff: Some(0),
        }
    }

    /** The smallest distance to a different truth value in this conjunction */
    fn conjunctive_dist(self) -> u32 {
        if !matches!(self.dtt, Some(0)) {
            // Distance to true is non-zero, so at least one clause is false,
            // and the conjunction is therefore false.
            // Therefore, we return the distance to true.
            self.dtt.map(|v| v.unsigned_abs()).unwrap_or(u32::MAX)
        } else {
            self.dff.map(|v| v.unsigned_abs()).unwrap_or(u32::MAX)
        }
    }

    /** The smallest distance to a different truth value in this disjunction */
    fn disjunctive_dist(self) -> u32 {
        if !matches!(self.dff, Some(0)) {
            // Distance to false is non-zero, so at least one clause is true,
            // and the disjunction is therefore true.
            // Therefore, we return the distance to false.
            self.dff.map(|v| v.unsigned_abs()).unwrap_or(u32::MAX)
        } else {
            self.dtt.map(|v| v.unsigned_abs()).unwrap_or(u32::MAX)
        }
    }
}

impl From<i32> for BiTruthDist {
    fn from(value: i32) -> Self {
        // Interpret negative values as distance to false
        if value < 0 {
            BiTruthDist {
                dtt: Some(0),
                dff: Some(value),
            }
        } else {
            BiTruthDist {
                dtt: Some(value),
                dff: Some(0),
            }
        }
    }
}

impl ops::BitAnd<BiTruthDist> for BiTruthDist {
    type Output = BiTruthDist;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn bitand(self, rhs: BiTruthDist) -> Self::Output {
        // Distance to true is sum of the operands' distance, since both of them must be true.
        // Distance to false is minimum of the operands' distance, since any of them must be false.
        BiTruthDist {
            dtt: match (self.dtt, rhs.dtt) {
                (Some(t1), Some(t2)) => Some(t1 + t2),
                _ => None,
            },
            dff: match (self.dff, rhs.dff) {
                (Some(dff1), Some(dff2)) => Some(min(dff1, dff2)),
                (dff1, None) => dff1,
                (None, dff2) => dff2,
            },
        }
    }
}

impl ops::BitOr<BiTruthDist> for BiTruthDist {
    type Output = BiTruthDist;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn bitor(self, rhs: BiTruthDist) -> Self::Output {
        // Distance to true is minimum of the operands' distance, since any of them must be true.
        // Distance to false is sum of the operands' distance, since both of them must be false.
        BiTruthDist {
            dtt: match (self.dtt, rhs.dtt) {
                (Some(dtt1), Some(dtt2)) => Some(min(dtt1, dtt2)),
                (dtt1, None) => dtt1,
                (None, dtt2) => dtt2,
            },
            dff: match (self.dff, rhs.dff) {
                (Some(dff1), Some(dff2)) => Some(dff1 + dff2),
                _ => None,
            },
        }
    }
}

impl ops::Not for BiTruthDist {
    type Output = BiTruthDist;

    fn not(self) -> Self::Output {
        // Flip distance to true and false
        BiTruthDist {
            dtt: self.dff.map(|v| -v),
            dff: self.dtt.map(|v| -v),
        }
    }
}

/// A [SearchStrategyBuilder] for the [InstabilityHeuristicSearch] strategy.
pub struct InstabilityHeuristicSearchBuilder {
    pub game: IntermediateLcgs,
}

impl SearchStrategyBuilder<AtlVertex, InstabilityHeuristicSearch>
    for InstabilityHeuristicSearchBuilder
{
    fn build(&self) -> InstabilityHeuristicSearch {
        InstabilityHeuristicSearch::new(self.game.clone())
    }
}

/// The [InstabilityHeuristicSearch] is a search strategy that uses a heuristic that estimates
/// how stable the constituent formulae of an edge are. That is, we prioritize edges where
/// one of the targets are close to being a different truth-value using the [BiTruthDist] metric.
/// To achieve this, it takes advantage of how LCGS represents states.
pub struct InstabilityHeuristicSearch {
    game: IntermediateLcgs,
    queue: PriorityQueue<Edge<AtlVertex>, u32>,
}

impl InstabilityHeuristicSearch {
    pub fn new(game: IntermediateLcgs) -> InstabilityHeuristicSearch {
        InstabilityHeuristicSearch {
            game,
            queue: PriorityQueue::default(),
        }
    }

    fn distance_of_edge(&self, edge: &Edge<AtlVertex>) -> u32 {
        match edge {
            // Use minimum vertex distance among targets
            Edge::Hyper(e) => e
                .targets
                .iter()
                .map(|target| self.bidist_of_vertex(target))
                .reduce(|rhs, lhs| rhs & lhs)
                .map(|bidist| bidist.conjunctive_dist())
                .unwrap_or(0),
            // Use vertex distance of target
            Edge::Negation(e) => self.bidist_of_vertex(&e.target).collapse_min(),
        }
    }

    fn bidist_of_vertex(&self, vertex: &AtlVertex) -> BiTruthDist {
        let state = self.game.state_from_index(vertex.state());
        self.bidist_of_state_phi(&state, &vertex.formula())
    }

    fn bidist_of_state_phi(&self, state: &State, phi: &Phi) -> BiTruthDist {
        match phi {
            Phi::True => BiTruthDist::tautology(),
            Phi::False => BiTruthDist::contradiction(),
            Phi::Proposition(prop) => self.bidist_of_proposition(state, prop),
            Phi::Not(sub) => !self.bidist_of_state_phi(state, sub),
            Phi::Or(rhs, lhs) => {
                self.bidist_of_state_phi(state, rhs) | self.bidist_of_state_phi(state, lhs)
            }
            Phi::And(rhs, lhs) => {
                self.bidist_of_state_phi(state, rhs) & self.bidist_of_state_phi(state, lhs)
            }
            Phi::DespiteNext { formula, .. } => self.bidist_of_state_phi(state, formula),
            Phi::EnforceNext { formula, .. } => self.bidist_of_state_phi(state, formula),
            Phi::DespiteUntil { pre, until, .. } => {
                self.bidist_of_state_phi(state, pre) | self.bidist_of_state_phi(state, until)
            }
            Phi::EnforceUntil { pre, until, .. } => {
                self.bidist_of_state_phi(state, pre) | self.bidist_of_state_phi(state, until)
            }
            Phi::DespiteEventually { formula, .. } => self.bidist_of_state_phi(state, formula),
            Phi::EnforceEventually { formula, .. } => self.bidist_of_state_phi(state, formula),
            Phi::DespiteInvariant { formula, .. } => self.bidist_of_state_phi(state, formula),
            Phi::EnforceInvariant { formula, .. } => self.bidist_of_state_phi(state, formula),
        }
    }

    fn bidist_of_proposition(&self, state: &State, prop: &Proposition) -> BiTruthDist {
        let decl = self.game.label_index_to_decl(*prop);
        if let DeclKind::Label(label) = &decl.kind {
            InstabilityHeuristicSearch::bidist_of_expr(state, &label.condition)
        } else {
            panic!("Non-propositions symbol in ATL formula")
        }
    }

    fn bidist_of_expr(state: &State, expr: &Expr) -> BiTruthDist {
        // We handle boolean operators using the operators defined on BiTruthDist.
        // For comparisons, the difference is evaluated and used as distance.
        // For the remaining expressions, we evaluate them and use their negative distance to 0 (false).
        match &expr.kind {
            // Boolean operators
            ExprKind::UnaryOp(UnaryOpKind::Not, e) => {
                !InstabilityHeuristicSearch::bidist_of_expr(state, e)
            }
            ExprKind::BinaryOp(BinaryOpKind::And, lhs, rhs) => {
                InstabilityHeuristicSearch::bidist_of_expr(state, lhs)
                    & InstabilityHeuristicSearch::bidist_of_expr(state, rhs)
            }
            ExprKind::BinaryOp(BinaryOpKind::Or, lhs, rhs) => {
                InstabilityHeuristicSearch::bidist_of_expr(state, lhs)
                    | InstabilityHeuristicSearch::bidist_of_expr(state, rhs)
            }
            ExprKind::BinaryOp(BinaryOpKind::Xor, lhs, rhs) => {
                (InstabilityHeuristicSearch::bidist_of_expr(state, lhs)
                    & !InstabilityHeuristicSearch::bidist_of_expr(state, rhs))
                    | (!InstabilityHeuristicSearch::bidist_of_expr(state, lhs)
                        & InstabilityHeuristicSearch::bidist_of_expr(state, rhs))
            }
            ExprKind::BinaryOp(BinaryOpKind::Implication, lhs, rhs) => {
                !InstabilityHeuristicSearch::bidist_of_expr(state, lhs)
                    | InstabilityHeuristicSearch::bidist_of_expr(state, rhs)
            }
            // Comparisons
            ExprKind::BinaryOp(BinaryOpKind::LessThan | BinaryOpKind::LessOrEqual, lhs, rhs) => {
                let evaluator = Evaluator::new(state);
                BiTruthDist::from(evaluator.eval(lhs) - evaluator.eval(rhs))
            }
            ExprKind::BinaryOp(
                BinaryOpKind::GreaterThan | BinaryOpKind::GreaterOrEqual,
                lhs,
                rhs,
            ) => {
                let evaluator = Evaluator::new(state);
                BiTruthDist::from(evaluator.eval(rhs) - evaluator.eval(lhs))
            }
            ExprKind::BinaryOp(BinaryOpKind::Equality, lhs, rhs) => {
                let evaluator = Evaluator::new(state);
                let abs_diff = (evaluator.eval(rhs) - evaluator.eval(lhs)).abs();
                if abs_diff == 0 {
                    BiTruthDist::from(-1) // One step from false
                } else {
                    BiTruthDist::from(abs_diff) // abs_diff from true
                }
            }
            ExprKind::BinaryOp(BinaryOpKind::Inequality, lhs, rhs) => {
                let evaluator = Evaluator::new(state);
                let abs_diff = (evaluator.eval(rhs) - evaluator.eval(lhs)).abs();
                if abs_diff == 0 {
                    BiTruthDist::from(10) // One step from true
                } else {
                    BiTruthDist::from(-abs_diff) // abs_diff from false
                }
            }
            // Others
            _ => {
                let abs_val = Evaluator::new(state).eval(expr).abs();
                if abs_val == 0 {
                    BiTruthDist::from(1) // One step from true (non-0)
                } else {
                    BiTruthDist::from(-abs_val) // abs_val from false (0)
                }
            }
        }
    }
}

impl SearchStrategy<AtlVertex> for InstabilityHeuristicSearch {
    /// Returns the edges with the highest priority (i.e lowest distance)
    fn next(&mut self) -> Option<Edge<AtlVertex>> {
        self.queue.pop().map(|entry| entry.0)
    }

    /// Queues a Vec of edges based on their estimate distance from being assigned true
    fn queue_new_edges(&mut self, edges: Vec<Edge<AtlVertex>>) {
        for edge in edges {
            let distance = self.distance_of_edge(&edge);
            self.queue.push(edge, u32::MAX - distance);
        }
    }
}
