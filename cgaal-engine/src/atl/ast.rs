use std::sync::Arc;
use crate::atl::Phi;
use crate::game_structure::{Player, Proposition};
use crate::parsing::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstAtl {
    pub span: Span,
    pub kind: AstAtlKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstAtlKind {
    True,
    False,
    Proposition(Proposition),
    Not(Arc<AstAtl>),
    And(Arc<AstAtl>, Arc<AstAtl>),
    Or(Arc<AstAtl>, Arc<AstAtl>),
    DespiteNext { players: Vec<Player>, expr: Arc<AstAtl> },
    EnforceNext { players: Vec<Player>, expr: Arc<AstAtl> },
    DespiteUntil { players: Vec<Player>, pre_expr: Arc<AstAtl>, end_expr: Arc<AstAtl> },
    EnforceUntil { players: Vec<Player>, pre_expr: Arc<AstAtl>, end_expr: Arc<AstAtl> },
    DespiteInvariantly { players: Vec<Player>, expr: Arc<AstAtl> },
    EnforceInvariantly { players: Vec<Player>, expr: Arc<AstAtl> },
    DespiteEventually { players: Vec<Player>, expr: Arc<AstAtl> },
    EnforceEventually { players: Vec<Player>, expr: Arc<AstAtl> },
    Error,
}

impl AstAtl {
    pub fn new(span: Span, kind: AstAtlKind) -> Self {
        AstAtl { span, kind }
    }

    pub fn convert(&self) -> Phi {
        match &self.kind {
            AstAtlKind::True => Phi::True,
            AstAtlKind::False => Phi::False,
            AstAtlKind::Proposition(p) => Phi::Proposition(*p),
            AstAtlKind::Not(expr) => Phi::Not(Arc::new(expr.convert())),
            AstAtlKind::And(lhs, rhs) => Phi::And(Arc::new(lhs.convert()), Arc::new(rhs.convert())),
            AstAtlKind::Or(lhs, rhs) => Phi::Or(Arc::new(lhs.convert()), Arc::new(rhs.convert())),
            AstAtlKind::DespiteNext { players, expr } => Phi::DespiteNext { players: players.clone(), formula: Arc::new(expr.convert()) },
            AstAtlKind::EnforceNext { players, expr } => Phi::EnforceNext { players: players.clone(), formula: Arc::new(expr.convert()) },
            AstAtlKind::DespiteUntil { players, pre_expr, end_expr } => Phi::DespiteUntil { players: players.clone(), pre: Arc::new(pre_expr.convert()), until: Arc::new(end_expr.convert()) },
            AstAtlKind::EnforceUntil { players, pre_expr, end_expr } => Phi::EnforceUntil { players: players.clone(), pre: Arc::new(pre_expr.convert()), until: Arc::new(end_expr.convert()) },
            AstAtlKind::DespiteInvariantly { players, expr } => Phi::DespiteInvariant { players: players.clone(), formula: Arc::new(expr.convert()) },
            AstAtlKind::EnforceInvariantly { players, expr } => Phi::EnforceInvariant { players: players.clone(), formula: Arc::new(expr.convert()) },
            AstAtlKind::DespiteEventually { players, expr } => Phi::DespiteEventually { players: players.clone(), formula: Arc::new(expr.convert()) },
            AstAtlKind::EnforceEventually { players, expr } => Phi::EnforceEventually { players: players.clone(), formula: Arc::new(expr.convert()) },
            AstAtlKind::Error => panic!("Error in AST"),
        }
    }
}