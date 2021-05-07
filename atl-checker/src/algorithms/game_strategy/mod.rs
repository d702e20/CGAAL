use crate::algorithms::certain_zero::common::VertexAssignment;
use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::algorithms::certain_zero::{distributed_certain_zero, CertainZeroResult};
use crate::algorithms::game_strategy::error::Error;
use crate::algorithms::game_strategy::format::PartialStrategyWithFormatting;
use crate::algorithms::game_strategy::partial::{compute_partial_strategy, PartialStrategy};
use crate::edg::atledg::pmoves::PartialMove;
use crate::edg::atledg::vertex::ATLVertex;
use crate::edg::atledg::ATLDependencyGraph;
use crate::game_structure::{GameStructure, Player, State};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

pub mod error;
pub mod format;
pub mod partial;

#[derive(Debug)]
pub struct ModelCheckResult {
    pub satisfied: bool,
    pub proof: Option<Result<SpecificationProof, Error>>,
}

/// Check game model starting from the given ATLVertex. Optionally returns a proof.
pub fn model_check<
    G: GameStructure + Clone + Debug + Send + Sync + 'static,
    S: SearchStrategy<ATLVertex> + Send + 'static,
    SB: SearchStrategyBuilder<ATLVertex, S>,
>(
    edg: ATLDependencyGraph<G>,
    v0: ATLVertex,
    worker_count: u64,
    ss_builder: SB,
    find_proof: bool,
) -> ModelCheckResult {
    let czr = distributed_certain_zero(
        edg.clone(),
        v0.clone(),
        worker_count,
        ss_builder,
        find_proof,
    );

    match czr {
        CertainZeroResult::RootAssignment(assignment) => ModelCheckResult {
            satisfied: assignment.to_bool().unwrap(),
            proof: None,
        },
        CertainZeroResult::AllFoundAssignments(all_ass) => ModelCheckResult {
            satisfied: all_ass.get(&v0).unwrap().to_bool().unwrap(),
            proof: Some(compute_game_strategy(&edg, &v0, &all_ass)),
        },
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum SpecificationProof {
    Strategy(PartialStrategy),
    /// Indicates that the given formula can be proven without a strategy. I.e. the formula does
    /// not have any path qualifiers in it
    NoStrategyNeeded,
}

pub fn compute_game_strategy<G: GameStructure>(
    graph: &ATLDependencyGraph<G>,
    v0: &ATLVertex,
    assignments: &HashMap<ATLVertex, VertexAssignment>,
) -> Result<SpecificationProof, Error> {
    // There are six cases:
    // - True enforce formula => a strategy is exists for the given players
    // - False enforce formula => a strategy does not exists for the given players
    // - True despite formula => a strategy does not exists for the given players
    // - False despite formula => a strategy is exists for the given players
    // - Formula with no path qualifiers => No need for a strategy
    // - Formula has nested path qualifiers => Unsupported

    let res = assignments.get(v0).expect("Assignment of v0 is undecided");
    let formula = v0.formula();

    if formula.has_nested_path_qualifiers() {
        // Unsupported
        return Err(Error::UnsupportedFormula);
    }

    match res {
        VertexAssignment::TRUE if formula.is_enforce() => Ok(SpecificationProof::Strategy(
            compute_partial_strategy(graph, v0, assignments),
        )),
        VertexAssignment::TRUE if formula.is_despite() => unimplemented!(),
        VertexAssignment::FALSE if formula.is_enforce() => unimplemented!(),
        VertexAssignment::FALSE if formula.is_despite() => Ok(SpecificationProof::Strategy(
            compute_partial_strategy(graph, v0, assignments),
        )),
        _ if formula.players().is_none() => Ok(SpecificationProof::NoStrategyNeeded),
        _ => panic!("Assignment of v0 is undecided"),
    }
}
