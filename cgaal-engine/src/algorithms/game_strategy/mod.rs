use crate::algorithms::certain_zero::common::VertexAssignment;
use crate::algorithms::certain_zero::search_strategy::{SearchStrategy, SearchStrategyBuilder};
use crate::algorithms::certain_zero::{distributed_certain_zero, CertainZeroResult};
use crate::algorithms::game_strategy::error::Error;
use crate::algorithms::game_strategy::partial::{
    find_strategy_moves_false_case,
    find_strategy_moves_true_case,
    find_strategy_moves_undecided_case, PartialStrategy,
};
use crate::edg::atledg::vertex::AtlVertex;
use crate::edg::atledg::AtlDependencyGraph;
use crate::game_structure::GameStructure;
use std::collections::HashMap;
use std::fmt::Debug;

pub mod error;
pub mod format;
pub mod partial;

#[derive(Debug)]
pub struct ModelCheckResult {
    pub satisfied: bool,
    pub proof: Option<Result<WitnessStrategy, Error>>,
}

/// Check game model starting from the given AtlVertex. Optionally returns a proof.
pub fn model_check<
    G: GameStructure + Clone + Debug + Send + Sync + 'static,
    S: SearchStrategy<AtlVertex> + Send + 'static,
    SB: SearchStrategyBuilder<AtlVertex, S>,
>(
    edg: AtlDependencyGraph<G>,
    v0: AtlVertex,
    worker_count: u64,
    ss_builder: SB,
    prioritise_back_propagation: bool,
    find_proof: bool,
) -> ModelCheckResult {
    let czr = distributed_certain_zero(
        edg.clone(),
        v0.clone(),
        worker_count,
        ss_builder,
        prioritise_back_propagation,
        find_proof,
    );

    match czr {
        CertainZeroResult::RootAssignment(assignment) => ModelCheckResult {
            satisfied: assignment.is_true(),
            proof: None,
        },
        CertainZeroResult::AllFoundAssignments(all_ass) => ModelCheckResult {
            satisfied: all_ass.get(&v0).unwrap().is_true(),
            proof: Some(compute_game_strategy(&edg, &v0, &all_ass)),
        },
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum WitnessStrategy {
    Strategy(PartialStrategy),
    NoStrategyExist,
    /// Indicates that the given formula can be proven without a strategy. I.e. the formula does
    /// not have any path qualifiers in it
    NoStrategyNeeded,
}

pub fn compute_game_strategy<G: GameStructure>(
    graph: &AtlDependencyGraph<G>,
    v0: &AtlVertex,
    assignments: &HashMap<AtlVertex, VertexAssignment>,
) -> Result<WitnessStrategy, Error> {
    // There are six cases:
    // - True enforce formula => a strategy is exists for the given players
    // - False enforce formula => a strategy does not exists for the given players
    // - True despite formula => a strategy does not exists for the given players
    // - False despite formula => a strategy is exists for the given players
    // - Formula with no path qualifiers => No need for a strategy
    // - Formula has nested path qualifiers => Unsupported

    let res = assignments.get(v0).unwrap();
    let formula = v0.formula();

    if formula.has_nested_path_qualifiers() {
        // Unsupported
        return Err(Error::UnsupportedFormula);
    }

    let move_to_pick = match res {
        VertexAssignment::True if formula.is_enforce() => {
            find_strategy_moves_true_case(graph, v0, assignments)
        }
        VertexAssignment::True if formula.is_despite() => {
            return Ok(WitnessStrategy::NoStrategyExist)
        },
        VertexAssignment::False if formula.is_enforce() => {
            return Ok(WitnessStrategy::NoStrategyExist)
        },
        VertexAssignment::False if formula.is_despite() => {
            find_strategy_moves_false_case(graph, v0, assignments)
        }
        VertexAssignment::Undecided if formula.is_despite() => {
            find_strategy_moves_undecided_case(graph, v0, assignments)
        }
        _ if !(formula.is_despite() || !formula.is_enforce()) => {
            return Ok(WitnessStrategy::NoStrategyNeeded)
        }
        _ => unreachable!(),
    };

    Ok(WitnessStrategy::Strategy(PartialStrategy {
        players: formula.players().unwrap().iter().copied().collect(),
        move_to_pick,
    }))
}
