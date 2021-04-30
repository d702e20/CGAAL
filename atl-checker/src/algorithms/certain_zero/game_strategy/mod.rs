use crate::algorithms::certain_zero::common::VertexAssignment;
use crate::algorithms::certain_zero::game_strategy::enforcing::compute_enforcing_strategy;
use crate::algorithms::certain_zero::game_strategy::error::Error;
use crate::edg::atledg::pmoves::PartialMove;
use crate::edg::atledg::vertex::ATLVertex;
use crate::edg::atledg::ATLDependencyGraph;
use crate::game_structure::{GameStructure, Player, State};
use std::collections::HashMap;

pub mod enforcing;
pub mod error;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum CertainZeroGameStrategy {
    Strategy(PartialStrategy),
    /// Indicates that the given formula can be proven without a strategy. I.e. the formula does
    /// not have any path qualifiers in it
    NoStrategyNeeded,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct PartialStrategy {
    /// The players for which this strategy applies
    players: Vec<Player>,
    /// A partial mapping from States to a partial move, where the given players have made
    /// a specific choice.
    move_to_pick: HashMap<State, PartialMove>,
}

pub fn compute_game_strategy<G: GameStructure>(
    graph: &ATLDependencyGraph<G>,
    v0: &ATLVertex,
    assignments: &HashMap<ATLVertex, VertexAssignment>,
) -> Result<CertainZeroGameStrategy, Error> {
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
        VertexAssignment::TRUE if formula.is_enforce() => Ok(CertainZeroGameStrategy::Strategy(
            compute_enforcing_strategy(graph, v0, assignments),
        )),
        VertexAssignment::TRUE if formula.is_despite() => unimplemented!(),
        VertexAssignment::FALSE if formula.is_enforce() => unimplemented!(),
        VertexAssignment::FALSE if formula.is_despite() => unimplemented!(),
        _ if formula.players().is_none() => Ok(CertainZeroGameStrategy::NoStrategyNeeded),
        _ => panic!("Assignment of v0 is undecided"),
    }
}
