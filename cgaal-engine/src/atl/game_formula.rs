use std::fmt::{Display, Formatter};

use joinery::prelude::*;

use crate::atl::Phi;
use crate::game_structure::GameStructure;

/// The GamePhi struct pairs an ATL formula with its game structure, allowing us to print
/// the formula using the names of players and labels that is defined by the game structure.
/// # Example
/// Given a Phi `formula` (`<<p1>> G p1.alive`) and a IntermediateLCGS `game_Structure`, you can
/// create a GamePhi with `formula.in_context_of(&game_structure)`. Using this in a print statement
/// like
/// ```ignore
/// println!("{}", formula.in_context_of(&game_structure))
/// ```
/// will print "`<<p1>> G p1.alive`" as opposed to "`<<0>> G 1`", which happens when you just write
/// ```ignore
/// println!("{}", formula)
/// ```
pub struct GamePhi<'a, G: GameStructure> {
    pub formula: &'a Phi,
    pub game: &'a G,
}

impl<'a, G: GameStructure> Display for GamePhi<'a, G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.formula {
            Phi::True => write!(f, "true"),
            Phi::False => write!(f, "false"),
            Phi::Proposition(id) => write!(f, "{}", self.game.label_name(*id)),
            Phi::Not(formula) => write!(f, "!({})", formula.in_context_of(self.game)),
            Phi::Or(left, right) => write!(
                f,
                "({} | {})",
                left.in_context_of(self.game),
                right.in_context_of(self.game)
            ),
            Phi::And(left, right) => write!(
                f,
                "({} & {})",
                left.in_context_of(self.game),
                right.in_context_of(self.game)
            ),
            Phi::DespiteNext { players, formula } => write!(
                f,
                "[[{}]] X {}",
                players
                    .iter()
                    .map(|i| self.game.player_name(*i))
                    .join_with(","),
                formula.in_context_of(self.game)
            ),
            Phi::EnforceNext { players, formula } => write!(
                f,
                "<<{}>> X {}",
                players
                    .iter()
                    .map(|i| self.game.player_name(*i))
                    .join_with(","),
                formula.in_context_of(self.game)
            ),
            Phi::DespiteUntil {
                players,
                pre,
                until,
            } => write!(
                f,
                "[[{}]] ({} U {})",
                players
                    .iter()
                    .map(|i| self.game.player_name(*i))
                    .join_with(","),
                pre.in_context_of(self.game),
                until.in_context_of(self.game)
            ),
            Phi::EnforceUntil {
                players,
                pre,
                until,
            } => write!(
                f,
                "<<{}>> ({} U {})",
                players
                    .iter()
                    .map(|i| self.game.player_name(*i))
                    .join_with(","),
                pre.in_context_of(self.game),
                until.in_context_of(self.game)
            ),
            Phi::DespiteEventually { players, formula } => write!(
                f,
                "[[{}]] F {}",
                players
                    .iter()
                    .map(|i| self.game.player_name(*i))
                    .join_with(","),
                formula.in_context_of(self.game)
            ),
            Phi::EnforceEventually { players, formula } => write!(
                f,
                "<<{}>> F {}",
                players
                    .iter()
                    .map(|i| self.game.player_name(*i))
                    .join_with(","),
                formula.in_context_of(self.game)
            ),
            Phi::DespiteInvariant { players, formula } => write!(
                f,
                "[[{}]] G {}",
                players
                    .iter()
                    .map(|i| self.game.player_name(*i))
                    .join_with(","),
                formula.in_context_of(self.game)
            ),
            Phi::EnforceInvariant { players, formula } => write!(
                f,
                "<<{}>> G {}",
                players
                    .iter()
                    .map(|i| self.game.player_name(*i))
                    .join_with(","),
                formula.in_context_of(self.game)
            ),
        }
    }
}
