use std::sync::Arc;
use serde::{Deserialize, Deserializer};
use std::any::{Any, TypeId};
use serde::de::{Visitor, SeqAccess, Error};
use serde::export::Formatter;

pub(crate) type Proposition = usize;
pub(crate) type Player = usize;
pub(crate) type State = usize;

#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
pub enum DynVec {
    NEST(Vec<Arc<DynVec>>),
    BASE(State),
}

pub(crate) fn transition_lookup(choices: &[usize], transitions: &DynVec) -> State {
    match transitions {
        DynVec::NEST(v) => {
            if choices.is_empty() {
                panic!("Fewer choices given than number of players in transitions");
            }

            println!("choice: {:?}", choices[0]);
            let choice = choices[0];
            let h: &DynVec = v.get(choice).expect("Out of bounds choice");

            transition_lookup(&choices[1..choices.len()], h)
        }
        DynVec::BASE(state) => *state,
    }
}
