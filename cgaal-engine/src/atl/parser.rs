use std::str::{self, FromStr};
use std::sync::Arc;

use pom::parser::{end, Parser};
use pom::parser::{list, one_of, seq, sym};

use super::Phi;
use crate::game_structure::{Player, Proposition};
use crate::parsing::{call2, ParseState};

/// Parse an ATL formula
pub fn parse_phi<'a, 'b: 'a, A: AtlExpressionParser>(
    expr_parser: &'b A,
    input: &'a str,
) -> Result<Phi, String> {
    let state = ParseState::default();
    let formula = ws() * conjunction(&state, expr_parser) - ws() - end();
    let phi = formula
        .parse(input.as_bytes())
        .expect("Parser may not fail");
    if state.has_errors() {
        Err(state.errors_as_str(input))
    } else {
        Ok(phi)
    }
}

/// Allows a CGS model to define custom player and proposition expressions. For instance,
/// in LCGS we want to be able to write "p2" as a player and "p2.alive" as a proposition, while
/// in json, players and propositions are numbers.
pub trait AtlExpressionParser {
    /// A parser that parses a player name
    fn player_parser(&self, state: &ParseState) -> Parser<u8, Player>;
    /// A parser that parses a proposition name
    fn proposition_parser(&self, state: &ParseState) -> Parser<u8, Proposition>;
}

/// Whitespace
fn ws<'a>() -> Parser<'a, u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

/// Parses an ATL formula (without whitespace around it)
pub(crate) fn conjunction<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    let conjunc = (disjunction(state, expr_parser) - ws() - sym(b'&') - ws()
        + call2(&conjunction, state, expr_parser))
    .map(|(lhs, rhs)| Phi::And(Arc::new(lhs), Arc::new(rhs)));
    conjunc | disjunction(state, expr_parser)
}

/// Parses an ATL term (Can't contain AND and without whitespace around it)
fn disjunction<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    let disjunc = (primary(state, expr_parser) - ws() - sym(b'|') - ws()
        + call2(&disjunction, state, expr_parser))
    .map(|(lhs, rhs)| Phi::Or(Arc::new(lhs), Arc::new(rhs)));
    disjunc | primary(state, expr_parser)
}

/// Parses a primary ATL formula (no ANDs or ORs)
fn primary<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    paren(state, expr_parser)
        | boolean()
        | proposition(state, expr_parser)
        | not(state, expr_parser)
        | enforce_next(state, expr_parser)
        | enforce_until(state, expr_parser)
        | enforce_eventually(state, expr_parser)
        | enforce_invariant(state, expr_parser)
        | despite_next(state, expr_parser)
        | despite_until(state, expr_parser)
        | despite_eventually(state, expr_parser)
        | despite_invariant(state, expr_parser)
}

/// Parses an ATL formula in parenthesis
fn paren<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    sym(b'(') * ws() * call2(&conjunction, state, expr_parser) - ws() - sym(b')')
}

/// Parses an enforce-coalition (path qualifier)
fn enforce_coalition<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Vec<usize>> {
    seq(b"<<") * ws() * players(state, expr_parser) - ws() - seq(b">>")
}

/// Parses a despite-coalition (path qualifier)
fn despite_coalition<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Vec<usize>> {
    seq(b"[[") * ws() * players(state, expr_parser) - ws() - seq(b"]]")
}

/// Parses an path formula starting with the NEXT (X) operator
fn next<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    sym(b'X') * ws() * call2(&conjunction, state, expr_parser)
}

/// Parses an path formula with the UNTIL operator
fn until<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, (Phi, Phi)> {
    sym(b'(') * ws() * call2(&conjunction, state, expr_parser) - ws() - sym(b'U') - ws()
        + call2(&conjunction, state, expr_parser)
        - ws()
        - sym(b')')
}

/// Parses an path formula starting with the EVENTUALLY (F/finally) operator
fn eventually<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    sym(b'F') * ws() * call2(&conjunction, state, expr_parser)
}

/// Parses an path formula starting with the INVARIANT (G/global) operator
fn invariant<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    sym(b'G') * ws() * call2(&conjunction, state, expr_parser)
}

/// Parses an ENFORCE-NEXT ATL formula
fn enforce_next<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (enforce_coalition(state, expr_parser) - ws() + next(state, expr_parser)).map(|(players, phi)| {
        Phi::EnforceNext {
            players,
            formula: Arc::new(phi),
        }
    })
}

/// Parses an ENFORCE-UNTIL ATL formula
fn enforce_until<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (enforce_coalition(state, expr_parser) - ws() + until(state, expr_parser)).map(
        |(players, (l, r))| Phi::EnforceUntil {
            players,
            pre: Arc::new(l),
            until: Arc::new(r),
        },
    )
}

/// Parses an ENFORCE-EVENTUALLY ATL formula
fn enforce_eventually<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (enforce_coalition(state, expr_parser) - ws() + eventually(state, expr_parser)).map(
        |(players, phi)| Phi::EnforceEventually {
            players,
            formula: Arc::new(phi),
        },
    )
}

/// Parses an ENFORCE-INVARIANT ATL formula
fn enforce_invariant<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (enforce_coalition(state, expr_parser) - ws() + invariant(state, expr_parser)).map(
        |(players, phi)| Phi::EnforceInvariant {
            players,
            formula: Arc::new(phi),
        },
    )
}

/// Parses an DESPITE-NEXT ATL formula
fn despite_next<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (despite_coalition(state, expr_parser) - ws() + next(state, expr_parser)).map(|(players, phi)| {
        Phi::DespiteNext {
            players,
            formula: Arc::new(phi),
        }
    })
}

/// Parses an DESPITE-UNTIL ATL formula
fn despite_until<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (despite_coalition(state, expr_parser) - ws() + until(state, expr_parser)).map(
        |(players, (l, r))| Phi::DespiteUntil {
            players,
            pre: Arc::new(l),
            until: Arc::new(r),
        },
    )
}

/// Parses an DESPITE-EVENTUALLY ATL formula
fn despite_eventually<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (despite_coalition(state, expr_parser) - ws() + eventually(state, expr_parser)).map(
        |(players, phi)| Phi::DespiteEventually {
            players,
            formula: Arc::new(phi),
        },
    )
}

/// Parses an DESPITE-INVARIANT ATL formula
fn despite_invariant<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (despite_coalition(state, expr_parser) - ws() + invariant(state, expr_parser)).map(
        |(players, phi)| Phi::DespiteInvariant {
            players,
            formula: Arc::new(phi),
        },
    )
}

/// Parses a proposition using the given [ATLExpressionParser].
fn proposition<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    expr_parser.proposition_parser(state).map(Phi::Proposition)
}

/// Parses a negated ATL formula
fn not<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    (sym(b'!') * ws() * call2(&primary, state, expr_parser)).map(|phi| Phi::Not(Arc::new(phi)))
}

/// Parses a boolean, either full uppercase or full lowercase
fn boolean<'a>() -> Parser<'a, u8, Phi> {
    seq(b"true").map(|_| Phi::True)
        | seq(b"TRUE").map(|_| Phi::True)
        | seq(b"false").map(|_| Phi::False)
        | seq(b"FALSE").map(|_| Phi::False)
}

/// Parses a comma-separated list of players using the given [ATLExpressionParser].
fn players<'a, A: AtlExpressionParser>(
    state: &'a ParseState,
    expr_parser: &'a A,
) -> Parser<'a, u8, Vec<Player>> {
    list(expr_parser.player_parser(state), ws() * sym(b',') * ws())
}

/// Parses a letter
fn alpha<'a>() -> Parser<'a, u8, u8> {
    one_of(b"abcdefghijklmnopqrstuvwxyz") | one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

/// Parses an identifier, starting with a letter but otherwise consisting of letters, numbers,
/// and underscores
pub fn identifier<'a>() -> Parser<'a, u8, String> {
    let chars = alpha() - (alpha() | digit() | sym(b'_')).repeat(0..);
    chars.collect().convert(|s| String::from_utf8(s.to_vec()))
}

/// Parser that parses a single digit 0-9
#[inline]
pub fn digit<'a>() -> Parser<'a, u8, u8> {
    one_of(b"0123456789")
}

/// Parser that parses a single digit 1-9 (not 0)
#[inline]
pub fn non_0_digit<'a>() -> Parser<'a, u8, u8> {
    one_of(b"123456789")
}

/// Parser that parses a typical positive integer number
pub fn number<'a>() -> Parser<'a, u8, usize> {
    let integer = (non_0_digit() - digit().repeat(0..)) | sym(b'0');
    integer
        .collect()
        .convert(str::from_utf8)
        .convert(usize::from_str)
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use pom::parser::Parser;

    use crate::atl::parser::{
        boolean, conjunction, despite_eventually, despite_invariant, despite_next, despite_coalition,
        despite_until, disjunction, enforce_eventually, enforce_invariant, enforce_next,
        enforce_coalition, enforce_until, eventually, invariant, next, not, number, paren,
        proposition, until, AtlExpressionParser,
    };
    use crate::atl::{parse_phi, Phi};
    use crate::game_structure::lcgs::ir::intermediate::IntermediateLcgs;
    use crate::game_structure::lcgs::parse::parse_lcgs;
    use crate::game_structure::{Player, Proposition};
    use crate::parsing::ParseState;

    struct TestModel;

    impl AtlExpressionParser for TestModel {
        fn player_parser(&self, state: &ParseState) -> Parser<u8, Player> {
            number()
        }

        fn proposition_parser(&self, state: &ParseState) -> Parser<u8, Proposition> {
            number()
        }
    }

    #[test]
    fn paren_1() {
        let state = ParseState::default();
        assert!(!state.has_errors());
        assert_eq!(paren(&state, &TestModel).parse(b"(true)"), Ok(Phi::True));
    }

    /// Test for a single player
    #[test]
    fn enforce_players_1() {
        let state = ParseState::default();
        assert_eq!(
            enforce_coalition(&state, &TestModel).parse(b"<<1>>"),
            Ok(vec![1usize])
        );
        assert!(!state.has_errors());
    }

    /// Test for two players
    #[test]
    fn enforce_players_2() {
        let state = ParseState::default();
        assert_eq!(
            enforce_coalition(&state, &TestModel).parse(b"<<4,9>>"),
            Ok(vec![4usize, 9usize])
        );
        assert!(!state.has_errors());
    }

    /// Test for three players
    #[test]
    fn enforce_players_3() {
        let state = ParseState::default();
        assert_eq!(
            enforce_coalition(&state, &TestModel).parse(b"<<203,23,4>>"),
            Ok(vec![203usize, 23usize, 4usize])
        );
        assert!(!state.has_errors());
    }

    /// The list of players is allowed to have whitespace after the separator
    #[test]
    fn enforce_players_4() {
        let state = ParseState::default();
        assert_eq!(
            enforce_coalition(&state, &TestModel).parse(b"<<203,  23>>"),
            Ok(vec![203usize, 23usize])
        );
        assert!(!state.has_errors());
    }

    /// Test for no players, should be valid
    #[test]
    fn enforce_players_5() {
        let state = ParseState::default();
        assert_eq!(
            enforce_coalition(&state, &TestModel).parse(b"<<>>"),
            Ok(vec![])
        );
        assert!(!state.has_errors());
    }

    /// Test for a single player
    #[test]
    fn despite_players_1() {
        let state = ParseState::default();
        assert_eq!(
            despite_coalition(&state, &TestModel).parse(b"[[1]]"),
            Ok(vec![1usize])
        );
        assert!(!state.has_errors());
    }

    /// Test for two players
    #[test]
    fn despite_players_2() {
        let state = ParseState::default();
        assert_eq!(
            despite_coalition(&state, &TestModel).parse(b"[[4,9]]"),
            Ok(vec![4usize, 9usize])
        );
        assert!(!state.has_errors());
    }

    /// Test for three players
    #[test]
    fn despite_players_3() {
        let state = ParseState::default();
        assert_eq!(
            despite_coalition(&state, &TestModel).parse(b"[[203,23,4]]"),
            Ok(vec![203usize, 23usize, 4usize])
        );
        assert!(!state.has_errors());
    }

    /// The list of players is allowed to have whitespace after the seperator
    #[test]
    fn despite_players_4() {
        let state = ParseState::default();
        assert_eq!(
            despite_coalition(&state, &TestModel).parse(b"[[203,  23]]"),
            Ok(vec![203usize, 23usize])
        );
        assert!(!state.has_errors());
    }

    /// Test for no players, should be valid
    #[test]
    fn despite_players_5() {
        let state = ParseState::default();
        assert_eq!(
            despite_coalition(&state, &TestModel).parse(b"[[]]"),
            Ok(vec![])
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn next_1() {
        let state = ParseState::default();
        assert_eq!(next(&state, &TestModel).parse(b"X true"), Ok(Phi::True));
        assert!(!state.has_errors());
    }

    #[test]
    fn next_2() {
        let state = ParseState::default();
        assert_eq!(next(&state, &TestModel).parse(b"Xtrue"), Ok(Phi::True));
        assert!(!state.has_errors());
    }

    #[test]
    fn until_1() {
        let state = ParseState::default();
        assert_eq!(
            until(&state, &TestModel).parse(b"( true U true )"),
            Ok((Phi::True, Phi::True))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn until_2() {
        let state = ParseState::default();
        assert_eq!(
            until(&state, &TestModel).parse(b"(trueUtrue)"),
            Ok((Phi::True, Phi::True))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn eventually_1() {
        let state = ParseState::default();
        assert_eq!(
            eventually(&state, &TestModel).parse(b"F true"),
            Ok(Phi::True)
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn eventually_2() {
        let state = ParseState::default();
        assert_eq!(
            eventually(&state, &TestModel).parse(b"Ftrue"),
            Ok(Phi::True)
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn invariant_1() {
        let state = ParseState::default();
        assert_eq!(
            invariant(&state, &TestModel).parse(b"G true"),
            Ok(Phi::True)
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn invariant_2() {
        let state = ParseState::default();
        assert_eq!(invariant(&state, &TestModel).parse(b"Gtrue"), Ok(Phi::True));
        assert!(!state.has_errors());
    }

    #[test]
    fn enforce_next_1() {
        let state = ParseState::default();
        assert_eq!(
            enforce_next(&state, &TestModel).parse(b"<<1 , 2>> X true"),
            Ok(Phi::EnforceNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn enforce_next_2() {
        let state = ParseState::default();
        assert_eq!(
            enforce_next(&state, &TestModel).parse(b"<<1,2>>Xtrue"),
            Ok(Phi::EnforceNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn enforce_until_1() {
        let state = ParseState::default();
        assert_eq!(
            enforce_until(&state, &TestModel).parse(b"<<1 , 2>> ( true U true )"),
            Ok(Phi::EnforceUntil {
                players: vec![1, 2],
                pre: Arc::new(Phi::True),
                until: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn enforce_until_2() {
        let state = ParseState::default();
        assert_eq!(
            enforce_until(&state, &TestModel).parse(b"<<1,2>>(trueUtrue)"),
            Ok(Phi::EnforceUntil {
                players: vec![1, 2],
                pre: Arc::new(Phi::True),
                until: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn enforce_eventually_1() {
        let state = ParseState::default();
        assert_eq!(
            enforce_eventually(&state, &TestModel).parse(b"<<1 , 2>> F true"),
            Ok(Phi::EnforceEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn enforce_eventually_2() {
        let state = ParseState::default();
        assert_eq!(
            enforce_eventually(&state, &TestModel).parse(b"<<1,2>>Ftrue"),
            Ok(Phi::EnforceEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn enforce_invariant_1() {
        let state = ParseState::default();
        assert_eq!(
            enforce_invariant(&state, &TestModel).parse(b"<<1 , 2>> G true"),
            Ok(Phi::EnforceInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn enforce_invariant_2() {
        let state = ParseState::default();
        assert_eq!(
            enforce_invariant(&state, &TestModel).parse(b"<<1,2>>Gtrue"),
            Ok(Phi::EnforceInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn despite_next_1() {
        let state = ParseState::default();
        assert_eq!(
            despite_next(&state, &TestModel).parse(b"[[1 , 2]] X true"),
            Ok(Phi::DespiteNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn despite_next_2() {
        let state = ParseState::default();
        assert_eq!(
            despite_next(&state, &TestModel).parse(b"[[1,2]]Xtrue"),
            Ok(Phi::DespiteNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn despite_until_1() {
        let state = ParseState::default();
        assert_eq!(
            despite_until(&state, &TestModel).parse(b"[[1 , 2]] ( true U true )"),
            Ok(Phi::DespiteUntil {
                players: vec![1, 2],
                pre: Arc::new(Phi::True),
                until: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn despite_until_2() {
        let state = ParseState::default();
        assert_eq!(
            despite_until(&state, &TestModel).parse(b"[[1,2]](trueUtrue)"),
            Ok(Phi::DespiteUntil {
                players: vec![1, 2],
                pre: Arc::new(Phi::True),
                until: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn despite_eventually_1() {
        let state = ParseState::default();
        assert_eq!(
            despite_eventually(&state, &TestModel).parse(b"[[1 , 2]] F true"),
            Ok(Phi::DespiteEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn despite_eventually_2() {
        let state = ParseState::default();
        assert_eq!(
            despite_eventually(&state, &TestModel).parse(b"[[1,2]]Ftrue"),
            Ok(Phi::DespiteEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn despite_invariant_1() {
        let state = ParseState::default();
        assert_eq!(
            despite_invariant(&state, &TestModel).parse(b"[[1 , 2]] G true"),
            Ok(Phi::DespiteInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn despite_invariant_2() {
        let state = ParseState::default();
        assert_eq!(
            despite_invariant(&state, &TestModel).parse(b"[[1,2]]Gtrue"),
            Ok(Phi::DespiteInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn proposition_1() {
        let state = ParseState::default();
        assert_eq!(
            proposition(&state, &TestModel).parse(b"1"),
            Ok(Phi::Proposition(1usize))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn proposition_2() {
        let state = ParseState::default();
        assert_eq!(
            proposition(&state, &TestModel).parse(b"1432"),
            Ok(Phi::Proposition(1432usize))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn proposition_3() {
        let state = ParseState::default();
        assert!(proposition(&state, &TestModel).parse(b"abc").is_err());
        assert!(!state.has_errors());
    }

    #[test]
    fn not_1() {
        let state = ParseState::default();
        assert_eq!(
            not(&state, &TestModel).parse(b"! true"),
            Ok(Phi::Not(Arc::new(Phi::True)))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn not_2() {
        let state = ParseState::default();
        assert_eq!(
            not(&state, &TestModel).parse(b"!true"),
            Ok(Phi::Not(Arc::new(Phi::True)))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn and_1() {
        let state = ParseState::default();
        assert_eq!(
            conjunction(&state, &TestModel).parse(b"true & false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn and_2() {
        let state = ParseState::default();
        assert_eq!(
            conjunction(&state, &TestModel).parse(b"true&false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn and_3() {
        // Right recursive?
        let state = ParseState::default();
        assert_eq!(
            conjunction(&state, &TestModel).parse(b"true&false&true"),
            Ok(Phi::And(
                Arc::new(Phi::True),
                Arc::new(Phi::And(Arc::new(Phi::False), Arc::new(Phi::True)))
            ))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn or_1() {
        let state = ParseState::default();
        assert_eq!(
            disjunction(&state, &TestModel).parse(b"true | false"),
            Ok(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn or_2() {
        let state = ParseState::default();
        assert_eq!(
            disjunction(&state, &TestModel).parse(b"true|false"),
            Ok(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn or_3() {
        // Right recursive?
        let state = ParseState::default();
        assert_eq!(
            disjunction(&state, &TestModel).parse(b"true|false |true"),
            Ok(Phi::Or(
                Arc::new(Phi::True),
                Arc::new(Phi::Or(Arc::new(Phi::False), Arc::new(Phi::True)))
            ))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn test_and_or_precedence_01() {
        let state = ParseState::default();
        assert_eq!(
            conjunction(&state, &TestModel).parse(b"true & false | true | false & true"),
            Ok(Phi::And(
                Arc::new(Phi::True),
                Arc::new(Phi::And(
                    Arc::new(Phi::Or(
                        Arc::new(Phi::False),
                        Arc::new(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
                    )),
                    Arc::new(Phi::True)
                ))
            ))
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn bool_true_upper() {
        assert_eq!(boolean().parse(b"TRUE"), Ok(Phi::True))
    }

    #[test]
    fn bool_true_lower() {
        assert_eq!(boolean().parse(b"true"), Ok(Phi::True))
    }

    #[test]
    fn bool_false_upper() {
        assert_eq!(boolean().parse(b"FALSE"), Ok(Phi::False))
    }

    #[test]
    fn bool_false_lower() {
        assert_eq!(boolean().parse(b"false"), Ok(Phi::False))
    }

    #[test]
    fn test_phi_01() {
        let state = ParseState::default();
        assert_eq!(
            conjunction(&state, &TestModel).parse(b"<<0>> F true"),
            Ok(Phi::EnforceEventually {
                players: vec![0usize],
                formula: Arc::new(Phi::True)
            })
        );
        assert!(!state.has_errors());
    }

    #[test]
    fn general_precedence_01() {
        let formula = parse_phi(&TestModel, "<<>> G true & false").unwrap();
        let expected = Phi::EnforceInvariant {
            players: vec![],
            formula: Arc::new(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False))),
        };
        assert_eq!(formula, expected)
    }

    #[test]
    fn general_precedence_02() {
        let formula = parse_phi(&TestModel, "!<<>> F !true & false").unwrap();
        let expected = Phi::Not(Arc::new(Phi::EnforceEventually {
            players: vec![],
            formula: Arc::new(Phi::And(
                Arc::new(Phi::Not(Arc::from(Phi::True))),
                Arc::new(Phi::False),
            )),
        }));
        assert_eq!(formula, expected)
    }

    #[test]
    fn general_precedence_03() {
        let formula = parse_phi(&TestModel, "[[]] F false | <<>> G true").unwrap();
        let expected = Phi::DespiteEventually {
            players: vec![],
            formula: Arc::new(Phi::Or(
                Arc::new(Phi::False),
                Arc::new(Phi::EnforceInvariant {
                    players: vec![],
                    formula: Arc::new(Phi::True),
                }),
            )),
        };
        assert_eq!(formula, expected)
    }

    #[test]
    fn general_precedence_04() {
        let formula = parse_phi(&TestModel, "!false & [[]] F <<>> G false | true").unwrap();
        let expected = Phi::And(
            Arc::new(Phi::Not(Arc::new(Phi::False))),
            Arc::new(Phi::DespiteEventually {
                players: vec![],
                formula: Arc::new(Phi::EnforceInvariant {
                    players: vec![],
                    formula: Arc::new(Phi::Or(Arc::new(Phi::False), Arc::new(Phi::True))),
                }),
            }),
        );
        assert_eq!(formula, expected)
    }

    #[test]
    fn test_atl_lcgs_01() {
        // Can we parse ATL coalitions that mentions players in an LCGS program
        let lcgs_program = "
        player p1 = something;

        template something
            [wait] 1;
        endtemplate
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(lcgs_program).unwrap()).unwrap();

        let atl_formula = "<<p1>>";
        let state = ParseState::default();
        let phi = enforce_coalition(&state, &lcgs).parse(&atl_formula.as_bytes());
        assert_eq!(phi, Ok(vec![0]));
        assert!(!state.has_errors());
    }

    #[test]
    fn test_atl_lcgs_02() {
        // Can we parse ATL formulas that mentions players in an LCGS program
        let lcgs_program = "
        player p1 = something;

        template something
            [wait] 1;
        endtemplate
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(lcgs_program).unwrap()).unwrap();

        let atl_formula = "<<p1>> F true";
        let phi = parse_phi(&lcgs, &atl_formula);
        assert_eq!(
            phi,
            Ok(Phi::EnforceEventually {
                players: vec![0],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn test_atl_lcgs_03() {
        // Can we parse ATL formulas that mentions labels in an LCGS program
        let lcgs_program = "
        player p1 = something;
        label test = 1;
        template something
            [wait] 1;
        endtemplate
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(lcgs_program).unwrap()).unwrap();

        let atl_formula = "<<>> F test";
        let phi = parse_phi(&lcgs, &atl_formula);
        assert_eq!(
            phi,
            Ok(Phi::EnforceEventually {
                players: vec![],
                formula: Arc::new(Phi::Proposition(0))
            })
        )
    }

    #[test]
    fn test_atl_lcgs_04() {
        // Can we parse ATL formulas that mentions players and labels in an LCGS program
        let lcgs_program = "
        player p1 = something;

        template something
            label test = 1;
            [wait] 1;
        endtemplate
        ";
        let lcgs = IntermediateLcgs::create(parse_lcgs(lcgs_program).unwrap()).unwrap();

        let atl_formula = "<<p1>> F p1.test";
        let phi = parse_phi(&lcgs, &atl_formula);
        assert_eq!(
            phi,
            Ok(Phi::EnforceEventually {
                players: vec![0],
                formula: Arc::new(Phi::Proposition(0))
            })
        )
    }
}
