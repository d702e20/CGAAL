use std::fmt::Debug;
use std::sync::Arc;

use pom::parser::Parser;
use pom::parser::{list, one_of, seq, sym};

use super::Phi;

fn ws<'a>() -> Parser<'a, u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

pub(crate) fn phi<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    paren(convert_player, convert_proposition)
        | boolean()
        | proposition(convert_proposition)
        | not(convert_player, convert_proposition)
        | or(convert_player, convert_proposition)
        | and(convert_player, convert_proposition)
        | enforce_next(convert_player, convert_proposition)
        | enforce_until(convert_player, convert_proposition)
        | enforce_eventually(convert_player, convert_proposition)
        | enforce_invariant(convert_player, convert_proposition)
        | despite_next(convert_player, convert_proposition)
        | despite_until(convert_player, convert_proposition)
        | despite_eventually(convert_player, convert_proposition)
        | despite_invariant(convert_player, convert_proposition)
}

/// A lazy phi parser used for recursive definitions.
/// Normally we make recursive parsers with the `call(phi)` that wraps a parser with lazy
/// invocation. But the `call` method does not allow us to pass our convert function. So we
/// make our own lazy phi parser.
fn lazy_phi<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    Parser::new(move |input: &'_ [u8], start: usize| {
        (phi(convert_player, convert_proposition).method)(input, start)
    })
}

fn paren<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>, CP: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    sym(b'(') * ws() * lazy_phi(convert_player, convert_proposition) - ws() - sym(b')')
}

fn enforce_players<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
) -> Parser<'a, u8, Vec<usize>> {
    seq(b"<<") * ws() * players(convert_player) - ws() - seq(b">>")
}

fn despite_players<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
) -> Parser<'a, u8, Vec<usize>> {
    seq(b"[[") * ws() * players(convert_player) - ws() - seq(b"]]")
}

fn next<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>, CP: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    sym(b'X') * ws() * lazy_phi(convert_player, convert_proposition)
}

fn until<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>, CP: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, (Phi, Phi)> {
    sym(b'(') * ws() * lazy_phi(convert_player, convert_proposition) - ws() - sym(b'U') - ws()
        + lazy_phi(convert_player, convert_proposition)
        - ws()
        - sym(b')')
}

fn eventually<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    sym(b'F') * ws() * lazy_phi(convert_player, convert_proposition)
}

fn invariant<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    sym(b'G') * ws() * lazy_phi(convert_player, convert_proposition)
}

fn enforce_next<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (enforce_players(convert_player) - ws() + next(convert_player, convert_proposition)).map(
        |(players, phi)| Phi::EnforceNext {
            players,
            formula: Arc::new(phi),
        },
    )
}

fn enforce_until<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (enforce_players(convert_player) - ws() + until(convert_player, convert_proposition)).map(
        |(players, (l, r))| Phi::EnforceUntil {
            players,
            pre: Arc::new(l),
            until: Arc::new(r),
        },
    )
}

fn enforce_eventually<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (enforce_players(convert_player) - ws() + eventually(convert_player, convert_proposition)).map(
        |(players, phi)| Phi::EnforceEventually {
            players,
            formula: Arc::new(phi),
        },
    )
}

fn enforce_invariant<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (enforce_players(convert_player) - ws() + invariant(convert_player, convert_proposition)).map(
        |(players, phi)| Phi::EnforceInvariant {
            players,
            formula: Arc::new(phi),
        },
    )
}

fn despite_next<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (despite_players(convert_player) - ws() + next(convert_player, convert_proposition)).map(
        |(players, phi)| Phi::DespiteNext {
            players,
            formula: Arc::new(phi),
        },
    )
}

fn despite_until<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (despite_players(convert_player) - ws() + until(convert_player, convert_proposition)).map(
        |(players, (l, r))| Phi::DespiteUntil {
            players,
            pre: Arc::new(l),
            until: Arc::new(r),
        },
    )
}

fn despite_eventually<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (despite_players(convert_player) - ws() + eventually(convert_player, convert_proposition)).map(
        |(players, phi)| Phi::DespiteEventually {
            players,
            formula: Arc::new(phi),
        },
    )
}

fn despite_invariant<
    'a,
    E: Debug,
    CPL: Fn(String) -> Result<usize, E>,
    CP: Fn(String) -> Result<usize, E>,
>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (despite_players(convert_player) - ws() + invariant(convert_player, convert_proposition)).map(
        |(players, phi)| Phi::DespiteInvariant {
            players,
            formula: Arc::new(phi),
        },
    )
}

fn proposition<'a, E: Debug, CP: Fn(String) -> Result<usize, E>>(
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (sym(b'"') * identifier().convert(convert_proposition) - sym(b'"'))
        .map(|id| Phi::Proposition(id))
}

fn not<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>, CP: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (sym(b'!') * ws() * lazy_phi(convert_player, convert_proposition))
        .map(|phi| Phi::Not(Arc::new(phi)))
}

fn or<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>, CP: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    (lazy_phi(convert_player, convert_proposition) - ws() - sym(b'|') - ws()
        + lazy_phi(convert_player, convert_proposition))
    .map(|(l, r)| Phi::Or(Arc::new(l), Arc::new(r)))
}

fn and<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>, CP: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
    convert_proposition: &'static CP,
) -> Parser<'a, u8, Phi> {
    let parser = lazy_phi(convert_player, convert_proposition) - ws() - sym(b'&') - ws()
        + lazy_phi(convert_player, convert_proposition);
    parser.map(|(l, r)| Phi::And(Arc::new(l), Arc::new(r)))
}

fn boolean<'a>() -> Parser<'a, u8, Phi> {
    seq(b"true").map(|_| Phi::True)
        | seq(b"TRUE").map(|_| Phi::True)
        | seq(b"false").map(|_| Phi::False)
        | seq(b"FALSE").map(|_| Phi::False)
}

fn players<'a, E: Debug, CPL: Fn(String) -> Result<usize, E>>(
    convert_player: &'static CPL,
) -> Parser<'a, u8, Vec<usize>> {
    list(
        identifier().convert(convert_player),
        ws() * sym(b',') * ws(),
    )
}

fn identifier<'a>() -> Parser<'a, u8, String> {
    let parser = (alpha() | num()).repeat(1..);
    parser.collect().convert(|s| String::from_utf8(s.to_vec()))
}

fn alpha<'a>() -> Parser<'a, u8, u8> {
    one_of(b"abcdefghijklmnopqrstuvwxyz") | one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

fn num<'a>() -> Parser<'a, u8, u8> {
    one_of(b"0123456789")
}

#[cfg(test)]
mod test {
    use std::convert::identity;
    use std::fmt::{Debug, Formatter};
    use std::sync::Arc;

    use crate::atl::formula::parser::{
        and, boolean, despite_eventually, despite_invariant, despite_next, despite_players,
        despite_until, enforce_eventually, enforce_invariant, enforce_next, enforce_players,
        enforce_until, eventually, identifier, invariant, next, not, or, paren, phi, proposition,
        until,
    };
    use crate::atl::formula::Phi;

    fn convert(id: String) -> Result<usize, Error> {
        Ok(0)
    }

    struct Error {}

    impl Debug for Error {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            unimplemented!()
        }
    }

    #[test]
    fn paren_1() {
        assert_eq!(paren(&convert, &convert).parse(b"(true)"), Ok(Phi::True))
    }

    fn convert_int(test: String) -> Result<usize, std::num::ParseIntError> {
        test.parse()
    }

    /// Test for a single player
    #[test]
    fn enforce_players_1() {
        assert_eq!(
            enforce_players(&convert_int).parse(b"<<1>>"),
            Ok(vec![1usize])
        )
    }

    /// Test for two players
    #[test]
    fn enforce_players_2() {
        assert_eq!(
            enforce_players(&convert_int).parse(b"<<4,9>>"),
            Ok(vec![4usize, 9usize])
        )
    }

    /// Test for three players
    #[test]
    fn enforce_players_3() {
        assert_eq!(
            enforce_players(&convert_int).parse(b"<<203,23,4>>"),
            Ok(vec![203usize, 23usize, 4usize])
        )
    }

    /// The list of players is allowed to have whitespace after the seperator
    #[test]
    fn enforce_players_4() {
        assert_eq!(
            enforce_players(&convert_int).parse(b"<<203,  23>>"),
            Ok(vec![203usize, 23usize])
        )
    }

    /// Test for no players, should be valid
    #[test]
    fn enforce_players_5() {
        assert_eq!(enforce_players(&convert_int).parse(b"<<>>"), Ok(vec![]))
    }

    /// Test for a single player
    #[test]
    fn despite_players_1() {
        assert_eq!(
            despite_players(&convert_int).parse(b"[[1]]"),
            Ok(vec![1usize])
        )
    }

    /// Test for two players
    #[test]
    fn despite_players_2() {
        assert_eq!(
            despite_players(&convert_int).parse(b"[[4,9]]"),
            Ok(vec![4usize, 9usize])
        )
    }

    /// Test for three players
    #[test]
    fn despite_players_3() {
        assert_eq!(
            despite_players(&convert_int).parse(b"[[203,23,4]]"),
            Ok(vec![203usize, 23usize, 4usize])
        )
    }

    /// The list of players is allowed to have whitespace after the seperator
    #[test]
    fn despite_players_4() {
        assert_eq!(
            despite_players(&convert_int).parse(b"[[203,  23]]"),
            Ok(vec![203usize, 23usize])
        )
    }

    /// Test for no players, should be valid
    #[test]
    fn despite_players_5() {
        assert_eq!(despite_players(&convert_int).parse(b"[[]]"), Ok(vec![]))
    }

    #[test]
    fn next_1() {
        assert_eq!(next(&convert, &convert).parse(b"X true"), Ok(Phi::True))
    }

    #[test]
    fn next_2() {
        assert_eq!(next(&convert, &convert).parse(b"Xtrue"), Ok(Phi::True))
    }

    #[test]
    fn until_1() {
        assert_eq!(
            until(&convert, &convert).parse(b"( true U true )"),
            Ok((Phi::True, Phi::True))
        )
    }

    #[test]
    fn until_2() {
        assert_eq!(
            until(&convert, &convert).parse(b"(trueUtrue)"),
            Ok((Phi::True, Phi::True))
        )
    }

    #[test]
    fn eventually_1() {
        assert_eq!(
            eventually(&convert, &convert).parse(b"F true"),
            Ok(Phi::True)
        )
    }

    #[test]
    fn eventually_2() {
        assert_eq!(
            eventually(&convert, &convert).parse(b"Ftrue"),
            Ok(Phi::True)
        )
    }

    #[test]
    fn invariant_1() {
        assert_eq!(
            invariant(&convert, &convert).parse(b"G true"),
            Ok(Phi::True)
        )
    }

    #[test]
    fn invariant_2() {
        assert_eq!(invariant(&convert, &convert).parse(b"Gtrue"), Ok(Phi::True))
    }

    #[test]
    fn enforce_next_1() {
        assert_eq!(
            enforce_next(&convert_int, &convert_int).parse(b"<<1 , 2>> X true"),
            Ok(Phi::EnforceNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_next_2() {
        assert_eq!(
            enforce_next(&convert_int, &convert_int).parse(b"<<1,2>>Xtrue"),
            Ok(Phi::EnforceNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_until_1() {
        assert_eq!(
            enforce_until(&convert_int, &convert_int).parse(b"<<1 , 2>> ( true U true )"),
            Ok(Phi::EnforceUntil {
                players: vec![1, 2],
                pre: Arc::new(Phi::True),
                until: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_until_2() {
        assert_eq!(
            enforce_until(&convert_int, &convert_int).parse(b"<<1,2>>(trueUtrue)"),
            Ok(Phi::EnforceUntil {
                players: vec![1, 2],
                pre: Arc::new(Phi::True),
                until: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_eventually_1() {
        assert_eq!(
            enforce_eventually(&convert_int, &convert_int).parse(b"<<1 , 2>> F true"),
            Ok(Phi::EnforceEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_eventually_2() {
        assert_eq!(
            enforce_eventually(&convert_int, &convert_int).parse(b"<<1,2>>Ftrue"),
            Ok(Phi::EnforceEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_invariant_1() {
        assert_eq!(
            enforce_invariant(&convert_int, &convert_int).parse(b"<<1 , 2>> G true"),
            Ok(Phi::EnforceInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_invariant_2() {
        assert_eq!(
            enforce_invariant(&convert_int, &convert_int).parse(b"<<1,2>>Gtrue"),
            Ok(Phi::EnforceInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_next_1() {
        assert_eq!(
            despite_next(&convert_int, &convert_int).parse(b"[[1 , 2]] X true"),
            Ok(Phi::DespiteNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_next_2() {
        assert_eq!(
            despite_next(&convert_int, &convert_int).parse(b"[[1,2]]Xtrue"),
            Ok(Phi::DespiteNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_until_1() {
        assert_eq!(
            despite_until(&convert_int, &convert_int).parse(b"[[1 , 2]] ( true U true )"),
            Ok(Phi::DespiteUntil {
                players: vec![1, 2],
                pre: Arc::new(Phi::True),
                until: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_until_2() {
        assert_eq!(
            despite_until(&convert_int, &convert_int).parse(b"[[1,2]](trueUtrue)"),
            Ok(Phi::DespiteUntil {
                players: vec![1, 2],
                pre: Arc::new(Phi::True),
                until: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_eventually_1() {
        assert_eq!(
            despite_eventually(&convert_int, &convert_int).parse(b"[[1 , 2]] F true"),
            Ok(Phi::DespiteEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_eventually_2() {
        assert_eq!(
            despite_eventually(&convert_int, &convert_int).parse(b"[[1,2]]Ftrue"),
            Ok(Phi::DespiteEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_invariant_1() {
        assert_eq!(
            despite_invariant(&convert_int, &convert_int).parse(b"[[1 , 2]] G true"),
            Ok(Phi::DespiteInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_invariant_2() {
        assert_eq!(
            despite_invariant(&convert_int, &convert_int).parse(b"[[1,2]]Gtrue"),
            Ok(Phi::DespiteInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn proposition_1() {
        assert_eq!(
            proposition(&convert_int).parse(b"\"1\""),
            Ok(Phi::Proposition(1usize))
        )
    }

    #[test]
    fn proposition_2() {
        assert_eq!(
            proposition(&convert_int).parse(b"\"1432\""),
            Ok(Phi::Proposition(1432usize))
        )
    }

    #[test]
    fn not_1() {
        assert_eq!(
            not(&convert, &convert).parse(b"! true"),
            Ok(Phi::Not(Arc::new(Phi::True)))
        )
    }

    #[test]
    fn not_2() {
        assert_eq!(
            not(&convert, &convert).parse(b"!true"),
            Ok(Phi::Not(Arc::new(Phi::True)))
        )
    }

    #[test]
    fn and_1() {
        assert_eq!(
            and(&convert, &convert).parse(b"true & false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn and_2() {
        assert_eq!(
            and(&convert, &convert).parse(b"true&false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn or_1() {
        assert_eq!(
            or(&convert, &convert).parse(b"true | false"),
            Ok(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn or_2() {
        assert_eq!(
            or(&convert, &convert).parse(b"true|false"),
            Ok(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
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
    fn identifier_1() {
        assert_eq!(identifier().parse(b"a"), Ok("a".to_string()))
    }

    #[test]
    fn identifier_2() {
        assert_eq!(identifier().parse(b"aaaa"), Ok("aaaa".to_string()))
    }

    #[test]
    fn identifier_3() {
        assert_eq!(identifier().parse(b"a123a"), Ok("a123a".to_string()))
    }

    #[test]
    fn identifier_5() {
        assert!(identifier().parse(b"").is_err())
    }
}
