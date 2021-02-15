use std::sync::Arc;

use pom::parser::{end, Parser};
use pom::parser::{list, one_of, seq, sym};

use super::Phi;
use std::str::{self, FromStr};

/// Parse an ATL formula
pub(crate) fn parse_phi<'a, A: ATLExpressionParser<'a>>(
    expr_parser: &'a A,
    input: &'a str,
) -> Result<Phi, String> {
    let formula = ws() * phi(expr_parser) - end();
    formula
        .parse(input.as_bytes())
        .map_err(|err| err.to_string())
}

/// Allows a CGS model to define custom player and proposition expressions. For instance,
/// in LCGS we want to be able to write "p2" as a player and "p2.alive" as a proposition, while
/// in json, players and propositions are numbers.
pub trait ATLExpressionParser<'a> {
    fn player_parser(&self) -> Parser<'a, u8, usize>;
    fn proposition_parser(&self) -> Parser<'a, u8, usize>;
}

fn ws<'a>() -> Parser<'a, u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

pub(crate) fn phi<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    paren(expr_parser)
        | boolean()
        | proposition(expr_parser)
        | not(expr_parser)
        | or(expr_parser)
        | and(expr_parser)
        | enforce_next(expr_parser)
        | enforce_until(expr_parser)
        | enforce_eventually(expr_parser)
        | enforce_invariant(expr_parser)
        | despite_next(expr_parser)
        | despite_until(expr_parser)
        | despite_eventually(expr_parser)
        | despite_invariant(expr_parser)
}

/// A lazy phi parser used for recursive definitions.
/// Normally we make recursive parsers with the `call(phi)` that wraps a parser with lazy
/// invocation. But the `call` method does not allow us to pass our convert function. So we
/// make our own lazy phi parser.
fn lazy_phi<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    Parser::new(move |input: &'_ [u8], start: usize| (phi(expr_parser).method)(input, start))
}

fn paren<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    sym(b'(') * ws() * lazy_phi(expr_parser) - ws() - sym(b')')
}

fn enforce_players<'a, A: ATLExpressionParser<'a>>(
    expr_parser: &'a A,
) -> Parser<'a, u8, Vec<usize>> {
    seq(b"<<") * ws() * players(expr_parser) - ws() - seq(b">>")
}

fn despite_players<'a, A: ATLExpressionParser<'a>>(
    expr_parser: &'a A,
) -> Parser<'a, u8, Vec<usize>> {
    seq(b"[[") * ws() * players(expr_parser) - ws() - seq(b"]]")
}

fn next<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    sym(b'X') * ws() * lazy_phi(expr_parser)
}

fn until<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, (Phi, Phi)> {
    sym(b'(') * ws() * lazy_phi(expr_parser) - ws() - sym(b'U') - ws() + lazy_phi(expr_parser)
        - ws()
        - sym(b')')
}

fn eventually<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    sym(b'F') * ws() * lazy_phi(expr_parser)
}

fn invariant<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    sym(b'G') * ws() * lazy_phi(expr_parser)
}

fn enforce_next<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (enforce_players(expr_parser) - ws() + next(expr_parser)).map(|(players, phi)| {
        Phi::EnforceNext {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn enforce_until<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (enforce_players(expr_parser) - ws() + until(expr_parser)).map(|(players, (l, r))| {
        Phi::EnforceUntil {
            players,
            pre: Arc::new(l),
            until: Arc::new(r),
        }
    })
}

fn enforce_eventually<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (enforce_players(expr_parser) - ws() + eventually(expr_parser)).map(|(players, phi)| {
        Phi::EnforceEventually {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn enforce_invariant<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (enforce_players(expr_parser) - ws() + invariant(expr_parser)).map(|(players, phi)| {
        Phi::EnforceInvariant {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn despite_next<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (despite_players(expr_parser) - ws() + next(expr_parser)).map(|(players, phi)| {
        Phi::DespiteNext {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn despite_until<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (despite_players(expr_parser) - ws() + until(expr_parser)).map(|(players, (l, r))| {
        Phi::DespiteUntil {
            players,
            pre: Arc::new(l),
            until: Arc::new(r),
        }
    })
}

fn despite_eventually<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (despite_players(expr_parser) - ws() + eventually(expr_parser)).map(|(players, phi)| {
        Phi::DespiteEventually {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn despite_invariant<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (despite_players(expr_parser) - ws() + invariant(expr_parser)).map(|(players, phi)| {
        Phi::DespiteInvariant {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn proposition<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    expr_parser
        .proposition_parser()
        .map(|id| Phi::Proposition(id))
}

fn not<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (sym(b'!') * ws() * lazy_phi(expr_parser)).map(|phi| Phi::Not(Arc::new(phi)))
}

fn or<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    (lazy_phi(expr_parser) - ws() - sym(b'|') - ws() + lazy_phi(expr_parser))
        .map(|(l, r)| Phi::Or(Arc::new(l), Arc::new(r)))
}

fn and<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Phi> {
    let parser = lazy_phi(expr_parser) - ws() - sym(b'&') - ws() + lazy_phi(expr_parser);
    parser.map(|(l, r)| Phi::And(Arc::new(l), Arc::new(r)))
}

fn boolean<'a>() -> Parser<'a, u8, Phi> {
    seq(b"true").map(|_| Phi::True)
        | seq(b"TRUE").map(|_| Phi::True)
        | seq(b"false").map(|_| Phi::False)
        | seq(b"FALSE").map(|_| Phi::False)
}

fn players<'a, A: ATLExpressionParser<'a>>(expr_parser: &'a A) -> Parser<'a, u8, Vec<usize>> {
    list(expr_parser.player_parser(), ws() * sym(b',') * ws())
}

fn alpha<'a>() -> Parser<'a, u8, u8> {
    one_of(b"abcdefghijklmnopqrstuvwxyz") | one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

pub fn word<'a>() -> Parser<'a, u8, String> {
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

    use crate::atl::formula::parser::{
        and, boolean, despite_eventually, despite_invariant, despite_next, despite_players,
        despite_until, enforce_eventually, enforce_invariant, enforce_next, enforce_players,
        enforce_until, eventually, invariant, next, not, number, or, paren, phi, proposition,
        until, ATLExpressionParser,
    };
    use crate::atl::formula::Phi;
    use pom::parser::Parser;

    struct TestModel;

    impl<'a> ATLExpressionParser<'a> for TestModel {
        fn player_parser(&self) -> Parser<'a, u8, usize> {
            number()
        }

        fn proposition_parser(&self) -> Parser<'a, u8, usize> {
            number()
        }
    }

    #[test]
    fn paren_1() {
        assert_eq!(paren(&TestModel).parse(b"(true)"), Ok(Phi::True))
    }

    /// Test for a single player
    #[test]
    fn enforce_players_1() {
        assert_eq!(
            enforce_players(&TestModel).parse(b"<<1>>"),
            Ok(vec![1usize])
        )
    }

    /// Test for two players
    #[test]
    fn enforce_players_2() {
        assert_eq!(
            enforce_players(&TestModel).parse(b"<<4,9>>"),
            Ok(vec![4usize, 9usize])
        )
    }

    /// Test for three players
    #[test]
    fn enforce_players_3() {
        assert_eq!(
            enforce_players(&TestModel).parse(b"<<203,23,4>>"),
            Ok(vec![203usize, 23usize, 4usize])
        )
    }

    /// The list of players is allowed to have whitespace after the separator
    #[test]
    fn enforce_players_4() {
        assert_eq!(
            enforce_players(&TestModel).parse(b"<<203,  23>>"),
            Ok(vec![203usize, 23usize])
        )
    }

    /// Test for no players, should be valid
    #[test]
    fn enforce_players_5() {
        assert_eq!(enforce_players(&TestModel).parse(b"<<>>"), Ok(vec![]))
    }

    /// Test for a single player
    #[test]
    fn despite_players_1() {
        assert_eq!(
            despite_players(&TestModel).parse(b"[[1]]"),
            Ok(vec![1usize])
        )
    }

    /// Test for two players
    #[test]
    fn despite_players_2() {
        assert_eq!(
            despite_players(&TestModel).parse(b"[[4,9]]"),
            Ok(vec![4usize, 9usize])
        )
    }

    /// Test for three players
    #[test]
    fn despite_players_3() {
        assert_eq!(
            despite_players(&TestModel).parse(b"[[203,23,4]]"),
            Ok(vec![203usize, 23usize, 4usize])
        )
    }

    /// The list of players is allowed to have whitespace after the seperator
    #[test]
    fn despite_players_4() {
        assert_eq!(
            despite_players(&TestModel).parse(b"[[203,  23]]"),
            Ok(vec![203usize, 23usize])
        )
    }

    /// Test for no players, should be valid
    #[test]
    fn despite_players_5() {
        assert_eq!(despite_players(&TestModel).parse(b"[[]]"), Ok(vec![]))
    }

    #[test]
    fn next_1() {
        assert_eq!(next(&TestModel).parse(b"X true"), Ok(Phi::True))
    }

    #[test]
    fn next_2() {
        assert_eq!(next(&TestModel).parse(b"Xtrue"), Ok(Phi::True))
    }

    #[test]
    fn until_1() {
        assert_eq!(
            until(&TestModel).parse(b"( true U true )"),
            Ok((Phi::True, Phi::True))
        )
    }

    #[test]
    fn until_2() {
        assert_eq!(
            until(&TestModel).parse(b"(trueUtrue)"),
            Ok((Phi::True, Phi::True))
        )
    }

    #[test]
    fn eventually_1() {
        assert_eq!(eventually(&TestModel).parse(b"F true"), Ok(Phi::True))
    }

    #[test]
    fn eventually_2() {
        assert_eq!(eventually(&TestModel).parse(b"Ftrue"), Ok(Phi::True))
    }

    #[test]
    fn invariant_1() {
        assert_eq!(invariant(&TestModel).parse(b"G true"), Ok(Phi::True))
    }

    #[test]
    fn invariant_2() {
        assert_eq!(invariant(&TestModel).parse(b"Gtrue"), Ok(Phi::True))
    }

    #[test]
    fn enforce_next_1() {
        assert_eq!(
            enforce_next(&TestModel).parse(b"<<1 , 2>> X true"),
            Ok(Phi::EnforceNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_next_2() {
        assert_eq!(
            enforce_next(&TestModel).parse(b"<<1,2>>Xtrue"),
            Ok(Phi::EnforceNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_until_1() {
        assert_eq!(
            enforce_until(&TestModel).parse(b"<<1 , 2>> ( true U true )"),
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
            enforce_until(&TestModel).parse(b"<<1,2>>(trueUtrue)"),
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
            enforce_eventually(&TestModel).parse(b"<<1 , 2>> F true"),
            Ok(Phi::EnforceEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_eventually_2() {
        assert_eq!(
            enforce_eventually(&TestModel).parse(b"<<1,2>>Ftrue"),
            Ok(Phi::EnforceEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_invariant_1() {
        assert_eq!(
            enforce_invariant(&TestModel).parse(b"<<1 , 2>> G true"),
            Ok(Phi::EnforceInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn enforce_invariant_2() {
        assert_eq!(
            enforce_invariant(&TestModel).parse(b"<<1,2>>Gtrue"),
            Ok(Phi::EnforceInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_next_1() {
        assert_eq!(
            despite_next(&TestModel).parse(b"[[1 , 2]] X true"),
            Ok(Phi::DespiteNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_next_2() {
        assert_eq!(
            despite_next(&TestModel).parse(b"[[1,2]]Xtrue"),
            Ok(Phi::DespiteNext {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_until_1() {
        assert_eq!(
            despite_until(&TestModel).parse(b"[[1 , 2]] ( true U true )"),
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
            despite_until(&TestModel).parse(b"[[1,2]](trueUtrue)"),
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
            despite_eventually(&TestModel).parse(b"[[1 , 2]] F true"),
            Ok(Phi::DespiteEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_eventually_2() {
        assert_eq!(
            despite_eventually(&TestModel).parse(b"[[1,2]]Ftrue"),
            Ok(Phi::DespiteEventually {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_invariant_1() {
        assert_eq!(
            despite_invariant(&TestModel).parse(b"[[1 , 2]] G true"),
            Ok(Phi::DespiteInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn despite_invariant_2() {
        assert_eq!(
            despite_invariant(&TestModel).parse(b"[[1,2]]Gtrue"),
            Ok(Phi::DespiteInvariant {
                players: vec![1, 2],
                formula: Arc::new(Phi::True)
            })
        )
    }

    #[test]
    fn proposition_1() {
        assert_eq!(
            proposition(&TestModel).parse(b"1"),
            Ok(Phi::Proposition(1usize))
        )
    }

    #[test]
    fn proposition_2() {
        assert_eq!(
            proposition(&TestModel).parse(b"1432"),
            Ok(Phi::Proposition(1432usize))
        )
    }

    #[test]
    fn not_1() {
        assert_eq!(
            not(&TestModel).parse(b"! true"),
            Ok(Phi::Not(Arc::new(Phi::True)))
        )
    }

    #[test]
    fn not_2() {
        assert_eq!(
            not(&TestModel).parse(b"!true"),
            Ok(Phi::Not(Arc::new(Phi::True)))
        )
    }

    #[test]
    fn and_1() {
        assert_eq!(
            and(&TestModel).parse(b"true & false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn and_2() {
        assert_eq!(
            and(&TestModel).parse(b"true&false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn or_1() {
        assert_eq!(
            or(&TestModel).parse(b"true | false"),
            Ok(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn or_2() {
        assert_eq!(
            or(&TestModel).parse(b"true|false"),
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
}
