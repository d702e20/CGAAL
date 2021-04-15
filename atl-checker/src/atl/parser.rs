use std::str::{self, FromStr};
use std::sync::Arc;

use pom::parser::{end, Parser};
use pom::parser::{list, one_of, seq, sym};

use super::Phi;
use crate::gamestructure::{Player, Proposition};

/// Parse an ATL formula
pub fn parse_phi<'a, 'b: 'a, A: ATLExpressionParser>(
    expr_parser: &'b A,
    input: &'a str,
) -> Result<Phi, String> {
    let formula = ws() * phi(expr_parser) - ws() - end();
    formula
        .parse(input.as_bytes())
        .map_err(|err| err.to_string())
}

/// Allows a CGS model to define custom player and proposition expressions. For instance,
/// in LCGS we want to be able to write "p2" as a player and "p2.alive" as a proposition, while
/// in json, players and propositions are numbers.
pub trait ATLExpressionParser {
    /// A parser that parses a player name
    fn player_parser(&self) -> Parser<u8, Player>;
    /// A parser that parses a proposition name
    fn proposition_parser(&self) -> Parser<u8, Proposition>;
}

/// Whitespace
fn ws<'a>() -> Parser<'a, u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

/// A lazy parser used for recursive definitions.
/// Normally we make recursive parsers with the `call(phi)` that wraps a parser with lazy
/// invocation. But the `call` method does not allow us to pass our converter. So we
/// make our own lazy parser.
fn lazy<'a, A: ATLExpressionParser, P: Fn(&'a A) -> Parser<u8, Phi>>(
    parser: &'a P,
    expr_parser: &'a A,
) -> Parser<'a, u8, Phi> {
    Parser::new(move |input: &'_ [u8], start: usize| (parser(expr_parser).method)(input, start))
}

/// Parses an ATL formula (without whitespace around it)
pub(crate) fn phi<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    // We have to take left-recursion and precedence into account when making parsers.
    // In ATL formulas, only AND and OR is subject to left-recursion, where AND have higher
    // precedence. So we split ATL formulas into layers: phi s (can contain AND),
    // terms (can contain OR), and primaries (stuff with low precedence and no left-recursion).
    let and = (term(expr_parser) - ws() - sym(b'&') - ws() + lazy(&phi, expr_parser))
        .map(|(lhs, rhs)| Phi::And(Arc::new(lhs), Arc::new(rhs)));
    and | term(expr_parser)
}

/// Parses an ATL term (Can't contain AND and without whitespace around it)
fn term<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    let or = (primary(expr_parser) - ws() - sym(b'|') - ws() + lazy(&term, expr_parser))
        .map(|(lhs, rhs)| Phi::Or(Arc::new(lhs), Arc::new(rhs)));
    or | primary(expr_parser)
}

/// Parses a primary ATL formula (no ANDs or ORs)
fn primary<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    paren(expr_parser)
        | boolean()
        | proposition(expr_parser)
        | not(expr_parser)
        | enforce_next(expr_parser)
        | enforce_until(expr_parser)
        | enforce_eventually(expr_parser)
        | enforce_invariant(expr_parser)
        | despite_next(expr_parser)
        | despite_until(expr_parser)
        | despite_eventually(expr_parser)
        | despite_invariant(expr_parser)
}

/// Parses an ATL formula in parenthesis
fn paren<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    sym(b'(') * ws() * lazy(&phi, expr_parser) - ws() - sym(b')')
}

/// Parses an enforce-coalition (path qualifier)
fn enforce_players<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Vec<usize>> {
    seq(b"<<") * ws() * players(expr_parser) - ws() - seq(b">>")
}

/// Parses a despite-coalition (path qualifier)
fn despite_players<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Vec<usize>> {
    seq(b"[[") * ws() * players(expr_parser) - ws() - seq(b"]]")
}

/// Parses an path formula starting with the NEXT (X) operator
fn next<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    sym(b'X') * ws() * lazy(&phi, expr_parser)
}

/// Parses an path formula with the UNTIL operator
fn until<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, (Phi, Phi)> {
    sym(b'(') * ws() * lazy(&phi, expr_parser) - ws() - sym(b'U') - ws() + lazy(&phi, expr_parser)
        - ws()
        - sym(b')')
}

/// Parses an path formula starting with the EVENTUALLY (F/finally) operator
fn eventually<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    sym(b'F') * ws() * lazy(&phi, expr_parser)
}

/// Parses an path formula starting with the INVARIANT (G/global) operator
fn invariant<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    sym(b'G') * ws() * lazy(&phi, expr_parser)
}

/// Parses an ENFORCE-NEXT ATL formula
fn enforce_next<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (enforce_players(expr_parser) - ws() + next(expr_parser)).map(|(players, phi)| {
        Phi::EnforceNext {
            players,
            formula: Arc::new(phi),
        }
    })
}

/// Parses an ENFORCE-UNTIL ATL formula
fn enforce_until<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (enforce_players(expr_parser) - ws() + until(expr_parser)).map(|(players, (l, r))| {
        Phi::EnforceUntil {
            players,
            pre: Arc::new(l),
            until: Arc::new(r),
        }
    })
}

/// Parses an ENFORCE-EVENTUALLY ATL formula
fn enforce_eventually<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (enforce_players(expr_parser) - ws() + eventually(expr_parser)).map(|(players, phi)| {
        Phi::EnforceEventually {
            players,
            formula: Arc::new(phi),
        }
    })
}

/// Parses an ENFORCE-INVARIANT ATL formula
fn enforce_invariant<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (enforce_players(expr_parser) - ws() + invariant(expr_parser)).map(|(players, phi)| {
        Phi::EnforceInvariant {
            players,
            formula: Arc::new(phi),
        }
    })
}

/// Parses an DESPITE-NEXT ATL formula
fn despite_next<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (despite_players(expr_parser) - ws() + next(expr_parser)).map(|(players, phi)| {
        Phi::DespiteNext {
            players,
            formula: Arc::new(phi),
        }
    })
}

/// Parses an DESPITE-UNTIL ATL formula
fn despite_until<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (despite_players(expr_parser) - ws() + until(expr_parser)).map(|(players, (l, r))| {
        Phi::DespiteUntil {
            players,
            pre: Arc::new(l),
            until: Arc::new(r),
        }
    })
}

/// Parses an DESPITE-EVENTUALLY ATL formula
fn despite_eventually<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (despite_players(expr_parser) - ws() + eventually(expr_parser)).map(|(players, phi)| {
        Phi::DespiteEventually {
            players,
            formula: Arc::new(phi),
        }
    })
}

/// Parses an DESPITE-INVARIANT ATL formula
fn despite_invariant<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (despite_players(expr_parser) - ws() + invariant(expr_parser)).map(|(players, phi)| {
        Phi::DespiteInvariant {
            players,
            formula: Arc::new(phi),
        }
    })
}

/// Parses a proposition using the given [ATLExpressionParser].
fn proposition<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    expr_parser
        .proposition_parser()
        .map(|id| Phi::Proposition(id))
}

/// Parses a negated ATL formula
fn not<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Phi> {
    (sym(b'!') * ws() * lazy(&phi, expr_parser)).map(|phi| Phi::Not(Arc::new(phi)))
}

/// Parses a boolean, either full uppercase or full lowercase
fn boolean<'a>() -> Parser<'a, u8, Phi> {
    seq(b"true").map(|_| Phi::True)
        | seq(b"TRUE").map(|_| Phi::True)
        | seq(b"false").map(|_| Phi::False)
        | seq(b"FALSE").map(|_| Phi::False)
}

/// Parses a comma-separated list of players using the given [ATLExpressionParser].
fn players<A: ATLExpressionParser>(expr_parser: &A) -> Parser<u8, Vec<usize>> {
    list(expr_parser.player_parser(), ws() * sym(b',') * ws())
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
        boolean, despite_eventually, despite_invariant, despite_next, despite_players,
        despite_until, enforce_eventually, enforce_invariant, enforce_next, enforce_players,
        enforce_until, eventually, invariant, next, not, number, paren, phi, proposition, term,
        until, ATLExpressionParser,
    };
    use crate::atl::{parse_phi, Phi};
    use crate::gamestructure::lcgs::ir::intermediate::IntermediateLCGS;
    use crate::gamestructure::lcgs::parse::parse_lcgs;
    use crate::gamestructure::{Player, Proposition};

    struct TestModel;

    impl ATLExpressionParser for TestModel {
        fn player_parser(&self) -> Parser<u8, Player> {
            number()
        }

        fn proposition_parser(&self) -> Parser<u8, Proposition> {
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
    fn proposition_3() {
        assert!(proposition(&TestModel).parse(b"abc").is_err())
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
            phi(&TestModel).parse(b"true & false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn and_2() {
        assert_eq!(
            phi(&TestModel).parse(b"true&false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn and_3() {
        // Right recursive?
        assert_eq!(
            phi(&TestModel).parse(b"true&false&true"),
            Ok(Phi::And(
                Arc::new(Phi::True),
                Arc::new(Phi::And(Arc::new(Phi::False), Arc::new(Phi::True)))
            ))
        )
    }

    #[test]
    fn or_1() {
        assert_eq!(
            term(&TestModel).parse(b"true | false"),
            Ok(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn or_2() {
        assert_eq!(
            term(&TestModel).parse(b"true|false"),
            Ok(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn or_3() {
        // Right recursive?
        assert_eq!(
            term(&TestModel).parse(b"true|false |true"),
            Ok(Phi::Or(
                Arc::new(Phi::True),
                Arc::new(Phi::Or(Arc::new(Phi::False), Arc::new(Phi::True)))
            ))
        )
    }

    #[test]
    fn test_and_or_precedence_01() {
        assert_eq!(
            phi(&TestModel).parse(b"true & false | true | false & true"),
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
    fn test_phi_01() {
        assert_eq!(
            phi(&TestModel).parse(b"<<0>> F true"),
            Ok(Phi::EnforceEventually {
                players: vec![0usize],
                formula: Arc::new(Phi::True)
            })
        )
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
        let lcgs = IntermediateLCGS::create(parse_lcgs(lcgs_program).unwrap()).unwrap();

        let atl_formula = "<<p1>>";
        let phi = enforce_players(&lcgs).parse(&atl_formula.as_bytes());
        assert_eq!(phi, Ok(vec![0]));
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
        let lcgs = IntermediateLCGS::create(parse_lcgs(lcgs_program).unwrap()).unwrap();

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
        let lcgs = IntermediateLCGS::create(parse_lcgs(lcgs_program).unwrap()).unwrap();

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
        let lcgs = IntermediateLCGS::create(parse_lcgs(lcgs_program).unwrap()).unwrap();

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
