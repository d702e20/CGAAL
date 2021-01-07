use std::fmt::Debug;
use std::string::FromUtf8Error;
use std::sync::Arc;

use pom::parser::{call, end, list, one_of, seq, sym};
use pom::Parser;

use super::Phi;

pub(crate) trait StrToId<E: Debug>: Fn(String) -> Result<usize, E> {}

fn ws() -> Parser<u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

pub(crate) fn phi<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    paren(convert)
        | boolean()
        | proposition(convert)
        | not(convert)
        | or(convert)
        | and(convert)
        | enforce_next(convert)
        | enforce_until(convert)
        | enforce_eventually(convert)
        | enforce_invariant(convert)
        | despite_next(convert)
        | despite_until(convert)
        | despite_eventually(convert)
        | despite_invariant(convert)
}

/// A lazy phi parser used for recursive definitions.
/// Normally we make recursive parsers with the `call(phi)` that wraps a parser with lazy
/// invocation. But the `call` method does not allow us to pass our convert function. So we
/// make our own lazy phi parser.
fn lazy_phi<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    Parser::new(move |input: &'_ [u8], start: usize| (phi(convert).method)(input, start))
}

fn paren<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    sym(b'(') * ws() * lazy_phi(convert) - ws() - sym(b')')
}

fn enforce_players<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Vec<usize>> {
    seq(b"<<") * ws() * players(convert) - ws() - seq(b">>")
}

fn despite_players<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Vec<usize>> {
    seq(b"[[") * ws() * players(convert) - ws() - seq(b"]]")
}

fn next<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    sym(b'X') * ws() * lazy_phi(convert)
}

fn until<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, (Phi, Phi)> {
    sym(b'(') * ws() * lazy_phi(convert) - ws() - sym(b'U') - ws() + lazy_phi(convert)
        - ws()
        - sym(b')')
}

fn eventually<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    sym(b'F') * ws() * lazy_phi(convert)
}

fn invariant<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    sym(b'G') * ws() * lazy_phi(convert)
}

fn enforce_next<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (enforce_players(convert) - ws() + next(convert)).map(|(players, phi)| Phi::EnforceNext {
        players,
        formula: Arc::new(phi),
    })
}

fn enforce_until<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (enforce_players(convert) - ws() + until(convert)).map(|(players, (l, r))| Phi::EnforceUntil {
        players,
        pre: Arc::new(l),
        until: Arc::new(r),
    })
}

fn enforce_eventually<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (enforce_players(convert) - ws() + eventually(convert)).map(|(players, phi)| {
        Phi::EnforceEventually {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn enforce_invariant<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (enforce_players(convert) - ws() + invariant(convert)).map(|(players, phi)| {
        Phi::EnforceInvariant {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn despite_next<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (despite_players(convert) - ws() + next(convert)).map(|(players, phi)| Phi::DespiteNext {
        players,
        formula: Arc::new(phi),
    })
}

fn despite_until<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (despite_players(convert) - ws() + until(convert)).map(|(players, (l, r))| Phi::DespiteUntil {
        players,
        pre: Arc::new(l),
        until: Arc::new(r),
    })
}

fn despite_eventually<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (despite_players(convert) - ws() + eventually(convert)).map(|(players, phi)| {
        Phi::DespiteEventually {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn despite_invariant<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (despite_players(convert) - ws() + invariant(convert)).map(|(players, phi)| {
        Phi::DespiteInvariant {
            players,
            formula: Arc::new(phi),
        }
    })
}

fn proposition<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (sym(b'"') * identifier().convert(convert) - sym(b'"')).map(|id| Phi::Proposition(id))
}

fn not<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    (sym(b'!') * ws() * lazy_phi(convert)).map(|phi| Phi::Not(Arc::new(phi)))
}

fn or<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    (lazy_phi(convert) - ws() - sym(b'|') - ws() + lazy_phi(convert))
        .map(|(l, r)| Phi::Or(Arc::new(l), Arc::new(r)))
}

fn and<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    let parser = lazy_phi(convert) - ws() - sym(b'&') - ws() + lazy_phi(convert);
    parser.map(|(l, r)| Phi::And(Arc::new(l), Arc::new(r)))
}

fn boolean() -> Parser<u8, Phi> {
    seq(b"true").map(|_| Phi::True)
        | seq(b"TRUE").map(|_| Phi::True)
        | seq(b"false").map(|_| Phi::False)
        | seq(b"FALSE").map(|_| Phi::False)
}

fn players<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Vec<usize>> {
    list(identifier().convert(convert), ws() * sym(b',') * ws())
}

fn identifier() -> Parser<u8, String> {
    let parser = alpha() + (alpha() | num()).repeat(0..);
    parser.collect().convert(|s| String::from_utf8(s.to_vec()))
}

fn alpha() -> Parser<u8, u8> {
    one_of(b"abcdefghijklmnopqrstuvwxyz") | one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

fn num() -> Parser<u8, u8> {
    one_of(b"0123456789")
}

#[cfg(test)]
mod test {
    use std::convert::identity;
    use std::fmt::{Debug, Formatter};
    use std::sync::Arc;

    use crate::atl::formula::parser::{and, boolean, identifier, not, or, StrToId};
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
    fn identifier_4() {
        assert!(identifier().parse(b"2").is_err())
    }

    #[test]
    fn identifier_5() {
        assert!(identifier().parse(b"").is_err())
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
    fn or_01() {
        assert_eq!(
            or(&convert).parse(b"true | false"),
            Ok(Phi::Or(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn and_01() {
        assert_eq!(
            and(&convert).parse(b"true & false"),
            Ok(Phi::And(Arc::new(Phi::True), Arc::new(Phi::False)))
        )
    }

    #[test]
    fn not_1() {
        assert_eq!(
            not(&convert).parse(b"!true"),
            Ok(Phi::Not(Arc::new(Phi::True)))
        )
    }
}
