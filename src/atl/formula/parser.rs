use super::Phi;
use pom::parser::{end, list, one_of, seq, sym};
use pom::Parser;
use std::fmt::Debug;
use std::string::FromUtf8Error;
use std::sync::Arc;

pub(crate) trait StrToId<E: Debug>: Fn(String) -> Result<usize, E> {}

fn space() -> Parser<u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

pub(crate) fn phi<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    paren(convert)
        | proposition(convert)
        | not(convert)
        | or(convert)
        | and(convert)
        | boolean()
        | enforce_next(convert)
        | enforce_until(convert)
        | enforce_eventually(convert)
        | enforce_invariant(convert)
        | despite_next(convert)
        | despite_until(convert)
        | despite_eventually(convert)
        | despite_invariant(convert)
}

fn paren<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    sym(b'(') * phi(convert) - sym(b')')
}

fn enforce_players<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Vec<usize>> {
    seq(b"<<").discard() * players(convert) - seq(b">>")
}

fn despite_players<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Vec<usize>> {
    seq(b"[[").discard() * players(convert) - seq(b"]]")
}

fn next<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    sym(b'X') * space() * phi(convert)
}

fn until<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, (Phi, Phi)> {
    sym(b'(') * space() * phi(convert) - space() - sym(b'U') + phi(convert) - space() - sym(b')')
}

fn eventually<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    sym(b'F') * space() * phi(convert)
}

fn invariant<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    sym(b'G') * space() * phi(convert)
}

fn enforce_next<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (enforce_players(convert) + next(convert)).map(|(players, phi)| Phi::EnforceNext {
        players,
        formula: Arc::new(phi),
    })
}

fn enforce_until<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (enforce_players(convert) + until(convert)).map(|(players, (l, r))| Phi::EnforceUntil {
        players,
        pre: Arc::new(l),
        until: Arc::new(r),
    })
}

fn enforce_eventually<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (enforce_players(convert) + eventually(convert)).map(|(players, phi)| Phi::EnforceEventually {
        players,
        formula: Arc::new(phi),
    })
}

fn enforce_invariant<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (enforce_players(convert) + invariant(convert)).map(|(players, phi)| Phi::EnforceInvariant {
        players,
        formula: Arc::new(phi),
    })
}

fn despite_next<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (despite_players(convert) + next(convert)).map(|(players, phi)| Phi::DespiteNext {
        players,
        formula: Arc::new(phi),
    })
}

fn despite_until<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (despite_players(convert) + until(convert)).map(|(players, (l, r))| Phi::DespiteUntil {
        players,
        pre: Arc::new(l),
        until: Arc::new(r),
    })
}

fn despite_eventually<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (despite_players(convert) + eventually(convert)).map(|(players, phi)| Phi::DespiteEventually {
        players,
        formula: Arc::new(phi),
    })
}

fn despite_invariant<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (despite_players(convert) + invariant(convert)).map(|(players, phi)| Phi::DespiteInvariant {
        players,
        formula: Arc::new(phi),
    })
}

fn proposition<E: Debug, C: Fn(String) -> Result<usize, E>>(
    convert: &'static C,
) -> Parser<u8, Phi> {
    (sym(b'"') * identifier().convert(convert) - sym(b'"')).map(|id| Phi::Proposition(id))
}

fn not<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    (sym(b'!').discard() * space() * phi(convert)).map(|phi| Phi::Not(Arc::new(phi)))
}

fn or<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    (phi(convert) - space() - sym(b'|') - space() + phi(convert))
        .map(|(l, r)| Phi::And(Arc::new(l), Arc::new(r)))
}

fn and<E: Debug, C: Fn(String) -> Result<usize, E>>(convert: &'static C) -> Parser<u8, Phi> {
    let parser = phi(convert) - space() - sym(b'&') - space() + phi(convert);
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
    list(identifier().convert(convert), space() * sym(b',') * space())
}

fn identifier() -> Parser<u8, String> {
    let parser = alpha() + (alpha() | num()).repeat(0..);
    parser.convert(|(s1, s2)| -> Result<String, FromUtf8Error> {
        Ok(vec![String::from_utf8(vec![s1])?, String::from_utf8(s2)?].concat())
    })
}

fn alpha() -> Parser<u8, u8> {
    one_of(b"abcdefghijklmnopqrstuvwxyz") | one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

fn num() -> Parser<u8, u8> {
    one_of(b"0123456789")
}

#[cfg(test)]
mod test {
    use crate::atl::formula::parser::{boolean, identifier, not, StrToId};
    use crate::atl::formula::Phi;
    use std::convert::identity;
    use std::fmt::{Debug, Formatter};
    use std::sync::Arc;

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
    fn not_1() {
        assert_eq!(
            not(&convert).parse(b"!true"),
            Ok(Phi::Not(Arc::new(Phi::True)))
        )
    }

    #[test]
    fn not_2() {
        assert!(not(&convert).parse(b"! true").is_err())
    }
}
