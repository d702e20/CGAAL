extern crate pom;
use pom::parser::*;
use self::pom::set::Set;
use std::str::{self, FromStr};
use std::ops::Add;

fn space() -> Parser<'static, u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
struct Span {
    begin: usize,
    end: usize,
}

trait WithSpan<'a, I, O: 'a> {
    fn with_span(self) -> Parser<'a, I, (Span, O)>;
}

impl<'a, I, O: 'a> WithSpan<'a, I, O> for Parser<'a, I, O> {
    fn with_span(self) -> Parser<'a, I, (Span, O)> {
        (empty().pos() + self + empty().pos())
            .map(|((begin, item), end)| (Span { begin, end }, item))
    }
}

fn number() -> Parser<'static, u8, i32> {
    let integer = one_of(b"123456789") - one_of(b"0123456789").repeat(0..) | sym(b'0');
    integer.collect().convert(str::from_utf8).convert(i32::from_str)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let input = br"   123  ";
        let parser = space() * number().with_span() - space();
        assert_eq!(parser.parse(input), Ok((Span { begin: 3, end: 6 }, 123 )));
    }
}