extern crate pom;
use pom::parser::*;
use self::pom::set::Set;
use std::str::{self, FromStr};
use std::ops::Add;
use std::rc::Rc;
use crate::lcgs::ast::{Expr, ExprKind, Identifier};
use std::collections::HashMap;
use crate::lcgs::ast::BinaryOpKind::{Addition, Multiplication, Subtraction, Division};
use crate::lcgs::ast::ExprKind::{Number, BinaryOp, Ident};

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

#[inline]
fn alpha() -> Parser<'static, u8, u8> {
    one_of(b"abcdefghijklmnopqrstuvwxyz") | one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

#[inline]
fn digit() -> Parser<'static, u8, u8> {
    one_of(b"0123456789")
}

#[inline]
fn non_0_digit() -> Parser<'static, u8, u8> {
    one_of(b"123456789")
}

fn space() -> Parser<'static, u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

fn number() -> Parser<'static, u8, Expr> {
    let integer = non_0_digit() - digit().repeat(0..) | sym(b'0');
    let parsed = integer.collect().convert(str::from_utf8).convert(i32::from_str);
    parsed.with_span().map(|(_span, v)| Expr { kind: Number(v) })
}

fn name() -> Parser<'static, u8, String> {
    let chars = alpha() - (alpha() | digit() | sym(b'_')).repeat(0..);
    chars.collect().convert(|s|String::from_utf8(s.to_vec()))
}

fn identifier() -> Parser<'static, u8, Identifier> {
    let identifier = (name() - sym(b'.')).opt() + name();
    identifier.with_span().map(|(_span, (owner, name))| Identifier { owner, name })
}

fn expr() -> Parser<'static, u8, Expr> {
    // TODO Spans and combining them
    let sum = term() + (space() * one_of(b"+-") - space() + term()).repeat(0..);
    sum.map(|(e1, mut e2s)| e2s.drain(..).fold(e1, |a, (op, b)| {
        let op_kind = match op {
            b'+' => Addition,
            b'-' => Subtraction,
            _ => panic!(),
        };
        Expr { kind: BinaryOp(op_kind, Rc::from(a), Rc::from(b)) }
    }))
}

fn term() -> Parser<'static, u8, Expr> {
    // TODO Spans and combining them
    let product = factor() + (space() * one_of(b"*/") - space() + factor()).repeat(0..);
    product.map(|(e1, mut e2s)| e2s.drain(..).fold(e1, |a, (op, b)| {
        let op_kind = match op {
            b'*' => Multiplication,
            b'/' => Division,
            _ => panic!(),
        };
        Expr { kind: BinaryOp(op_kind, Rc::from(a), Rc::from(b)) }
    }))
}

fn factor() -> Parser<'static, u8, Expr> {
    number()
        | identifier().map(|i| Expr { kind: Ident(Rc::from(i))})
        | (sym(b'(') * space() * call(expr) - space() - sym(b')'))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ident_01() {
        let input = br"abc_ident_1";
        let parser = identifier();
        assert_eq!(parser.parse(input), Ok(Identifier {
            owner: None,
            name: "abc_ident_1".into()
        }));
    }

    #[test]
    fn test_ident_02() {
        // Can't start with a digit
        let input = br"123abc";
        let parser = identifier();
        assert!(parser.parse(input).is_err())
    }

    #[test]
    fn test_ident_03() {
        let input = br"player.variable";
        let parser = identifier();
        assert_eq!(parser.parse(input), Ok(Identifier {
            owner: Some("player".into()),
            name: "variable".into()
        }));
    }

    #[test]
    fn test_add_01() {
        let input = br"1 + 2";
        let parser = expr();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Addition,
                Rc::from(Expr { kind: Number(1) }),
                Rc::from(Expr { kind: Number(2) }))
        }));
    }

    #[test]
    fn test_add_02() {
        let input = br"1 + 2 + 3";
        let parser = expr();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Addition,
                Rc::from(Expr {
                    kind: BinaryOp(
                        Addition,
                        Rc::from(Expr { kind: Number(1) }),
                        Rc::from(Expr { kind: Number(2) })
                    )
                }),
                Rc::from(Expr { kind: Number(3) })
            )
        }));
    }

    #[test]
    fn test_sub_01() {
        let input = br"1 - 2 - 3";
        let parser = expr();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Subtraction,
                Rc::from(Expr {
                    kind: BinaryOp(
                        Subtraction,
                        Rc::from(Expr { kind: Number(1) }),
                        Rc::from(Expr { kind: Number(2) })
                    )
                }),
                Rc::from(Expr { kind: Number(3) })
            )
        }));
    }

    #[test]
    fn test_mul_01() {
        let input = br"1 * 2";
        let parser = term();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Multiplication,
                Rc::from(Expr { kind: Number(1) }),
                Rc::from(Expr { kind: Number(2) }))
        }));
    }

    #[test]
    fn test_mul_02() {
        let input = br"1 * 2 * 3";
        let parser = term();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Multiplication,
                Rc::from(Expr {
                    kind: BinaryOp(
                        Multiplication,
                        Rc::from(Expr { kind: Number(1) }),
                        Rc::from(Expr { kind: Number(2) })
                    )
                }),
                Rc::from(Expr { kind: Number(3) })
            )
        }));
    }

    #[test]
    fn test_div_01() {
        let input = br"1 / 2 / 3";
        let parser = term();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Division,
                Rc::from(Expr {
                    kind: BinaryOp(
                        Division,
                        Rc::from(Expr { kind: Number(1) }),
                        Rc::from(Expr { kind: Number(2) })
                    )
                }),
                Rc::from(Expr { kind: Number(3) })
            )
        }));
    }

    #[test]
    fn test_par_01() {
        let input = br"(1 + 2) * 3";
        let parser = expr();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Multiplication,
                Rc::from(Expr {
                    kind: BinaryOp(
                        Addition,
                        Rc::from(Expr { kind: Number(1) }),
                        Rc::from(Expr { kind: Number(2) })
                    )
                }),
                Rc::from(Expr { kind: Number(3) }),
            )
        }));
    }

    #[test]
    fn test_add_mul_precedence_01() {
        let input = br"1 + 2 * 3";
        let parser = expr();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Addition,
                Rc::from(Expr { kind: Number(1) }),
                Rc::from(Expr {
                    kind: BinaryOp(
                        Multiplication,
                        Rc::from(Expr { kind: Number(2) }),
                        Rc::from(Expr { kind: Number(3) })
                    )
                }),
            )
        }));
    }

    #[test]
    fn test_add_mul_precedence_02() {
        let input = br"1 * 2 + 3 * 4 + 5";
        let parser = expr();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Addition,
                Rc::from(Expr {
                    kind: BinaryOp(
                        Addition,
                        Rc::from(Expr {
                            kind: BinaryOp(
                                Multiplication,
                                Rc::from(Expr { kind: Number(1) }),
                                Rc::from(Expr { kind: Number(2) })
                            )
                        }),
                        Rc::from(Expr {
                            kind: BinaryOp(
                                Multiplication,
                                Rc::from(Expr { kind: Number(3) }),
                                Rc::from(Expr { kind: Number(4) })
                            )
                        })
                    )
                }),
                Rc::from(Expr { kind: Number(5) })
            )
        }));
    }

    #[test]
    fn test_precedence_01() {
        let input = br"1 * (2 + 3) / 4 + 5";
        let parser = expr();
        assert_eq!(parser.parse(input), Ok(Expr {
            kind: BinaryOp(
                Addition,
                Rc::from(Expr {
                    kind: BinaryOp(
                        Division,
                        Rc::from(Expr {
                            kind: BinaryOp(
                                Multiplication,
                                Rc::from(Expr { kind: Number(1) }),
                                Rc::from(Expr {
                                    kind: BinaryOp(
                                        Addition,
                                        Rc::from(Expr { kind: Number(2) }),
                                        Rc::from(Expr { kind: Number(3) }),
                                    )
                                }),
                            )
                        }),
                        Rc::from(Expr { kind: Number(4) }),
                    )
                }),
                Rc::from(Expr { kind: Number(5) })
            )
        }));
    }
}
