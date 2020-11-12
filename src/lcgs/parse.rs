extern crate pom;

use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::Add;
use std::rc::Rc;
use std::str::{self, FromStr};
use std::vec::Drain;

use pom::parser::*;

use crate::lcgs::ast::BinaryOpKind::{Addition, Division, Multiplication, Subtraction};
use crate::lcgs::ast::ExprKind::{BinaryOp, Ident, Number};
use crate::lcgs::ast::{BinaryOpKind, Expr, ExprKind, Identifier, StateVarDecl, TypeRange};
use crate::lcgs::precedence::Associativity::RightToLeft;
use crate::lcgs::precedence::{precedence, Precedence};

use self::pom::set::Set;

/// A `Span` describes the position of a slice of text in the original program.
/// Usually used to describe what text an AST node was created from.
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
struct Span {
    begin: usize,
    end: usize,
}

trait WithSpan<'a, I, O: 'a> {
    fn with_span(self) -> Parser<'a, I, (Span, O)>;
}

impl<'a, I, O: 'a> WithSpan<'a, I, O> for Parser<'a, I, O> {
    /// Make the parser note the beginning and end position and
    /// include it in the result as a `Span`
    fn with_span(self) -> Parser<'a, I, (Span, O)> {
        (empty().pos() + self + empty().pos())
            .map(|((begin, item), end)| (Span { begin, end }, item))
    }
}

/// Parser to parse an ASCII alphabet character
#[inline]
fn alpha() -> Parser<'static, u8, u8> {
    one_of(b"abcdefghijklmnopqrstuvwxyz") | one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

/// Parser that parses a single digit 0-9
#[inline]
fn digit() -> Parser<'static, u8, u8> {
    one_of(b"0123456789")
}

/// Parser that parses a single digit 1-9 (not 0)
#[inline]
fn non_0_digit() -> Parser<'static, u8, u8> {
    one_of(b"123456789")
}

/// Parser that parses 0 or more whitespace characters discards them. Include newlines and tabs.
fn space() -> Parser<'static, u8, ()> {
    one_of(b" \t\r\n").repeat(0..).discard()
}

/// Parser that parses a typical positive integer number
fn number() -> Parser<'static, u8, Expr> {
    let integer = non_0_digit() - digit().repeat(0..) | sym(b'0');
    let parsed = integer
        .collect()
        .convert(str::from_utf8)
        .convert(i32::from_str);
    parsed
        .with_span()
        .map(|(_span, v)| Expr { kind: Number(v) })
}

/// Parser that parses a symbol name. It must start with an alpha character, but subsequent
/// characters can be digits or "_" too.
fn name() -> Parser<'static, u8, String> {
    let chars = alpha() - (alpha() | digit() | sym(b'_')).repeat(0..);
    chars.collect().convert(|s| String::from_utf8(s.to_vec()))
}

/// Parser that parses a name with an optional owner and returns an `Identifier`.
/// I.e. "health" or "p1.health"
fn identifier() -> Parser<'static, u8, Identifier> {
    let identifier = (name() - sym(b'.')).opt() + name();
    identifier
        .with_span()
        .map(|(_span, (owner, name))| Identifier { owner, name })
}

/// Parser that parses binary operators
fn binop() -> Parser<'static, u8, BinaryOpKind> {
    one_of(b"+-*/").map(BinaryOpKind::from)
}

/// Combine a list of expressions and binary operators to a single `Expr` with correct
/// precedence and associativity. See https://en.wikipedia.org/wiki/Operator-precedence_parser
fn solve_binary_precedence(
    mut lhs: Expr,
    prec_min: i32,
    es: &mut Peekable<Drain<(BinaryOpKind, Expr)>>,
) -> Expr {
    // Peek at next operator. If it has a greater precedence that prec_min we take it
    while let Some((op, _)) = es.peek() {
        if prec_min <= precedence(op).0 {
            let (op, mut rhs) = es.next().unwrap();
            let Precedence(op_prec, _) = precedence(&op);
            // Peek at the next operator. If has an even greater precedence or similar precedence
            // and right-associative, then our rhs can consist of multiple expressions
            let mut next = es.peek();
            while let Some((op2, _)) = &next {
                let Precedence(op2_prec, op2_ass) = precedence(&op2);
                if op_prec < op2_prec || (op_prec == op2_prec && op2_ass == RightToLeft) {
                    // Built right hand side using recursion
                    rhs = solve_binary_precedence(rhs, op2_prec, es);
                    next = es.peek();
                } else {
                    break;
                }
            }
            // Combine lhs and rhs using the given binary operator
            lhs = Expr {
                kind: BinaryOp(op, Rc::from(lhs), Rc::from(rhs)),
            }
        } else {
            break;
        }
    }
    lhs
}

/// Parser that parses an expression consisting of binary operators and primary expressions
fn expr() -> Parser<'static, u8, Expr> {
    // TODO Spans and combining them
    let binexpr = primary() + (space() * binop() - space() + primary()).repeat(0..);
    binexpr.map(|(e, mut es)| solve_binary_precedence(e, 0, &mut es.drain(..).peekable()))
}

/// Parser that parses an primary expression, i.e. number, identifier, or a parenthesised expression
fn primary() -> Parser<'static, u8, Expr> {
    number()
        | identifier().map(|i| Expr {
            kind: Ident(Rc::from(i)),
        })
        | (sym(b'(') * space() * call(expr) - space() - sym(b')'))
}

/// Parser that parses a type range, e.g. "`[0 .. max_health]`"
fn type_range() -> Parser<'static, u8, TypeRange> {
    let inner = expr() - space() - seq(b"..") - space() + expr();
    let bracked = sym(b'[') * space() * inner - space() - sym(b']');
    bracked.map(|(min, max)| TypeRange { min, max })
}

/// Parser that parses a variable, e.g.
/// "`health : [0 .. max_health] init max_health`"
fn var_decl() -> Parser<'static, u8, StateVarDecl> {
    let base = name() - space() - sym(b':') - space() + type_range();
    let init = seq(b"init") * space() * expr();
    let whole = base - space() + init;
    whole.map(|((name, range), init_e)| StateVarDecl {
        name,
        range,
        initial_value: init_e,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env::var;

    #[test]
    fn test_ident_01() {
        // Should be a valid identifier
        let input = br"abc_ident_1";
        let parser = identifier();
        assert_eq!(
            parser.parse(input),
            Ok(Identifier {
                owner: None,
                name: "abc_ident_1".into()
            })
        );
    }

    #[test]
    fn test_ident_02() {
        // Identifier can't start with a digit
        let input = br"123abc";
        let parser = identifier();
        assert!(parser.parse(input).is_err())
    }

    #[test]
    fn test_ident_03() {
        // Should be a valid identifier with owner
        let input = br"player.variable";
        let parser = identifier();
        assert_eq!(
            parser.parse(input),
            Ok(Identifier {
                owner: Some("player".into()),
                name: "variable".into()
            })
        );
    }

    #[test]
    fn test_add_01() {
        // Basic addition
        let input = br"1 + 2";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
                kind: BinaryOp(
                    Addition,
                    Rc::from(Expr { kind: Number(1) }),
                    Rc::from(Expr { kind: Number(2) })
                )
            })
        );
    }

    #[test]
    fn test_add_02() {
        // Basic addition with three numbers
        let input = br"1 + 2 + 3";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
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
            })
        );
    }

    #[test]
    fn test_sub_01() {
        // Basic subtraction with three numbers
        let input = br"1 - 2 - 3";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
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
            })
        );
    }

    #[test]
    fn test_mul_01() {
        // Basic multiplications
        let input = br"1 * 2";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
                kind: BinaryOp(
                    Multiplication,
                    Rc::from(Expr { kind: Number(1) }),
                    Rc::from(Expr { kind: Number(2) })
                )
            })
        );
    }

    #[test]
    fn test_mul_02() {
        // Basic multiplications with three numbers
        let input = br"1 * 2 * 3";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
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
            })
        );
    }

    #[test]
    fn test_div_01() {
        // Basic division with three numbers
        let input = br"1 / 2 / 3";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
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
            })
        );
    }

    #[test]
    fn test_par_01() {
        // Parentheses should break precedence
        let input = br"(1 + 2) * 3";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
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
            })
        );
    }

    #[test]
    fn test_add_mul_precedence_01() {
        // Precedence between addition and multiplication
        let input = br"1 + 2 * 3";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
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
            })
        );
    }

    #[test]
    fn test_add_mul_precedence_02() {
        // Precedence between addition and multiplication
        let input = br"1 * 2 + 3 * 4 + 5";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
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
            })
        );
    }

    #[test]
    fn test_precedence_01() {
        // Precedence between mathematical operators
        let input = br"1 * (2 + 3) / 4 + 5";
        let parser = expr();
        assert_eq!(
            parser.parse(input),
            Ok(Expr {
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
            })
        );
    }

    #[test]
    fn test_type_range_01() {
        // Simple type range
        let input = br"[0..20]";
        let parser = type_range();
        assert_eq!(
            parser.parse(input),
            Ok(TypeRange {
                min: Expr { kind: Number(0) },
                max: Expr { kind: Number(20) },
            })
        );
    }

    #[test]
    fn test_type_range_02() {
        // Simple type range with spaces inside
        let input = br"[   0  ..  20  ]";
        let parser = type_range();
        assert_eq!(
            parser.parse(input),
            Ok(TypeRange {
                min: Expr { kind: Number(0) },
                max: Expr { kind: Number(20) },
            })
        );
    }

    #[test]
    fn test_var_decl_01() {
        // Simple var decl
        let input = br"health : [0 .. max_health] init max_health";
        let parser = var_decl();
        assert_eq!(
            parser.parse(input),
            Ok(StateVarDecl {
                name: "health".to_string(),
                range: TypeRange {
                    min: Expr { kind: Number(0) },
                    max: Expr {
                        kind: Ident(Rc::from(Identifier {
                            owner: None,
                            name: "max_health".to_string()
                        }))
                    },
                },
                initial_value: Expr {
                    kind: Ident(Rc::from(Identifier {
                        owner: None,
                        name: "max_health".to_string()
                    }))
                }
            })
        );
    }
}
