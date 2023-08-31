use crate::parsing::ast::{BinaryOpKind, Coalition, CoalitionKind, Expr, ExprKind, UnaryOpKind};
use crate::parsing::errors::ErrorLog;
use crate::parsing::lexer::Lexer;
use crate::parsing::parser::ParseError::Unexpected;
use crate::parsing::span::Span;
use crate::parsing::token::{Token, TokenKind};
use std::iter::Peekable;
use std::sync::Arc;

const SYNC_TOKEN_HIERARCHY: [TokenKind; 5] = [
    TokenKind::Rrbracket,
    TokenKind::Rrangle,
    TokenKind::Rparen,
    TokenKind::Semi,
    TokenKind::KwEndTemplate,
];

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    pub errors: ErrorLog,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser {
        Parser {
            lexer: lexer.peekable(),
            errors: ErrorLog::new(),
        }
    }

    pub fn expr(&mut self, min_prec: u8) -> Expr {
        // Pratt parsing/precedence climbing: https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
        let mut lhs = self.term();
        let span_start = lhs.span;
        loop {
            let Some(op): Option<BinaryOpKind> = self.lexer.peek().and_then(|t| (*t.kind()).try_into().ok()) else { return lhs };
            if op.precedence() < min_prec {
                return lhs;
            }
            self.lexer.next();
            let new_prec = op.precedence() + if op.is_right_associative() { 0 } else { 1 };
            let rhs = self.expr(new_prec);
            let span = span_start + rhs.span;
            let kind = ExprKind::Binary(op, lhs.into(), rhs.into());
            lhs = Expr::new(span, kind);
        }
    }

    pub fn path_expr(&mut self) -> Expr {
        match self.lexer.peek().map(|t| t.kind()) {
            Some(TokenKind::Lparen) => {
                let begin = self.lexer.next().unwrap().span;
                let lhs = self.expr(BinaryOpKind::Until.precedence());
                let Ok(_) = self.expect_and_consume(TokenKind::Word("U".to_string())) else {
                    return Expr::new_error();
                };
                let rhs = self.expr(0);
                let end = self.expect_and_consume(TokenKind::Rparen).unwrap();
                Expr::new(begin + end, ExprKind::Binary(BinaryOpKind::Until, lhs.into(), rhs.into()))
            },
            Some(TokenKind::Word(w)) if w == "F" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr(0);
                Expr::new(begin + expr.span, ExprKind::Unary(UnaryOpKind::Eventually, expr.into()))
            },

            Some(TokenKind::Word(w)) if w == "G" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr(0);
                Expr::new(begin + expr.span, ExprKind::Unary(UnaryOpKind::Invariantly, expr.into()))
            },
            // Unexpected
            Some(t) => Err(Unexpected(self.lexer.next())),
            None => Err(Unexpected(None)),
        }
    }

    pub fn term(&mut self) -> Expr {
        match self.lexer.peek().map(|t| t.kind()) {
            Some(TokenKind::Lparen) => self.paren(),
            Some(TokenKind::True) => {
                let tok = self.lexer.next().unwrap();
                Expr::new(tok.span, ExprKind::True)
            }
            Some(TokenKind::False) => {
                let tok = self.lexer.next().unwrap();
                Expr::new(tok.span, ExprKind::False)
            }
            Some(TokenKind::Word(_)) => self.ident(),
            Some(TokenKind::Llangle) => self.enforce_coalition(),
            Some(TokenKind::Llbracket) => self.despite_coalition(),
            // Unexpected
            Some(t) => Err(Unexpected(self.lexer.next())),
            None => Err(Unexpected(None)),
        }
    }

    pub fn paren(&mut self) -> Expr {
        let Ok(begin) = self.expect_and_consume(TokenKind::Llangle) else {
            return Expr::new_error();
        };
        let expr = self.expr(0);
        let Ok(end) = self.expect_and_consume(TokenKind::Rrangle) else {
            return Expr::new_error();
        };
        Expr::new(begin + end, ExprKind::Paren(Arc::new(expr)))
    }

    pub fn ident(&mut self) -> Expr {
        let tok = self.lexer.next().unwrap();
        Expr::new(tok.span, ExprKind::Ident(tok.kind.to_string()))
    }

    pub fn enforce_coalition(&mut self) -> Expr {
        let Ok(coal_begin) = self.expect_and_consume(TokenKind::Llangle) else {
            return Expr::new_error();
        };
        let players = self.players();
        let Ok(coal_end) = self.expect_and_consume(TokenKind::Rrangle) else {
            return Expr::new_error();
        };
        let expr = self.path_expr();
        Expr::new(
            coal_begin + expr.span,
            ExprKind::Coalition(Coalition::new(
                coal_begin + coal_end,
                players,
                CoalitionKind::Enforce,
                Arc::new(expr),
            )),
        )
    }

    pub fn despite_coalition(&mut self) -> Expr {
        let Ok(coal_begin) = self.expect_and_consume(TokenKind::Llbracket) else {
            return Expr::new_error();
        };
        let players = self.players();
        let Ok(coal_end) = self.expect_and_consume(TokenKind::Rrbracket) else {
            return Expr::new_error();
        };
        let expr = self.path_expr();
        Expr::new(
            coal_begin + expr.span,
            ExprKind::Coalition(Coalition::new(
                coal_begin + coal_end,
                players,
                CoalitionKind::Despite,
                Arc::new(expr),
            )),
        )
    }

    pub fn players(&mut self) -> Vec<Expr> {
        let mut players = vec![];
        loop {
            match self.lexer.peek().map(|t| t.kind()) {
                Some(TokenKind::Word(w)) => {
                    let tok = self.lexer.next().unwrap();
                    let p = Expr::new(tok.span, ExprKind::Ident(w.to_string()));
                    players.push(p);
                }
                _ => break,
            }
            if self.next_is(TokenKind::Comma) {
                self.lexer.next();
            } else {
                break;
            }
        }
        players
    }

    fn next_is(&mut self, kind: TokenKind) -> bool {
        matches!(self.lexer.peek().map(|t| t.kind()), Some(k) if k == &kind)
    }

    fn expect_and_consume(&mut self, kind: TokenKind) -> Result<Span, ParseError> {
        match self.lexer.peek().map(|t| t.kind()) {
            Some(k) if k == &kind => Ok(self.lexer.next().unwrap().span),
            Some(_) => {
                let tok = self.lexer.next().unwrap();
                self.errors.log(
                    tok.span,
                    format!("Expected '{}', found '{}'", kind, tok.kind),
                );
                Err(Unexpected(Some(tok)))
            }
            None => {
                self.errors
                    .log_msg(format!("Expected '{}', found EOF", kind));
                Err(Unexpected(self.lexer.peek().cloned()))
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    Unexpected(Option<Token>),
}
