use crate::parsing::ast::{BinaryOpKind, Coalition, CoalitionKind, Expr, ExprKind, UnaryOpKind};
use crate::parsing::errors::ErrorLog;
use crate::parsing::lexer::Lexer;
use crate::parsing::parser::ParseError::Unexpected;
use crate::parsing::span::Span;
use crate::parsing::token::{Token, TokenKind};
use std::iter::Peekable;
use std::sync::Arc;

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

    pub fn expect_end(&mut self) {
        match self.lexer.next() {
            None => {}
            Some(tok) => {
                self.errors
                    .log(tok.span, format!("Unexpected '{}', expected EOF", tok.kind));
            }
        }
        while let Some(_) = self.lexer.next() {}
    }

    pub fn expr(&mut self, min_prec: u8) -> Expr {
        // Pratt parsing/precedence climbing: https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
        let mut lhs = self.term();
        let span_start = lhs.span;
        loop {
            let Some(op): Option<BinaryOpKind> = self.lexer.peek().and_then(|t| t.kind().clone().try_into().ok()) else { return lhs };
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
                Expr::new(
                    begin + end,
                    ExprKind::Binary(BinaryOpKind::Until, lhs.into(), rhs.into()),
                )
            }
            Some(TokenKind::Word(w)) if w == "F" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr(0);
                Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Eventually, expr.into()),
                )
            }

            Some(TokenKind::Word(w)) if w == "G" => {
                let begin = self.lexer.next().unwrap().span;
                let expr = self.expr(0);
                Expr::new(
                    begin + expr.span,
                    ExprKind::Unary(UnaryOpKind::Invariantly, expr.into()),
                )
            }
            // Unexpected
            Some(_) => {
                let tok = self.lexer.next().unwrap();
                self.errors.log(
                    tok.span,
                    format!("Unexpected '{}', expected path expression", tok.kind),
                );
                Expr::new_error()
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected path expression".to_string());
                Expr::new_error()
            }
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
            Some(_) => {
                let tok = self.lexer.next().unwrap();
                self.errors.log(
                    tok.span,
                    format!("Unexpected '{}', expected expression term", tok.kind),
                );
                Expr::new_error()
            }
            None => {
                self.errors
                    .log_msg("Unexpected EOF, expected expression term".to_string());
                Expr::new_error()
            }
        }
    }

    pub fn paren(&mut self) -> Expr {
        let Ok(begin) = self.expect_and_consume(TokenKind::Lparen) else {
            return Expr::new_error();
        };
        let expr = self.expr(0);
        let Ok(end) = self.expect_consume_or_recover(TokenKind::Rparen) else {
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
            match self.lexer.peek() {
                Some(Token {
                    span,
                    kind: TokenKind::Word(w),
                }) => {
                    let p = Expr::new(span.clone(), ExprKind::Ident(w.to_string()));
                    players.push(p);
                    self.lexer.next().unwrap();
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
        matches!(self.lexer.peek(), Some(tok) if tok.kind == kind)
    }

    fn expect_and_consume(&mut self, kind: TokenKind) -> Result<Span, ParseError> {
        match self.lexer.next() {
            Some(tok) if tok.kind == kind => Ok(tok.span),
            Some(tok) => {
                self.errors.log(
                    tok.span,
                    format!("Unexpected '{}', expected '{}'", tok.kind, kind),
                );
                Err(Unexpected(Some(tok)))
            }
            None => {
                self.errors
                    .log_msg(format!("Unexpected EOF, expected '{}'", kind));
                Err(Unexpected(None))
            }
        }
    }

    fn expect_consume_or_recover(&mut self, kind: TokenKind) -> Result<Span, ParseError> {
        self.expect_and_consume(kind.clone()).or_else(|err| {
            while let Some(tok) = self.lexer.next() {
                if tok.kind == kind {
                    return Ok(tok.span);
                }
            }
            Err(err)
        })
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    Unexpected(Option<Token>),
}
