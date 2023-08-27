mod errors;
mod lexer;
pub mod span;
mod token;

use crate::parsing::errors::ErrorLog;
use crate::parsing::span::Span;
use pom::parser::*;
use std::cell::RefCell;
use std::fmt::Display;
use crate::atl::{identifier, number};

/// A trait that allows us to extent parser with a helper function that extracts the span of
/// the parsed piece of text
pub trait WithSpan<'a, I, O: 'a> {
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

/// The `ParseState` is a struct carried around during parsing containing extra information
/// about the state of the parser, such as the errors encountered
#[derive(Debug, Default)]
pub struct ParseState<I> {
    pub errors: RefCell<ErrorLog>,
    sync_tokens: RefCell<Vec<I>>,
}

/// Create a lazy parser used for recursive definitions.
/// This function is similar to `call` but allows passing one argument to the parser function.
#[allow(unused)]
pub(crate) fn call1<'a, I, O, A, P: Fn(&'a A) -> Parser<I, O>>(
    parser: &'a P,
    arg: &'a A,
) -> Parser<'a, I, O> {
    Parser::new(move |input: &'_ [I], start: usize| (parser(arg).method)(input, start))
}

/// Create a lazy parser used for recursive definitions.
/// This function is similar to `call` but allows passing two arguments to the parser function.
pub(crate) fn call2<'a, I, O, A, B, P: Fn(&'a A, &'a B) -> Parser<'a, I, O>>(
    parser: &'a P,
    arg1: &'a A,
    arg2: &'a B,
) -> Parser<'a, I, O> {
    Parser::new(move |input: &'_ [I], start: usize| (parser(arg1, arg2).method)(input, start))
}

/// Creates a parser that will run the given parser.
/// If the given parser fails to parse, an error is reported using the given error message,
/// and parsing will continue with no input consumed.
#[allow(unused)]
pub(crate) fn parse_or_skip<'a, I: Eq + Display, O: 'a>(
    parse_state: &'a ParseState<I>,
    parser: Parser<'a, I, O>,
    err_msg: &'a str,
) -> Parser<'a, I, Option<O>> {
    Parser::new(
        move |input: &'_ [I], start: usize| match (parser.method)(input, start) {
            Ok((out, pos)) => Ok((Some(out), pos)),
            Err(_) => {
                let span = Span::new(start, start + 1);
                parse_state
                    .errors
                    .borrow_mut()
                    .log(span, err_msg.to_string());
                Ok((None, start))
            }
        },
    )
}

/// Creates a parser that will run the given parser.
/// If the given parser fails to parse, an error is reported using the given error message,
/// and parsing will continue at EOF.
#[allow(unused)]
pub(crate) fn parse_or_abort<'a, I: Eq + Display, O: 'a>(
    parse_state: &'a ParseState<I>,
    parser: Parser<'a, I, O>,
    err_msg: &'a str,
) -> Parser<'a, I, Option<O>> {
    Parser::new(
        move |input: &'_ [I], start: usize| match (parser.method)(input, start) {
            Ok((out, pos)) => Ok((Some(out), pos)),
            Err(_) => {
                let span = Span::new(start, input.len());
                parse_state
                    .errors
                    .borrow_mut()
                    .log(span, err_msg.to_string());
                Ok((None, input.len()))
            }
        },
    )
}

pub(crate) fn recover<'a, I: Eq, O: 'a>(state: &'a ParseState<I>) -> Parser<'a, I, Result<(), Span>> {
    Parser::new(|input: &'_ [I], start: usize| {
        let sync_pos = input[start..]
            .iter()
            .position(|c| state.sync_tokens.borrow().contains(c))
            .map(|p| p + start)
            .unwrap_or(input.len());
        let span = Span::new(start, sync_pos);
        Ok((Err(span), sync_pos))
    })
}

pub(crate) fn unexpected_abort<'a, O: 'a + Clone>(state: &'a ParseState<u8>, res: O) -> Parser<'a, u8, O> {
    (identifier().discard() | number().discard() | any().discard()).with_span()
        .map(move |(span, _)| {
            state.errors.borrow_mut().log(span, "Unexpected token".to_string());
            res.clone()
        }) - any().repeat(..)
}

/// Creates a parser that will run the given parser and then consume the synchronization token.
/// If the given parser fails to parse, an error is reported using the given error message,
/// and parsing will continue at the next occurrence of the synchronization token.
/// The resulting Err will contained the skipped span.
#[allow(unused)]
pub(crate) fn parse_or_sync<'a, I: Copy + Eq + Display, O: 'a>(
    parse_state: &'a ParseState<I>,
    parser: Parser<'a, I, O>,
    sync: I,
    err_msg: &'a str,
) -> Parser<'a, I, Result<O, Span>> {
    Parser::new(move |input: &'_ [I], start: usize| {
        parse_state.sync_tokens.borrow_mut().push(sync);
        let res = match (parser.method)(input, start) {
            Ok((out, pos)) => {
                if input.get(pos) == Some(&sync) {
                    Ok((Ok(out), pos + 1))
                } else {
                    let span = Span::new(pos, pos + 1);
                    parse_state
                        .errors
                        .borrow_mut()
                        .log(span, format!("Missing '{}'", sync));
                    Ok((Ok(out), pos))
                }
            }
            Err(_) => {
                let sync_pos = input[start..]
                    .iter()
                    .position(|c| c == &sync)
                    .map(|p| p + start + 1)
                    .or(input[start..]
                        .iter()
                        .position(|c| parse_state.sync_tokens.borrow().contains(c))
                        .map(|p| p + start))
                    .unwrap_or(input.len());
                let span = Span::new(start, sync_pos);
                parse_state
                    .errors
                    .borrow_mut()
                    .log(span, err_msg.to_string());
                Ok((Err(span), sync_pos))
            }
        };
        parse_state.sync_tokens.borrow_mut().pop();
        res
    })
}

#[cfg(test)]
mod test {
    use crate::parsing::{parse_or_abort, parse_or_skip, parse_or_sync, ParseState};
    use pom::parser::{end, seq, sym};
    use pom::set::Set;

    #[test]
    fn parse_or_skip_001() {
        let state = ParseState::default();
        let parser = (parse_or_skip(&state, seq(b"foo"), "error").map(|res| res.unwrap_or(b""))
            + seq(b"bar"))
        .convert(|(fst, snd)| String::from_utf8([fst, snd].concat().to_vec()))
            - end();

        // Input is expected string "foobar"
        let res = parser.parse(b"foobar").expect("Parser must not fail");
        assert_eq!("foobar", res);
        assert!(state.errors.borrow().is_empty());
    }

    #[test]
    fn parse_or_skip_002() {
        let state = ParseState::default();
        let parser = (parse_or_skip(&state, seq(b"foo"), "error").map(|res| res.unwrap_or(b""))
            + seq(b"bar"))
        .convert(|(fst, snd)| String::from_utf8([fst, snd].concat().to_vec()))
            - end();

        // Input is unexpected, but parser will continue
        let res = parser.parse(b"bar").expect("Parser must not fail");
        assert_eq!("bar", res);
        assert_eq!(1, state.errors.borrow().len());
    }

    #[test]
    fn parse_or_abort_001() {
        let state = ParseState::default();
        let parser = (parse_or_abort(&state, seq(b"foo"), "error").map(|res| res.unwrap_or(b""))
            + parse_or_abort(&state, seq(b"bar"), "error").map(|res| res.unwrap_or(b"")))
        .convert(|(fst, snd)| String::from_utf8([fst, snd].concat().to_vec()))
            - end();

        // Input is expected string "foobar"
        let res = parser.parse(b"foobar").expect("Parser must not fail");
        assert_eq!("foobar", res);
        assert!(state.errors.borrow().is_empty());
    }

    #[test]
    fn parse_or_abort_002() {
        let state = ParseState::default();
        let parser = (parse_or_abort(&state, seq(b"foo"), "error").map(|res| res.unwrap_or(b""))
            + parse_or_abort(&state, seq(b"bar"), "error").map(|res| res.unwrap_or(b"")))
        .convert(|(fst, snd)| String::from_utf8([fst, snd].concat().to_vec()))
            - end();

        // Input is unexpected, so parser will abort
        let res = parser.parse(b"goobar").expect("Parser must not fail");
        assert_eq!("", res);
        assert_eq!(2, state.errors.borrow().len());
    }

    #[test]
    fn parse_or_sync_001() {
        // parse_or_sync works on happy path
        let state = ParseState::default();
        let inner = seq(b"foo");
        let parser = sym(b'(') * parse_or_sync(&state, inner, b')', "error") - seq(b"ok") - end();

        let res = parser.parse(b"(foo)ok").expect("Parser must not fail");
        assert_eq!(b"foo", res.unwrap());
        assert!(state.errors.borrow().is_empty());
    }

    #[test]
    fn parse_or_sync_002() {
        // parse_or_sync synchronizes when inner parser fails
        let state = ParseState::default();
        let inner = seq(b"foo");
        let parser = sym(b'(') * parse_or_sync(&state, inner, b')', "error") - seq(b"ok") - end();

        let res = parser.parse(b"(bar)ok").expect("Parser must not fail");
        assert!(res.is_none());
        assert_eq!(1, state.errors.borrow().len());
    }

    #[test]
    fn parse_or_sync_003() {
        // nested parse_or_sync synchronizes when inner parser fails
        let state = ParseState::default();
        let inner = seq(b"foo");
        let middle = sym(b'(') * parse_or_sync(&state, inner, b')', "error");
        let parser = sym(b'(') * parse_or_sync(&state, middle, b')', "error") - seq(b"ok") - end();

        let res = parser.parse(b"((bar))ok").expect("Parser must not fail");
        assert!(res.is_some()); // Outer parser succeeds despite inner parser failing
        assert_eq!(1, state.errors.borrow().len());
    }

    #[test]
    fn parse_or_sync_004() {
        // nested parse_or_sync synchronizes at first sync token
        let state = ParseState::default();
        let inner = seq(b"foo");
        let middle = sym(b'[') * parse_or_sync(&state, inner, b']', "error");
        let parser = sym(b'(') * parse_or_sync(&state, middle, b')', "error") - seq(b"ok") - end();

        let res = parser.parse(b"([bar)ok").expect("Parser must not fail");
        assert!(res.is_some()); // Outer parser succeeds despite inner parser failing
        assert_eq!(1, state.errors.borrow().len());
    }
}
