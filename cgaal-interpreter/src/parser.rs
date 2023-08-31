use crate::lexer::{CGAALLexer, Lexer};
use crate::op::Op;
use crate::token::Token;
use cgaal_engine::game_structure::{Action, Player, Proposition};
use std::fmt;
use std::fmt::Formatter;

/// All the interesting types are implemented as usize.The TypedValue enum is provided
/// to help keep track of the types of the indexes.
#[derive(Clone)]
pub enum TypedValue {
    Action(Option<String>, Option<Action>),
    Player(Option<String>, Option<Player>),
    Label(Option<String>, Option<Proposition>),
}

impl TypedValue {
    /// Check if there exists a identifier for the TypedValue
    pub fn has_name(&self) -> bool {
        match self {
            TypedValue::Action(s, _) | TypedValue::Player(s, _) | TypedValue::Label(s, _) => {
                s.is_some()
            }
        }
    }

    /// Checks if there exists a index for the TypedValue
    pub fn has_value(&self) -> bool {
        match self {
            TypedValue::Action(_, i) | TypedValue::Player(_, i) | TypedValue::Label(_, i) => {
                i.is_some()
            }
        }
    }
}

impl fmt::Display for TypedValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypedValue::Action(_, _) => {
                write!(f, "Action")
            }
            TypedValue::Player(_, _) => {
                write!(f, "Player")
            }
            TypedValue::Label(_, _) => {
                write!(f, "Label")
            }
        }
    }
}

pub trait Parser {
    /// Parse a string and return a Operation
    fn parse(&mut self, input: &str) -> Result<Op, String>;
}

pub struct CGAALParser {
    lexer: CGAALLexer,
}

impl Parser for CGAALParser {
    fn parse(&mut self, input: &str) -> Result<Op, String> {
        self.lexer = CGAALLexer::new(input);
        self.lexer.advance();

        match self.lexer.curr_token() {
            Token::Eof => Ok(Op::Skip),
            Token::Start => Ok(self.start()),
            Token::Display => self.display(),
            _ => Err(format!(
                "Expected a valid command, the buffer is: {}, and Token is: {}",
                self.lexer.get_value(),
                self.lexer.curr_token()
            )),
        }
    }
}

impl CGAALParser {
    /// Initiate the Parser with an empty Lexer
    pub fn new() -> Self {
        Self {
            lexer: CGAALLexer::new(""),
        }
    }

    /// The start token returns a Skip operation
    fn start(&mut self) -> Op {
        Op::Skip
    }

    fn display(&mut self) -> Result<Op, String> {
        self.lexer.advance();
        match self.lexer.curr_token() {
            Token::Labels => Ok(Op::DisplayAll {
                value: TypedValue::Label(None, None),
            }),
            Token::Players => Ok(Op::DisplayAll {
                value: TypedValue::Player(None, None),
            }),
            Token::Player => {
                self.lexer.advance();
                match self.player() {
                    Ok(player) => Ok(Op::Display { value: player }),
                    Err(err) => Err(err),
                }
            }
            _ => Err(format!(
                "Unexpected token! Can't display: {}",
                self.lexer.curr_token()
            )),
        }
    }

    /// Returns a TypedValue::Player depending on the next token the players name or index is filled
    /// into the TypedValue.
    fn player(&mut self) -> Result<TypedValue, String> {
        if Token::Number == self.lexer.curr_token() {
            match self.number() {
                Ok(number) => Ok(TypedValue::Player(None, Some(number))),
                Err(err) => Err(err),
            }
        } else if Token::Identifier == self.lexer.curr_token() {
            Ok(TypedValue::Player(Some(self.identifier()), None))
        } else {
            Err(format!("Unexpected token: {}", self.lexer.curr_token()))
        }
    }

    /// Tries to parse the buffer as an usize if it succeeds the given number is returned
    fn number(&mut self) -> Result<usize, String> {
        match self.lexer.get_value().parse::<usize>() {
            Ok(value) => Ok(value),
            Err(err) => Err(format!("Couldn't parse the integer: {}", err)),
        }
    }

    /// Returns the value of the buffer as a string
    fn identifier(&mut self) -> String {
        self.lexer.get_value()
    }
}
