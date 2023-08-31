use phf::phf_map;
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, Eq, PartialEq)]
pub enum Token {
    Start,
    Eof,
    Eol,
    Identifier,
    Number,
    Move,
    Moves,
    Action,
    Actions,
    Players,
    Player,
    Display,
    Labels,
    //    Transitions,
    //    Moves,
    //    State,
}

/// Static map from their respective symbols to their corresponding Token.
pub static TOKENS: phf::Map<&'static str, Token> = phf_map! {
    "TOKEN::START" => Token::Start,
    "TOKEN::EOF" => Token::Eof,
    "TOKEN::EOL" => Token::Eol,
    "TOKEN::IDENTIFIER" => Token::Identifier,
    "TOKEN::NUMBER" => Token::Number,
    "move" => Token::Move,
    "moves" => Token::Moves,
    "number" => Token::Number,
    "players" => Token::Players,
    "player" => Token::Player,
    "Labels" =>  Token::Labels,
    "display" => Token::Display,
    "show" => Token::Display,
};

impl Token {
    /// Converting a string to the corresponding Token
    pub fn get_token_from(symbol: &str) -> Option<Token> {
        TOKENS.get(symbol).cloned()
    }

    /// Finding the first key-value pair, where the value matches the Token.
    /// Be aware that it is possible that the same token have multiple keys, and therefore
    /// this function returns the first match.
    pub fn to_string(&self) -> Option<String> {
        TOKENS.into_iter().find_map(|(key, value)| {
            if value == self {
                Some(key.to_string())
            } else {
                None
            }
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string().unwrap())
    }
}
