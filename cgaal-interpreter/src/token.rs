use phf::phf_map;
use std::fmt;
use std::fmt::Formatter;

#[derive(Clone, Eq, PartialEq)]
pub enum Token {
    Start,
    EOF,
    EOL,
    Identifier,
    Number,
    Move,
    Players,
    //    Player,
    //    Transitions,
    //    Moves,
    //    State,
    //    Action,
}

/// Static map from their respective symbols to their corresponding Token
pub static TOKENS: phf::Map<&'static str, Token> = phf_map! {
    "TOKEN::START" => Token::Start,
    "TOKEN::EOF" => Token::EOF,
    "TOKEN::EOL" => Token::EOL,
    "TOKEN::IDENTIFIER" => Token::Identifier,
    "TOKEN::NUMBER" => Token::Number,
    "move" => Token::Move,
    "number" => Token::Number,
    "players" => Token::Players,
};

impl Token {
    /// Converting a string to the corresponding Token
    pub fn get_token_from(symbol: &str) -> Option<Token> {
        TOKENS.get(symbol).cloned()
    }

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
