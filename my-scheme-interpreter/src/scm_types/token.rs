use crate::scm_types::number::ScmNumber;
use crate::scm_types::string::{ScmChar, ScmString};
use std::fmt;

// Tokens /////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(ScmString),
    Boolean(bool),
    Number(ScmNumber),
    Character(ScmChar),
    String(ScmString),
    LParen,
    RParen,
    VecOpen,
    Quote,
    BackTick,
    Comma,
    CommaAt,
    Dot,
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Identifier(s) => write!(f, "{}", s.to_string()),
            Token::Boolean(b) => write!(f, "{}", b),
            Token::Number(n) => write!(f, "{}", n.to_string()),
            Token::Character(c) => write!(f, "{}", c.to_string()),
            Token::String(s) => write!(f, "{}", s.to_string()),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::VecOpen => write!(f, "#("),
            Token::Quote => write!(f, "'"),
            Token::BackTick => write!(f, "`"),
            Token::Comma => write!(f, ","),
            Token::CommaAt => write!(f, ",@"),
            Token::Dot => write!(f, "."),
            Token::EOF => write!(f, "EOF"),
        }
    }
}
