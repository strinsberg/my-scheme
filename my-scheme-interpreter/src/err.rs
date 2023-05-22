use crate::string::Str;
use crate::types::Type;
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    OutOfRange,
    ArgsNotList,
    Arity,
    BadArg(usize),
    DivideByZero,
    CantParseNum(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScanError {
    Eof(usize),
    BadChar(usize, char),
    BadToken(usize, String),
    BadIdentifier(usize, String),
    BadEscape(usize, String),
    BadNumber(usize, String),
    MultiLineString(usize),
}

pub enum UserError {
    Undeclared(Str),
    ArgType(String, Type, Value),
    OutOfRange(usize, Value),
    Arity(Value),
    Syntax(Value),
}

impl UserError {
    pub fn bad_arg(name: &str, t: Type, val: Value) -> UserError {
        UserError::ArgType(name.to_owned(), t, val)
    }

    pub fn range(idx: usize, val: Value) -> UserError {
        UserError::OutOfRange(idx, val)
    }
}
