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

// Scan/Read Error ////////////////////////////////////////////////////////////

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

impl std::fmt::Display for ScanError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ScanError::Eof(line) => {
                write!(f, "ReadError: Line: {line}, unexpected EOF")
            }
            ScanError::BadToken(line, tk) => {
                write!(f, "ReadError: Line: {line}, unexpected token: {tk}")
            }
            ScanError::BadChar(line, ch) => {
                write!(f, "ReadError: Line: {line}, unexpected character: {ch}")
            }
            ScanError::BadIdentifier(line, id) => {
                write!(f, "ReadError: Line: {line}, invalid identifier: {id}")
            }
            ScanError::BadEscape(line, s) => {
                write!(f, "ReadError: Line: {line}, invalid escape character: {s}")
            }
            ScanError::BadNumber(line, n) => {
                write!(f, "ReadError: Line: {line}, invalid number: {n}")
            }
            ScanError::MultiLineString(line) => {
                write!(
                    f,
                    "ReadError: Line: {line}, string literals cannot span multiple lines"
                )
            }
        }
    }
}

// UserError //////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum UserError {
    Undeclared(Str),
    ArgType(String, Type, Value),
    OutOfRange(usize, Value),
    Arity(Value),
    Syntax(Value),
    ReadError(ScanError),
}

impl UserError {
    pub fn bad_arg(name: &str, t: Type, val: Value) -> UserError {
        UserError::ArgType(name.to_owned(), t, val)
    }

    pub fn range(idx: usize, val: Value) -> UserError {
        UserError::OutOfRange(idx, val)
    }
}

impl std::fmt::Display for UserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UserError::Undeclared(name) => {
                write!(f, "Error: undeclared symbol: {name}")
            }
            UserError::Arity(val) => {
                write!(f, "Error: incorrect argument count in {val}")
            }
            UserError::ArgType(name, kind, expr) => {
                write!(f, "Error in {name}: {expr} is not a {kind}")
            }
            UserError::OutOfRange(idx, val) => {
                write!(f, "Error: {idx} is out of range for {val}")
            }
            UserError::Syntax(expr) => {
                write!(f, "Error: invalid syntax: {expr}")
            }
            UserError::ReadError(err) => {
                write!(f, "{}", err)
            }
        }
    }
}
