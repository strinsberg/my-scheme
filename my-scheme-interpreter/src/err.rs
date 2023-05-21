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
