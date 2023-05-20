#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    OutOfRange,
    DividByZero,
    ArgsNotList,
    Arity,
    BadArg(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScanError {
    Eof(usize),
    BadChar(usize, char),
    BadIdentifier(usize, String),
    BadEscape(usize, String),
    BadNumber(usize, String),
    MultiLineString(usize),
}
