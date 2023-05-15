use crate::scanner::Token;
use crate::types::ScmVal;
use std::fmt;

pub type ScmResult<T> = Result<T, ScmErr>;
pub type ValResult = ScmResult<ScmVal>;
pub type ScanResult = ScmResult<Token>;

#[derive(Debug, Clone, PartialEq)]
pub enum ScmErr {
    // reader errors
    BadToken(usize, Token),
    BadChar(usize, char),
    BadIdentifier(usize, String),
    BadEscape(usize, String),
    BadNumber(usize, String),
    MultiLineString(usize),
    // eval error
    Undeclared(String),
    Arity(String, usize),
    BadArgType(String, String, ScmVal),
    BadArithmetic(String, ScmVal, ScmVal),
    // TODO add a variant that holds ArithErr and try to replace all arithmetic
    // errors with it?
    BadBinding(ScmVal),
    // seq errors
    OutOfBounds(usize, usize),
    RangeError(String, ScmVal, ScmVal),
    // General
    Syntax(ScmVal),
    InnerDefine,
    UserError(String, String, Vec<ScmVal>),
}

impl fmt::Display for ScmErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScmErr::BadToken(line, tk) => {
                write!(f, "ReadError: Line: {line}, unexpected token: {tk}")
            }
            ScmErr::BadChar(line, ch) => {
                write!(f, "ReadError: Line: {line}, unexpected character: {ch}")
            }
            ScmErr::BadIdentifier(line, id) => {
                write!(f, "ReadError: Line: {line}, invalid identifier: {id}")
            }
            ScmErr::BadEscape(line, s) => {
                write!(f, "ReadError: Line: {line}, invalid escape character: {s}")
            }
            ScmErr::BadNumber(line, n) => {
                write!(f, "ReadError: Line: {line}, invalid number: {n}")
            }
            ScmErr::MultiLineString(line) => {
                write!(
                    f,
                    "ReadError: Line: {line}, string literals cannot span multiple lines"
                )
            }
            ScmErr::Undeclared(sym) => {
                write!(f, "Error: undeclared symbol: {sym}")
            }
            ScmErr::Arity(name, n) => {
                write!(f, "Error in {name}: requires at least {n} arguments")
            }
            ScmErr::BadArgType(name, kind, expr) => {
                write!(f, "Error in {name}: {expr} is not a {kind}")
            }
            ScmErr::BadArithmetic(op, left, right) => {
                write!(f, "Error: invalid arithmetic: ({op} {left} {right})")
            }
            ScmErr::BadBinding(key) => {
                write!(f, "Error: invalid binding var: {key}")
            }
            ScmErr::OutOfBounds(idx, len) => {
                write!(
                    f,
                    "Error: index out of bounds: got {idx} when length is {len}"
                )
            }
            ScmErr::RangeError(name, idx, seq) => {
                write!(f, "Error in {name}: {idx} is out of range for {seq}")
            }
            ScmErr::Syntax(expr) => {
                write!(f, "Error: invalid syntax: {expr}")
            }
            ScmErr::UserError(name, msg, irritants) => {
                if irritants.len() == 0 {
                    write!(f, "Error in {name}: {msg}")
                } else {
                    write!(
                        f,
                        "Error in {name}: {msg}\nIrritants: {}",
                        irritants
                            .iter()
                            .map(|v| v.to_extern())
                            .collect::<Vec<String>>()
                            .join(" ")
                    )
                }
            }
            ScmErr::InnerDefine => {
                write!(f, "Error: define only allowed at top-level")
            }
        }
    }
}
