use crate::scm_types::builtin::Builtin;
use crate::scm_types::number::ScmNumber;
use crate::scm_types::scm_val::{Pointer, ScmVal};
use crate::scm_types::token::Token;
use std::fmt;

pub type ScmResult<T> = Result<T, ScmErr>;
pub type ValResult = ScmResult<ScmVal>;
pub type TcoResult = ScmResult<(ScmVal, Pointer)>;
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
    BadArgType(String, usize, String),
    BadArithmetic(Builtin, ScmNumber, ScmNumber),
    BadUnaryArithmetic(Builtin, ScmNumber),
    BadCall(String),
    BadBindings(String),
    EmptyBody,
    // seq errors
    OutOfBounds(usize, usize),
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
                write!(f, "Error: {name} requires at least {n} arguments")
            }
            ScmErr::BadArgType(name, pos, kind) => {
                write!(f, "Error: {name} requires argument {pos} be type {kind} ")
            }
            ScmErr::BadArithmetic(op, left, right) => {
                write!(f, "Error: bad arithmetic: ({:?} {left} {right})", op)
            }
            ScmErr::BadUnaryArithmetic(op, left) => {
                write!(f, "Error: bad arithmetic: ({:?} {left})", op)
            }
            ScmErr::BadCall(val) => {
                write!(f, "Error: invalid function call or procedure: {val}")
            }
            ScmErr::BadBindings(list) => {
                write!(f, "Error: invalid binding list: {list}")
            }
            ScmErr::EmptyBody => {
                write!(f, "Error: empty lambda body")
            }
            ScmErr::OutOfBounds(idx, len) => {
                write!(
                    f,
                    "Error: index out of bounds: got {idx} when length is {len}"
                )
            } //e => write!(f, "{:?}", e),
        }
    }
}
