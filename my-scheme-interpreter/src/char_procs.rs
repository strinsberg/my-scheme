use crate::char::Char;
use crate::err::Error;
use crate::proc::Proc;
use crate::proc_utils as utils;
use crate::types::{Arity, Type};
use crate::value::Value;

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![]
}

// Char Procedures ////////////////////////////////////////////////////////////

// Predicates
// char?, char-alphabetic?, char-numeric?, char-whitespace?, char-upper-case?, char-lower-case?
// char-unsup?, char-alpha-numeric?

// Comparisson
// char=?, char<?, char>?, char<=?, char>=?
// char-ci=?, char-ci<?, char-ci>?, char-ci<=?, char-ci>=?

// Conversion
// char-upcase, char-downcase, char-integer, integer-char

// Testing ////////////////////////////////////////////////////////////////////
