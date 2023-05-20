use crate::array::Array;
use crate::cell::{Cell, CellValue};
use crate::char::Char;
use crate::env::Env;
use crate::number::Num;
use crate::proc::{Closure, Proc};
use crate::rep::{DisplayRep, ExternalRep};
use crate::string::Str;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    // Atoms
    Bool(bool),
    Char(Char),
    Number(Num),
    Procedure(Rc<Proc<Value>>),
    Symbol(Rc<Str>),
    Closure(Rc<Closure<Value>>),
    // Collections
    Pair(Rc<Cell<Value>>),
    String(Rc<Str>),
    Array(Rc<Array<Value>>),
    Env(Rc<Env<Str, Value>>),
    Empty,
    // Other
    Special(Box<SpecialForm>),
    Undefined,
}

impl Value {
    // Extractors //

    pub fn get_bool(val: Value) -> Option<bool> {
        match val {
            Value::Bool(b) => Some(b),
            _ => None,
        }
    }

    pub fn get_char<'a>(val: &'a Value) -> Option<&'a Char> {
        match val {
            Value::Char(ch) => Some(ch),
            _ => None,
        }
    }

    pub fn get_number<'a>(val: &'a Value) -> Option<&'a Num> {
        match val {
            Value::Number(n) => Some(n),
            _ => None,
        }
    }

    pub fn get_int(val: &Value) -> Option<i64> {
        match val {
            Value::Number(Num::Int(n)) => Some(*n),
            _ => None,
        }
    }

    pub fn get_symbol_str<'a>(val: &'a Value) -> Option<&'a Str> {
        match val {
            Value::Symbol(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_string<'a>(val: &'a Value) -> Option<&'a Str> {
        match val {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_pair_cell<'a>(val: &'a Value) -> Option<&'a Cell<Value>> {
        match val {
            Value::Pair(cell) => Some(cell),
            _ => None,
        }
    }

    pub fn get_array<'a>(val: &'a Value) -> Option<&'a Array<Value>> {
        match val {
            Value::Array(arr) => Some(arr),
            _ => None,
        }
    }

    pub fn get_procedure<'a>(val: &'a Value) -> Option<&'a Proc<Value>> {
        match val {
            Value::Procedure(p) => Some(p),
            _ => None,
        }
    }

    pub fn get_closure<'a>(val: &'a Value) -> Option<&'a Closure<Value>> {
        match val {
            Value::Closure(p) => Some(p),
            _ => None,
        }
    }
}

// Value Traits ///////////////////////////////////////////////////////////////

// From Traits/Constructors //

impl Default for Value {
    fn default() -> Value {
        Value::Empty
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Value {
        Value::Number(Num::Int(i))
    }
}

impl From<char> for Value {
    fn from(ch: char) -> Value {
        Value::Char(Char::from(ch))
    }
}

impl From<Str> for Value {
    fn from(s: Str) -> Value {
        Value::String(Rc::new(s))
    }
}

impl From<Cell<Value>> for Value {
    fn from(cell: Cell<Value>) -> Value {
        Value::Pair(Rc::new(cell))
    }
}

impl From<Array<Value>> for Value {
    fn from(arr: Array<Value>) -> Value {
        Value::Array(Rc::new(arr))
    }
}

// Cell Value //

impl CellValue<Value> for Value {
    fn get_cell(&self) -> Option<Cell<Value>> {
        match self {
            Value::Pair(cell) => Some(cell.as_ref().clone()),
            _ => None,
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Value::Empty => true,
            _ => false,
        }
    }
}

// Representations //

impl DisplayRep for Value {
    fn to_display(&self) -> String {
        // TODO implement for all types properly and then use for implementing Display
        format!("{:?}", self)
    }
}

impl ExternalRep for Value {
    fn to_external(&self) -> String {
        // TODO implement for all types properly and then use for implementing Debug
        format!("{:?}", self)
    }
}

// Special Form ///////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum SpecialForm {
    If(Vec<Value>),
    Set(Value),
    And(Vec<Value>),
    Or(Vec<Value>),
    Cond(Vec<Value>, bool, Vec<Value>),
    Case(Value, Vec<Value>),
}
