use crate::array::Array;
use crate::cell_mut::{Cell, CellValue};
use crate::char::Char;
use crate::env::Env;
use crate::err::Error;
use crate::number::Num;
use crate::proc::{Closure, Proc};
use crate::rep::{DisplayRep, ExternalRep};
use crate::string::Str;
//use crate::vector::Vector;
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
    Pair(Cell<Value>),
    String(Rc<Str>),
    //Vector(Rc<Vector<Value>>),
    Array(Rc<Array<Value>>),
    Env(Rc<Env<Str, Value>>),
    Empty,
    // Other
    Special(Box<SpecialForm>),
    //Port(Rc<Port>),
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

impl From<Cell<Value>> for Value {
    fn from(cell: Cell<Value>) -> Value {
        Value::Pair(cell)
    }
}

// Cell Value //

impl CellValue<Value> for Value {
    fn get_cell(&self) -> Option<Cell<Value>> {
        match self {
            Value::Pair(cell) => Some(cell.clone()),
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
