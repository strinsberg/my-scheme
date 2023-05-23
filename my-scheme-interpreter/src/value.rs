use crate::array::Array;
use crate::cell::{Cell, CellValue};
use crate::char::Char;
use crate::env::Env;
use crate::number::Num;
use crate::proc::{Closure, Proc};
use crate::rep::{DisplayRep, ExternalRep};
use crate::string::Str;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
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
    pub fn list_from_vec(vec: Vec<Value>, val: Value) -> Value {
        let mut result = val;
        for v in vec.iter().rev() {
            result = Value::from(Cell::new(v.clone(), Some(result)));
        }
        result
    }

    pub fn symbol(string: Str) -> Value {
        Value::Symbol(Rc::new(string))
    }

    // Predicates //

    pub fn is_true(&self) -> bool {
        match self {
            Value::Bool(false) => false,
            _ => true,
        }
    }

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

    pub fn get_uint(val: &Value) -> Option<i64> {
        match val {
            Value::Number(Num::Int(n)) if n >= &0 => Some(*n),
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

impl From<f64> for Value {
    fn from(f: f64) -> Value {
        Value::Number(Num::Flt(f))
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

impl From<&str> for Value {
    fn from(s: &str) -> Value {
        Value::String(Rc::new(Str::from(s)))
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

impl From<Closure<Value>> for Value {
    fn from(c: Closure<Value>) -> Value {
        Value::Closure(Rc::new(c))
    }
}

impl From<Proc<Value>> for Value {
    fn from(p: Proc<Value>) -> Value {
        Value::Procedure(Rc::new(p))
    }
}

impl From<SpecialForm> for Value {
    fn from(f: SpecialForm) -> Value {
        Value::Special(Box::new(f))
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
        match self {
            Value::Bool(val) => format!("#{}", if *val { "t" } else { "f" }),
            Value::Char(val) => val.to_display(),
            Value::Number(val) => val.to_display(),
            Value::Procedure(val) => val.to_display(),
            Value::Symbol(val) => val.to_display(),
            Value::Closure(val) => val.to_display(),
            Value::Pair(val) => val.to_display(),
            Value::String(val) => val.to_display(),
            Value::Array(val) => val.to_display(),
            Value::Empty => "()".to_string(),
            Value::Env(_) => "#<environment>".to_string(),
            Value::Special(_) => "#<special-form>".to_string(),
            Value::Undefined => "#<undefined>".to_string(),
        }
    }
}

impl ExternalRep for Value {
    fn to_external(&self) -> String {
        match self {
            Value::Bool(val) => format!("#{}", if *val { "t" } else { "f" }),
            Value::Char(val) => val.to_external(),
            Value::Number(val) => val.to_external(),
            Value::Procedure(val) => val.to_external(),
            Value::Symbol(val) => val.to_display(),
            Value::Closure(val) => val.to_external(),
            Value::Pair(val) => val.to_external(),
            Value::String(val) => val.to_external(),
            Value::Array(val) => val.to_external(),
            Value::Empty => "()".to_string(),
            Value::Env(_) => "#<environment>".to_string(),
            Value::Special(_) => "#<special-form>".to_string(),
            Value::Undefined => "#<undefined>".to_string(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_display())
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Value{{ {} }}", self.to_external())
    }
}

// Special Form ///////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum SpecialForm {
    If(Value, Option<Value>),
    Set(Str),
    And(Value),
    Or(Value),
    Cond(Value, bool, Value),
    Case(Value, Value),
}
