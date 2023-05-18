use crate::array::Array;
use crate::cell::Cell;
use crate::char::Char;
use crate::env::Env;
use crate::number::Num;
use crate::proc::{Closure, Proc};
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
    Pair(Rc<Cell<Value>>),
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

#[derive(Debug, Clone, PartialEq)]
pub enum SpecialForm {
    If(Vec<Value>),
    Set(Value),
    And(Vec<Value>),
    Or(Vec<Value>),
    Cond(Vec<Value>, bool, Vec<Value>),
    Case(Value, Vec<Value>),
}
