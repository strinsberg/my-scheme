use crate::data::cell::Cell;
use crate::data::err::Error;
use crate::data::number::Num;
use crate::data::types::Type;
use crate::data::value::Value;
use std::rc::Rc;

// TODO add all remaining number procedures from the standard.

// Number Procedures //////////////////////////////////////////////////////////

// Predicates //

pub fn is_number(val: Value) -> Result<Value, Error> {
    match Value::get_number(&val) {
        Some(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn is_real(val: Value) -> Result<Value, Error> {
    match Value::get_number(&val) {
        Some(num) if num.is_flt() => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn is_rational(val: Value) -> Result<Value, Error> {
    match Value::get_number(&val) {
        Some(num) if num.is_rat() => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn is_integer(val: Value) -> Result<Value, Error> {
    match Value::get_number(&val) {
        Some(num) if num.is_int() => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

// Basic Arithmetic //

pub fn cell_arith(
    cell: Rc<Cell<Value>>,
    init: Num,
    func: fn(a: Num, b: Num) -> Result<Num, Error>,
) -> Result<Value, Error> {
    let mut result = init;
    for val in cell.values() {
        let num = Value::get_number(&val).ok_or(Error::BadType(Type::Number, val.clone()))?;
        result = func(result, *num)?;
    }
    Ok(Value::Number(result))
}

pub fn add(args: Value) -> Result<Value, Error> {
    match args {
        Value::Pair(cell) => cell_arith(cell, Num::Int(0), |a, b| a.add(&b)),
        Value::Empty => Ok(Value::Number(Num::Int(0))),
        _ => panic!("args list should be list"),
    }
}

pub fn multiply(args: Value) -> Result<Value, Error> {
    match args {
        Value::Pair(cell) => cell_arith(cell, Num::Int(1), |a, b| a.mult(&b)),
        Value::Empty => Ok(Value::Number(Num::Int(1))),
        _ => panic!("args list should be list"),
    }
}

pub fn subtract(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_arith(cell, number.clone(), |a, b| a.sub(&b)),
        Value::Empty => Ok(Value::Number(number.negate())),
        _ => panic!("rest args list should be list"),
    }
}

pub fn divide(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_arith(cell, number.clone(), |a, b| a.div(&b)),
        Value::Empty => Ok(Value::Number(number.invert())),
        _ => panic!("rest args list should be list"),
    }
}

// Comparisson //

pub fn cell_comp(
    cell: Rc<Cell<Value>>,
    init: Num,
    func: fn(a: Num, b: Num) -> bool,
) -> Result<Value, Error> {
    let mut last = init;
    for val in cell.values() {
        let num = Value::get_number(&val).ok_or(Error::BadType(Type::Number, val.clone()))?;
        if !func(last, *num) {
            return Ok(Value::Bool(false));
        }
        last = num.clone();
    }
    Ok(Value::Bool(true))
}

pub fn num_eq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a == b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

pub fn num_less(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a < b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

pub fn num_greater(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a > b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

pub fn num_leq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a <= b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

pub fn num_geq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a <= b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

// Testing ////////////////////////////////////////////////////////////////////
