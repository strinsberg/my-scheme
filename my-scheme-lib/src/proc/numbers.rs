use crate::data::cell::Cell;
use crate::data::err::Error;
use crate::data::number::Num;
use crate::data::proc::Proc;
use crate::data::types::{Arity, Type};
use crate::data::value::Value;
use crate::proc::utils;
use std::rc::Rc;

// TODO add all remaining number procedures from the standard.

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![
        // predicates
        Proc::new("number?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_number(first)
        }),
        // TODO these are supposed to be a tower, i.e. an integer is a real
        Proc::new("real?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_real(first)
        }),
        Proc::new("rational?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_rational(first)
        }),
        Proc::new("integer?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_integer(first)
        }),
        // basic arithmetic
        Proc::new("+", Arity::Collect(Type::Number), |args| add(args.clone())),
        Proc::new("*", Arity::Collect(Type::Number), |args| {
            multiply(args.clone())
        }),
        Proc::new("-", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            subtract(first, rest)
        }),
        Proc::new("/", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            divide(first, rest)
        }),
        // Comparisson
        Proc::new("=", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            num_eq(first, rest)
        }),
        Proc::new("<", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            num_less(first, rest)
        }),
        Proc::new(">", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            num_greater(first, rest)
        }),
        Proc::new(
            "<=",
            Arity::Rest(vec![Type::Number], Type::Number),
            |args| {
                let (first, rest) = utils::rest_take_1(args)?;
                num_leq(first, rest)
            },
        ),
        Proc::new(
            ">=",
            Arity::Rest(vec![Type::Number], Type::Number),
            |args| {
                let (first, rest) = utils::rest_take_1(args)?;
                num_geq(first, rest)
            },
        ),
    ]
}

// Number Procedures //////////////////////////////////////////////////////////

// Predicates //

fn is_number(val: Value) -> Result<Value, Error> {
    match Value::get_number(&val) {
        Some(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn is_real(val: Value) -> Result<Value, Error> {
    match Value::get_number(&val) {
        Some(num) if num.is_flt() => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn is_rational(val: Value) -> Result<Value, Error> {
    match Value::get_number(&val) {
        Some(num) if num.is_rat() => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn is_integer(val: Value) -> Result<Value, Error> {
    match Value::get_number(&val) {
        Some(num) if num.is_int() => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

// Basic Arithmetic //

fn cell_arith(
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

fn multiply(args: Value) -> Result<Value, Error> {
    match args {
        Value::Pair(cell) => cell_arith(cell, Num::Int(1), |a, b| a.mult(&b)),
        Value::Empty => Ok(Value::Number(Num::Int(1))),
        _ => panic!("args list should be list"),
    }
}

fn subtract(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_arith(cell, number.clone(), |a, b| a.sub(&b)),
        Value::Empty => Ok(Value::Number(number.negate())),
        _ => panic!("rest args list should be list"),
    }
}

fn divide(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_arith(cell, number.clone(), |a, b| a.div(&b)),
        Value::Empty => Ok(Value::Number(number.invert())),
        _ => panic!("rest args list should be list"),
    }
}

// Comparisson //

fn cell_comp(
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

fn num_eq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a == b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

fn num_less(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a < b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

fn num_greater(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a > b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

fn num_leq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a <= b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

fn num_geq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadType(Type::Number, first.clone()))?;
    match rest {
        Value::Pair(cell) => cell_comp(cell, number.clone(), |a, b| a <= b),
        Value::Empty => Ok(Value::Bool(false)),
        _ => panic!("rest args list should be list"),
    }
}

// Testing ////////////////////////////////////////////////////////////////////
