use crate::err::Error;
use crate::number::Num;
use crate::proc::Proc;
use crate::proc_utils as utils;
use crate::types::{Arity, Type};
use crate::value::Value;

// TODO add all remaining number procedures from the standard.

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![
        // predicates
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

fn add(args: Value) -> Result<Value, Error> {
    if args == Value::Empty {
        return Ok(Value::Number(Num::Int(0)));
    }
    let cell = Value::get_pair_cell(&args).ok_or(Error::ArgsNotList)?;
    let mut result = Num::Int(0);
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 1))?;
        result = result.add(num)?;
    }
    Ok(Value::Number(result))
}

fn multiply(args: Value) -> Result<Value, Error> {
    if args == Value::Empty {
        return Ok(Value::Number(Num::Int(1)));
    }
    let cell = Value::get_pair_cell(&args).ok_or(Error::ArgsNotList)?;
    let mut result = Num::Int(1);
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 1))?;
        result = result.mult(num)?;
    }
    Ok(Value::Number(result))
}

fn subtract(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadArg(1))?;
    if rest == Value::Empty {
        return Ok(Value::Number(number.negate()));
    }

    let cell = Value::get_pair_cell(&rest).ok_or(Error::ArgsNotList)?;
    let mut result = number.clone();
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 2))?;
        result = result.sub(num)?;
    }
    Ok(Value::Number(result))
}

fn divide(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadArg(1))?;
    if rest == Value::Empty {
        return Ok(Value::Number(number.invert()));
    }

    let cell = Value::get_pair_cell(&rest).ok_or(Error::ArgsNotList)?;
    let mut result = number.clone();
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 2))?;
        result = result.div(num)?;
    }
    Ok(Value::Number(result))
}

// Comparisson //

fn num_eq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadArg(1))?;
    if rest == Value::Empty {
        return Ok(Value::Bool(true));
    }

    let cell = Value::get_pair_cell(&rest).ok_or(Error::ArgsNotList)?;
    let mut last = number.clone();
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 2))?;
        if last != *num {
            return Ok(Value::Bool(false));
        }
        last = num.clone();
    }
    Ok(Value::Bool(true))
}

fn num_less(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadArg(1))?;
    if rest == Value::Empty {
        return Ok(Value::Bool(true));
    }

    let cell = Value::get_pair_cell(&rest).ok_or(Error::ArgsNotList)?;
    let mut last = number.clone();
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 2))?;
        if last >= *num {
            return Ok(Value::Bool(false));
        }
        last = num.clone();
    }
    Ok(Value::Bool(true))
}

fn num_greater(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadArg(1))?;
    if rest == Value::Empty {
        return Ok(Value::Bool(true));
    }

    let cell = Value::get_pair_cell(&rest).ok_or(Error::ArgsNotList)?;
    let mut last = number.clone();
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 2))?;
        if last <= *num {
            return Ok(Value::Bool(false));
        }
        last = num.clone();
    }
    Ok(Value::Bool(true))
}

fn num_leq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadArg(1))?;
    if rest == Value::Empty {
        return Ok(Value::Bool(true));
    }

    let cell = Value::get_pair_cell(&rest).ok_or(Error::ArgsNotList)?;
    let mut last = number.clone();
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 2))?;
        if last > *num {
            return Ok(Value::Bool(false));
        }
        last = num.clone();
    }
    Ok(Value::Bool(true))
}

fn num_geq(first: Value, rest: Value) -> Result<Value, Error> {
    let number = Value::get_number(&first).ok_or(Error::BadArg(1))?;
    if rest == Value::Empty {
        return Ok(Value::Bool(true));
    }

    let cell = Value::get_pair_cell(&rest).ok_or(Error::ArgsNotList)?;
    let mut last = number.clone();
    for (i, val) in cell.values().enumerate() {
        let num = Value::get_number(&val).ok_or(Error::BadArg(i + 2))?;
        if last < *num {
            return Ok(Value::Bool(false));
        }
        last = num.clone();
    }
    Ok(Value::Bool(true))
}

// Testing ////////////////////////////////////////////////////////////////////
