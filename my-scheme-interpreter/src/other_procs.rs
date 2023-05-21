use crate::cell::Cell;
use crate::err::Error;
use crate::proc::Proc;
use crate::proc_utils as utils;
use crate::string::Str;
use crate::types::{Arity, Type};
use crate::value::Value;
use std::rc::Rc;

// TODO add in eq?, eqv?, and equal?

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![
        // Boolean
        Proc::new("boolean?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_bool(first)
        }),
        Proc::new("not", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_bool(first)
        }),
        // Symbol
        Proc::new("symbol?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_symbol(first)
        }),
        Proc::new("symbol->string", Arity::Fixed(vec![Type::Symbol]), |args| {
            let first = utils::fixed_take_1(args)?;
            symbol_to_string(first)
        }),
        Proc::new("string->symbol", Arity::Fixed(vec![Type::String]), |args| {
            let first = utils::fixed_take_1(args)?;
            string_to_symbol(first)
        }),
        // Control flow
        Proc::new("procedure?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_procedure(first)
        }),
        Proc::new(
            "map",
            Arity::Fixed(vec![Type::Proc(1), Type::Pair]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                map(first, second)
            },
        ),
        Proc::new(
            "for-each",
            Arity::Fixed(vec![Type::Proc(1), Type::Pair]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                for_each(first, second)
            },
        ),
        Proc::new(
            "filter",
            Arity::Fixed(vec![Type::Proc(1), Type::Pair]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                filter(first, second)
            },
        ),
        Proc::new(
            "fold",
            Arity::Fixed(vec![Type::Any, Type::Proc(2), Type::Pair]),
            |args| {
                let (first, second, third) = utils::fixed_take_3(args)?;
                fold(first, second, third)
            },
        ),
        // Eval/Apply
        // These exist here just because they need procedures, but they are
        // essentially special forms that get evaluated by the vm directly.
        Proc::new("eval", Arity::Fixed(vec![Type::Any, Type::Env]), |_| {
            panic!("eval is evaluated by the vm")
        }),
        Proc::new(
            "apply",
            Arity::Fixed(vec![
                Type::Any,
                Type::dots(Type::Any),
                Type::list(Type::Any),
            ]),
            |_| panic!("apply is evaluated by the vm"),
        ),
    ]
}

// Boolean Procedures /////////////////////////////////////////////////////////

fn is_bool(val: Value) -> Result<Value, Error> {
    match val {
        Value::Bool(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

// Symbol Procedures //////////////////////////////////////////////////////////

fn is_symbol(val: Value) -> Result<Value, Error> {
    match val {
        Value::Symbol(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn symbol_to_string(symbol: Value) -> Result<Value, Error> {
    let s = Value::get_symbol_str(&symbol).ok_or(Error::BadArg(1))?;
    let mut chars = Vec::new();
    for ch in s.chars() {
        chars.push(ch.clone())
    }
    Ok(Value::from(Str::from(chars)))
}

fn string_to_symbol(string: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let mut chars = Vec::new();
    for ch in s.chars() {
        chars.push(ch.clone())
    }
    Ok(Value::Symbol(Rc::new(Str::from(chars))))
}

// Control Flow ///////////////////////////////////////////////////////////////

fn is_procedure(val: Value) -> Result<Value, Error> {
    match val {
        Value::Procedure(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn map(procedure: Value, list: Value) -> Result<Value, Error> {
    let proc = Value::get_procedure(&procedure).ok_or(Error::BadArg(1))?;
    match proc.arity {
        Arity::Fixed(ref args) if args.len() != 1 => return Err(Error::BadArg(1)),
        Arity::Rest(ref args, _) if args.len() > 1 => return Err(Error::BadArg(1)),
        _ => (),
    }
    let func = proc.func;
    let cell = Value::get_pair_cell(&list).ok_or(Error::BadArg(2))?;
    let values: Vec<Value> = cell.values().collect();

    let mut result = Value::Empty;
    for val in values.iter().rev() {
        result = Value::from(Cell::new(func(val)?, Some(result)));
    }
    Ok(result)
}

fn for_each(procedure: Value, list: Value) -> Result<Value, Error> {
    let proc = Value::get_procedure(&procedure).ok_or(Error::BadArg(1))?;
    match proc.arity {
        Arity::Fixed(ref args) if args.len() != 1 => return Err(Error::BadArg(1)),
        Arity::Rest(ref args, _) if args.len() > 1 => return Err(Error::BadArg(1)),
        _ => (),
    }
    let func = proc.func;

    let cell = Value::get_pair_cell(&list).ok_or(Error::BadArg(2))?;
    let values: Vec<Value> = cell.values().collect();

    for val in values.iter().rev() {
        func(val)?;
    }
    Ok(Value::default())
}

fn filter(procedure: Value, list: Value) -> Result<Value, Error> {
    let proc = Value::get_procedure(&procedure).ok_or(Error::BadArg(1))?;
    match proc.arity {
        Arity::Fixed(ref args) if args.len() != 1 => return Err(Error::BadArg(1)),
        Arity::Rest(ref args, _) if args.len() > 1 => return Err(Error::BadArg(1)),
        _ => (),
    }
    let func = proc.func;
    let cell = Value::get_pair_cell(&list).ok_or(Error::BadArg(2))?;
    let values: Vec<Value> = cell.values().collect();

    let mut result = Value::Empty;
    for val in values.iter().rev() {
        if let Value::Bool(true) = func(val)? {
            result = Value::from(Cell::new(val.clone(), Some(result)));
        }
    }
    Ok(result)
}

fn fold(initial: Value, procedure: Value, list: Value) -> Result<Value, Error> {
    let proc = Value::get_procedure(&procedure).ok_or(Error::BadArg(2))?;
    match proc.arity {
        Arity::Fixed(ref args) if args.len() != 2 => return Err(Error::BadArg(2)),
        Arity::Rest(ref args, _) if args.len() > 2 => return Err(Error::BadArg(2)),
        _ => (),
    }
    let func = proc.func;
    let cell = Value::get_pair_cell(&list).ok_or(Error::BadArg(3))?;

    let mut acc = initial;
    for val in cell.values() {
        acc = func(&Value::from(Cell::new(acc, Some(val.clone()))))?;
    }
    Ok(acc)
}

// Testing ////////////////////////////////////////////////////////////////////
