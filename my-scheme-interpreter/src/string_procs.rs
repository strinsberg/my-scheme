use crate::cell::Cell;
use crate::char::Char;
use crate::err::Error;
use crate::proc::Proc;
use crate::proc_utils as utils;
use crate::string::Str;
use crate::types::{Arity, Type};
use crate::value::Value;

// TODO testing
// TODO substring, string-append, string-copy
// TODO add string comparissons, use char comparrisons.

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![
        // Construct
        Proc::new("string", Arity::Collect(Type::Char), |args| {
            new_string(args.clone())
        }),
        Proc::new(
            "make-string",
            Arity::Fixed(vec![Type::UInt, Type::opt(Type::Char)]),
            |args| {
                let (first, second) = utils::opt_last_take_2(args)?;
                make_string(first, second)
            },
        ),
        // Predicates
        Proc::new("string?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_string(first)
        }),
        // Non-Mutating
        Proc::new("string-length", Arity::Fixed(vec![Type::String]), |args| {
            let first = utils::fixed_take_1(args)?;
            string_length(first)
        }),
        Proc::new(
            "string-ref",
            Arity::Fixed(vec![Type::String, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_ref(first, second)
            },
        ),
        Proc::new("list->string", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            list_to_string(first)
        }),
        Proc::new("string->list", Arity::Fixed(vec![Type::String]), |args| {
            let first = utils::fixed_take_1(args)?;
            string_to_list(first)
        }),
        // Mutating
        Proc::new(
            "string-set!",
            Arity::Fixed(vec![Type::String, Type::UInt, Type::Char]),
            |args| {
                let (first, second, third) = utils::fixed_take_3(args)?;
                string_set(first, second, third)
            },
        ),
        Proc::new(
            "string-fill!",
            Arity::Fixed(vec![Type::String, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_fill(first, second)
            },
        ),
    ]
}

// String Procedures //////////////////////////////////////////////////////////

// Constructors //

fn new_string(args: Value) -> Result<Value, Error> {
    let cell = Value::get_pair_cell(&args).ok_or(Error::ArgsNotList)?;
    let mut chars = Vec::new();
    for (i, val) in cell.values().enumerate() {
        let ch = Value::get_char(&val).ok_or(Error::BadArg(i + 1))?;
        chars.push(ch.clone())
    }
    Ok(Value::from(Str::from(chars)))
}

fn make_string(size: Value, fill: Option<Value>) -> Result<Value, Error> {
    let size = Value::get_int(&size).ok_or(Error::BadArg(1))?;
    if size < 0 {
        return Err(Error::BadArg(1));
    }
    let val = match fill {
        Some(val) => Value::get_char(&val).ok_or(Error::BadArg(2))?.clone(),
        None => Char::from('\0'),
    };
    Ok(Value::from(Str::new(val, size as usize)))
}

// Predicates //

fn is_string(string: Value) -> Result<Value, Error> {
    match Value::get_string(&string) {
        Some(_) => Ok(Value::Bool(true)),
        None => Ok(Value::Bool(false)),
    }
}

// Non-Mutating Procedures //

fn string_length(string: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    Ok(Value::from(s.len() as i64))
}

fn string_ref(string: Value, index: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let idx = Value::get_int(&index).ok_or(Error::BadArg(2))?;
    if idx < 0 {
        return Err(Error::BadArg(1));
    }
    match s.get(idx as usize) {
        Some(val) => Ok(Value::Char(val.clone())),
        None => Err(Error::OutOfRange),
    }
}

fn list_to_string(pair: Value) -> Result<Value, Error> {
    let cell = Value::get_pair_cell(&pair).ok_or(Error::BadArg(1))?;
    let mut chars = Vec::new();
    for val in cell.values() {
        let ch = Value::get_char(&val).ok_or(Error::BadArg(1))?;
        chars.push(ch.clone())
    }
    Ok(Value::from(Str::from(chars)))
}

fn string_to_list(string: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let mut result = Value::Empty;

    for i in s.len() - 1..=0 {
        result = Value::from(Cell::new(
            Value::Char(s.get(i).expect("index should not be out of range").clone()),
            Some(result),
        ));
    }
    Ok(result)
}

// Mutating Procedures //

fn string_set(string: Value, index: Value, val: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let idx = Value::get_int(&index).ok_or(Error::BadArg(2))?;
    if idx < 0 {
        return Err(Error::BadArg(1));
    }
    let ch = Value::get_char(&val).ok_or(Error::BadArg(3))?;
    match s.set(ch.clone(), idx as usize) {
        Some(val) => Ok(Value::Char(val)),
        None => Err(Error::OutOfRange),
    }
}

fn string_fill(string: Value, val: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let ch = Value::get_char(&val).ok_or(Error::BadArg(2))?;
    s.fill(ch.clone());
    Ok(string)
}

// Testing ////////////////////////////////////////////////////////////////////
