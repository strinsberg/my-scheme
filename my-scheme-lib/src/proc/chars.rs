use crate::data::char::Char;
use crate::data::err::Error;
use crate::data::proc::Proc;
use crate::data::types::{Arity, Type};
use crate::data::value::Value;
use crate::proc::utils;

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![
        Proc::new("char?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_char(first)
        }),
        Proc::new("char-alphabetic?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_alpha(first)
        }),
        Proc::new("char-numeric?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_numeric(first)
        }),
        Proc::new("char-whitespace?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_whitespace(first)
        }),
        Proc::new("char-upper-case?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_upcase(first)
        }),
        Proc::new("char-lower-case?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_downcase(first)
        }),
        Proc::new(
            "char-alphanumeric?",
            Arity::Fixed(vec![Type::Char]),
            |args| {
                let first = utils::fixed_take_1(args)?;
                is_alphanumeric(first)
            },
        ),
        Proc::new("char-unsup?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_unsup(first)
        }),
        // Comparisson
        Proc::new(
            "char=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_eq(first, second)
            },
        ),
        Proc::new(
            "char<?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_less(first, second)
            },
        ),
        Proc::new(
            "char>?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_greater(first, second)
            },
        ),
        Proc::new(
            "char<=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_leq(first, second)
            },
        ),
        Proc::new(
            "char>=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_geq(first, second)
            },
        ),
        Proc::new(
            "char-ci=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_eq_ci(first, second)
            },
        ),
        Proc::new(
            "char-ci<?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_less_ci(first, second)
            },
        ),
        Proc::new(
            "char-ci>?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_greater_ci(first, second)
            },
        ),
        Proc::new(
            "char-ci<=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_leq_ci(first, second)
            },
        ),
        Proc::new(
            "char-ci>=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                char_geq_ci(first, second)
            },
        ),
        // Conversion
        Proc::new("char-upcase", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            char_upcase(first)
        }),
        Proc::new("char-downcase", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            char_downcase(first)
        }),
        Proc::new("char->integer", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            char_to_integer(first)
        }),
        Proc::new("integer->char", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            integer_to_char(first)
        }),
    ]
}

// Char Procedures ////////////////////////////////////////////////////////////

// Predicates
// char?, char-alphabetic?, char-numeric?, char-whitespace?, char-upper-case?, char-lower-case?
// char-unsup?, char-alpha-numeric?

fn is_char(val: Value) -> Result<Value, Error> {
    match Value::get_char(&val) {
        Some(_) => Ok(Value::Bool(true)),
        None => Ok(Value::Bool(false)),
    }
}

fn is_alpha(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_alpha() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn is_numeric(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_numeric() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn is_alphanumeric(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_alphanumeric() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn is_whitespace(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_whitespace() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn is_unsup(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_unsup() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn is_upcase(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_upper_case() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn is_downcase(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_lower_case() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

// Comparisson
fn char_eq(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() == ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_less(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() < ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_greater(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() > ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_leq(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() <= ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_geq(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() >= ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_eq_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() == ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_less_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() < ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_greater_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() > ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_leq_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() <= ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn char_geq_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() >= ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

// Conversion
// char-upcase, char-downcase, char-integer, integer-char
fn char_upcase(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    Ok(Value::Char(ch.to_upper_case()))
}

fn char_downcase(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    Ok(Value::Char(ch.to_lower_case()))
}

fn char_to_integer(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    Ok(Value::from(ch.to_int()))
}

fn integer_to_char(val: Value) -> Result<Value, Error> {
    let int = Value::get_int(&val).ok_or(Error::BadType(Type::Int, val.clone()))?;
    Ok(Value::Char(Char::from(int)))
}

// Testing ////////////////////////////////////////////////////////////////////
