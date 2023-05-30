use crate::data::char::Char;
use crate::data::err::Error;
use crate::data::types::Type;
use crate::data::value::Value;

// Char Procedures ////////////////////////////////////////////////////////////

// Predicates
// char?, char-alphabetic?, char-numeric?, char-whitespace?, char-upper-case?, char-lower-case?
// char-unsup?, char-alpha-numeric?

pub fn is_char(val: Value) -> Result<Value, Error> {
    match Value::get_char(&val) {
        Some(_) => Ok(Value::Bool(true)),
        None => Ok(Value::Bool(false)),
    }
}

pub fn is_alpha(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_alpha() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn is_numeric(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_numeric() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn is_alphanumeric(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_alphanumeric() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn is_whitespace(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_whitespace() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn is_unsup(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_unsup() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn is_upcase(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_upper_case() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn is_downcase(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    match ch.is_lower_case() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

// Comparisson
pub fn char_eq(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() == ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_less(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() < ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_greater(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() > ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_leq(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() <= ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_geq(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_byte() >= ch2.to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_eq_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() == ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_less_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() < ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_greater_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() > ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_leq_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() <= ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

pub fn char_geq_ci(val: Value, other: Value) -> Result<Value, Error> {
    let ch1 = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    let ch2 = Value::get_char(&other).ok_or(Error::BadType(Type::Char, other.clone()))?;
    match ch1.to_lower_case().to_byte() >= ch2.to_lower_case().to_byte() {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

// Conversion
// char-upcase, char-downcase, char-integer, integer-char
pub fn char_upcase(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    Ok(Value::Char(ch.to_upper_case()))
}

pub fn char_downcase(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    Ok(Value::Char(ch.to_lower_case()))
}

pub fn char_to_integer(val: Value) -> Result<Value, Error> {
    let ch = Value::get_char(&val).ok_or(Error::BadType(Type::Char, val.clone()))?;
    Ok(Value::from(ch.to_int()))
}

pub fn integer_to_char(val: Value) -> Result<Value, Error> {
    let int = Value::get_int(&val).ok_or(Error::BadType(Type::Int, val.clone()))?;
    Ok(Value::Char(Char::from(int)))
}

// Testing ////////////////////////////////////////////////////////////////////
