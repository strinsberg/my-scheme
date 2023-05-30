use crate::data::err::Error;
use crate::data::string::Str;
use crate::data::types::Type;
use crate::data::value::Value;
use std::rc::Rc;

// Boolean Procedures /////////////////////////////////////////////////////////

pub fn is_bool(val: Value) -> Result<Value, Error> {
    match val {
        Value::Bool(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn not(val: Value) -> Result<Value, Error> {
    match val {
        Value::Bool(b) => Ok(Value::Bool(!b)),
        _ => Ok(Value::Bool(false)),
    }
}

// Symbol Procedures //////////////////////////////////////////////////////////

pub fn is_symbol(val: Value) -> Result<Value, Error> {
    match val {
        Value::Symbol(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn symbol_to_string(symbol: Value) -> Result<Value, Error> {
    let s = Value::get_symbol_str(&symbol).ok_or(Error::BadType(Type::Symbol, symbol.clone()))?;
    let mut chars = Vec::new();
    for ch in s.chars() {
        chars.push(ch.clone())
    }
    Ok(Value::from(Str::from(chars)))
}

pub fn string_to_symbol(string: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadType(Type::String, string.clone()))?;
    let mut chars = Vec::new();
    for ch in s.chars() {
        chars.push(ch.clone())
    }
    Ok(Value::Symbol(Rc::new(Str::from(chars))))
}

// Control Flow ///////////////////////////////////////////////////////////////

pub fn is_procedure(val: Value) -> Result<Value, Error> {
    match val {
        Value::Procedure(_) | Value::Closure(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

// Equality

pub fn are_eqv(first: Value, second: Value) -> Result<Value, Error> {
    // The only thing off about this possibly is that they say something about
    // closures not being eqv when they return different args or have different
    // side effects. Since they must be the same closure to be eqv this does not make
    // perfect sense to me, but perhaps it means that some closures that have the
    // same location may closure on local state or have side effects that would
    // make them not equal even when they are the same closure?
    //
    // NOTE it is worth thinking about when Rc::eq might not work to compare
    // two objects, I assume it compares the location both point to which should
    // be reliable as even if the data could be moved it would have to be updated for
    // all Rc values.
    match (first, second) {
        (Value::Bool(f), Value::Bool(s)) => Ok(Value::Bool(f == s)),
        (Value::Symbol(f), Value::Symbol(s)) => Ok(Value::Bool(Rc::ptr_eq(&f, &s) || f == s)),
        (Value::Number(f), Value::Number(s)) => Ok(Value::Bool(f.eqv(&s))),
        (Value::Char(f), Value::Char(s)) => Ok(Value::Bool(f == s)),
        (Value::Procedure(f), Value::Procedure(s)) => Ok(Value::Bool(Rc::ptr_eq(&f, &s))),
        (Value::Closure(f), Value::Closure(s)) => Ok(Value::Bool(Rc::ptr_eq(&f, &s))),
        (Value::Array(f), Value::Array(s)) => Ok(Value::Bool(Rc::ptr_eq(&f, &s))),
        (Value::String(f), Value::String(s)) => Ok(Value::Bool(Rc::ptr_eq(&f, &s))),
        (Value::Pair(f), Value::Pair(s)) => Ok(Value::Bool(Rc::ptr_eq(&f, &s))),
        (Value::Empty, Value::Empty) => Ok(Value::Bool(true)),
        (Value::Undefined, Value::Undefined) => Ok(Value::Bool(true)),
        // Do not care about special forms, or environments for now
        _ => Ok(Value::Bool(false)),
    }
}

pub fn are_eq(first: Value, second: Value) -> Result<Value, Error> {
    // The things that the standard says that eq? may differ from eqv? are
    // things that we do not have the ability to do. I.e. numbers and characters
    // are not held in pointers so there is no way to do pointer comparisson.
    // So we will just use eqv? for eq?.
    are_eqv(first, second)
}

pub fn are_equal(first: Value, second: Value) -> Result<Value, Error> {
    // Assuming that our Partial equal implementations correctly check the
    // equality of collections recursively based on contents and not pointers
    // just changing the collections from Rc::eq to == should do the trick.
    // The standard says that generally objects are equal? if they print the
    // same, so we can always use that fact in tests to see that objects that
    // are equal? also print the same and identify when that might not be true.
    // Either way there are plenty of examples in the standard and it is easy
    // to think of others to test deep equality.
    match (first, second) {
        (Value::Bool(f), Value::Bool(s)) => Ok(Value::Bool(f == s)),
        (Value::Symbol(f), Value::Symbol(s)) => Ok(Value::Bool(f == s)),
        (Value::Number(f), Value::Number(s)) => Ok(Value::Bool(f.eqv(&s))),
        (Value::Char(f), Value::Char(s)) => Ok(Value::Bool(f == s)),
        (Value::Procedure(f), Value::Procedure(s)) => Ok(Value::Bool(f == s)),
        (Value::Closure(f), Value::Closure(s)) => Ok(Value::Bool(f == s)),
        (Value::Array(f), Value::Array(s)) => Ok(Value::Bool(f == s)),
        (Value::String(f), Value::String(s)) => Ok(Value::Bool(f == s)),
        (Value::Pair(f), Value::Pair(s)) => Ok(Value::Bool(f == s)),
        (Value::Empty, Value::Empty) => Ok(Value::Bool(true)),
        (Value::Undefined, Value::Undefined) => Ok(Value::Bool(true)),
        // Do not care about special forms, or environments for now
        _ => Ok(Value::Bool(false)),
    }
}

// Testing ////////////////////////////////////////////////////////////////////
