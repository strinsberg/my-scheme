use crate::err::Error;
use crate::value::Value;

// For Fixed Arg Lists ////////////////////////////////////////////////////////

pub fn fixed_take_1(args: &Value) -> Result<Value, Error> {
    let mut iter = match Value::get_pair_cell(args) {
        Some(cell) => cell.values(),
        None => return Err(Error::ArgsNotList),
    };
    match iter.next() {
        Some(val) => Ok(val),
        None => Err(Error::Arity),
    }
}

pub fn fixed_take_2(args: &Value) -> Result<(Value, Value), Error> {
    let mut iter = match Value::get_pair_cell(args) {
        Some(cell) => cell.values(),
        None => return Err(Error::ArgsNotList),
    };
    let first = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    let second = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    Ok((first, second))
}

pub fn fixed_take_3(args: &Value) -> Result<(Value, Value, Value), Error> {
    let mut iter = match Value::get_pair_cell(args) {
        Some(cell) => cell.values(),
        None => return Err(Error::ArgsNotList),
    };
    let first = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    let second = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    let third = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    Ok((first, second, third))
}

// For Optional Arg Lists /////////////////////////////////////////////////////

pub fn opt_last_take_2(args: &Value) -> Result<(Value, Option<Value>), Error> {
    let mut iter = match Value::get_pair_cell(args) {
        Some(cell) => cell.values(),
        None => return Err(Error::ArgsNotList),
    };
    let first = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    Ok((first, iter.next()))
}
