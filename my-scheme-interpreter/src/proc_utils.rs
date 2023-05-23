use crate::cell::{CellIter, CellValueIter};
use crate::err::Error;
use crate::value::Value;

pub fn validate_args_list(args: &Value) {
    match args {
        Value::Pair(_) | Value::Empty => (),
        _ => panic!("argument list should be a list: {:?}", args),
    }
}

fn args_to_values(args: &Value) -> Result<CellValueIter<Value>, Error> {
    validate_args_list(args);
    match Value::get_pair_cell(args) {
        Some(cell) => Ok(cell.values()),
        None => return Err(Error::Arity),
    }
}

fn args_to_cells(args: &Value) -> Result<CellIter<Value>, Error> {
    validate_args_list(args);
    match Value::get_pair_cell(args) {
        Some(cell) => Ok(cell.cells()),
        None => return Err(Error::Arity),
    }
}

// For Fixed Arg Lists ////////////////////////////////////////////////////////

pub fn fixed_take_1(args: &Value) -> Result<Value, Error> {
    let mut iter = args_to_values(args)?;
    match iter.next() {
        Some(val) => Ok(val),
        None => Err(Error::Arity),
    }
}

pub fn fixed_take_2(args: &Value) -> Result<(Value, Value), Error> {
    let mut iter = args_to_values(args)?;
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
    let mut iter = args_to_values(args)?;
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

// For Rest Arg Lists /////////////////////////////////////////////////////////

pub fn rest_take_1(args: &Value) -> Result<(Value, Value), Error> {
    validate_args_list(args);
    let cell = match Value::get_pair_cell(args) {
        Some(cell) => cell,
        None => return Err(Error::Arity),
    };
    let first = cell.head().clone();
    let second = match cell.tail().clone() {
        Some(val) => val,
        None => Value::Empty,
    };
    Ok((first, second))
}

pub fn rest_take_2(args: &Value) -> Result<(Value, Value, Value), Error> {
    let mut iter = args_to_cells(args)?;
    let first = match iter.next() {
        Some(cell) => cell.head().clone(),
        None => return Err(Error::Arity),
    };
    let (second, third) = match iter.next() {
        Some(cell) => (
            cell.head().clone(),
            match cell.tail().clone() {
                Some(val) => val,
                None => Value::Empty,
            },
        ),
        None => return Err(Error::Arity),
    };
    Ok((first, second, third))
}

// For Optional Arg Lists /////////////////////////////////////////////////////

pub fn opt_last_take_2(args: &Value) -> Result<(Value, Option<Value>), Error> {
    let mut iter = args_to_values(args)?;
    let first = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    Ok((first, iter.next()))
}

pub fn opt_last_take_3(args: &Value) -> Result<(Value, Value, Option<Value>), Error> {
    let mut iter = args_to_values(args)?;
    let first = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    let second = match iter.next() {
        Some(val) => val,
        None => return Err(Error::Arity),
    };
    Ok((first, second, iter.next()))
}
