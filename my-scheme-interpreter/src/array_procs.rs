use crate::array::Array;
use crate::err::Error;
use crate::proc::Proc;
use crate::proc_utils as utils;
use crate::types::{Arity, Type};
use crate::value::Value;

// TODO testing

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![
        // Construct
        Proc::new("vector", Arity::Collect(Type::Any), |args| {
            utils::validate_args_list(args);
            new_array(args.clone())
        }),
        Proc::new(
            "make-vector",
            Arity::Fixed(vec![Type::UInt, Type::opt(Type::Any)]),
            |args| {
                let (first, second) = utils::opt_last_take_2(args)?;
                make_array(first, second)
            },
        ),
        // Predicate
        Proc::new("vector?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_array(first)
        }),
        // Non-Mutating
        Proc::new("vector-length", Arity::Fixed(vec![Type::Array]), |args| {
            let first = utils::fixed_take_1(args)?;
            array_length(first)
        }),
        Proc::new(
            "vector-ref",
            Arity::Fixed(vec![Type::Array, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                array_ref(first, second)
            },
        ),
        Proc::new(
            "list->vector",
            Arity::Fixed(vec![Type::list(Type::Any)]),
            |args| {
                let first = utils::fixed_take_1(args)?;
                list_to_array(first)
            },
        ),
        Proc::new("vector->list", Arity::Fixed(vec![Type::Array]), |args| {
            let first = utils::fixed_take_1(args)?;
            array_to_list(first)
        }),
        // Mutating
        Proc::new(
            "vector-set!",
            Arity::Fixed(vec![Type::Array, Type::UInt, Type::Any]),
            |args| {
                let (first, second, third) = utils::fixed_take_3(args)?;
                array_set(first, second, third)
            },
        ),
        Proc::new(
            "vector-fill!",
            Arity::Fixed(vec![Type::Array, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                array_fill(first, second)
            },
        ),
    ]
}

// Array Procedures ///////////////////////////////////////////////////////////

// Construct //

fn new_array(args: Value) -> Result<Value, Error> {
    Ok(Value::from(Array::from(args)))
}

fn make_array(size: Value, fill: Option<Value>) -> Result<Value, Error> {
    let size = Value::get_uint(&size).ok_or(Error::BadType(Type::UInt, size))?;
    let val = match fill {
        Some(val) => val.clone(),
        None => Value::default(),
    };
    Ok(Value::from(Array::new(val, size as usize)))
}

// Predicates //

fn is_array(array: Value) -> Result<Value, Error> {
    match Value::get_array(&array) {
        Some(_) => Ok(Value::Bool(true)),
        None => Ok(Value::Bool(false)),
    }
}

// Non-Mutating Procedures //

fn array_length(array: Value) -> Result<Value, Error> {
    let arr = Value::get_array(&array).ok_or(Error::BadType(Type::Array, array.clone()))?;
    Ok(Value::from(arr.len() as i64))
}

fn array_ref(array: Value, index: Value) -> Result<Value, Error> {
    let arr = Value::get_array(&array).ok_or(Error::BadType(Type::Array, array.clone()))?;
    let idx = Value::get_uint(&index).ok_or(Error::BadType(Type::UInt, index))?;
    match arr.get(idx as usize) {
        Some(val) => Ok(val.clone()),
        None => Err(Error::BadIndex(idx as usize, array.clone())),
    }
}

fn list_to_array(pair: Value) -> Result<Value, Error> {
    match pair {
        Value::Pair(_) | Value::Empty => Ok(Value::from(Array::from(pair))),
        _ => Err(Error::BadType(Type::Array, pair)),
    }
}

fn array_to_list(array: Value) -> Result<Value, Error> {
    let arr = Value::get_array(&array).ok_or(Error::BadType(Type::Array, array.clone()))?;
    let result = Value::list_from_vec(arr.values().collect(), Value::Empty);
    Ok(result)
}

// Mutating Procedures //

fn array_set(array: Value, index: Value, val: Value) -> Result<Value, Error> {
    let arr = Value::get_array(&array).ok_or(Error::BadType(Type::Array, array.clone()))?;
    let idx = Value::get_uint(&index).ok_or(Error::BadType(Type::UInt, index))?;
    match arr.set(val, idx as usize) {
        Some(val) => Ok(val),
        None => Err(Error::BadIndex(idx as usize, array)),
    }
}

fn array_fill(array: Value, val: Value) -> Result<Value, Error> {
    let arr = Value::get_array(&array).ok_or(Error::BadType(Type::Array, array.clone()))?;
    arr.fill(val);
    Ok(array)
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_array_procs_new_array() {
        let arg_vec = vec![Value::from(1), Value::from(2), Value::from(3)];
        let args = Value::list_from_vec(arg_vec.clone(), Value::Empty);
        assert_eq!(new_array(args), Ok(Value::from(Array::from(arg_vec))));
        assert_eq!(
            new_array(Value::Empty),
            Ok(Value::from(Array::from(vec![])))
        );
    }

    #[test]
    fn test_array_procs_make_array() {
        let vec = vec![Value::default(), Value::default(), Value::default()];
        assert_eq!(
            make_array(Value::from(3), None),
            Ok(Value::from(Array::from(vec)))
        );

        let vec = vec![Value::from(1), Value::from(1), Value::from(1)];
        assert_eq!(
            make_array(Value::from(3), Some(Value::from(1))),
            Ok(Value::from(Array::from(vec)))
        );

        assert_eq!(
            make_array(Value::from("hello"), None),
            Err(Error::BadType(Type::UInt, Value::from("hello")))
        );
    }

    #[test]
    fn test_array_procs_is_array() {
        assert_eq!(
            is_array(Value::from(Array::from(vec![]))),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            is_array(Value::from(Array::from(vec![
                Value::from(1),
                Value::from("hello")
            ]))),
            Ok(Value::Bool(true))
        );
        assert_eq!(is_array(Value::from(99)), Ok(Value::Bool(false)));
        assert_eq!(is_array(Value::from("hello")), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_array_procs_length() {
        let arr = Value::from(Array::from(vec![
            Value::from(1),
            Value::from(2),
            Value::from(3),
        ]));
        assert_eq!(array_length(arr.clone()), Ok(Value::from(3)));

        assert_eq!(
            array_length(Value::from("hello")),
            Err(Error::BadType(Type::Array, Value::from("hello")))
        );
    }

    #[test]
    fn test_array_procs_ref() {
        let arr = Value::from(Array::from(vec![
            Value::from(1),
            Value::from(2),
            Value::from(3),
        ]));
        assert_eq!(array_ref(arr.clone(), Value::from(0)), Ok(Value::from(1)));
        assert_eq!(array_ref(arr.clone(), Value::from(1)), Ok(Value::from(2)));
        assert_eq!(array_ref(arr.clone(), Value::from(2)), Ok(Value::from(3)));
        assert_eq!(
            array_ref(arr.clone(), Value::from(3)),
            Err(Error::BadIndex(3, arr.clone()))
        );

        assert_eq!(
            array_ref(Value::from("hello"), Value::from(0)),
            Err(Error::BadType(Type::Array, Value::from("hello")))
        );
        assert_eq!(
            array_ref(arr, Value::from("hello")),
            Err(Error::BadType(Type::UInt, Value::from("hello")))
        );
    }

    #[test]
    fn test_array_procs_to_list() {
        let arr = Value::from(Array::from(vec![
            Value::from(1),
            Value::from(2),
            Value::from(3),
        ]));
        let list = Value::list_from_vec(
            vec![Value::from(1), Value::from(2), Value::from(3)],
            Value::Empty,
        );
        assert_eq!(array_to_list(arr), Ok(list));
        assert_eq!(
            array_to_list(Value::from("hello")),
            Err(Error::BadType(Type::Array, Value::from("hello")))
        );
    }

    #[test]
    fn test_array_procs_from_list() {
        let arr = Value::from(Array::from(vec![
            Value::from(1),
            Value::from(2),
            Value::from(3),
        ]));
        let list = Value::list_from_vec(
            vec![Value::from(1), Value::from(2), Value::from(3)],
            Value::Empty,
        );
        assert_eq!(list_to_array(list), Ok(arr));
        assert_eq!(
            list_to_array(Value::Empty),
            Ok(Value::from(Array::from(vec![])))
        );
        assert_eq!(
            list_to_array(Value::from("hello")),
            Err(Error::BadType(Type::Array, Value::from("hello")))
        );
    }

    #[test]
    fn test_array_procs_set() {
        let arr = Value::from(Array::from(vec![
            Value::from(1),
            Value::from(2),
            Value::from(3),
        ]));
        let set_arr = Value::from(Array::from(vec![
            Value::from(1),
            Value::from(99),
            Value::from(3),
        ]));
        assert_eq!(
            array_set(arr.clone(), Value::from(1), Value::from(99)),
            Ok(Value::from(2))
        );
        assert_eq!(
            array_set(arr.clone(), Value::from(3), Value::from(99)),
            Err(Error::BadIndex(3, set_arr.clone()))
        );
        assert_eq!(arr.clone(), set_arr);

        assert_eq!(
            array_set(Value::from("hello"), Value::from(0), Value::from(2)),
            Err(Error::BadType(Type::Array, Value::from("hello")))
        );
        assert_eq!(
            array_set(arr, Value::from("hello"), Value::from(2)),
            Err(Error::BadType(Type::UInt, Value::from("hello")))
        );
    }

    #[test]
    fn test_array_procs_fill() {
        let arr = Value::from(Array::from(vec![
            Value::from(1),
            Value::from(2),
            Value::from(3),
        ]));
        let set_arr = Value::from(Array::from(vec![
            Value::from(99),
            Value::from(99),
            Value::from(99),
        ]));
        assert_eq!(array_fill(arr.clone(), Value::from(99)), Ok(set_arr));

        assert_eq!(
            array_fill(Value::from("hello"), Value::from(0)),
            Err(Error::BadType(Type::Array, Value::from("hello")))
        );
    }
}
