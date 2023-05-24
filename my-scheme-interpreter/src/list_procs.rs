use crate::cell::Cell;
use crate::err::Error;
use crate::proc::Proc;
use crate::proc_utils as utils;
use crate::types::{Arity, Type};
use crate::value::Value;

// NOTE car, cdr, variants will stay in std.scm
// TODO test new procedures

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![
        // Core //
        Proc::new("car", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            car(first)
        }),
        Proc::new("cdr", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            cdr(first)
        }),
        Proc::new("cons", Arity::Fixed(vec![Type::Any, Type::Any]), |args| {
            let (first, second) = utils::fixed_take_2(args)?;
            cons(first, second)
        }),
        Proc::new("list", Arity::Collect(Type::Any), |args| {
            new_list(args.clone())
        }),
        // Predicates //
        Proc::new("list?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_list(first)
        }),
        Proc::new("pair?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_pair(first)
        }),
        Proc::new("null?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_null(first)
        }),
        // Non-Mutating //
        Proc::new("length", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            list_length(first)
        }),
        Proc::new(
            "list-tail",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                list_tail(first, second)
            },
        ),
        Proc::new(
            "list-ref",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                list_ref(first, second)
            },
        ),
        Proc::new(
            "append",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, rest) = utils::rest_take_1(args)?;
                list_append(first, rest)
            },
        ),
        Proc::new("reverse", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            list_reverse(first)
        }),
        // Mutating //
        Proc::new(
            "set-car!",
            Arity::Fixed(vec![Type::Pair, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                set_car(first, second)
            },
        ),
        Proc::new(
            "set-cdr!",
            Arity::Fixed(vec![Type::Pair, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                set_cdr(first, second)
            },
        ),
    ]
}

// Scheme List/Pair Procedures ////////////////////////////////////////////////

// Core Procedures //

fn car(pair: Value) -> Result<Value, Error> {
    let cell = Value::get_pair_cell(&pair).ok_or(Error::BadType(Type::Pair, pair.clone()))?;
    Ok(cell.head().clone())
}

fn cdr(pair: Value) -> Result<Value, Error> {
    let cell = Value::get_pair_cell(&pair).ok_or(Error::BadType(Type::Pair, pair.clone()))?;
    match cell.tail().clone() {
        Some(tail) => Ok(tail),
        None => Ok(Value::Empty),
    }
}

fn cons(first: Value, second: Value) -> Result<Value, Error> {
    let tail = match second {
        Value::Empty => None,
        _ => Some(second.clone()),
    };
    Ok(Value::from(Cell::new(first.clone(), tail)))
}

fn new_list(args: Value) -> Result<Value, Error> {
    match args {
        Value::Empty => Ok(Value::Empty),
        _ => {
            Value::get_pair_cell(&args).expect("args list should be list");
            Ok(args.clone())
        }
    }
}

// Predicates //

fn is_pair(value: Value) -> Result<Value, Error> {
    match Value::get_pair_cell(&value) {
        Some(_) => Ok(Value::Bool(true)),
        None => Ok(Value::Bool(false)),
    }
}

fn is_list(value: Value) -> Result<Value, Error> {
    match value {
        Value::Pair(_) | Value::Empty => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn is_null(value: Value) -> Result<Value, Error> {
    match value {
        Value::Empty => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

// Non-Mutating Procedures //

fn list_length(pair: Value) -> Result<Value, Error> {
    match pair {
        Value::Pair(cell) => {
            let mut len = 0;
            for _ in cell.values() {
                len += 1;
            }
            Ok(Value::from(len))
        }
        Value::Empty => Ok(Value::from(0)),
        _ => Err(Error::BadType(Type::list(Type::Any), pair)),
    }
}

fn list_tail(pair: Value, index: Value) -> Result<Value, Error> {
    let idx = Value::get_uint(&index).ok_or(Error::BadType(Type::UInt, index))? as usize;
    if idx == 0 {
        return Ok(pair);
    }

    match pair.clone() {
        Value::Pair(cell) => {
            for (i, cell) in cell.cells().enumerate() {
                if idx == i + 1 {
                    return match cell.tail().clone() {
                        Some(v) => Ok(v),
                        None => Ok(Value::Empty),
                    };
                }
            }
            Err(Error::BadIndex(idx, pair))
        }
        Value::Empty => Err(Error::BadIndex(idx, pair)),
        _ => Err(Error::BadType(Type::list(Type::Any), pair)),
    }
}

fn list_ref(pair: Value, index: Value) -> Result<Value, Error> {
    let idx = Value::get_uint(&index).ok_or(Error::BadType(Type::UInt, index))? as usize;
    match pair.clone() {
        Value::Pair(cell) => {
            for (i, value) in cell.values().enumerate() {
                if idx == i {
                    return Ok(value.clone());
                }
            }
            Err(Error::BadIndex(idx, pair))
        }
        Value::Empty => Err(Error::BadIndex(idx, pair)),
        _ => Err(Error::BadType(Type::list(Type::Any), pair)),
    }
}

fn list_append(pair: Value, rest: Value) -> Result<Value, Error> {
    let mut arg_vec = vec![pair.clone()];
    if rest != Value::Empty {
        let args = Value::get_pair_cell(&rest).expect("rest args list should be list");
        for arg in args.values() {
            arg_vec.push(arg);
        }
    }

    let last = arg_vec[arg_vec.len() - 1].clone();
    let mut res_vec = Vec::new();
    for arg in arg_vec[..arg_vec.len() - 1].iter() {
        if arg != &Value::Empty {
            let cell = Value::get_pair_cell(arg)
                .ok_or(Error::BadType(Type::list(Type::Any), arg.clone()))?;
            for val in cell.values() {
                res_vec.push(val.clone());
            }
        }
    }

    Ok(Value::list_from_vec(res_vec, last))
}

fn list_reverse(pair: Value) -> Result<Value, Error> {
    if pair == Value::Empty {
        return Ok(pair.clone());
    }

    let cell =
        Value::get_pair_cell(&pair).ok_or(Error::BadType(Type::list(Type::Any), pair.clone()))?;
    if cell.is_dotted() {
        return Ok(Value::from(Cell::new(
            cell.tail()
                .clone()
                .expect("dotted tail should not be none")
                .clone(),
            Some(cell.head().clone()),
        )));
    }

    let mut res = Value::from(Cell::new(cell.head().clone(), None));
    match cell.tail().clone() {
        Some(tail) => {
            let tail_cell = Value::get_pair_cell(&tail)
                .expect("tail cell should not be None if val is not Dotted");
            for value in tail_cell.values() {
                res = Value::from(Cell::new(value, Some(res)));
            }
            Ok(res)
        }
        None => Ok(res),
    }
}

// Mutating Procedures //

fn set_car(pair: Value, val: Value) -> Result<Value, Error> {
    let cell =
        Value::get_pair_cell(&pair).ok_or(Error::BadType(Type::list(Type::Any), pair.clone()))?;
    Ok(cell.set_head(val.clone()))
}

fn set_cdr(pair: Value, val: Value) -> Result<Value, Error> {
    let cell =
        Value::get_pair_cell(&pair).ok_or(Error::BadType(Type::list(Type::Any), pair.clone()))?;
    let result = match val {
        Value::Empty => cell.set_tail(None),
        _ => cell.set_tail(Some(val.clone())),
    };
    match result {
        Some(v) => Ok(v),
        None => Ok(Value::Empty),
    }
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell::CellValue;

    // Helpers //

    fn make_list_5() -> Value {
        let list = Value::from(Cell::new(Value::from(5), None));
        let list2 = Value::from(Cell::new(Value::from(4), Some(list)));
        let list3 = Value::from(Cell::new(Value::from(3), Some(list2)));
        let list4 = Value::from(Cell::new(Value::from(2), Some(list3)));
        Value::from(Cell::new(Value::from(1), Some(list4)))
    }

    fn make_list_6() -> Value {
        let list = Value::from(Cell::new(Value::from(6), None));
        let list2 = Value::from(Cell::new(Value::from(5), Some(list)));
        let list3 = Value::from(Cell::new(Value::from(4), Some(list2)));
        let list4 = Value::from(Cell::new(Value::from(3), Some(list3)));
        let list5 = Value::from(Cell::new(Value::from(2), Some(list4)));
        Value::from(Cell::new(Value::from(1), Some(list5)))
    }

    fn make_list_6_dotted() -> Value {
        let list = Value::from(Cell::new(Value::from(5), Some(Value::from(6))));
        let list2 = Value::from(Cell::new(Value::from(4), Some(list)));
        let list3 = Value::from(Cell::new(Value::from(3), Some(list2)));
        let list4 = Value::from(Cell::new(Value::from(2), Some(list3)));
        Value::from(Cell::new(Value::from(1), Some(list4)))
    }

    // Pair and List Functions //

    #[test]
    fn test_list_length() {
        let list = make_list_5();
        assert_eq!(list_length(list.clone()), Ok(Value::from(5)));

        let list = make_list_6_dotted();
        assert_eq!(list_length(list.clone()), Ok(Value::from(6)));

        assert_eq!(list_length(Value::Empty), Ok(Value::from(0)));
        assert_eq!(
            list_length(Value::from(8)),
            Err(Error::BadType(Type::list(Type::Any), Value::from(8)))
        );
    }

    #[test]
    fn test_list_ref() {
        let list = make_list_5();
        assert_eq!(list_ref(list.clone(), Value::from(0)), Ok(Value::from(1)));
        assert_eq!(list_ref(list.clone(), Value::from(1)), Ok(Value::from(2)));
        assert_eq!(list_ref(list.clone(), Value::from(2)), Ok(Value::from(3)));
        assert_eq!(list_ref(list.clone(), Value::from(3)), Ok(Value::from(4)));
        assert_eq!(list_ref(list.clone(), Value::from(4)), Ok(Value::from(5)));
        assert_eq!(
            list_ref(list.clone(), Value::from(5)),
            Err(Error::BadIndex(5, list.clone()))
        );

        let list = make_list_6_dotted();
        assert_eq!(list_ref(list.clone(), Value::from(0)), Ok(Value::from(1)));
        assert_eq!(list_ref(list.clone(), Value::from(1)), Ok(Value::from(2)));
        assert_eq!(list_ref(list.clone(), Value::from(2)), Ok(Value::from(3)));
        assert_eq!(list_ref(list.clone(), Value::from(3)), Ok(Value::from(4)));
        assert_eq!(list_ref(list.clone(), Value::from(4)), Ok(Value::from(5)));
        assert_eq!(list_ref(list.clone(), Value::from(5)), Ok(Value::from(6)));
        assert_eq!(
            list_ref(list.clone(), Value::from(6)),
            Err(Error::BadIndex(6, list.clone()))
        );

        // other
        assert_eq!(
            list_ref(list.clone(), Value::from('a')),
            Err(Error::BadType(Type::UInt, Value::from('a')))
        );
        assert_eq!(
            list_ref(Value::from(8), Value::from(2)),
            Err(Error::BadType(Type::list(Type::Any), Value::from(8)))
        );
        assert_eq!(
            list_ref(Value::Empty, Value::from(0)),
            Err(Error::BadIndex(0, Value::Empty))
        );
    }

    #[test]
    fn test_list_tail() {
        let list = make_list_5();
        assert_eq!(
            list_tail(list.clone(), Value::from(0))
                .unwrap()
                .get_cell()
                .unwrap()
                .head()
                .clone(),
            Value::from(1)
        );
        assert_eq!(
            list_tail(list.clone(), Value::from(4))
                .unwrap()
                .get_cell()
                .unwrap()
                .head()
                .clone(),
            Value::from(5)
        );
        assert_eq!(list_tail(list.clone(), Value::from(5)), Ok(Value::Empty));
        assert_eq!(
            list_tail(list.clone(), Value::from(6)),
            Err(Error::BadIndex(6, list.clone()))
        );

        let list = make_list_6_dotted();
        assert_eq!(
            list_tail(list.clone(), Value::from(0))
                .unwrap()
                .get_cell()
                .unwrap()
                .head()
                .clone(),
            Value::from(1)
        );
        assert_eq!(
            list_tail(list.clone(), Value::from(4))
                .unwrap()
                .get_cell()
                .unwrap()
                .head()
                .clone(),
            Value::from(5)
        );
        assert_eq!(
            list_tail(list.clone(), Value::from(5)).unwrap(),
            Value::from(6)
        );
        assert_eq!(
            list_tail(list.clone(), Value::from(6)),
            Err(Error::BadIndex(6, list.clone()))
        );

        // Other
        assert_eq!(
            list_tail(list.clone(), Value::from('a')),
            Err(Error::BadType(Type::UInt, Value::from('a')))
        );
        assert_eq!(
            list_tail(Value::Empty, Value::from(1)),
            Err(Error::BadIndex(1, Value::Empty))
        );
        assert_eq!(list_tail(Value::Empty, Value::from(0)), Ok(Value::Empty));
    }

    #[test]
    fn test_list_reverse() {
        let list = make_list_5();
        let rev_list = list_reverse(list.clone()).unwrap();
        let cell = rev_list.get_cell().unwrap();
        let mut iter = cell.values();
        assert_eq!(iter.next(), Some(Value::from(5)));
        assert_eq!(iter.next(), Some(Value::from(4)));
        assert_eq!(iter.next(), Some(Value::from(3)));
        assert_eq!(iter.next(), Some(Value::from(2)));
        assert_eq!(iter.next(), Some(Value::from(1)));
        assert_eq!(iter.next(), None);

        let list = Value::from(make_list_6_dotted());
        let rev_list = list_reverse(list.clone()).unwrap();
        let cell = rev_list.get_cell().unwrap();
        let mut iter = cell.values();
        assert_eq!(iter.next(), Some(Value::from(6)));
        assert_eq!(iter.next(), Some(Value::from(5)));
        assert_eq!(iter.next(), Some(Value::from(4)));
        assert_eq!(iter.next(), Some(Value::from(3)));
        assert_eq!(iter.next(), Some(Value::from(2)));
        assert_eq!(iter.next(), Some(Value::from(1)));
        assert_eq!(iter.next(), None);

        // Other
        assert_eq!(list_reverse(Value::Empty), Ok(Value::Empty));
        assert_eq!(
            list_reverse(Value::from(5)),
            Err(Error::BadType(Type::list(Type::Any), Value::from(5)))
        );
    }

    #[test]
    fn test_list_append() {
        let list = make_list_5();
        let rest =
            Value::list_from_vec(vec![list.clone(), list.clone(), list.clone()], Value::Empty);
        let list2 = list_append(list.clone(), rest).unwrap();
        assert_eq!(list_ref(list2.clone(), Value::from(0)), Ok(Value::from(1)));
        assert_eq!(list_ref(list2.clone(), Value::from(4)), Ok(Value::from(5)));
        assert_eq!(list_ref(list2.clone(), Value::from(9)), Ok(Value::from(5)));
        assert_eq!(list_ref(list2.clone(), Value::from(14)), Ok(Value::from(5)));
        assert_eq!(list_ref(list2.clone(), Value::from(19)), Ok(Value::from(5)));
        assert_eq!(
            list_ref(list2.clone(), Value::from(20)),
            Err(Error::BadIndex(20, list2.clone()))
        );

        let list = make_list_6_dotted();
        let rest =
            Value::list_from_vec(vec![list.clone(), list.clone(), list.clone()], Value::Empty);
        let list2 = list_append(list.clone(), rest).unwrap();
        assert_eq!(list_ref(list2.clone(), Value::from(0)), Ok(Value::from(1)));
        assert_eq!(list_ref(list2.clone(), Value::from(5)), Ok(Value::from(6)));
        assert_eq!(list_ref(list2.clone(), Value::from(11)), Ok(Value::from(6)));
        assert_eq!(list_ref(list2.clone(), Value::from(17)), Ok(Value::from(6)));
        assert_eq!(list_ref(list2.clone(), Value::from(23)), Ok(Value::from(6)));
        assert_eq!(
            list_ref(list2.clone(), Value::from(24)),
            Err(Error::BadIndex(24, list2.clone()))
        );

        // Other
        assert_eq!(
            list_append(
                Value::Empty,
                Value::from(Cell::new(make_list_6_dotted(), None))
            ),
            Ok(make_list_6_dotted())
        );
        assert_eq!(
            list_append(make_list_6_dotted(), Value::Empty),
            Ok(make_list_6_dotted())
        );
        assert_eq!(
            list_append(Value::from(5), list.clone()),
            Err(Error::BadType(Type::list(Type::Any), Value::from(5)))
        );
        assert_eq!(
            list_append(make_list_5(), Value::from(Cell::new(Value::from(6), None))),
            Ok(make_list_6_dotted())
        );
    }
}
