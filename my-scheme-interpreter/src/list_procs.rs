use crate::cell::Cell;
use crate::err::Error;
use crate::proc::Proc;
use crate::proc_utils as utils;
use crate::types::{Arity, Type};
use crate::value::Value;

// NOTE car, cdr, variants will stay in std.scm
// TODO Now that these are called with wrappers that produce values and not &values
// remove the & from all of them, it is unecessary.
// TODO test new procedures

// Exported Procedures ////////////////////////////////////////////////////////

pub fn make_procs() -> Vec<Proc<Value>> {
    vec![
        // Core //
        Proc::new("car", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            car(&first)
        }),
        Proc::new("cdr", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            cdr(&first)
        }),
        Proc::new("cons", Arity::Fixed(vec![Type::Any, Type::Any]), |args| {
            let (first, second) = utils::fixed_take_2(args)?;
            cons(&first, &second)
        }),
        Proc::new("list", Arity::Collect(Type::Any), |args| new_list(&args)),
        // Predicates //
        Proc::new("list?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_list(&first)
        }),
        Proc::new("pair?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_pair(&first)
        }),
        Proc::new("null?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            is_null(&first)
        }),
        // Non-Mutating //
        Proc::new("length", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            list_length(&first)
        }),
        Proc::new(
            "list-tail",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                list_tail(&first, &second)
            },
        ),
        Proc::new(
            "list-ref",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                list_ref(&first, &second)
            },
        ),
        Proc::new(
            "append",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                list_append(&first, &second)
            },
        ),
        Proc::new("reverse", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            list_reverse(&first)
        }),
        // Mutating //
        Proc::new(
            "set-car!",
            Arity::Fixed(vec![Type::Pair, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                set_car(&first, &second)
            },
        ),
        Proc::new(
            "set-cdr!",
            Arity::Fixed(vec![Type::Pair, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                set_cdr(&first, &second)
            },
        ),
    ]
}

// Scheme List/Pair Procedures ////////////////////////////////////////////////

// Core Procedures //

fn car(pair: &Value) -> Result<Value, Error> {
    let cell = Value::get_pair_cell(pair).ok_or(Error::BadArg(1))?;
    Ok(cell.head().clone())
}

fn cdr(pair: &Value) -> Result<Value, Error> {
    let cell = Value::get_pair_cell(pair).ok_or(Error::BadArg(1))?;
    match cell.tail().clone() {
        Some(tail) => Ok(tail),
        None => Ok(Value::Empty),
    }
}

fn cons(first: &Value, second: &Value) -> Result<Value, Error> {
    if second == &Value::Empty {
        Ok(Value::from(Cell::new(first.clone(), None)))
    } else {
        Ok(Value::from(Cell::new(first.clone(), Some(second.clone()))))
    }
}

fn new_list(args: &Value) -> Result<Value, Error> {
    Value::get_pair_cell(args).ok_or(Error::ArgsNotList)?;
    Ok(args.clone())
}

// Predicates //

fn is_pair(value: &Value) -> Result<Value, Error> {
    match Value::get_pair_cell(value) {
        Some(_) => Ok(Value::Bool(true)),
        None => Ok(Value::Bool(false)),
    }
}

fn is_list(value: &Value) -> Result<Value, Error> {
    is_pair(value)
}

fn is_null(value: &Value) -> Result<Value, Error> {
    match value {
        Value::Empty => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

// Non-Mutating Procedures //

fn list_length(pair: &Value) -> Result<Value, Error> {
    if pair == &Value::Empty {
        return Ok(Value::from(0));
    }

    let cell = Value::get_pair_cell(pair).ok_or(Error::BadArg(1))?;
    let mut len = 0;
    for _ in cell.values() {
        len += 1;
    }
    Ok(Value::from(len))
}

fn list_tail(pair: &Value, index: &Value) -> Result<Value, Error> {
    let idx = Value::get_int(index).ok_or(Error::BadArg(2))? as usize;
    if idx == 0 {
        return Ok(pair.clone());
    } else if pair == &Value::Empty {
        return Err(Error::OutOfRange);
    }

    let cell = Value::get_pair_cell(pair).ok_or(Error::BadArg(1))?;
    for (i, cell) in cell.cells().enumerate() {
        if idx == i + 1 {
            return match cell.tail().clone() {
                Some(v) => Ok(v),
                None => Ok(Value::Empty),
            };
        }
    }
    Err(Error::OutOfRange)
}

fn list_ref(pair: &Value, index: &Value) -> Result<Value, Error> {
    if pair == &Value::Empty {
        return Err(Error::OutOfRange);
    }

    let cell = Value::get_pair_cell(pair).ok_or(Error::BadArg(1))?;
    let idx = Value::get_int(index).ok_or(Error::BadArg(2))? as usize;
    for (i, value) in cell.values().enumerate() {
        if idx == i {
            return Ok(value.clone());
        }
    }
    Err(Error::OutOfRange)
}

fn list_append(pair: &Value, other: &Value) -> Result<Value, Error> {
    if pair == &Value::Empty {
        return Ok(other.clone());
    }

    let rev = &list_reverse(pair).or(Err(Error::BadArg(1)))?;
    let rev_cell = Value::get_pair_cell(rev).expect("reversed list should not be None");
    let mut result = match other {
        Value::Empty => None,
        o => Some(o.clone()),
    };

    for value in rev_cell.values() {
        result = Some(Value::from(Cell::new(value.clone(), result)));
    }
    Ok(result.expect("result should not be None"))
}

fn list_reverse(pair: &Value) -> Result<Value, Error> {
    if pair == &Value::Empty {
        return Ok(pair.clone());
    }

    let cell = Value::get_pair_cell(pair).ok_or(Error::BadArg(1))?;
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

fn set_car(pair: &Value, val: &Value) -> Result<Value, Error> {
    let cell = Value::get_pair_cell(pair).ok_or(Error::BadArg(1))?;
    Ok(cell.set_head(val.clone()))
}

fn set_cdr(pair: &Value, val: &Value) -> Result<Value, Error> {
    let cell = Value::get_pair_cell(pair).ok_or(Error::BadArg(1))?;
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
        assert_eq!(list_length(&list), Ok(Value::from(5)));

        let list = make_list_6_dotted();
        assert_eq!(list_length(&list), Ok(Value::from(6)));

        assert_eq!(list_length(&Value::Empty), Ok(Value::from(0)));
        assert_eq!(list_length(&Value::from(8)), Err(Error::BadArg(1)));
    }

    #[test]
    fn test_list_ref() {
        let list = make_list_5();
        assert_eq!(list_ref(&list, &Value::from(0)), Ok(Value::from(1)));
        assert_eq!(list_ref(&list, &Value::from(1)), Ok(Value::from(2)));
        assert_eq!(list_ref(&list, &Value::from(2)), Ok(Value::from(3)));
        assert_eq!(list_ref(&list, &Value::from(3)), Ok(Value::from(4)));
        assert_eq!(list_ref(&list, &Value::from(4)), Ok(Value::from(5)));
        assert_eq!(list_ref(&list, &Value::from(5)), Err(Error::OutOfRange));

        let list = make_list_6_dotted();
        assert_eq!(list_ref(&list, &Value::from(0)), Ok(Value::from(1)));
        assert_eq!(list_ref(&list, &Value::from(1)), Ok(Value::from(2)));
        assert_eq!(list_ref(&list, &Value::from(2)), Ok(Value::from(3)));
        assert_eq!(list_ref(&list, &Value::from(3)), Ok(Value::from(4)));
        assert_eq!(list_ref(&list, &Value::from(4)), Ok(Value::from(5)));
        assert_eq!(list_ref(&list, &Value::from(5)), Ok(Value::from(6)));
        assert_eq!(list_ref(&list, &Value::from(6)), Err(Error::OutOfRange));

        // other
        assert_eq!(list_ref(&list, &Value::from('a')), Err(Error::BadArg(2)));
        assert_eq!(
            list_ref(&Value::from(8), &Value::from(2)),
            Err(Error::BadArg(1))
        );
        assert_eq!(
            list_ref(&Value::Empty, &Value::from(0)),
            Err(Error::OutOfRange)
        );
    }

    #[test]
    fn test_list_tail() {
        let list = make_list_5();
        assert_eq!(
            list_tail(&list, &Value::from(0))
                .unwrap()
                .get_cell()
                .unwrap()
                .head()
                .clone(),
            Value::from(1)
        );
        assert_eq!(
            list_tail(&list, &Value::from(4))
                .unwrap()
                .get_cell()
                .unwrap()
                .head()
                .clone(),
            Value::from(5)
        );
        assert_eq!(list_tail(&list, &Value::from(5)), Ok(Value::Empty));
        assert_eq!(list_tail(&list, &Value::from(6)), Err(Error::OutOfRange));

        let list = make_list_6_dotted();
        assert_eq!(
            list_tail(&list, &Value::from(0))
                .unwrap()
                .get_cell()
                .unwrap()
                .head()
                .clone(),
            Value::from(1)
        );
        assert_eq!(
            list_tail(&list, &Value::from(4))
                .unwrap()
                .get_cell()
                .unwrap()
                .head()
                .clone(),
            Value::from(5)
        );
        assert_eq!(list_tail(&list, &Value::from(5)).unwrap(), Value::from(6));
        assert_eq!(list_tail(&list, &Value::from(6)), Err(Error::OutOfRange));

        // Other
        assert_eq!(list_tail(&list, &Value::from('a')), Err(Error::BadArg(2)));
        assert_eq!(
            list_tail(&Value::Empty, &Value::from(1)),
            Err(Error::OutOfRange)
        );
        assert_eq!(list_tail(&Value::Empty, &Value::from(0)), Ok(Value::Empty));
    }

    #[test]
    fn test_list_reverse() {
        let list = make_list_5();
        let rev_list = list_reverse(&list).unwrap();
        let cell = rev_list.get_cell().unwrap();
        let mut iter = cell.values();
        assert_eq!(iter.next(), Some(Value::from(5)));
        assert_eq!(iter.next(), Some(Value::from(4)));
        assert_eq!(iter.next(), Some(Value::from(3)));
        assert_eq!(iter.next(), Some(Value::from(2)));
        assert_eq!(iter.next(), Some(Value::from(1)));
        assert_eq!(iter.next(), None);

        let list = Value::from(make_list_6_dotted());
        let rev_list = list_reverse(&list).unwrap();
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
        assert_eq!(list_reverse(&Value::Empty), Ok(Value::Empty));
        assert_eq!(list_reverse(&Value::from(5)), Err(Error::BadArg(1)));
    }

    #[test]
    fn test_list_append() {
        let list = make_list_5();
        let list2 = list_append(&list, &list).unwrap();
        assert_eq!(list_ref(&list2, &Value::from(0)), Ok(Value::from(1)));
        assert_eq!(list_ref(&list2, &Value::from(1)), Ok(Value::from(2)));
        assert_eq!(list_ref(&list2, &Value::from(2)), Ok(Value::from(3)));
        assert_eq!(list_ref(&list2, &Value::from(3)), Ok(Value::from(4)));
        assert_eq!(list_ref(&list2, &Value::from(4)), Ok(Value::from(5)));
        assert_eq!(list_ref(&list2, &Value::from(5)), Ok(Value::from(1)));
        assert_eq!(list_ref(&list2, &Value::from(6)), Ok(Value::from(2)));
        assert_eq!(list_ref(&list2, &Value::from(7)), Ok(Value::from(3)));
        assert_eq!(list_ref(&list2, &Value::from(8)), Ok(Value::from(4)));
        assert_eq!(list_ref(&list2, &Value::from(9)), Ok(Value::from(5)));
        assert_eq!(list_ref(&list2, &Value::from(10)), Err(Error::OutOfRange));

        let list = make_list_6_dotted();
        let list2 = list_append(&list, &list).unwrap();
        assert_eq!(list_ref(&list2, &Value::from(0)), Ok(Value::from(1)));
        assert_eq!(list_ref(&list2, &Value::from(1)), Ok(Value::from(2)));
        assert_eq!(list_ref(&list2, &Value::from(2)), Ok(Value::from(3)));
        assert_eq!(list_ref(&list2, &Value::from(3)), Ok(Value::from(4)));
        assert_eq!(list_ref(&list2, &Value::from(4)), Ok(Value::from(5)));
        assert_eq!(list_ref(&list2, &Value::from(5)), Ok(Value::from(6)));
        assert_eq!(list_ref(&list2, &Value::from(6)), Ok(Value::from(1)));
        assert_eq!(list_ref(&list2, &Value::from(7)), Ok(Value::from(2)));
        assert_eq!(list_ref(&list2, &Value::from(8)), Ok(Value::from(3)));
        assert_eq!(list_ref(&list2, &Value::from(9)), Ok(Value::from(4)));
        assert_eq!(list_ref(&list2, &Value::from(10)), Ok(Value::from(5)));
        assert_eq!(list_ref(&list2, &Value::from(11)), Ok(Value::from(6)));
        assert_eq!(list_ref(&list2, &Value::from(12)), Err(Error::OutOfRange));

        // Other
        assert_eq!(list_append(&Value::Empty, &list), Ok(list.clone()));
        assert_eq!(list_append(&list, &Value::Empty), Ok(make_list_6()));
        assert_eq!(list_append(&Value::from(5), &list), Err(Error::BadArg(1)));
        assert_eq!(
            list_append(&make_list_5(), &Value::from(6)),
            Ok(make_list_6_dotted())
        );
    }
}
