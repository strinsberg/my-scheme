use crate::char::Char;
use crate::err::Error;
use crate::proc::Proc;
use crate::proc_utils as utils;
use crate::string::Str;
use crate::types::{Arity, Type};
use crate::value::Value;

// TODO testing

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
        Proc::new(
            "substring",
            Arity::Fixed(vec![Type::String, Type::UInt, Type::UInt]),
            |args| {
                let (first, second, third) = utils::fixed_take_3(args)?;
                substring(first, second, third)
            },
        ),
        Proc::new(
            "string-append",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, rest) = utils::rest_take_1(args)?;
                string_append(first, rest)
            },
        ),
        Proc::new("string-copy", Arity::Fixed(vec![Type::String]), |args| {
            let first = utils::fixed_take_1(args)?;
            string_copy(first)
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
        // Comparisson
        Proc::new(
            "string=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_eq(first, second)
            },
        ),
        Proc::new(
            "string<?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_less(first, second)
            },
        ),
        Proc::new(
            "string>?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_greater(first, second)
            },
        ),
        Proc::new(
            "string<=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_leq(first, second)
            },
        ),
        Proc::new(
            "string>=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_geq(first, second)
            },
        ),
        Proc::new(
            "string-ci=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_eq_ci(first, second)
            },
        ),
        Proc::new(
            "string-ci<?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_less_ci(first, second)
            },
        ),
        Proc::new(
            "string-ci>?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_greater_ci(first, second)
            },
        ),
        Proc::new(
            "string-ci<=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_leq_ci(first, second)
            },
        ),
        Proc::new(
            "string-ci>=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                string_geq_ci(first, second)
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
        return Err(Error::BadArg(2));
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
    let result = Value::list_from_vec(
        s.chars().map(|ch| Value::Char(ch.clone())).collect(),
        Value::Empty,
    );
    Ok(result)
}

fn substring(string: Value, begin: Value, end: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let first = Value::get_int(&begin).ok_or(Error::BadArg(2))?;
    if first < 0 {
        return Err(Error::BadArg(2));
    }
    let last = Value::get_int(&end).ok_or(Error::BadArg(3))?;
    if last < 0 {
        return Err(Error::BadArg(3));
    }
    Ok(Value::from(s.substring(first as usize, last as usize)))
}

fn string_append(string: Value, rest: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let mut result = s.clone();
    // TODO this is the bad way, as it creates a new allocation for every append
    match Value::get_pair_cell(&rest) {
        Some(cell) => {
            for (i, other) in cell.values().enumerate() {
                let o = Value::get_string(&other).ok_or(Error::BadArg(i + 2))?;
                result = result.append(o.clone());
            }
        }
        None if rest == Value::Empty => return Ok(Value::from(result)),
        None => return Err(Error::ArgsNotList),
    }
    Ok(Value::from(result))
}

fn string_copy(string: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let chars: Vec<Char> = s.chars().map(|ch| ch.clone()).collect();
    Ok(Value::from(Str::from(chars)))
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

// Comparissons //

fn string_eq(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    match s == o {
        true => Ok(Value::Bool(true)),
        false => Ok(Value::Bool(false)),
    }
}

fn string_less(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    if s.len() == 0 && o.len() == 0 {
        return Ok(Value::Bool(false));
    }

    let mut equal = s.len() == o.len();
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_byte() > ch2.to_byte() {
                    return Ok(Value::Bool(false));
                } else if ch.to_byte() < ch2.to_byte() {
                    equal = false;
                }
            }
            None => return Ok(Value::Bool(false)),
        }
    }

    Ok(Value::Bool(true && !equal))
}

fn string_greater(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    if s.len() == 0 && o.len() == 0 {
        return Ok(Value::Bool(false));
    }

    let mut equal = true;
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_byte() < ch2.to_byte() {
                    return Ok(Value::Bool(false));
                } else if ch.to_byte() > ch2.to_byte() {
                    equal = false;
                }
            }
            None => {
                equal = false;
                break;
            }
        }
    }

    Ok(Value::Bool(true && !equal))
}

fn string_leq(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_byte() > ch2.to_byte() {
                    return Ok(Value::Bool(false));
                }
            }
            None => return Ok(Value::Bool(false)),
        }
    }
    Ok(Value::Bool(true))
}

fn string_geq(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_byte() < ch2.to_byte() {
                    return Ok(Value::Bool(false));
                }
            }
            None => break,
        }
    }
    Ok(Value::Bool(true && s.len() >= o.len()))
}

// case insensitive

fn string_eq_ci(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_lower_case().to_byte() != ch2.to_lower_case().to_byte() {
                    return Ok(Value::Bool(false));
                }
            }
            None => break,
        }
    }
    Ok(Value::Bool(true && s.len() == o.len()))
}

fn string_less_ci(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    if s.len() == 0 && o.len() == 0 {
        return Ok(Value::Bool(false));
    }

    let mut equal = s.len() == o.len();
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_lower_case().to_byte() > ch2.to_lower_case().to_byte() {
                    return Ok(Value::Bool(false));
                } else if ch.to_lower_case().to_byte() < ch2.to_lower_case().to_byte() {
                    equal = false;
                }
            }
            None => return Ok(Value::Bool(false)),
        }
    }

    Ok(Value::Bool(true && !equal))
}

fn string_greater_ci(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    if s.len() == 0 && o.len() == 0 {
        return Ok(Value::Bool(false));
    }

    let mut equal = true;
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_lower_case().to_byte() < ch2.to_lower_case().to_byte() {
                    return Ok(Value::Bool(false));
                } else if ch.to_lower_case().to_byte() > ch2.to_lower_case().to_byte() {
                    equal = false;
                }
            }
            None => {
                equal = false;
                break;
            }
        }
    }

    Ok(Value::Bool(true && !equal))
}

fn string_leq_ci(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_lower_case().to_byte() > ch2.to_lower_case().to_byte() {
                    return Ok(Value::Bool(false));
                }
            }
            None => return Ok(Value::Bool(false)),
        }
    }
    Ok(Value::Bool(true))
}

fn string_geq_ci(string: Value, other: Value) -> Result<Value, Error> {
    let s = Value::get_string(&string).ok_or(Error::BadArg(1))?;
    let o = Value::get_string(&other).ok_or(Error::BadArg(2))?;
    for (i, ch) in s.chars().enumerate() {
        match o.get(i) {
            Some(ch2) => {
                if ch.to_lower_case().to_byte() < ch2.to_lower_case().to_byte() {
                    return Ok(Value::Bool(false));
                }
            }
            None => break,
        }
    }
    Ok(Value::Bool(true && s.len() >= o.len()))
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_procs_new() {
        let arg_vec = vec![Value::from('a'), Value::from('b'), Value::from('c')];
        let args = Value::list_from_vec(arg_vec.clone(), Value::Empty);
        assert_eq!(new_string(args), Ok(Value::from("abc")));

        assert_eq!(new_string(Value::from(1)), Err(Error::ArgsNotList));

        let arg_vec = vec![Value::from('a'), Value::from(1), Value::from('c')];
        let args = Value::list_from_vec(arg_vec.clone(), Value::Empty);
        assert_eq!(new_string(args), Err(Error::BadArg(2)));
    }

    #[test]
    fn test_string_procs_make() {
        assert_eq!(make_string(Value::from(3), None), Ok(Value::from("\0\0\0")));
        assert_eq!(
            make_string(Value::from(3), Some(Value::from('a'))),
            Ok(Value::from("aaa"))
        );

        assert_eq!(
            make_string(Value::from("hello"), None),
            Err(Error::BadArg(1))
        );
        assert_eq!(
            make_string(Value::from(3), Some(Value::from(1))),
            Err(Error::BadArg(2))
        );
    }

    #[test]
    fn test_string_procs_is_string() {
        assert_eq!(is_string(Value::from("")), Ok(Value::Bool(true)));
        assert_eq!(is_string(Value::from("hello")), Ok(Value::Bool(true)));
        assert_eq!(is_string(Value::from(99)), Ok(Value::Bool(false)));
        assert_eq!(is_string(Value::from('a')), Ok(Value::Bool(false)));
    }

    #[test]
    fn test_string_procs_length() {
        assert_eq!(string_length(Value::from("")), Ok(Value::from(0)));
        assert_eq!(string_length(Value::from("hello")), Ok(Value::from(5)));

        assert_eq!(string_length(Value::from(9)), Err(Error::BadArg(1)));
    }

    #[test]
    fn test_string_procs_ref() {
        let string = Value::from("abc");
        assert_eq!(
            string_ref(string.clone(), Value::from(0)),
            Ok(Value::from('a'))
        );
        assert_eq!(
            string_ref(string.clone(), Value::from(1)),
            Ok(Value::from('b'))
        );
        assert_eq!(
            string_ref(string.clone(), Value::from(2)),
            Ok(Value::from('c'))
        );
        assert_eq!(
            string_ref(string.clone(), Value::from(3)),
            Err(Error::OutOfRange)
        );

        assert_eq!(
            string_ref(Value::from(1), Value::from(0)),
            Err(Error::BadArg(1))
        );
        assert_eq!(
            string_ref(string, Value::from("hello")),
            Err(Error::BadArg(2))
        );
    }

    #[test]
    fn test_string_procs_to_list() {
        let string = Value::from("abc");
        let list = Value::list_from_vec(
            vec![Value::from('a'), Value::from('b'), Value::from('c')],
            Value::Empty,
        );
        assert_eq!(string_to_list(string), Ok(list));
        assert_eq!(string_to_list(Value::from(8)), Err(Error::BadArg(1)));
    }

    #[test]
    fn test_string_procs_from_list() {
        let string = Value::from("abc");
        let list = Value::list_from_vec(
            vec![Value::from('a'), Value::from('b'), Value::from('c')],
            Value::Empty,
        );
        assert_eq!(list_to_string(list), Ok(string));

        let list = Value::list_from_vec(
            vec![Value::from('a'), Value::from(1), Value::from('c')],
            Value::Empty,
        );
        assert_eq!(list_to_string(list), Err(Error::BadArg(1)));
        assert_eq!(list_to_string(Value::from("hello")), Err(Error::BadArg(1)));
    }

    #[test]
    fn test_string_procs_append() {
        let string = Value::from("hello");
        let args =
            Value::list_from_vec(vec![Value::from(", "), Value::from("world!")], Value::Empty);
        assert_eq!(
            string_append(string, args),
            Ok(Value::from("hello, world!"))
        );
    }

    #[test]
    fn test_string_procs_substring() {
        assert_eq!(
            substring(Value::from("hello"), Value::from(0), Value::from(10)),
            Ok(Value::from("hello"))
        );
        assert_eq!(
            substring(Value::from("hello"), Value::from(2), Value::from(4)),
            Ok(Value::from("ll"))
        );
        assert_eq!(
            substring(Value::from("hello"), Value::from(5), Value::from(0)),
            Ok(Value::from(""))
        );

        assert_eq!(
            substring(Value::from(1), Value::from(0), Value::from(4)),
            Err(Error::BadArg(1))
        );
        assert_eq!(
            substring(Value::from("hello"), Value::from('a'), Value::from(4)),
            Err(Error::BadArg(2))
        );
        assert_eq!(
            substring(Value::from("hello"), Value::from(4), Value::from('a')),
            Err(Error::BadArg(3))
        );
    }

    #[test]
    fn test_string_procs_copy() {
        assert_eq!(string_copy(Value::from("hello")), Ok(Value::from("hello")));
        // TODO this does not really check to ensure they are not the exact same Value
        // i.e. the copy should be a newly allocated string not one that uses the
        // Rc from the original string.
    }

    // Mutation Procs //

    #[test]
    fn test_string_procs_set() {
        let string = Value::from("abc");
        let set_string = Value::from("aHc");
        assert_eq!(
            string_set(string.clone(), Value::from(1), Value::from('H')),
            Ok(Value::from('b'))
        );
        assert_eq!(
            string_set(string.clone(), Value::from(3), Value::from('H')),
            Err(Error::OutOfRange)
        );
        assert_eq!(string.clone(), set_string);

        assert_eq!(
            string_set(Value::from(9), Value::from(0), Value::from('H')),
            Err(Error::BadArg(1))
        );
        assert_eq!(
            string_set(string.clone(), Value::from("hello"), Value::from('H')),
            Err(Error::BadArg(2))
        );
        assert_eq!(
            string_set(string.clone(), Value::from(1), Value::from(99)),
            Err(Error::BadArg(3))
        );
    }

    #[test]
    fn test_string_procs_fill() {
        let string = Value::from("abc");
        let set_string = Value::from("ppp");
        assert_eq!(
            string_fill(string.clone(), Value::from('p')),
            Ok(set_string)
        );

        assert_eq!(
            string_fill(Value::from(0), Value::from('p')),
            Err(Error::BadArg(1))
        );
    }

    // Comparissons //

    #[test]
    fn test_string_procs_equal() {
        assert_eq!(
            string_eq(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_eq(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_eq(Value::from(""), Value::from("")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_eq(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_eq(Value::from("ABC"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
    }

    #[test]
    fn test_string_procs_less() {
        assert_eq!(
            string_less(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less(Value::from(""), Value::from("")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_less(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less(Value::from("aHc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_string_procs_greater() {
        assert_eq!(
            string_greater(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater(Value::from(""), Value::from("")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_greater(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_greater(Value::from("aHc"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
    }

    #[test]
    fn test_string_procs_leq() {
        assert_eq!(
            string_leq(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq(Value::from(""), Value::from("")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_leq(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_leq(Value::from("aHc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_string_procs_geq() {
        assert_eq!(
            string_geq(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq(Value::from(""), Value::from("")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_geq(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq(Value::from("aHc"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
    }

    // Comparisson Case Insensetive //

    #[test]
    fn test_string_procs_equal_ci() {
        assert_eq!(
            string_eq_ci(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_eq_ci(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_eq_ci(Value::from(""), Value::from("")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_eq_ci(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_eq_ci(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_eq_ci(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_eq_ci(Value::from("ABC"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_string_procs_less_ci() {
        assert_eq!(
            string_less_ci(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less_ci(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less_ci(Value::from("ABC"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less_ci(Value::from(""), Value::from("")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less_ci(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_less_ci(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_less_ci(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_less_ci(Value::from("aHc"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
    }

    #[test]
    fn test_string_procs_greater_ci() {
        assert_eq!(
            string_greater_ci(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater_ci(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater_ci(Value::from("abc"), Value::from("ABC")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater_ci(Value::from(""), Value::from("")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater_ci(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater_ci(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_greater_ci(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_greater_ci(Value::from("aHc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_string_procs_leq_ci() {
        assert_eq!(
            string_leq_ci(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq_ci(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq_ci(Value::from("ABC"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq_ci(Value::from(""), Value::from("")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq_ci(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq_ci(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_leq_ci(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_leq_ci(Value::from("aHc"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
    }

    #[test]
    fn test_string_procs_geq_ci() {
        assert_eq!(
            string_geq_ci(Value::from("abc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq_ci(Value::from("ABC"), Value::from("ABC")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq_ci(Value::from("abc"), Value::from("ABC")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq_ci(Value::from(""), Value::from("")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq_ci(Value::from("ab"), Value::from("abc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_geq_ci(Value::from("abc"), Value::from("ab")),
            Ok(Value::Bool(true))
        );
        assert_eq!(
            string_geq_ci(Value::from("abc"), Value::from("aHc")),
            Ok(Value::Bool(false))
        );
        assert_eq!(
            string_geq_ci(Value::from("aHc"), Value::from("abc")),
            Ok(Value::Bool(true))
        );
    }
}
