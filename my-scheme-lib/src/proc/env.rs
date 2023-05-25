use crate::data::env::Env;
use crate::data::proc::Proc;
use crate::data::string::Str;
use crate::data::types::Arity;
use crate::data::types::Type;
use crate::data::value::Value;
use crate::proc::arrays;
use crate::proc::chars;
use crate::proc::lists;
use crate::proc::numbers;
use crate::proc::others;
use crate::proc::strings;
use crate::proc::utils;
use std::rc::Rc;

// Env Creation ///////////////////////////////////////////////////////////////

pub fn null_env() -> Rc<Env<Str, Value>> {
    let env = Env::new();
    for proc in make_list_procs()
        .iter()
        .chain(make_array_procs().iter())
        .chain(make_char_procs().iter())
        .chain(make_string_procs().iter())
        .chain(make_number_procs().iter())
        .chain(make_other_procs().iter())
        .chain(make_null_env_procs().iter())
    {
        env.insert(proc.name.clone(), Value::from(proc.clone()));
    }
    Rc::new(env)
}

fn make_null_env_procs() -> Vec<Proc<Value>> {
    vec![Proc::new("null-environment", Arity::Fixed(vec![]), |_| {
        Ok(Value::Env(null_env()))
    })]
}

// Standard R5RS Procedures ///////////////////////////////////////////////////

/*** Lists ***/

fn make_list_procs() -> Vec<Proc<Value>> {
    vec![
        // Core //
        Proc::new("car", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            lists::car(first)
        }),
        Proc::new("cdr", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            lists::cdr(first)
        }),
        Proc::new("cons", Arity::Fixed(vec![Type::Any, Type::Any]), |args| {
            let (first, second) = utils::fixed_take_2(args)?;
            lists::cons(first, second)
        }),
        Proc::new("list", Arity::Collect(Type::Any), |args| {
            lists::new_list(args.clone())
        }),
        // Predicates //
        Proc::new("list?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            lists::is_list(first)
        }),
        Proc::new("pair?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            lists::is_pair(first)
        }),
        Proc::new("null?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            lists::is_null(first)
        }),
        // Non-Mutating //
        Proc::new("length", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            lists::list_length(first)
        }),
        Proc::new(
            "list-tail",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                lists::list_tail(first, second)
            },
        ),
        Proc::new(
            "list-ref",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                lists::list_ref(first, second)
            },
        ),
        Proc::new(
            "append",
            Arity::Fixed(vec![Type::Pair, Type::UInt]),
            |args| {
                let (first, rest) = utils::rest_take_1(args)?;
                lists::list_append(first, rest)
            },
        ),
        Proc::new("reverse", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            lists::list_reverse(first)
        }),
        // Mutating //
        Proc::new(
            "set-car!",
            Arity::Fixed(vec![Type::Pair, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                lists::set_car(first, second)
            },
        ),
        Proc::new(
            "set-cdr!",
            Arity::Fixed(vec![Type::Pair, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                lists::set_cdr(first, second)
            },
        ),
    ]
}

/*** Arrays ***/

fn make_array_procs() -> Vec<Proc<Value>> {
    vec![
        // Construct
        Proc::new("vector", Arity::Collect(Type::Any), |args| {
            utils::validate_args_list(args);
            arrays::new_array(args.clone())
        }),
        Proc::new(
            "make-vector",
            Arity::Fixed(vec![Type::UInt, Type::opt(Type::Any)]),
            |args| {
                let (first, second) = utils::opt_last_take_2(args)?;
                arrays::make_array(first, second)
            },
        ),
        // Predicate
        Proc::new("vector?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            arrays::is_array(first)
        }),
        // Non-Mutating
        Proc::new("vector-length", Arity::Fixed(vec![Type::Array]), |args| {
            let first = utils::fixed_take_1(args)?;
            arrays::array_length(first)
        }),
        Proc::new(
            "vector-ref",
            Arity::Fixed(vec![Type::Array, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                arrays::array_ref(first, second)
            },
        ),
        Proc::new(
            "list->vector",
            Arity::Fixed(vec![Type::list(Type::Any)]),
            |args| {
                let first = utils::fixed_take_1(args)?;
                arrays::list_to_array(first)
            },
        ),
        Proc::new("vector->list", Arity::Fixed(vec![Type::Array]), |args| {
            let first = utils::fixed_take_1(args)?;
            arrays::array_to_list(first)
        }),
        // Mutating
        Proc::new(
            "vector-set!",
            Arity::Fixed(vec![Type::Array, Type::UInt, Type::Any]),
            |args| {
                let (first, second, third) = utils::fixed_take_3(args)?;
                arrays::array_set(first, second, third)
            },
        ),
        Proc::new(
            "vector-fill!",
            Arity::Fixed(vec![Type::Array, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                arrays::array_fill(first, second)
            },
        ),
    ]
}

/*** Strings ***/

fn make_string_procs() -> Vec<Proc<Value>> {
    vec![
        // Construct
        Proc::new("string", Arity::Collect(Type::Char), |args| {
            strings::new_string(args.clone())
        }),
        Proc::new(
            "make-string",
            Arity::Fixed(vec![Type::UInt, Type::opt(Type::Char)]),
            |args| {
                let (first, second) = utils::opt_last_take_2(args)?;
                strings::make_string(first, second)
            },
        ),
        // Predicates
        Proc::new("string?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            strings::is_string(first)
        }),
        // Non-Mutating
        Proc::new("string-length", Arity::Fixed(vec![Type::String]), |args| {
            let first = utils::fixed_take_1(args)?;
            strings::string_length(first)
        }),
        Proc::new(
            "string-ref",
            Arity::Fixed(vec![Type::String, Type::UInt]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_ref(first, second)
            },
        ),
        Proc::new("list->string", Arity::Fixed(vec![Type::Pair]), |args| {
            let first = utils::fixed_take_1(args)?;
            strings::list_to_string(first)
        }),
        Proc::new("string->list", Arity::Fixed(vec![Type::String]), |args| {
            let first = utils::fixed_take_1(args)?;
            strings::string_to_list(first)
        }),
        Proc::new(
            "substring",
            Arity::Fixed(vec![Type::String, Type::UInt, Type::UInt]),
            |args| {
                let (first, second, third) = utils::fixed_take_3(args)?;
                strings::substring(first, second, third)
            },
        ),
        Proc::new(
            "string-append",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, rest) = utils::rest_take_1(args)?;
                strings::string_append(first, rest)
            },
        ),
        Proc::new("string-copy", Arity::Fixed(vec![Type::String]), |args| {
            let first = utils::fixed_take_1(args)?;
            strings::string_copy(first)
        }),
        // Mutating
        Proc::new(
            "string-set!",
            Arity::Fixed(vec![Type::String, Type::UInt, Type::Char]),
            |args| {
                let (first, second, third) = utils::fixed_take_3(args)?;
                strings::string_set(first, second, third)
            },
        ),
        Proc::new(
            "string-fill!",
            Arity::Fixed(vec![Type::String, Type::Any]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_fill(first, second)
            },
        ),
        // Comparisson
        Proc::new(
            "string=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_eq(first, second)
            },
        ),
        Proc::new(
            "string<?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_less(first, second)
            },
        ),
        Proc::new(
            "string>?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_greater(first, second)
            },
        ),
        Proc::new(
            "string<=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_leq(first, second)
            },
        ),
        Proc::new(
            "string>=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_geq(first, second)
            },
        ),
        Proc::new(
            "string-ci=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_eq_ci(first, second)
            },
        ),
        Proc::new(
            "string-ci<?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_less_ci(first, second)
            },
        ),
        Proc::new(
            "string-ci>?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_greater_ci(first, second)
            },
        ),
        Proc::new(
            "string-ci<=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_leq_ci(first, second)
            },
        ),
        Proc::new(
            "string-ci>=?",
            Arity::Fixed(vec![Type::String, Type::String]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                strings::string_geq_ci(first, second)
            },
        ),
    ]
}

/*** Chars ***/

fn make_char_procs() -> Vec<Proc<Value>> {
    vec![
        Proc::new("char?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::is_char(first)
        }),
        Proc::new("char-alphabetic?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::is_alpha(first)
        }),
        Proc::new("char-numeric?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::is_numeric(first)
        }),
        Proc::new("char-whitespace?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::is_whitespace(first)
        }),
        Proc::new("char-upper-case?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::is_upcase(first)
        }),
        Proc::new("char-lower-case?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::is_downcase(first)
        }),
        Proc::new(
            "char-alphanumeric?",
            Arity::Fixed(vec![Type::Char]),
            |args| {
                let first = utils::fixed_take_1(args)?;
                chars::is_alphanumeric(first)
            },
        ),
        Proc::new("char-unsup?", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::is_unsup(first)
        }),
        // Comparisson
        Proc::new(
            "char=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_eq(first, second)
            },
        ),
        Proc::new(
            "char<?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_less(first, second)
            },
        ),
        Proc::new(
            "char>?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_greater(first, second)
            },
        ),
        Proc::new(
            "char<=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_leq(first, second)
            },
        ),
        Proc::new(
            "char>=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_geq(first, second)
            },
        ),
        Proc::new(
            "char-ci=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_eq_ci(first, second)
            },
        ),
        Proc::new(
            "char-ci<?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_less_ci(first, second)
            },
        ),
        Proc::new(
            "char-ci>?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_greater_ci(first, second)
            },
        ),
        Proc::new(
            "char-ci<=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_leq_ci(first, second)
            },
        ),
        Proc::new(
            "char-ci>=?",
            Arity::Fixed(vec![Type::Char, Type::Char]),
            |args| {
                let (first, second) = utils::fixed_take_2(args)?;
                chars::char_geq_ci(first, second)
            },
        ),
        // Conversion
        Proc::new("char-upcase", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::char_upcase(first)
        }),
        Proc::new("char-downcase", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::char_downcase(first)
        }),
        Proc::new("char->integer", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::char_to_integer(first)
        }),
        Proc::new("integer->char", Arity::Fixed(vec![Type::Char]), |args| {
            let first = utils::fixed_take_1(args)?;
            chars::integer_to_char(first)
        }),
    ]
}

/*** Numbers ***/

fn make_number_procs() -> Vec<Proc<Value>> {
    vec![
        // predicates
        Proc::new("number?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            numbers::is_number(first)
        }),
        // TODO these are supposed to be a tower, i.e. an integer is a real
        Proc::new("real?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            numbers::is_real(first)
        }),
        Proc::new("rational?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            numbers::is_rational(first)
        }),
        Proc::new("integer?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            numbers::is_integer(first)
        }),
        // basic arithmetic
        Proc::new("+", Arity::Collect(Type::Number), |args| {
            numbers::add(args.clone())
        }),
        Proc::new("*", Arity::Collect(Type::Number), |args| {
            numbers::multiply(args.clone())
        }),
        Proc::new("-", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            numbers::subtract(first, rest)
        }),
        Proc::new("/", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            numbers::divide(first, rest)
        }),
        // Comparisson
        Proc::new("=", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            numbers::num_eq(first, rest)
        }),
        Proc::new("<", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            numbers::num_less(first, rest)
        }),
        Proc::new(">", Arity::Rest(vec![Type::Number], Type::Number), |args| {
            let (first, rest) = utils::rest_take_1(args)?;
            numbers::num_greater(first, rest)
        }),
        Proc::new(
            "<=",
            Arity::Rest(vec![Type::Number], Type::Number),
            |args| {
                let (first, rest) = utils::rest_take_1(args)?;
                numbers::num_leq(first, rest)
            },
        ),
        Proc::new(
            ">=",
            Arity::Rest(vec![Type::Number], Type::Number),
            |args| {
                let (first, rest) = utils::rest_take_1(args)?;
                numbers::num_geq(first, rest)
            },
        ),
    ]
}

/*** Symbol, Bool, Control Flow ***/

fn make_other_procs() -> Vec<Proc<Value>> {
    vec![
        // Boolean
        Proc::new("boolean?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            others::is_bool(first)
        }),
        Proc::new("not", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            others::not(first)
        }),
        // Symbol
        Proc::new("symbol?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            others::is_symbol(first)
        }),
        Proc::new("symbol->string", Arity::Fixed(vec![Type::Symbol]), |args| {
            let first = utils::fixed_take_1(args)?;
            others::symbol_to_string(first)
        }),
        Proc::new("string->symbol", Arity::Fixed(vec![Type::String]), |args| {
            let first = utils::fixed_take_1(args)?;
            others::string_to_symbol(first)
        }),
        // Control flow
        Proc::new("procedure?", Arity::Fixed(vec![Type::Any]), |args| {
            let first = utils::fixed_take_1(args)?;
            others::is_procedure(first)
        }),
        // Equality
        Proc::new("eqv?", Arity::Fixed(vec![Type::Any, Type::Any]), |args| {
            let (first, second) = utils::fixed_take_2(args)?;
            others::are_eqv(first, second)
        }),
        Proc::new("eq?", Arity::Fixed(vec![Type::Any, Type::Any]), |args| {
            let (first, second) = utils::fixed_take_2(args)?;
            others::are_eq(first, second)
        }),
        Proc::new("equal?", Arity::Fixed(vec![Type::Any, Type::Any]), |args| {
            let (first, second) = utils::fixed_take_2(args)?;
            others::are_equal(first, second)
        }),
        // Eval/Apply
        // These exist here just because they need procedures, but they are
        // essentially special forms that get evaluated by the vm directly.
        Proc::new("eval", Arity::Fixed(vec![Type::Any, Type::Env]), |_| {
            panic!("eval is evaluated by the vm")
        }),
        Proc::new(
            "apply",
            Arity::Fixed(vec![
                Type::Proc,
                Type::dots(Type::Any),
                Type::list(Type::Any),
            ]),
            |_| panic!("apply is evaluated by the vm"),
        ),
    ]
}
