use my_scheme_lib::data::cell::Cell;
use my_scheme_lib::data::env::Env;
use my_scheme_lib::data::err::Error;
use my_scheme_lib::data::number::Num;
use my_scheme_lib::data::proc::Lambda;
use my_scheme_lib::data::string::Str;
use my_scheme_lib::data::value::Value;
use my_scheme_lib::proc::env::null_env;
use std::rc::Rc;

// Procedures and Applications //

pub fn lambda(
    name: Option<&str>,
    env: Rc<Env<Str, Value>>,
    func: fn(Vec<Value>, Rc<Env<Str, Value>>) -> Result<Value, Error>,
) -> Result<Value, Error> {
    Ok(Value::Lambda(Rc::new(Lambda::new(
        name.map(|s| Str::from(s)),
        env,
        func,
    ))))
}

pub fn apply(proc: Value, args: Vec<Value>) -> Result<Value, Error> {
    match proc {
        Value::Lambda(lambda) => {
            let f = lambda.func;
            f(args, lambda.env.clone())
        }
        Value::Procedure(p) => {
            let f = p.func;
            f(&Value::list_from_vec(args, Value::Empty))
        }
        val => Err(Error::NotAProcedure(val.clone())),
    }
}

// Data Wrappers //

pub fn symbol(s: &str) -> Value {
    Value::Symbol(Rc::new(Str::from(s)))
}

pub fn rational(a: i64, b: i64) -> Value {
    Value::Number(Num::Rat(a, b))
}

pub fn cons(a: Value, b: Value) -> Value {
    Value::from(Cell::new(a, Some(b)))
}

// Environment Wrappers //

pub fn new_env() -> Rc<Env<Str, Value>> {
    null_env()
}

pub fn put(env: &Rc<Env<Str, Value>>, key: &str, val: Value) {
    env.insert(Str::from(key), val)
}

pub fn get(env: &Rc<Env<Str, Value>>, key: &str) -> Result<Value, Error> {
    env.lookup(&Str::from(key))
        .ok_or(Error::Undeclared(key.to_string()))
}

pub fn set(env: &Rc<Env<Str, Value>>, key: &str, val: Value) -> Result<Value, Error> {
    match env.set(Str::from(key), val) {
        true => Ok(Value::Empty),
        false => Err(Error::Undeclared(key.to_string())),
    }
}

pub fn push(env: &Rc<Env<Str, Value>>) -> Rc<Env<Str, Value>> {
    Env::add_scope(env.clone())
}

// List Macro //

#[macro_export]
macro_rules! list {
    ( $( $x:expr ),* ) => {
        {
        let mut result = Value::Empty;
        $(
            result = cons($x, result);
        )*
        result
        }
    }
}
