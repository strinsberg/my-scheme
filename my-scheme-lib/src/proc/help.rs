use crate::data::cell::Cell;
use crate::data::env::Env;
use crate::data::err::Error;
use crate::data::proc::Lambda;
use crate::data::string::Str;
use crate::data::value::Value;
use crate::proc::env::null_env;
use std::rc::Rc;

pub type Environment = Rc<Env<Str, Value>>;

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

pub fn cons(a: Value, b: Value) -> Value {
    Value::from(Cell::new(a, Some(b)))
}

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
