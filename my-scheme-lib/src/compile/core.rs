use crate::data::err::Error;
use crate::data::number::Num;
use crate::data::proc::Lambda;
use crate::data::string::Str;
use crate::data::value::Value;
use crate::proc::lists;
use std::rc::Rc;

pub fn car(a: Value) -> Result<Value, Error> {
    lists::cdr(a)
}

pub fn cdr(a: Value) -> Result<Value, Error> {
    lists::cdr(a)
}

pub fn cons(a: Value, b: Value) -> Result<Value, Error> {
    lists::cons(a, b)
}

pub fn lambda(name: Option<Str>, func: fn(Vec<Value>) -> Result<Value, Error>) -> Value {
    Value::Lambda(Rc::new(Lambda::new(name, func)))
}

pub fn apply(proc: Value, args: Vec<Value>) -> Result<Value, Error> {
    match proc {
        Value::Lambda(lambda) => {
            let f = lambda.func;
            f(args)
        }
        _ => Err(Error::NotAProcedure(proc)),
    }
}

pub fn rational(a: i64, b: i64) -> Value {
    Value::Number(Num::Rat(a, b))
}
