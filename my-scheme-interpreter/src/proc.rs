use crate::env::Env;
use crate::err::Error;
use crate::rep::{DisplayRep, ExternalRep};
use crate::string::Str;
use crate::types::{Arity, Type};
use std::fmt::Debug;
use std::rc::Rc;

// Formals ////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Formals {
    Collect(Str),
    Fixed(Vec<Str>),
    Rest(Vec<Str>, Str),
}

// Procedure //////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct Proc<T> {
    pub name: Str,
    pub arity: Arity,
    pub types: Vec<Type>,
    pub func: fn(Vec<T>) -> Result<T, Error>,
}

impl<T> Proc<T> {
    pub fn new(
        name: &str,
        arity: Arity,
        types: Vec<Type>,
        func: fn(Vec<T>) -> Result<T, Error>,
    ) -> Proc<T> {
        Proc {
            name: Str::from(name),
            arity: arity,
            types: types,
            func: func,
        }
    }
}

impl<T> DisplayRep for Proc<T>
where
    T: Debug + DisplayRep,
{
    fn to_display(&self) -> String {
        "s".to_owned()
    }
}

impl<T> ExternalRep for Proc<T>
where
    T: Debug + ExternalRep,
{
    fn to_external(&self) -> String {
        "s".to_owned()
    }
}

// Closure ////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct Closure<T>
where
    T: Clone,
{
    pub name: Str,
    pub env: Rc<Env<Str, T>>,
    pub formals: Formals,
    pub body: Vec<T>,
}

impl<T> Closure<T>
where
    T: Clone,
{
    pub fn new(name: Str, env: Rc<Env<Str, T>>, formals: Formals, body: Vec<T>) -> Closure<T> {
        Closure {
            name: name,
            env: env,
            formals: formals,
            body: body,
        }
    }

    pub fn arity(&self) -> usize {
        match &self.formals {
            Formals::Collect(_) => 0,
            Formals::Fixed(vec) | Formals::Rest(vec, _) => vec.len(),
        }
    }
}

impl<T> DisplayRep for Closure<T>
where
    T: Debug + Clone + DisplayRep,
{
    fn to_display(&self) -> String {
        "s".to_owned()
    }
}

impl<T> ExternalRep for Closure<T>
where
    T: Debug + Clone + ExternalRep,
{
    fn to_external(&self) -> String {
        "s".to_owned()
    }
}

// Testing ////////////////////////////////////////////////////////////////////
