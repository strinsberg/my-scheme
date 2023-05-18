use crate::env::Env;
use crate::rep::{DisplayRep, ExternalRep};
use crate::string::Str;
use crate::types::Type;
use std::fmt::Debug;
use std::rc::Rc;

// Formals ////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Formals<T> {
    Collect(T),
    Fixed(Vec<T>),
    Rest(Vec<T>, T),
}

// Procedure //////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct Proc<T> {
    pub name: Str,
    pub formals: Formals<T>,
    pub signature: Vec<Type>,
}

impl<T> Proc<T> {
    pub fn new(name: Str, formals: Formals<T>, signature: Vec<Type>) -> Proc<T> {
        Proc {
            name: name,
            formals: formals,
            signature: signature,
        }
    }

    pub fn arity(&self) -> usize {
        match &self.formals {
            Formals::Collect(_) => 0,
            Formals::Fixed(vec) | Formals::Rest(vec, _) => vec.len(),
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
    pub formals: Formals<T>,
    pub body: Vec<T>,
}

impl<T> Closure<T>
where
    T: Clone,
{
    pub fn new(name: Str, env: Rc<Env<Str, T>>, formals: Formals<T>, body: Vec<T>) -> Closure<T> {
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
