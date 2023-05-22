use crate::env::Env;
use crate::err::Error;
use crate::rep::{DisplayRep, ExternalRep};
use crate::string::Str;
use crate::types::Arity;
use std::fmt::Debug;
use std::rc::Rc;

// Procedure //////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct Proc<T> {
    pub name: Str,
    pub arity: Arity,
    pub func: fn(&T) -> Result<T, Error>,
}

impl<T> Proc<T> {
    pub fn new(name: &str, arity: Arity, func: fn(&T) -> Result<T, Error>) -> Proc<T> {
        Proc {
            name: Str::from(name),
            arity: arity,
            func: func,
        }
    }
}

// Proc Traits ////////////////////////////////////////////////////////////////

impl<T> PartialEq for Proc<T> {
    fn eq(&self, other: &Proc<T>) -> bool {
        self.name == other.name && self.arity == other.arity
    }

    fn ne(&self, other: &Proc<T>) -> bool {
        !self.eq(other)
    }
}

// Representation //

impl<T> DisplayRep for Proc<T>
where
    T: Clone + Debug + DisplayRep + ExternalRep,
{
    fn to_display(&self) -> String {
        format!("#<procedure {}>", self.name.to_string())
    }
}

impl<T> ExternalRep for Proc<T>
where
    T: Clone + Debug + DisplayRep + ExternalRep,
{
    fn to_external(&self) -> String {
        format!("#<procedure {}>", self.name.to_string())
    }
}

impl<T> std::fmt::Display for Proc<T>
where
    T: Clone + Debug + DisplayRep + ExternalRep,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_display())
    }
}

impl<T> std::fmt::Debug for Proc<T>
where
    T: Clone + Debug + DisplayRep + ExternalRep,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Cell{{ {} }}", self.to_external())
    }
}

// Closure ////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct Closure<T>
where
    T: Clone,
{
    pub name: Option<Str>,
    pub env: Rc<Env<Str, T>>,
    pub formals: Formals,
    pub body: T,
}

impl<T> Closure<T>
where
    T: Clone,
{
    pub fn new(name: Option<Str>, env: Rc<Env<Str, T>>, formals: Formals, body: T) -> Closure<T> {
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

// Closure Traits /////////////////////////////////////////////////////////////

impl<T> DisplayRep for Closure<T>
where
    T: Debug + Clone + DisplayRep,
{
    fn to_display(&self) -> String {
        format!("{:?}", self)
    }
}

impl<T> ExternalRep for Closure<T>
where
    T: Debug + Clone + ExternalRep,
{
    fn to_external(&self) -> String {
        format!("{:?}", self)
    }
}

// Formals ////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Formals {
    Collect(Rc<Str>),
    Fixed(Vec<Rc<Str>>),
    Rest(Vec<Rc<Str>>, Rc<Str>),
}

// Testing ////////////////////////////////////////////////////////////////////
