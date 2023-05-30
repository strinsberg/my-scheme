use crate::data::env::Env;
use crate::data::err::Error;
use crate::data::rep::{DisplayRep, ExternalRep};
use crate::data::string::Str;
use crate::data::types::Arity;
use std::cell::RefCell;
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
        write!(f, "Procedure{{ {} }}", self.to_external())
    }
}

// Closure ////////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq)]
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
        match self.name.clone() {
            Some(name) => format!("#<procedure {}>", name),
            None => format!("#<closure>",),
        }
    }
}

impl<T> ExternalRep for Closure<T>
where
    T: Debug + Clone + ExternalRep,
{
    fn to_external(&self) -> String {
        match self.name.clone() {
            Some(name) => format!("#<procedure {}>", name),
            None => format!("#<closure>",),
        }
    }
}

impl<T> std::fmt::Display for Closure<T>
where
    T: Clone + Debug + DisplayRep + ExternalRep,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_display())
    }
}

impl<T> std::fmt::Debug for Closure<T>
where
    T: Clone + Debug + DisplayRep + ExternalRep,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Closure{{ {} }}", self.to_external())
    }
}

// Formals ////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Formals {
    Collect(Rc<Str>),
    Fixed(Vec<Rc<Str>>),
    Rest(Vec<Rc<Str>>, Rc<Str>),
}

// Promise ////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct Promise<T>
where
    T: Clone + Debug + PartialEq,
{
    env: Rc<Env<Str, T>>,
    val: RefCell<Option<T>>,
    thunk: fn(Rc<Env<Str, T>>) -> Result<T, Error>,
}

impl<T> Promise<T>
where
    T: Clone + Debug + PartialEq,
{
    pub fn new(env: Rc<Env<Str, T>>, thunk: fn(Rc<Env<Str, T>>) -> Result<T, Error>) -> Promise<T> {
        Promise {
            env: env,
            val: RefCell::new(None),
            thunk: thunk,
        }
    }

    pub fn get(&self) -> Result<T, Error> {
        let val = self.val.borrow().clone();
        match val {
            Some(v) => Ok(v),
            None => {
                let thunk = self.thunk;
                let v = thunk(self.env.clone())?;
                self.val.replace(Some(v.clone()));
                Ok(v)
            }
        }
    }
}

impl<T> PartialEq for Promise<T>
where
    T: Clone + Debug + PartialEq,
{
    fn eq(&self, other: &Promise<T>) -> bool {
        match self.val.borrow().clone() {
            Some(v) => match other.val.borrow().clone() {
                Some(v2) => v == v2,
                None => false,
            },
            None => false,
        }
    }

    fn ne(&self, other: &Promise<T>) -> bool {
        !self.eq(other)
    }
}

impl<T> DisplayRep for Promise<T>
where
    T: Debug + Clone + PartialEq + DisplayRep,
{
    fn to_display(&self) -> String {
        match self.val.borrow().clone() {
            Some(val) => val.to_display(),
            None => format!("#<promise>",),
        }
    }
}

impl<T> ExternalRep for Promise<T>
where
    T: Debug + Clone + PartialEq + ExternalRep,
{
    fn to_external(&self) -> String {
        match self.val.borrow().clone() {
            Some(val) => val.to_external(),
            None => format!("#<promise>",),
        }
    }
}

impl<T> std::fmt::Display for Promise<T>
where
    T: Clone + Debug + PartialEq + DisplayRep + ExternalRep,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_display())
    }
}

impl<T> std::fmt::Debug for Promise<T>
where
    T: Clone + Debug + PartialEq + DisplayRep + ExternalRep,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Lambda{{ {} }}", self.to_external())
    }
}
