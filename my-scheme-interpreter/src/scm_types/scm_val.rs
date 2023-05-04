use crate::scm_types::builtin::Builtin;
use crate::scm_types::number::ScmNumber;
use crate::scm_types::string::{ScmChar, ScmString};
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// This is an attempt to add Rc<RefCell> and Rc<Cell> to the interpreter instead
// of using a heap that needs garbage collection.

// Scheme Values //////////////////////////////////////////////////////////////

pub type Pointer = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScmVal {
    Number(ScmNumber),
    Boolean(bool),
    Character(ScmChar),
    Symbol(Rc<ScmString>),
    Closure(Rc<Closure>),
    Core(Builtin),
    Pair(Rc<RefCell<ConsCell>>),
    Env(Rc<RefCell<Map>>),
    String(Rc<ScmString>),
    StringMut(Rc<RefCell<ScmString>>),
    Vector(Rc<Vec<ScmVal>>),
    VectorMut(Rc<RefCell<Vec<ScmVal>>>),
    HashMap(Rc<Map>),
    HashMapMut(Rc<RefCell<Map>>),
    Undefined,
    Empty,
}

impl Hash for ScmVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            ScmVal::Number(val) => val.hash(state),
            ScmVal::Boolean(val) => val.hash(state),
            ScmVal::Character(val) => val.hash(state),
            ScmVal::Symbol(val) => val.hash(state),
            ScmVal::String(val) => val.hash(state),
            ScmVal::StringMut(val) => val.borrow().hash(state),
            ScmVal::Closure(val) => val.hash(state),
            ScmVal::Core(val) => val.hash(state),
            ScmVal::Pair(val) => val.borrow().hash(state),
            ScmVal::Env(val) => val.borrow().hash(state),
            ScmVal::Vector(val) => val.hash(state),
            ScmVal::VectorMut(val) => val.borrow().hash(state),
            ScmVal::HashMap(val) => val.hash(state),
            ScmVal::HashMapMut(val) => val.borrow().hash(state),
            val => val.hash(state),
        }
    }
}

// Struct to wrap HashMap /////////////////////////////////////////////////////
//
// Required because can't derive PartialEq with a HashMap in the ScmVal enum.
// Equality of hash maps will be implemented in scheme instead, because it
// requires looking over all of the internal values and calling equal? recursively
// like a list or vector. Eqv? can just use the pointer which may need to be
// setup in here.

#[derive(Debug, Clone)]
pub struct Map {
    pub hash_map: HashMap<ScmVal, ScmVal>,
}

impl Map {
    pub fn new(map: HashMap<ScmVal, ScmVal>) -> Map {
        Map { hash_map: map }
    }
}

impl PartialEq for Map {
    fn eq(&self, _other: &Map) -> bool {
        false
    }

    fn ne(&self, _other: &Map) -> bool {
        false
    }
}

impl Eq for Map {}

// TODO this is almost certainly inefficient
impl Hash for Map {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash_map.keys().for_each(|x| x.hash(state));
        self.hash_map.values().for_each(|x| x.hash(state));
    }
}

// Closure Wrapper ////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Closure {
    pub env: Pointer,
    pub params: Vec<ScmVal>,
    pub body: Vec<ScmVal>,
}

impl Closure {
    pub fn new(env: Pointer, params: Vec<ScmVal>, body: Vec<ScmVal>) -> Closure {
        Closure {
            env: env,
            params: params,
            body: body,
        }
    }
}

// Cons Cells /////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsCell {
    pub head: ScmVal,
    pub tail: ScmVal,
}

impl ConsCell {
    pub fn default() -> ConsCell {
        ConsCell {
            head: ScmVal::Empty,
            tail: ScmVal::Empty,
        }
    }

    pub fn new(head: ScmVal, tail: ScmVal) -> ConsCell {
        ConsCell {
            head: head,
            tail: tail,
        }
    }

    pub fn is_dotted(&self) -> bool {
        match self.tail {
            ScmVal::Pair(_) | ScmVal::Empty => false,
            _ => true,
        }
    }
}
