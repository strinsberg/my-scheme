use crate::scm_types::builtin::Builtin;
use crate::scm_types::number::ScmNumber;
use crate::scm_types::string::{ScmChar, ScmString};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

// Scheme Values //////////////////////////////////////////////////////////////

pub type Pointer = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ScmVal {
    Number(ScmNumber),
    Boolean(bool),
    Character(ScmChar),
    Symbol(Box<ScmString>),
    SymbolRef(Pointer),
    SymbolBox(Box<ScmString>),
    StringRef(Pointer),
    StringBox(Box<ScmString>),
    ClosureRef(Pointer),
    ClosureBox(Box<Closure>),
    Core(Builtin),
    Pair(Pointer),
    Env(Pointer),
    Vector(Vec<ScmVal>), // depricate
    VectorRef(Pointer),
    VectorBox(Box<Vec<ScmVal>>),
    HashMapBox(Box<Map>),
    HashMapRef(Pointer),
    Undefined,
    Empty,
}

// Struct to wrap HashMap /////////////////////////////////////////////////////
//
// Required because can't derive PartialEq with a HashMap in the ScmVal enum.
// So we wrap it and implement PartialEq ourselves, which for now is just always
// false. Scheme code can go farther if desired for testing equality, if we add
// maps to the language. For now all they will be for is to improve the environment
// lookup performance.

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

#[derive(Debug, Clone, PartialEq)]
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
