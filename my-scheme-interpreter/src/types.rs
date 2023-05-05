use crate::number::ScmNumber;
use crate::string::{ScmChar, ScmString};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::zip;
use std::rc::Rc;

// Scheme Values //////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScmVal {
    Number(ScmNumber),
    Boolean(bool),
    Character(ScmChar),
    Symbol(Rc<ScmString>),
    Closure(Rc<Closure>),
    Core(Builtin),
    Pair(Rc<RefCell<ConsCell>>),
    Env(Rc<RefCell<Env>>),
    String(Rc<ScmString>),
    StringMut(Rc<RefCell<ScmString>>),
    Vector(Rc<Vec<ScmVal>>),
    VectorMut(Rc<RefCell<Vec<ScmVal>>>),
    HashMap(Rc<Map>),
    HashMapMut(Rc<RefCell<Map>>),
    Undefined,
    Empty,
}

impl ScmVal {
    // Constructor Helpers //

    pub fn new_sym(string: &str) -> ScmVal {
        ScmVal::Symbol(Rc::new(ScmString::new(string)))
    }

    pub fn new_char(ch: char) -> ScmVal {
        ScmVal::Character(ScmChar::from_char(ch))
    }

    pub fn new_int(i: i64) -> ScmVal {
        ScmVal::Number(ScmNumber::Integer(i))
    }

    pub fn new_float(f: f64) -> ScmVal {
        ScmVal::Number(ScmNumber::Float(f))
    }

    pub fn new_pair(head: ScmVal, tail: ScmVal) -> ScmVal {
        ScmVal::Pair(Rc::new(RefCell::new(ConsCell::new(head, tail))))
    }

    pub fn new_str_mut(string: &str) -> ScmVal {
        ScmVal::StringMut(Rc::new(RefCell::new(ScmString::new(string))))
    }

    pub fn new_str_mut_from_scmstring(string: ScmString) -> ScmVal {
        ScmVal::StringMut(Rc::new(RefCell::new(string)))
    }

    pub fn new_str(string: &str) -> ScmVal {
        ScmVal::String(Rc::new(ScmString::new(string)))
    }

    pub fn str_eq(&self, s: &str) -> bool {
        match self {
            ScmVal::StringMut(ref_str) => ref_str.borrow().to_string() == s,
            ScmVal::String(rc_str) => rc_str.to_string() == s,
            _ => panic!("should be ScmVal::StringMut"),
        }
    }

    pub fn new_closure(closure: Closure) -> ScmVal {
        ScmVal::Closure(Rc::new(closure))
    }

    pub fn new_vec_mut(vec: Vec<ScmVal>) -> ScmVal {
        ScmVal::VectorMut(Rc::new(RefCell::new(vec)))
    }

    pub fn new_vec(vec: Vec<ScmVal>) -> ScmVal {
        ScmVal::Vector(Rc::new(vec))
    }

    pub fn new_map_mut(map: HashMap<ScmVal, ScmVal>) -> ScmVal {
        ScmVal::HashMapMut(Rc::new(RefCell::new(Map::new(map))))
    }

    pub fn new_map(map: HashMap<ScmVal, ScmVal>) -> ScmVal {
        ScmVal::HashMap(Rc::new(Map::new(map)))
    }

    pub fn null_env() -> ScmVal {
        ScmVal::Env(ScmVal::null_env_rc())
    }

    pub fn null_env_rc() -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env::new_with_bindings(vec![
            (ScmVal::new_sym("apply"), ScmVal::Core(Builtin::Apply)),
            (ScmVal::new_sym("eval"), ScmVal::Core(Builtin::Eval)),
            (
                ScmVal::new_sym("null-environment"),
                ScmVal::Core(Builtin::BaseEnv),
            ),
            // List core
            (ScmVal::new_sym("cons"), ScmVal::Core(Builtin::Cons)),
            (ScmVal::new_sym("car"), ScmVal::Core(Builtin::Car)),
            (ScmVal::new_sym("cdr"), ScmVal::Core(Builtin::Cdr)),
            // Core Arithmetic
            (ScmVal::new_sym("+"), ScmVal::Core(Builtin::Sum)),
            (ScmVal::new_sym("-"), ScmVal::Core(Builtin::Subtract)),
            (ScmVal::new_sym("*"), ScmVal::Core(Builtin::Product)),
            (ScmVal::new_sym("/"), ScmVal::Core(Builtin::Divide)),
            // Comparisson
            (ScmVal::new_sym("eqv?"), ScmVal::Core(Builtin::Eqv)),
            // Type Predicates
        ])))
    }

    // If end is ScmVal::Empty it will be list otherwise dotted list
    pub fn vec_to_list(end: ScmVal, values: Vec<ScmVal>) -> ScmVal {
        values
            .into_iter()
            .rev()
            .fold(end, |acc, v| ScmVal::new_pair(v, acc))
    }

    pub fn list_to_vec(val: ScmVal) -> Vec<ScmVal> {
        match val {
            ScmVal::Pair(cell) => ListValIter::new(cell).collect(),
            ScmVal::Empty => vec![],
            _ => panic!("should be ScmVal::Pair"),
        }
    }
}

// ScmVal Hash //

impl Hash for ScmVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ScmVal::Number(val) => val.hash(state),
            ScmVal::Boolean(val) => val.hash(state),
            ScmVal::Character(val) => val.hash(state),
            ScmVal::Symbol(val) => val.hash(state),
            ScmVal::String(val) => val.hash(state),
            ScmVal::StringMut(val) => val.borrow().hash(state),
            ScmVal::Closure(_) => panic!("Closures cannot be hashed"),
            ScmVal::Core(val) => val.hash(state),
            ScmVal::Pair(val) => val.borrow().hash(state),
            ScmVal::Env(_) => panic!("Environemnt cannot be hashed"),
            ScmVal::Vector(val) => val.hash(state),
            ScmVal::VectorMut(val) => val.borrow().hash(state),
            ScmVal::HashMap(_) => panic!("Hash map cannot be hashed"),
            ScmVal::HashMapMut(_) => panic!("Hash map cannot be hashed"),
            val => val.hash(state),
        }
    }
}

// ScmVal Display //

// This is not tail recursive and may be an issue if used in print etr.
impl fmt::Display for ScmVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScmVal::Number(val) => write!(f, "{}", val),
            ScmVal::Boolean(val) => write!(f, "#{}", if *val { "t" } else { "f" }),
            ScmVal::Character(val) => write!(f, "{}", val),
            ScmVal::Symbol(val) => write!(f, "{}", val),
            ScmVal::String(val) => write!(f, "{}", val),
            ScmVal::StringMut(val) => write!(f, "{}", val.borrow()),
            ScmVal::Closure(_) => write!(f, "#<CLOSURE>"),
            ScmVal::Core(val) => write!(f, "#<PROCEDURE {:?}>", val),
            ScmVal::Pair(val) => display_list(f, Rc::clone(val)),
            ScmVal::Env(_) => write!(f, "#<ENVIRONMENT>"),
            ScmVal::Vector(val) => display_vec(f, Rc::clone(val)),
            ScmVal::VectorMut(val) => display_vec_mut(f, Rc::clone(val)),
            ScmVal::HashMap(val) => write!(f, "{:?}", val),
            ScmVal::HashMapMut(val) => write!(f, "{:?}", val.borrow()),
            ScmVal::Empty => write!(f, "()"),
            val => write!(f, "{:?}", val),
        }
    }
}

fn display_list(f: &mut fmt::Formatter, cell: Rc<RefCell<ConsCell>>) -> fmt::Result {
    let iter = ListValIterAndDot::new(cell);
    let mut dotted = false;
    let vec_string: Vec<String> = iter
        .map(|(v, d)| {
            dotted |= d;
            v.to_string()
        })
        .collect();

    if dotted {
        write!(
            f,
            "({} . {})",
            vec_string[..vec_string.len() - 1].join(" "),
            vec_string[vec_string.len() - 1]
        )
    } else {
        write!(f, "({})", vec_string.join(" "))
    }
}

fn display_vec(f: &mut fmt::Formatter, vec: Rc<Vec<ScmVal>>) -> fmt::Result {
    let vec_string: Vec<String> = vec.iter().map(|v| v.to_string()).collect();
    write!(f, "#({})", vec_string.join(" "))
}

fn display_vec_mut(f: &mut fmt::Formatter, vec: Rc<RefCell<Vec<ScmVal>>>) -> fmt::Result {
    let vec_string: Vec<String> = vec.borrow().iter().map(|v| v.to_string()).collect();
    write!(f, "#({})", vec_string.join(" "))
}

// Builtin Procedures /////////////////////////////////////////////////////////

// A way to properly identify builtin procedures so that their rust functions
// can be used and they appear different from Closures. The u8 is the arity.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Builtin {
    Cons,
    Car,
    Cdr,
    Eval,
    Apply,
    Sum,
    Subtract,
    Product,
    Divide,
    EQ,
    Eqv,
    BaseEnv,
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

// ConsCell Iterators //

#[derive(Debug, Clone)]
pub struct ListValIter {
    iter: ListValIterAndDot,
}

impl ListValIter {
    pub fn new(cell: Rc<RefCell<ConsCell>>) -> ListValIter {
        ListValIter {
            iter: ListValIterAndDot::new(cell),
        }
    }
}

impl Iterator for ListValIter {
    type Item = ScmVal;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some((v, _)) => Some(v),
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ListValIterAndDot {
    pub dot: bool,
    cell: Option<Rc<RefCell<ConsCell>>>,
}

impl ListValIterAndDot {
    pub fn new(cell: Rc<RefCell<ConsCell>>) -> ListValIterAndDot {
        ListValIterAndDot {
            dot: false,
            cell: Some(cell),
        }
    }
}

impl Iterator for ListValIterAndDot {
    type Item = (ScmVal, bool);

    fn next(&mut self) -> Option<Self::Item> {
        match self.cell.clone() {
            Some(cell) => {
                if self.dot {
                    self.cell = None;
                    return Some((cell.borrow().tail.clone(), self.dot));
                }
                match cell.borrow().tail {
                    ScmVal::Pair(ref next) => self.cell = Some(Rc::clone(next)),
                    ScmVal::Empty => self.cell = None,
                    _ => self.dot = true,
                };
                Some((cell.borrow().head.clone(), self.dot))
            }
            None => None,
        }
    }
}

// Struct to wrap HashMap /////////////////////////////////////////////////////
//
// Required because can't derive equality or Hash with a HashMap in the ScmVal enum.
// Equality of hash maps will be implemented in scheme instead, because it
// requires looking over all of the internal values and calling equal? recursively
// like a list or vector. Eqv? can just use the pointer which may need to be
// setup in here.

#[derive(Debug, Clone)]
pub struct Map {
    pub contents: HashMap<ScmVal, ScmVal>,
}

impl Map {
    pub fn new(map: HashMap<ScmVal, ScmVal>) -> Map {
        Map { contents: map }
    }

    pub fn default() -> Map {
        Map {
            contents: HashMap::new(),
        }
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

impl Hash for Map {
    fn hash<H: Hasher>(&self, _: &mut H) {
        panic!("Hash map cannot be hashed");
    }
}

// Closure ////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure {
    pub env: Rc<RefCell<Env>>,
    pub params: Vec<ScmVal>,
    pub body: Vec<ScmVal>,
}

impl Closure {
    pub fn new(env: Rc<RefCell<Env>>, params: Vec<ScmVal>, body: Vec<ScmVal>) -> Closure {
        Closure {
            env: env,
            params: params,
            body: body,
        }
    }
}

impl Hash for Closure {
    fn hash<H: Hasher>(&self, _: &mut H) {
        panic!("Closure cannot be hashed");
    }
}

// Linked List Environment ////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env {
    pub scope: Map,
    pub next: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            scope: Map::default(),
            next: None,
        }
    }

    pub fn new_with_bindings(bindings: Vec<(ScmVal, ScmVal)>) -> Env {
        let mut env = Env::new();
        env.insert_all(bindings);
        env
    }

    pub fn add_scope(env: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        let new_env = Env {
            scope: Map::default(),
            next: Some(env),
        };
        Rc::new(RefCell::new(new_env))
    }

    // Binds a list of symbols to a list of values in a new scope
    pub fn bind_in_new_env(
        env: Rc<RefCell<Env>>,
        params: Vec<ScmVal>,
        args: Vec<ScmVal>,
    ) -> Rc<RefCell<Env>> {
        let new_env = Env::add_scope(env);
        {
            // TODO not sure the scope is required
            new_env.borrow_mut().insert_all(zip(params, args).collect());
        }
        new_env
    }

    // Returns the value for the first time the key is found in any scope.
    pub fn lookup(&self, key: ScmVal) -> Option<ScmVal> {
        match self.scope.contents.get(&key) {
            Some(val) => Some(val.clone()),
            None => match self.next {
                Some(ref next) => next.borrow().lookup(key),
                None => None,
            },
        }
    }

    // Inserts a binding into the top scope of the environment.
    // If a key exists in the top scope already it will be rebound.
    pub fn insert(&mut self, key: ScmVal, val: ScmVal) {
        self.scope.contents.insert(key, val);
    }

    // Inserts a vector of key value pairs as bindings in the top scope.
    pub fn insert_all(&mut self, pairs: Vec<(ScmVal, ScmVal)>) {
        for (key, val) in pairs.iter() {
            self.insert(key.clone(), val.clone());
        }
    }

    // Sets a new value for the first key that matches in any scope.
    // Returns Some(ScmVal::Empty) when the value was set, None if it was not found.
    pub fn set(&mut self, key: ScmVal, val: ScmVal) -> Option<ScmVal> {
        match self.scope.contents.contains_key(&key) {
            true => {
                self.scope.contents.insert(key, val);
                Some(ScmVal::Empty)
            }
            false => match self.next {
                Some(ref next) => next.borrow_mut().set(key, val),
                None => None,
            },
        }
    }
}

impl Hash for Env {
    fn hash<H: Hasher>(&self, _: &mut H) {
        panic!("Env cannot be hashed");
    }
}
