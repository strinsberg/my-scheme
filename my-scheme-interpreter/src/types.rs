use crate::number::ScmNumber;
use crate::string::{ScmChar, ScmString};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// TODO count cons cells? Probably requires using ScmVal::cons everywhere a pair
// is made instead of ScmVal::new_pair. If it is used in vec_to_list then it helps
// catch those places too. This is not really a priority, as I am not sure that
// other schemes do this. Counting lists is discouraged because it is o(n) and
// counting them would stop that, but would increase the space needed for cells
// by a fair amount just to allow quick counting.
// TODO add name to closure?

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
    DottedPair(Rc<RefCell<ConsCell>>),
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

    pub fn new_dotted_pair(head: ScmVal, tail: ScmVal) -> ScmVal {
        ScmVal::DottedPair(Rc::new(RefCell::new(ConsCell::new(head, tail))))
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

    // Helpers //

    // If end is ScmVal::Empty it will be list otherwise dotted list
    pub fn vec_to_list(values: Vec<ScmVal>, end: ScmVal) -> ScmVal {
        match end {
            ScmVal::Pair(_) | ScmVal::Empty => values
                .into_iter()
                .rev()
                .fold(end, |acc, v| ScmVal::new_pair(v, acc)),
            _ => values
                .into_iter()
                .rev()
                .fold(end, |acc, v| ScmVal::new_dotted_pair(v, acc)),
        }
    }

    pub fn list_to_vec(val: ScmVal) -> Option<Vec<ScmVal>> {
        match val {
            ScmVal::Pair(cell) => Some(ListValIter::new(cell).collect()),
            ScmVal::DottedPair(cell) => Some(ListValIter::new(cell).collect()),
            ScmVal::Empty => Some(vec![]),
            _ => None,
        }
    }

    pub fn cons(val: ScmVal, rest: ScmVal) -> ScmVal {
        match rest {
            ScmVal::Pair(_) => ScmVal::new_pair(val, rest),
            ScmVal::DottedPair(_) => ScmVal::new_dotted_pair(val, rest),
            ScmVal::Empty => ScmVal::new_pair(val, ScmVal::Empty),
            _ => ScmVal::new_dotted_pair(val, rest),
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
            ScmVal::Closure(_) => write!(f, "#<closure>"),
            ScmVal::Core(val) => write!(f, "#<procedure {}>", val),
            ScmVal::Pair(val) => display_list(f, Rc::clone(val)),
            ScmVal::DottedPair(val) => display_improper_list(f, Rc::clone(val)),
            ScmVal::Env(_) => write!(f, "#<environment>"),
            ScmVal::Vector(val) => display_vec(f, Rc::clone(val)),
            ScmVal::VectorMut(val) => display_vec_mut(f, Rc::clone(val)),
            ScmVal::HashMap(val) => write!(f, "{:?}", val),
            ScmVal::HashMapMut(val) => write!(f, "{:?}", val.borrow()),
            ScmVal::Empty => write!(f, "()"),
            val => write!(f, "{:?}", val),
        }
    }
}

fn display_improper_list(f: &mut fmt::Formatter, cell: Rc<RefCell<ConsCell>>) -> fmt::Result {
    let vec_string: Vec<String> = ListValIter::new(cell).map(|v| v.to_string()).collect();
    write!(
        f,
        "({} . {})",
        vec_string[..vec_string.len() - 1].join(" "),
        vec_string[vec_string.len() - 1]
    )
}

fn display_list(f: &mut fmt::Formatter, cell: Rc<RefCell<ConsCell>>) -> fmt::Result {
    let vec_string: Vec<String> = ListValIter::new(cell).map(|v| v.to_string()).collect();
    write!(f, "({})", vec_string.join(" "))
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
    // arithmetic
    Sum,
    Subtract,
    Product,
    Divide,
    // predicates
    IsEmpty,
    IsBool,
    IsSymbol,
    IsChar,
    IsNumber,
    IsString,
    IsProcedure,
    IsPair,
    IsVector,
    // other
    EQ,
    Eqv,
    BaseEnv,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Builtin::Sum => "+",
            Builtin::Subtract => "-",
            Builtin::Product => "*",
            Builtin::Divide => "/",
            //
            Builtin::IsEmpty => "null?",
            Builtin::IsBool => "boolean?",
            Builtin::IsSymbol => "symbol?",
            Builtin::IsChar => "char?",
            Builtin::IsNumber => "number?",
            Builtin::IsString => "string?",
            Builtin::IsProcedure => "procedure?",
            Builtin::IsPair => "pair?",
            Builtin::IsVector => "vector?",
            //
            Builtin::EQ => "eq?",
            Builtin::Eqv => "eqv?",
            Builtin::BaseEnv => "null-environment",
            b => return write!(f, "{}", format!("{}", b).to_lowercase()),
        };
        write!(f, "{}", s)
    }
}

// Cons Cells /////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConsCell {
    pub count: usize,
    pub head: ScmVal,
    pub tail: ScmVal,
}

impl ConsCell {
    pub fn default() -> ConsCell {
        ConsCell {
            count: 0,
            head: ScmVal::Empty,
            tail: ScmVal::Empty,
        }
    }

    pub fn new(head: ScmVal, tail: ScmVal) -> ConsCell {
        ConsCell {
            count: 0,
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
    last: bool,
    cell: Option<Rc<RefCell<ConsCell>>>,
}

impl ListValIter {
    pub fn new(cell: Rc<RefCell<ConsCell>>) -> ListValIter {
        ListValIter {
            last: false,
            cell: Some(cell),
        }
    }
}

impl Iterator for ListValIter {
    type Item = ScmVal;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cell.clone() {
            Some(cell) => {
                if self.last {
                    self.cell = None;
                    return Some(cell.borrow().tail.clone());
                }
                match cell.borrow().tail {
                    ScmVal::DottedPair(ref next) | ScmVal::Pair(ref next) => {
                        self.cell = Some(Rc::clone(next))
                    }
                    ScmVal::Empty => self.cell = None,
                    _ => self.last = true,
                };
                Some(cell.borrow().head.clone())
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
pub enum Formals {
    Collect(ScmVal),
    Fixed(Vec<ScmVal>),
    Rest(Vec<ScmVal>, ScmVal),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure {
    pub env: Rc<RefCell<Env>>,
    pub params: Formals,
    pub body: Vec<ScmVal>,
}

impl Closure {
    pub fn new(env: Rc<RefCell<Env>>, params: Formals, body: Vec<ScmVal>) -> Closure {
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

// Environment ////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env {
    pub scope: Map,
    pub next: Option<Rc<RefCell<Env>>>,
}

// Implementation is in env_impl because it needs error and error needs other
// stuff and to make it work I would have to move error in here too, and there
// are enough things in here.
