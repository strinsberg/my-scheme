use crate::number::ScmNumber;
use crate::string::{ScmChar, ScmString};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// TODO TODO TODO VERY IMPORTANT, the hashing that is setup is no good at all.
// It will loop forever on some things. The only thing that should be hashed are
// symbols for the env until I figure out a better way. For the list iterators
// we can just use the pointer of the cell and hash that, but I am not confident
// that in the long run (maybe even during list iteration) that that pointer will
// stay valid enough to be used as a hash value. I am going to make all hashing
// a panic until I get it figured out. This won't affect the standard, but will
// affect adding hash tables later.
//
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
    PairMut(Rc<RefCell<ConsCell>>),
    Pair(Rc<ConsCell>),
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
        ScmVal::Pair(Rc::new(ConsCell::new(head, tail)))
    }

    pub fn new_pair_mut(head: ScmVal, tail: ScmVal) -> ScmVal {
        ScmVal::PairMut(Rc::new(RefCell::new(ConsCell::new(head, tail))))
    }

    pub fn new_str_mut(string: &str) -> ScmVal {
        ScmVal::StringMut(Rc::new(RefCell::new(ScmString::new(string))))
    }

    pub fn new_str_from_scmstring(string: ScmString) -> ScmVal {
        ScmVal::String(Rc::new(string))
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
        values
            .into_iter()
            .rev()
            .fold(end, |acc, v| ScmVal::new_pair(v, acc))
    }

    // If end is ScmVal::Empty it will be list otherwise dotted list
    pub fn vec_to_list_mut(values: Vec<ScmVal>, end: ScmVal) -> ScmVal {
        values
            .into_iter()
            .rev()
            .fold(end, |acc, v| ScmVal::new_pair_mut(v, acc))
    }

    pub fn list_to_vec(val: ScmVal) -> Option<(Vec<ScmVal>, bool, bool)> {
        let mut dotted = false;
        let mut cyclic = false;
        let mut vals = Vec::new();

        for pair in ListPairIter::new(val) {
            match pair {
                ScmVal::Pair(cell) => {
                    vals.push(cell.head.clone());
                    if cell.is_dotted() {
                        dotted = true;
                        vals.push(cell.tail.clone());
                    }
                }
                ScmVal::PairMut(cell) => {
                    vals.push(cell.borrow().head.clone());
                    if cell.borrow().is_dotted() {
                        dotted = true;
                        vals.push(cell.borrow().tail.clone());
                    }
                }
                ScmVal::Undefined => {
                    cyclic = true;
                    dotted = true;
                }
                _ => return None,
            }
        }

        Some((vals, dotted, cyclic))
    }

    pub fn cons(val: ScmVal, rest: ScmVal) -> ScmVal {
        match rest {
            ScmVal::Pair(_) => ScmVal::new_pair(val, rest),
            _ => ScmVal::new_pair_mut(val, rest),
        }
    }

    // External representation //

    pub fn to_extern(&self) -> String {
        match self {
            ScmVal::Number(val) => val.to_string(),
            ScmVal::Boolean(val) => format!("#{}", if *val { "t" } else { "f" }),
            ScmVal::Character(val) => val.to_extern(),
            ScmVal::Symbol(val) => val.to_string(),
            ScmVal::String(val) => val.to_extern(),
            ScmVal::StringMut(val) => val.borrow().to_extern(),
            ScmVal::Closure(_) => format!("#<closure>"),
            ScmVal::Core(val) => format!("#<procedure {}>", val),
            ScmVal::Pair(_) => ScmVal::extern_list(self.clone()),
            ScmVal::PairMut(_) => ScmVal::extern_list(self.clone()),
            ScmVal::Env(_) => format!("#<environment>"),
            ScmVal::Vector(val) => ScmVal::extern_vec(Rc::clone(val)),
            ScmVal::VectorMut(val) => ScmVal::extern_vec_mut(Rc::clone(val)),
            ScmVal::HashMap(val) => format!("{:?}", val),
            ScmVal::HashMapMut(val) => format!("{:?}", val.borrow()),
            ScmVal::Empty => "()".to_string(),
            val => format!("{:?}", val),
        }
    }

    fn extern_list(val: ScmVal) -> String {
        // TODO just use list_to_vec
        let mut dotted = false;
        let mut strings = Vec::new();

        for pair in ListPairIter::new(val) {
            match pair {
                ScmVal::Pair(cell) => {
                    strings.push(cell.head.to_extern());
                    if cell.is_dotted() {
                        dotted = true;
                        strings.push(cell.tail.to_extern());
                    }
                }
                ScmVal::PairMut(cell) => {
                    strings.push(cell.borrow().head.to_extern());
                    if cell.borrow().is_dotted() {
                        dotted = true;
                        strings.push(cell.borrow().tail.to_extern());
                    }
                }
                ScmVal::Undefined => {
                    dotted = true;
                    strings.push("#cycle#".to_owned());
                }
                _ => panic!("list iterator should only contain pairs: {:?}", pair),
            }
        }

        if dotted {
            format!(
                "({} . {})",
                strings[..strings.len() - 1].join(" "),
                strings[strings.len() - 1]
            )
        } else {
            format!("({})", strings.join(" "))
        }
    }

    fn extern_vec(vec: Rc<Vec<ScmVal>>) -> String {
        let vec_string: Vec<String> = vec.iter().map(|v| v.to_extern()).collect();
        format!("#({})", vec_string.join(" "))
    }

    fn extern_vec_mut(vec: Rc<RefCell<Vec<ScmVal>>>) -> String {
        let vec_string: Vec<String> = vec.borrow().iter().map(|v| v.to_extern()).collect();
        format!("#({})", vec_string.join(" "))
    }
}

// ScmVal Hash //

impl Hash for ScmVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ScmVal::Symbol(val) => val.hash(state),
            _ => panic!("cannot hash anything but symbols"),
        }
    }
}

// ScmVal Display //

// TODO this cannot be used for prn and display where one should give the
// external representation. There needs to be one that shows a string as "string"
// and one that just shows string.
//
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
            ScmVal::Pair(_) => display_list(f, self.clone()),
            ScmVal::PairMut(_) => display_list(f, self.clone()),
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

fn display_list(f: &mut fmt::Formatter, val: ScmVal) -> fmt::Result {
    // TODO just use list_to_vec
    let mut dotted = false;
    let mut strings = Vec::new();

    for pair in ListPairIter::new(val) {
        match pair {
            ScmVal::Pair(cell) => {
                strings.push(cell.head.to_string());
                if cell.is_dotted() {
                    dotted = true;
                    strings.push(cell.tail.to_string());
                }
            }
            ScmVal::PairMut(cell) => {
                strings.push(cell.borrow().head.to_string());
                if cell.borrow().is_dotted() {
                    dotted = true;
                    strings.push(cell.borrow().tail.to_string());
                }
            }
            ScmVal::Undefined => {
                dotted = true;
                strings.push("#cycle#".to_owned());
            }
            _ => panic!("list iterator should only contain pairs: {:?}", pair),
        }
    }

    if dotted {
        write!(
            f,
            "({} . {})",
            strings[..strings.len() - 1].join(" "),
            strings[strings.len() - 1]
        )
    } else {
        write!(f, "({})", strings.join(" "))
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
    // arithmetic
    Sum,
    Subtract,
    Product,
    Divide,
    // predicates
    IsBool,
    IsSymbol,
    IsChar,
    IsNumber,
    IsString,
    IsProcedure,
    IsPair,
    IsVector,
    // Lists,
    SetCar,
    SetCdr,
    IsList,
    // Vectors
    MakeVec,
    Vector,
    VecSet,
    VecRef,
    VecLen,
    VecToList,
    VecFill,
    // TODO no push, pop, and concat/append in the standards???
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
            Builtin::IsBool => "boolean?",
            Builtin::IsSymbol => "symbol?",
            Builtin::IsChar => "char?",
            Builtin::IsNumber => "number?",
            Builtin::IsString => "string?",
            Builtin::IsProcedure => "procedure?",
            Builtin::IsPair => "pair?",
            Builtin::IsVector => "vector?",
            //
            Builtin::SetCar => "set-car?",
            Builtin::SetCdr => "set-car?",
            // Vectors
            Builtin::MakeVec => "make-vector",
            Builtin::VecSet => "vector-set!",
            Builtin::VecRef => "vector-ref",
            Builtin::VecLen => "vector-length",
            Builtin::VecToList => "vector->list",
            Builtin::VecFill => "vector-fill!",
            //
            Builtin::EQ => "eq?",
            Builtin::Eqv => "eqv?",
            Builtin::BaseEnv => "null-environment",
            b => return write!(f, "{}", format!("{:?}", b).to_lowercase()),
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
            ScmVal::Pair(_) | ScmVal::PairMut(_) | ScmVal::Empty => false,
            _ => true,
        }
    }
}

// ConsCell Iterators //

#[derive(Debug, Clone)]
pub struct ListPairIter {
    seen: HashSet<*const ConsCell>,
    pair: ScmVal,
}

impl ListPairIter {
    pub fn new(pair: ScmVal) -> ListPairIter {
        ListPairIter {
            seen: HashSet::new(),
            pair: pair,
        }
    }
}

// Terminates on cycles with last before none being ScmVal::Undefined
impl Iterator for ListPairIter {
    type Item = ScmVal;

    fn next(&mut self) -> Option<Self::Item> {
        match self.pair.clone() {
            ScmVal::Pair(cell) => {
                let next = self.pair.clone();
                if self.seen.contains(&Rc::as_ptr(&cell)) {
                    self.pair = ScmVal::Empty;
                    return Some(ScmVal::Undefined);
                } else if cell.is_dotted() {
                    self.pair = ScmVal::Empty;
                } else {
                    self.pair = cell.tail.clone();
                }
                self.seen.insert(Rc::as_ptr(&cell));
                Some(next)
            }
            ScmVal::PairMut(cell) => {
                let next = self.pair.clone();
                if self
                    .seen
                    .contains(&(RefCell::as_ptr(&cell) as *const ConsCell))
                {
                    self.pair = ScmVal::Empty;
                    return Some(ScmVal::Undefined);
                } else if cell.borrow().is_dotted() {
                    self.pair = ScmVal::Empty;
                } else {
                    self.pair = cell.borrow().tail.clone();
                }
                self.seen.insert(RefCell::as_ptr(&cell));
                Some(next)
            }
            ScmVal::Empty => None,
            _ => panic!(
                "list iterator should not contain non-list ScmVal: {:?}",
                self.pair
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ListValIter {
    seen: HashSet<ScmVal>,
    done: bool,
    last: ScmVal,
    pair: ScmVal,
}

impl ListValIter {
    pub fn new(pair: ScmVal) -> ListValIter {
        ListValIter {
            seen: HashSet::new(),
            done: false,
            last: ScmVal::Empty,
            pair: pair,
        }
    }
}

// Terminates on cycles, returns undefined before none
impl Iterator for ListValIter {
    type Item = ScmVal;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        } else if self.seen.contains(&self.pair.clone()) {
            self.done = true;
            return Some(ScmVal::Undefined);
        } else {
            self.seen.insert(self.pair.clone());
        }

        match self.last {
            ScmVal::Empty => (),
            _ => {
                self.done = true;
                return Some(self.last.clone());
            }
        }

        match self.pair.clone() {
            ScmVal::Pair(cell) => {
                if cell.is_dotted() {
                    self.last = cell.tail.clone();
                } else {
                    self.pair = cell.tail.clone();
                }
                Some(cell.head.clone())
            }
            ScmVal::PairMut(cell) => {
                if cell.borrow().is_dotted() {
                    self.last = cell.borrow().tail.clone();
                } else {
                    self.pair = cell.borrow().tail.clone();
                }
                Some(cell.borrow().head.clone())
            }
            ScmVal::Empty => {
                self.done = true;
                None
            }
            _ => panic!(
                "list iterator should not contain non-list ScmVal: {:?}",
                self.pair
            ),
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

// Environment ////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env {
    pub scope: Map,
    pub next: Option<Rc<RefCell<Env>>>,
}

// Implementation is in env_impl because it needs error and error needs other
// stuff and to make it work I would have to move error in here too, and there
// are enough things in here.
