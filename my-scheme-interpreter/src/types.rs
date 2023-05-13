use crate::builtin::Builtin;
use crate::number::ScmNumber;
use crate::string::{ScmChar, ScmString};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// TODO the special forms should be in an Rc in the ScmVal so that their size
// does not affect the size of ScmVal. Also, provide constructors for them.

// Scheme Values //////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecialForm {
    If(Vec<ScmVal>),
    Set(ScmVal),
    And(Vec<ScmVal>),
    Or(Vec<ScmVal>),
    Cond(Vec<ScmVal>, bool, Vec<ScmVal>),
    Case(ScmVal, Vec<ScmVal>),
}

// Anything that holds data that is more than a simple type uses an Pointer. This
// should keep the enum variants small, even a vector is a struct with 3
// elements and an Rc should only be 2. Also, a clone of an ScmVal is an Rc::clone()
// on the pointer and should have low overhead, i.e. not copying a vector or a
// string completely.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScmVal {
    // Simple variants
    Boolean(bool),
    Character(ScmChar),
    Number(ScmNumber),
    Core(Builtin, u8),
    // Atoms that use Rc
    Symbol(Rc<ScmString>),
    Closure(Rc<Closure>),
    // Collections
    NewPair(Rc<Cell>),
    String(Rc<ScmString>),
    StringMut(Rc<RefCell<ScmString>>),
    Vector(Rc<Vec<ScmVal>>),
    VectorMut(Rc<RefCell<Vec<ScmVal>>>),
    Env(Rc<RefCell<Env>>),
    Empty,
    // Other
    Special(Box<SpecialForm>),
    Undefined,
    Cyclic,
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
        ScmVal::NewPair(Rc::new(Cell::new(head, tail)))
    }

    pub fn new_pair_mut(head: ScmVal, tail: ScmVal) -> ScmVal {
        ScmVal::NewPair(Rc::new(Cell::new_mut(head, tail)))
    }

    pub fn new_str_mut(string: &str) -> ScmVal {
        ScmVal::StringMut(Rc::new(RefCell::new(ScmString::new(string))))
    }

    pub fn new_str_from_scmstring(string: ScmString) -> ScmVal {
        ScmVal::String(Rc::new(string))
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

    pub fn new_if(args: Vec<ScmVal>) -> ScmVal {
        ScmVal::Special(Box::new(SpecialForm::If(args)))
    }

    pub fn new_set(val: ScmVal) -> ScmVal {
        ScmVal::Special(Box::new(SpecialForm::Set(val)))
    }

    pub fn new_and(args: Vec<ScmVal>) -> ScmVal {
        ScmVal::Special(Box::new(SpecialForm::And(args)))
    }

    pub fn new_or(args: Vec<ScmVal>) -> ScmVal {
        ScmVal::Special(Box::new(SpecialForm::Or(args)))
    }

    pub fn new_cond(branch: Vec<ScmVal>, arrow: bool, args: Vec<ScmVal>) -> ScmVal {
        ScmVal::Special(Box::new(SpecialForm::Cond(branch, arrow, args)))
    }

    pub fn new_case(val: ScmVal, args: Vec<ScmVal>) -> ScmVal {
        ScmVal::Special(Box::new(SpecialForm::Case(val, args)))
    }

    // List/Vector Helpers //

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

    pub fn list_to_vec(val: &ScmVal) -> Option<(Vec<ScmVal>, bool)> {
        match val {
            ScmVal::NewPair(cell) => {
                let mut dotted = false;
                let mut vals = Vec::new();

                for cell in CellIter::new(cell.clone()) {
                    vals.push(cell.clone_head());
                    if cell.is_dotted() {
                        dotted = true;
                        vals.push(cell.clone_tail());
                    }
                }
                Some((vals, dotted))
            }
            ScmVal::Empty => return Some((vec![], false)),
            _ => return None,
        }
    }

    pub fn cons(val: ScmVal, rest: ScmVal) -> ScmVal {
        match rest {
            ScmVal::NewPair(ref cell) if !cell.mutable => ScmVal::new_pair(val, rest),
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
            ScmVal::Closure(val) => ScmVal::extern_closure(val.name.clone()),
            ScmVal::Core(val, _) => format!("#<procedure {}>", val),
            ScmVal::NewPair(_) => ScmVal::extern_list(self.clone()),
            ScmVal::Env(_) => format!("#<environment>"),
            ScmVal::Vector(val) => ScmVal::extern_vec(Rc::clone(val)),
            ScmVal::VectorMut(val) => ScmVal::extern_vec_mut(Rc::clone(val)),
            ScmVal::Empty => "()".to_string(),
            ScmVal::Cyclic => "#cyclic#".to_string(),
            val => format!("{:?}", val),
        }
    }

    fn extern_closure(name: String) -> String {
        if name == "no-name" {
            format!("#<closure>")
        } else {
            format!("#<procedure {name}>")
        }
    }

    fn extern_list(val: ScmVal) -> String {
        let (vec, dotted) =
            ScmVal::list_to_vec(&val).expect("extern list should only be passed a pair");
        let strings: Vec<String> = vec.iter().map(|v| v.to_extern()).collect();
        ScmVal::format_list(&strings, dotted)
    }

    fn format_list(strings: &[String], dotted: bool) -> String {
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

// Display the output version of the object. Uses extern for all objects that
// display with their external representation.
// This is not tail recursive and may be an issue if used in print etr.
impl fmt::Display for ScmVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScmVal::String(val) => write!(f, "{}", val),
            ScmVal::StringMut(val) => write!(f, "{}", val.borrow()),
            ScmVal::NewPair(_) => display_list(f, self.clone()),
            ScmVal::Vector(val) => display_vec(f, Rc::clone(val)),
            ScmVal::VectorMut(val) => display_vec_mut(f, Rc::clone(val)),
            ScmVal::Empty => write!(f, "()"),
            val => write!(f, "{}", val.to_extern()),
        }
    }
}

fn display_list(f: &mut fmt::Formatter, val: ScmVal) -> fmt::Result {
    let (vec, dotted) =
        ScmVal::list_to_vec(&val).expect("extern list should only be passed a pair");
    let strings: Vec<String> = vec.iter().map(|v| v.to_string()).collect();
    write!(f, "{}", ScmVal::format_list(&strings, dotted))
}

fn display_vec(f: &mut fmt::Formatter, vec: Rc<Vec<ScmVal>>) -> fmt::Result {
    let vec_string: Vec<String> = vec.iter().map(|v| v.to_string()).collect();
    write!(f, "#({})", vec_string.join(" "))
}

fn display_vec_mut(f: &mut fmt::Formatter, vec: Rc<RefCell<Vec<ScmVal>>>) -> fmt::Result {
    let vec_string: Vec<String> = vec.borrow().iter().map(|v| v.to_string()).collect();
    write!(f, "#({})", vec_string.join(" "))
}

// Cons Cells /////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cell {
    pub mutable: bool,
    pub head: RefCell<ScmVal>,
    pub tail: RefCell<ScmVal>,
}

impl Cell {
    pub fn new(head: ScmVal, tail: ScmVal) -> Cell {
        Cell {
            mutable: false,
            head: RefCell::new(head),
            tail: RefCell::new(tail),
        }
    }

    pub fn new_mut(head: ScmVal, tail: ScmVal) -> Cell {
        Cell {
            mutable: true,
            head: RefCell::new(head),
            tail: RefCell::new(tail),
        }
    }

    pub fn set_head(&self, val: ScmVal) -> Option<ScmVal> {
        if self.mutable {
            Some(self.head.replace(val))
        } else {
            None
        }
    }

    pub fn set_tail(&self, val: ScmVal) -> Option<ScmVal> {
        if self.mutable {
            Some(self.tail.replace(val))
        } else {
            None
        }
    }

    pub fn clone_head(&self) -> ScmVal {
        self.head.borrow().clone()
    }

    pub fn clone_tail(&self) -> ScmVal {
        self.tail.borrow().clone()
    }

    pub fn is_dotted(&self) -> bool {
        match self.clone_tail() {
            ScmVal::Empty | ScmVal::NewPair(_) => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CellValIter {
    done: bool,
    last: ScmVal,
    pair: ScmVal,
}

impl CellValIter {
    pub fn new(pair: ScmVal) -> CellValIter {
        CellValIter {
            done: false,
            last: ScmVal::Empty,
            pair: pair,
        }
    }
}

// Does not check for cycles
impl Iterator for CellValIter {
    type Item = ScmVal;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        match self.last {
            ScmVal::Empty => (),
            _ => {
                self.done = true;
                return Some(self.last.clone());
            }
        }

        match self.pair.clone() {
            ScmVal::NewPair(cell) => {
                if cell.is_dotted() {
                    self.last = cell.clone_tail();
                } else {
                    self.pair = cell.clone_tail();
                }
                Some(cell.clone_head())
            }
            ScmVal::Empty => {
                self.done = true;
                None
            }
            _ => panic!(
                "list iterator should not save non-list ScmVal: {:?}",
                self.pair
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CellIter {
    done: bool,
    cell: Rc<Cell>,
}

impl CellIter {
    pub fn new(cell: Rc<Cell>) -> CellIter {
        CellIter {
            done: false,
            cell: cell,
        }
    }
}

impl Iterator for CellIter {
    type Item = Rc<Cell>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        } else if self.cell.is_dotted() {
            self.done = true;
            return Some(self.cell.clone());
        }

        match self.cell.clone_tail() {
            ScmVal::NewPair(cell) => {
                let current = self.cell.clone();
                self.cell = cell;
                Some(current)
            }
            ScmVal::Empty => {
                self.done = true;
                Some(self.cell.clone())
            }
            _ => None,
        }
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
    pub name: String,
    pub env: Rc<RefCell<Env>>,
    pub params: Formals,
    pub body: Vec<ScmVal>,
}

impl Closure {
    pub fn new(name: &str, env: Rc<RefCell<Env>>, params: Formals, body: Vec<ScmVal>) -> Closure {
        Closure {
            name: name.to_owned(),
            env: env,
            params: params,
            body: body,
        }
    }

    // TODO add a way to get the required arity from the formals
}

// Environment ////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env {
    pub scope: HashMap<ScmVal, ScmVal>,
    pub next: Option<Rc<RefCell<Env>>>,
}

// Implementation is in env_impl because it needs error and error needs other
// stuff and to make it work I would have to move error in here too, and there
// are enough things in here.
