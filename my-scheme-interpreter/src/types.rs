//use crate::builtin::Builtin;
//use crate::number::Num;
//use crate::string::{ScmChar, ScmString};
//use std::cell::RefCell;
//use std::collections::HashMap;
//use std::fmt;
//use std::hash::{Hash, Hasher};
//use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Any,
    Bool,
    Char,
    Number,
    Int,
    UInt,
    Float,
    Symbol,
    Procedure(Box<Arity>),
    String,
    Pair,
    Empty,
    Array,
    Port,
    Env,
    ListOf(Box<Type>),
    Opt(Box<Type>),
}

impl Type {
    pub fn proc(arity: Arity) -> Type {
        Type::Procedure(Box::new(arity))
    }

    pub fn list(t: Type) -> Type {
        Type::ListOf(Box::new(t))
    }

    pub fn opt(t: Type) -> Type {
        Type::Opt(Box::new(t))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arity {
    Collect(Type),
    Fixed(Vec<Type>),
    Rest(Vec<Type>, Type),
}

/*
////////////////////////////////////////////////////////////////////////////////

// TODO create a trait for things that have a to_extern method like display. It
// can just return a string. But that way we can force all the T types to implement
// it as a trait and then easily distinguish between to to_string/Display and to_extern.
// TODO rename ScmVal to just Val and implement all the traits, CellValue etc.
// TODO rename ScmString and ScmChar to Str and Chr
// TODO break these types out of here and put them in their own files, closure
// and some vector wrapper, since they are supposed to be arrays. The string
// will have a RefCell so that it can keep track of it's own mutability, like
// Cell.
// TODO when creating and using env with Val use the contents of a symbol as the
// type for keys, so Str which will require unwrapping and checking for symbol
// before accessing so we can ensure no other types are used as env keys.

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
    Number(Num),
    Core(Builtin, u8),
    // Atoms that use Rc
    NewSymbol(Rc<StringRef>),
    Closure(Rc<Closure>),
    // Collections
    NewPair(Rc<Cell>),
    NewString(Rc<StringRef>),
    NewVec(Rc<VecRef>),
    Env(Rc<Env>),
    Empty,
    // Other
    Special(Box<SpecialForm>),
    Undefined,
    Cyclic,
}

impl ScmVal {
    // Constructor Helpers //

    pub fn new_sym(string: &str) -> ScmVal {
        ScmVal::NewSymbol(Rc::new(StringRef::new(string)))
    }

    pub fn sym_from_scm_str(string: ScmString) -> ScmVal {
        ScmVal::NewSymbol(Rc::new(StringRef {
            mutable: false,
            string: RefCell::new(string),
        }))
    }

    pub fn new_char(ch: char) -> ScmVal {
        ScmVal::Character(ScmChar::from_char(ch))
    }

    pub fn new_int(i: i64) -> ScmVal {
        ScmVal::Number(Num::Int(i))
    }

    pub fn new_float(f: f64) -> ScmVal {
        ScmVal::Number(Num::Flt(f))
    }

    pub fn new_pair(head: ScmVal, tail: ScmVal) -> ScmVal {
        ScmVal::NewPair(Rc::new(Cell::new(head, tail)))
    }

    pub fn new_pair_mut(head: ScmVal, tail: ScmVal) -> ScmVal {
        ScmVal::NewPair(Rc::new(Cell::new_mut(head, tail)))
    }

    pub fn new_str(string: &str) -> ScmVal {
        ScmVal::NewString(Rc::new(StringRef::new(string)))
    }

    pub fn new_str_mut(string: &str) -> ScmVal {
        ScmVal::NewString(Rc::new(StringRef::new_mut(string)))
    }

    pub fn from_scm_str(string: ScmString, mutable: bool) -> ScmVal {
        ScmVal::NewString(Rc::new(StringRef {
            mutable: mutable,
            string: RefCell::new(string),
        }))
    }

    pub fn new_closure(closure: Closure) -> ScmVal {
        ScmVal::Closure(Rc::new(closure))
    }

    pub fn new_vec(vec: Vec<ScmVal>) -> ScmVal {
        ScmVal::NewVec(Rc::new(VecRef::new(vec)))
    }

    pub fn new_vec_mut(vec: Vec<ScmVal>) -> ScmVal {
        ScmVal::NewVec(Rc::new(VecRef::new_mut(vec)))
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
    pub fn vec_to_list(values: &[ScmVal], end: ScmVal) -> ScmVal {
        values
            .into_iter()
            .rev()
            .fold(end, |acc, v| ScmVal::new_pair(v.clone(), acc))
    }

    // If end is ScmVal::Empty it will be list otherwise dotted list
    pub fn vec_to_list_mut(values: &[ScmVal], end: ScmVal) -> ScmVal {
        values
            .into_iter()
            .rev()
            .fold(end, |acc, v| ScmVal::new_pair_mut(v.clone(), acc))
    }

    // Iterate a pair and return a vetor of the elements.
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

    // If the list is immutable it will keep it immutable
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
            ScmVal::NewSymbol(val) => val.to_string(),
            ScmVal::NewString(val) => val.string.borrow().to_extern(),
            ScmVal::Closure(val) => ScmVal::extern_closure(val.name.clone()),
            ScmVal::Core(val, _) => format!("#<procedure {}>", val),
            ScmVal::NewPair(_) => ScmVal::extern_list(self.clone()),
            ScmVal::Env(_) => format!("#<environment>"),
            ScmVal::NewVec(val) => ScmVal::extern_vec(Rc::clone(val)),
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

    fn extern_vec(vec: Rc<VecRef>) -> String {
        let vec_string: Vec<String> = vec.vec.borrow().iter().map(|v| v.to_extern()).collect();
        format!("#({})", vec_string.join(" "))
    }
}

// ScmVal Hash //

impl Hash for ScmVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ScmVal::NewSymbol(val) => val.string.borrow().hash(state),
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
            ScmVal::NewString(val) => write!(f, "{}", val.to_string()),
            ScmVal::NewPair(_) => display_list(f, self.clone()),
            ScmVal::NewVec(val) => display_vec(f, Rc::clone(val)),
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

fn display_vec(f: &mut fmt::Formatter, vec: Rc<VecRef>) -> fmt::Result {
    let vec_string: Vec<String> = vec.vec.borrow().iter().map(|v| v.to_string()).collect();
    write!(f, "#({})", vec_string.join(" "))
}

// Cons Cells /////////////////////////////////////////////////////////////////

// TODO this is replaced by cell::Cell<T>
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

// TODO remake this like other structures we have used, but keep it in here
// because of all of them it really does not make sense apart from the scheme
// value. Implement all the traits we have for the other ones for this one too.
// Val should mostly just be calling those traits on its wrapped types.
// TODO can we nest Fromals inside of Closure?
// TODO also special forms are kind of like closures, in that they contain information
// for some code to be run, so group the closer

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Formals {
    Collect(ScmVal),
    Fixed(Vec<ScmVal>),
    Rest(Vec<ScmVal>, ScmVal),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure {
    pub name: String,
    pub env: Rc<Env>,
    pub params: Formals,
    pub body: Vec<ScmVal>,
}

impl Closure {
    pub fn new(name: &str, env: Rc<Env>, params: Formals, body: Vec<ScmVal>) -> Closure {
        Closure {
            name: name.to_owned(),
            env: env,
            params: params,
            body: body,
        }
    }

    // TODO add a way to get the required arity from the formals
}

// String Wrapper /////////////////////////////////////////////////////////////

// Like Cell we use a RefCell inside to allow mutability, while keeping the
// ScmVal as just one variant. Could possibly add this to string as is or try
// and integrate it into the struct in there, though they feel as though they
// have different purposes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringRef {
    pub mutable: bool,
    pub string: RefCell<ScmString>,
}

impl StringRef {
    pub fn new(val: &str) -> StringRef {
        StringRef {
            mutable: false,
            string: RefCell::new(ScmString::new(val)),
        }
    }

    pub fn new_mut(val: &str) -> StringRef {
        StringRef {
            mutable: true,
            string: RefCell::new(ScmString::new(val)),
        }
    }

    pub fn set_char(&self, ch: ScmChar, idx: usize) -> bool {
        if self.mutable && idx < self.len() {
            self.string.borrow_mut().chars[idx] = ch;
            true
        } else {
            false
        }
    }

    pub fn get_char(&self, idx: usize) -> Option<ScmChar> {
        if idx < self.len() {
            Some(self.string.borrow_mut().chars[idx].clone())
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.string.borrow().chars.len()
    }

    pub fn to_string(&self) -> String {
        self.string.borrow().to_string()
    }
}

// Vector Wrapper /////////////////////////////////////////////////////////////

// Like Cell we use a RefCell inside to allow mutability, while keeping the
// ScmVal as just one variant.
// TODO this is replaced by Vector<T>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VecRef {
    pub mutable: bool,
    pub vec: RefCell<Vec<ScmVal>>,
}

impl VecRef {
    pub fn new(vec: Vec<ScmVal>) -> VecRef {
        VecRef {
            mutable: false,
            vec: RefCell::new(vec),
        }
    }

    pub fn new_mut(vec: Vec<ScmVal>) -> VecRef {
        VecRef {
            mutable: true,
            vec: RefCell::new(vec),
        }
    }

    pub fn set(&self, val: ScmVal, idx: usize) -> bool {
        if self.mutable && idx < self.len() {
            self.vec.borrow_mut()[idx] = val;
            true
        } else {
            false
        }
    }

    pub fn get(&self, idx: usize) -> Option<ScmVal> {
        if idx < self.len() {
            Some(self.vec.borrow_mut()[idx].clone())
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.vec.borrow().len()
    }
}

// Environment ////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env {
    pub scope: RefCell<HashMap<ScmVal, ScmVal>>,
    pub next: Option<Rc<Env>>,
}

// Implementation is in env_impl because it needs error and error needs other
// stuff and to make it work I would have to move error in here too, and there
// are enough things in here.
*/
