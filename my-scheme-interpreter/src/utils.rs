use crate::heap::{Heap, ListPtrIter, ListValIter};
use crate::scm_types::error::ValResult;
use crate::scm_types::number::ScmNumber;
use crate::scm_types::scm_val::{Closure, Map, Pointer, ScmVal};
use crate::scm_types::string::{ScmChar, ScmString};
use std::collections::HashMap;

// ScmVal helpers /////////////////////////////////////////////////////////////

// Symbols //

pub fn new_sym(string: &str) -> ScmVal {
    ScmVal::Symbol(Box::new(ScmString::new(string)))
}

pub fn extract_symbol_name(sym: ScmVal) -> String {
    match sym {
        ScmVal::Symbol(scm_str) => scm_str.to_string(),
        _ => panic!("should be ScmVal::Symbol: {:?}", sym),
    }
}

// Characters //

pub fn new_char(ch: char) -> ScmVal {
    ScmVal::Character(ScmChar::from_char(ch))
}

// Numbers //

pub fn new_int(i: i64) -> ScmVal {
    ScmVal::Number(ScmNumber::Integer(i))
}

pub fn new_float(f: f64) -> ScmVal {
    ScmVal::Number(ScmNumber::Float(f))
}

// Strings //

pub fn new_str(string: &str, heap: &mut Heap) -> ValResult {
    new_str_scm(ScmString::new(string), heap)
}

pub fn new_str_scm(string: ScmString, heap: &mut Heap) -> ValResult {
    let ptr = heap.cons(ScmVal::StringBox(Box::new(string)), ScmVal::Empty);
    Ok(ScmVal::StringRef(ptr))
}

pub fn str_eq_scm(s: &str, string: ScmVal, heap: &Heap) -> bool {
    extract_scm_string(string, heap).to_string() == s
}

pub fn extract_scm_string(string: ScmVal, heap: &Heap) -> ScmString {
    let ptr = match string {
        ScmVal::StringRef(ptr) => ptr,
        _ => panic!("should be ScmVal::StringRef"),
    };
    extract_scm_string_ptr(ptr, heap)
}

pub fn extract_scm_string_ptr(ptr: Pointer, heap: &Heap) -> ScmString {
    let string_box = heap.car(ptr);
    match string_box {
        ScmVal::StringBox(scm_string) => (*scm_string).clone(),
        _ => panic!("should be pointer to ScmVal::StringBox"),
    }
}

// Vectors ////////////////////////////////////////////////////////////////////

pub fn new_closure(closure: Closure, heap: &mut Heap) -> ValResult {
    let ptr = heap.cons(ScmVal::ClosureBox(Box::new(closure)), ScmVal::Empty);
    Ok(ScmVal::ClosureRef(ptr))
}

pub fn extract_closure(val: ScmVal, heap: &Heap) -> Closure {
    let ptr = match val {
        ScmVal::ClosureRef(ptr) => ptr,
        _ => panic!("should be ScmVal::ClosureRef"),
    };
    extract_closure_ptr(ptr, heap)
}

pub fn extract_closure_ptr(ptr: Pointer, heap: &Heap) -> Closure {
    let closure_box = heap.car(ptr);
    match closure_box {
        ScmVal::ClosureBox(closure) => (*closure).clone(),
        _ => panic!("should be pointer to ScmVal::ClosureBox"),
    }
}

// Vectors ////////////////////////////////////////////////////////////////////

pub fn new_vec(vec: Vec<ScmVal>, heap: &mut Heap) -> ValResult {
    let ptr = heap.cons(ScmVal::VectorBox(Box::new(vec)), ScmVal::Empty);
    Ok(ScmVal::VectorRef(ptr))
}

pub fn extract_vec(val: ScmVal, heap: &Heap) -> Vec<ScmVal> {
    let ptr = match val {
        ScmVal::VectorRef(ptr) => ptr,
        _ => panic!("should be ScmVal::VectorRef"),
    };
    extract_vec_ptr(ptr, heap)
}

pub fn extract_vec_ptr(ptr: Pointer, heap: &Heap) -> Vec<ScmVal> {
    let vec_box = heap.car(ptr);
    match vec_box {
        ScmVal::VectorBox(vec) => (*vec).clone(),
        _ => panic!("should be pointer to ScmVal::VectorBox"),
    }
}

// HashMap ////////////////////////////////////////////////////////////////////

pub fn new_map(map: HashMap<ScmVal, ScmVal>, heap: &mut Heap) -> ValResult {
    let ptr = heap.cons(ScmVal::HashMapBox(Box::new(Map::new(map))), ScmVal::Empty);
    Ok(ScmVal::HashMapRef(ptr))
}

pub fn empty_box_map() -> ScmVal {
    ScmVal::HashMapBox(Box::new(Map::new(HashMap::new())))
}

pub fn extract_map(val: ScmVal, heap: &mut Heap) -> HashMap<ScmVal, ScmVal> {
    let ptr = match val {
        ScmVal::HashMapRef(ptr) => ptr,
        _ => panic!("should be ScmVal::HashMapRef"),
    };
    extract_map_ptr(ptr, heap)
}

pub fn extract_map_ptr(ptr: Pointer, heap: &Heap) -> HashMap<ScmVal, ScmVal> {
    let map_box = heap.car(ptr);
    match map_box {
        ScmVal::HashMapBox(map) => map.hash_map.clone(),
        _ => panic!("should be pointer to ScmVal::HashMapBox"),
    }
}

// ScmVal to_string ///////////////////////////////////////////////////////////
//
// Needs the heap so it cannot be implemented in the types module as it is
// required by the heap. It could be implemented in core along with a print etc.
// but it will save some space to implement a helper here.
//
// This is not tail recursive, so if asked to print a really large nested list it
// could fail. Perhaps it could be written in scheme with the right core methods.

pub fn val_to_string(val: ScmVal, heap: &Heap) -> String {
    match val {
        ScmVal::Number(num) => num.to_string(),
        ScmVal::Boolean(b) => match b {
            true => "#t".to_owned(),
            false => "#f".to_owned(),
        },
        ScmVal::Character(ch) => format!("{ch}"),
        ScmVal::Symbol(s) => s.to_string(),
        ScmVal::StringRef(ptr) => extract_scm_string_ptr(ptr, heap).to_string(),
        ScmVal::Pair(ptr) => pair_to_string(ptr, heap),
        ScmVal::Env(ptr) => format!("#<ENVIRONMENT {ptr}"),
        ScmVal::ClosureRef(ptr) => format!("#<CLOSURE {ptr}>"),
        ScmVal::Core(op) => format!("#<PROCEDURE {:?}>", op),
        ScmVal::Vector(vec) => vec_to_string(vec, heap),
        ScmVal::Undefined => "<undefined>".to_owned(),
        ScmVal::Empty => "()".to_owned(),
        v => format!("{:?}", v),
    }
}

fn pair_to_string(ptr: Pointer, heap: &Heap) -> String {
    let mut values = Vec::new();
    for p in ListPtrIter::new(ptr, heap) {
        let cell = heap.get_cell(p);
        values.push(cell.head.clone());

        // if it has a dotted pair end list and format it correctly
        if cell.is_dotted() {
            return format!(
                "({} . {})",
                vals_vec_to_string(values, heap),
                val_to_string(cell.tail, heap)
            );
        }
    }

    // just a regular list
    format!("({})", vals_vec_to_string(values, heap))
}

fn vec_to_string(vec: Vec<ScmVal>, heap: &Heap) -> String {
    format!("#({})", vals_vec_to_string(vec, heap))
}

fn vals_vec_to_string(vec: Vec<ScmVal>, heap: &Heap) -> String {
    vec.into_iter()
        .map(|v| val_to_string(v, heap))
        .collect::<Vec<String>>()
        .join(" ")
}

// Convert List and Vec ///////////////////////////////////////////////////////

pub fn list_to_vec(list: ScmVal, heap: &Heap) -> Vec<ScmVal> {
    match list {
        ScmVal::Pair(ptr) => ListValIter::new(ptr, heap).collect(),
        ScmVal::Empty => vec![],
        _ => panic!("list should be ScmVal::Pair: {:?}", list),
    }
}

pub fn vec_to_list(vec: Vec<ScmVal>, heap: &mut Heap) -> ScmVal {
    vec.into_iter()
        .rev()
        .fold(ScmVal::Empty, |acc, val| ScmVal::Pair(heap.cons(val, acc)))
}

// Debugging //////////////////////////////////////////////////////////////////

// Turns an ScmVal::Pair into the debug string of it's values.
// Is not recursive for nested Pairs.
pub fn debug_list(val: ScmVal, heap: &mut Heap) -> String {
    match val {
        ScmVal::Pair(p) => format!("{:?}", ListValIter::new(p, heap).collect::<Vec<ScmVal>>()),
        _ => "**ListDebugFailed**".to_owned(),
    }
}
