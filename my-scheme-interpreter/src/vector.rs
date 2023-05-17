use std::cell::{Ref, RefCell};

// TODO implement Display, ExternalRep, Default, etc.
// TODO instead of the external iterator by borrowing the vec, we could implement
// map, fold, rfold, reverse, etc. on this type itself and they could return
// new Vectors instead on having to use the iterator of the internal vec and
// then call Vector::from() on the results. The first way is just going to
// wrap the second way, but could make some things simpler for Val and core_procs.
// Could also implement some of the vector functions on this type so that core
// procs is only responsible for checking argument types and maybe arity? We could
// do this for all of the types, implement the functionality needed for scheme
// functions inside the struct. The core procs can do the work to give the methods
// what they need, like ensuring the index is correct and passing correct argument
// types.

// Arithmetic Errors //////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum VecErr {
    Immutable,
    OutOfRange,
}

// Vector Type ////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct Vector<T>
where
    T: Clone + Default,
{
    mutable: bool,
    vec: RefCell<Vec<T>>,
}

impl<T> Vector<T>
where
    T: Clone + Default,
{
    // Constructors //

    pub fn new(val: T, size: usize) -> Vector<T> {
        Vector {
            mutable: false,
            vec: RefCell::new(vec![val; size]),
        }
    }

    pub fn new_mut(val: T, size: usize) -> Vector<T> {
        Vector {
            mutable: true,
            vec: RefCell::new(vec![val; size]),
        }
    }

    pub fn from(vec: Vec<T>) -> Vector<T> {
        Vector {
            mutable: false,
            vec: RefCell::new(vec),
        }
    }

    pub fn from_mut(vec: Vec<T>) -> Vector<T> {
        Vector {
            mutable: true,
            vec: RefCell::new(vec),
        }
    }

    // Access //

    pub fn set(&self, val: T, idx: usize) -> Result<T, VecErr> {
        if !self.mutable {
            Err(VecErr::Immutable)
        } else if idx >= self.len() {
            Err(VecErr::OutOfRange)
        } else {
            self.vec.borrow_mut()[idx] = val;
            Ok(T::default())
        }
    }

    pub fn get(&self, idx: usize) -> Option<T> {
        if idx < self.len() {
            Some(self.vec.borrow()[idx].clone())
        } else {
            None
        }
    }

    pub fn borrow_vec(&self) -> Ref<'_, Vec<T>> {
        self.vec.borrow()
    }

    // Information //

    pub fn len(&self) -> usize {
        self.vec.borrow().len()
    }

    pub fn is_mut(&self) -> bool {
        self.mutable
    }
}

// Tests //////////////////////////////////////////////////////////////////////
