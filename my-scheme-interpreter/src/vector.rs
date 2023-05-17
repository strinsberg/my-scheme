use std::cell::{Ref, RefCell};

// Arithmetic Errors //////////////////////////////////////////////////////////

#[derive(Debug, PartialEq)]
pub enum VecErr {
    Immutable,
    OutOfRange,
}

// Vector Type ////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
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
