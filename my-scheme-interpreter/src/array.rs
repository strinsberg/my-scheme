use crate::rep::{DisplayRep, ExternalRep};
use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq)]
pub struct Array<T> {
    vals: Vec<T>,
}

impl<T> Array<T> {
    // Access //

    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx < self.len() {
            Some(&self.vals[idx])
        } else {
            None
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.vals.iter()
    }

    // Information //

    pub fn len(&self) -> usize {
        self.vals.len()
    }
}

// Traits /////////////////////////////////////////////////////////////////////

impl<T> Default for Array<T> {
    fn default() -> Array<T> {
        Array { vals: vec![] }
    }
}

impl<T> From<Vec<T>> for Array<T> {
    fn from(vec: Vec<T>) -> Array<T> {
        Array { vals: vec }
    }
}

impl<T> DisplayRep for Array<T>
where
    T: Debug + DisplayRep,
{
    fn to_display(&self) -> String {
        "s".to_owned()
    }
}

impl<T> ExternalRep for Array<T>
where
    T: Debug + ExternalRep,
{
    fn to_external(&self) -> String {
        "s".to_owned()
    }
}

// Testing ////////////////////////////////////////////////////////////////////
