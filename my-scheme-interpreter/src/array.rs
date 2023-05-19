use crate::cell_mut::CellValue;
use crate::rep::{DisplayRep, ExternalRep};
use std::cell::{Ref, RefCell};
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Array<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    vals: Rc<Vec<RefCell<T>>>,
}

impl<T> Array<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    pub fn new(val: T, size: usize) -> Array<T> {
        let mut vec = Vec::with_capacity(size);
        for _ in 0..size {
            vec.push(RefCell::new(val.clone()));
        }
        Array { vals: Rc::new(vec) }
    }

    pub fn fill(&self, val: T) {
        for v in self.vals.iter() {
            v.replace(val.clone());
        }
    }

    // Access //

    pub fn get(&self, idx: usize) -> Option<Ref<'_, T>> {
        if idx < self.len() {
            Some(self.vals[idx].borrow())
        } else {
            None
        }
    }

    pub fn set(&self, val: T, idx: usize) -> Option<T> {
        if idx < self.len() {
            Some(self.vals[idx].replace(val))
        } else {
            None
        }
    }

    pub fn values(&self) -> ArrayIter<T> {
        ArrayIter::new(self)
    }

    // Information //

    pub fn len(&self) -> usize {
        self.vals.len()
    }
}

// Traits /////////////////////////////////////////////////////////////////////

impl<T> From<Vec<T>> for Array<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    fn from(vec: Vec<T>) -> Array<T> {
        Array {
            vals: Rc::new(vec.into_iter().map(|v| RefCell::new(v)).collect()),
        }
    }
}

impl<T> From<T> for Array<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    fn from(val: T) -> Array<T> {
        match val.get_cell() {
            Some(cell) => Array {
                vals: Rc::new(cell.values().map(|v| RefCell::new(v)).collect()),
            },
            None => Array {
                vals: Rc::new(vec![RefCell::new(val)]),
            },
        }
    }
}

// Representaitons //

impl<T> DisplayRep for Array<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    fn to_display(&self) -> String {
        format!(
            "#({})",
            self.values()
                .map(|v| v.to_display())
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

impl<T> ExternalRep for Array<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    fn to_external(&self) -> String {
        format!(
            "#({})",
            self.values()
                .map(|v| v.to_external())
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

// Iterator ///////////////////////////////////////////////////////////////////

pub struct ArrayIter<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    idx: usize,
    arr: Rc<Vec<RefCell<T>>>,
}

impl<T> ArrayIter<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    pub fn new(array: &Array<T>) -> ArrayIter<T> {
        ArrayIter {
            idx: 0,
            arr: Rc::clone(&array.vals),
        }
    }
}

impl<T> Iterator for ArrayIter<T>
where
    T: Clone + CellValue<T> + ExternalRep + DisplayRep,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.arr.len() {
            None
        } else {
            let val = self.arr[self.idx].borrow();
            self.idx += 1;
            Some(val.clone())
        }
    }
}

// Testing ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell_mut::Cell;

    // Test CellValue //

    #[derive(Clone, Debug, PartialEq)]
    enum TestVal {
        Int(i64),
        Pair(Cell<TestVal>),
    }

    impl CellValue<TestVal> for TestVal {
        fn get_cell(&self) -> Option<Cell<TestVal>> {
            match self {
                TestVal::Int(_) => None,
                TestVal::Pair(cell) => Some(cell.clone()),
            }
        }

        fn is_empty(&self) -> bool {
            false
        }
    }

    impl ExternalRep for TestVal {
        fn to_external(&self) -> String {
            match self {
                TestVal::Int(i) => format!("#{}", i),
                TestVal::Pair(cell) => format!("{}", cell.to_external()),
            }
        }
    }

    impl DisplayRep for TestVal {
        fn to_display(&self) -> String {
            match self {
                TestVal::Int(i) => format!("{}", i),
                TestVal::Pair(cell) => format!("{}", cell.to_display()),
            }
        }
    }

    // Tests //

    #[test]
    fn test_array_new() {
        let arr = Array::new(TestVal::Int(1), 4);
        assert_eq!(arr.get(0).unwrap().clone(), TestVal::Int(1));
        assert_eq!(arr.get(1).unwrap().clone(), TestVal::Int(1));
        assert_eq!(arr.get(2).unwrap().clone(), TestVal::Int(1));
        assert_eq!(arr.get(3).unwrap().clone(), TestVal::Int(1));
        assert!(arr.get(4).is_none());
    }

    #[test]
    fn test_array_fill() {
        let arr = Array::new(TestVal::Int(1), 4);
        arr.fill(TestVal::Int(4));
        assert_eq!(arr.get(0).unwrap().clone(), TestVal::Int(4));
        assert_eq!(arr.get(1).unwrap().clone(), TestVal::Int(4));
        assert_eq!(arr.get(2).unwrap().clone(), TestVal::Int(4));
        assert_eq!(arr.get(3).unwrap().clone(), TestVal::Int(4));
        assert!(arr.get(4).is_none());
    }

    #[test]
    fn test_array_length() {
        let arr = Array::from(vec![TestVal::Int(1), TestVal::Int(2), TestVal::Int(3)]);
        assert_eq!(arr.len(), 3);
    }

    #[test]
    fn test_array_get() {
        let arr = Array::from(vec![TestVal::Int(1), TestVal::Int(2), TestVal::Int(3)]);
        assert_eq!(arr.get(0).unwrap().clone(), TestVal::Int(1));
        assert_eq!(arr.get(1).unwrap().clone(), TestVal::Int(2));
        assert_eq!(arr.get(2).unwrap().clone(), TestVal::Int(3));
        assert!(arr.get(3).is_none());
    }

    #[test]
    fn test_array_set() {
        let arr = Array::from(vec![TestVal::Int(1), TestVal::Int(2), TestVal::Int(3)]);
        assert_eq!(
            arr.set(TestVal::Int(88), 0).unwrap().clone(),
            TestVal::Int(1)
        );
        assert_eq!(
            arr.set(TestVal::Int(99), 1).unwrap().clone(),
            TestVal::Int(2)
        );
        assert!(arr.set(TestVal::Int(100), 3).is_none());
        assert_eq!(arr.get(0).unwrap().clone(), TestVal::Int(88));
        assert_eq!(arr.get(1).unwrap().clone(), TestVal::Int(99));
        assert_eq!(arr.get(2).unwrap().clone(), TestVal::Int(3));
    }

    #[test]
    fn test_array_iter() {
        let arr = Array::from(vec![TestVal::Int(1), TestVal::Int(2), TestVal::Int(3)]);
        let mut iter = arr.values();
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_array_from_list() {
        let cell = Cell::new(TestVal::Int(5), None);
        let cell2 = Cell::new(TestVal::Int(4), Some(TestVal::Pair(cell.clone())));
        let cell3 = Cell::new(TestVal::Int(3), Some(TestVal::Pair(cell2.clone())));
        let cell4 = Cell::new(TestVal::Int(2), Some(TestVal::Pair(cell3.clone())));
        let cell5 = Cell::new(TestVal::Int(1), Some(TestVal::Pair(cell4.clone())));

        let arr = Array::from(TestVal::Pair(cell5));
        let mut iter = arr.values();
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_array_display() {
        let arr = Array::from(vec![TestVal::Int(1), TestVal::Int(2), TestVal::Int(3)]);
        assert_eq!(arr.to_display(), "#(1 2 3)".to_owned());
        assert_eq!(arr.to_external(), "#(#1 #2 #3)".to_owned());
    }
}
