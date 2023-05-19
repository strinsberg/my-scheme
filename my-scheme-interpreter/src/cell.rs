use crate::rep::{DisplayRep, ExternalRep};
use std::cell::{Ref, RefCell};
use std::rc::Rc;

// Cell Value Trait ///////////////////////////////////////////////////////////

pub trait CellValue<T>
where
    T: Clone + DisplayRep + ExternalRep,
{
    fn is_empty(&self) -> bool;
    fn get_cell(&self) -> Option<Cell<T>>;
}

// Cons Cell //////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq)]
pub struct Cell<T> {
    head: Rc<RefCell<T>>,
    tail: Rc<RefCell<Option<T>>>,
}

impl<T> Cell<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    // Constructors //

    pub fn new(head: T, tail: Option<T>) -> Cell<T> {
        Cell {
            head: Rc::new(RefCell::new(head)),
            tail: Rc::new(RefCell::new(tail)),
        }
    }

    // Access //

    pub fn head(&self) -> Ref<'_, T> {
        self.head.borrow()
    }

    pub fn tail(&self) -> Ref<'_, Option<T>> {
        self.tail.borrow()
    }

    pub fn values(&self) -> CellValueIter<T> {
        CellValueIter::new(self.clone())
    }

    pub fn cells(&self) -> CellIter<T> {
        CellIter::new(self.clone())
    }

    // Information //

    pub fn is_dotted(&self) -> bool {
        match self.tail().clone() {
            Some(val) => {
                if val.is_empty() {
                    false
                } else {
                    match val.get_cell() {
                        Some(_) => false,
                        None => true,
                    }
                }
            }
            None => false,
        }
    }
}

// Traits /////////////////////////////////////////////////////////////////////

impl<T> DisplayRep for Cell<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    fn to_display(&self) -> String {
        let mut strings = vec![];
        for cell in self.cells() {
            strings.push(cell.head().to_display());
            if cell.is_dotted() {
                strings.push(".".to_owned());
                strings.push(
                    cell.tail()
                        .clone()
                        .expect("dotted tail should not be none")
                        .to_display(),
                );
            }
        }
        format!("({})", strings.join(" "))
    }
}

impl<T> ExternalRep for Cell<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    fn to_external(&self) -> String {
        let mut strings = vec![];
        for cell in self.cells() {
            strings.push(cell.head().to_external());
            if cell.is_dotted() {
                strings.push(".".to_owned());
                strings.push(
                    cell.tail()
                        .clone()
                        .expect("dotted tail should not be none")
                        .to_external(),
                );
            }
        }
        format!("({})", strings.join(" "))
    }
}

impl<T> std::fmt::Display for Cell<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_display())
    }
}

impl<T> std::fmt::Debug for Cell<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Cell{{ {} }}", self.to_external())
    }
}

// Cell Iterator //////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct CellIter<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    cell: Option<Cell<T>>,
}

impl<'a, T> CellIter<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    pub fn new(cell: Cell<T>) -> CellIter<T> {
        CellIter { cell: Some(cell) }
    }
}

impl<T> Iterator for CellIter<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    type Item = Cell<T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cell {
            Some(ref cell) => {
                let next = match *cell.tail() {
                    Some(ref val) => val.get_cell(),
                    None => None,
                };
                let current = Some(cell.clone());
                self.cell = next;
                current
            }
            None => None,
        }
    }
}

// CellValue Iterator /////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct CellValueIter<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    last: Option<T>,
    iter: CellIter<T>,
}

impl<T> CellValueIter<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    pub fn new(cell: Cell<T>) -> CellValueIter<T> {
        CellValueIter {
            last: None,
            iter: CellIter::new(cell),
        }
    }
}

impl<T> Iterator for CellValueIter<T>
where
    T: Clone + CellValue<T> + DisplayRep + ExternalRep,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(val) = self.last.clone() {
            self.last = None;
            return Some(val);
        }

        let next = self.iter.next();
        match next {
            Some(ref cell) => {
                let value = cell.head();
                if cell.is_dotted() {
                    self.last = cell.tail().clone();
                }
                Some(value.clone())
            }
            None => None,
        }
    }
}

// Testing ////////////////////////////////////////////////////////////////////
#[cfg(test)]
mod tests {
    use super::*;

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

    // Helpers //

    fn make_list_5() -> Cell<TestVal> {
        let cell = Cell::new(TestVal::Int(5), None);
        let cell2 = Cell::new(TestVal::Int(4), Some(TestVal::Pair(cell.clone())));
        let cell3 = Cell::new(TestVal::Int(3), Some(TestVal::Pair(cell2.clone())));
        let cell4 = Cell::new(TestVal::Int(2), Some(TestVal::Pair(cell3.clone())));
        let cell5 = Cell::new(TestVal::Int(1), Some(TestVal::Pair(cell4.clone())));
        cell5
    }

    fn make_list_6_dotted() -> Cell<TestVal> {
        let cell = Cell::new(TestVal::Int(5), Some(TestVal::Int(6)));
        let cell2 = Cell::new(TestVal::Int(4), Some(TestVal::Pair(cell.clone())));
        let cell3 = Cell::new(TestVal::Int(3), Some(TestVal::Pair(cell2.clone())));
        let cell4 = Cell::new(TestVal::Int(2), Some(TestVal::Pair(cell3.clone())));
        let cell5 = Cell::new(TestVal::Int(1), Some(TestVal::Pair(cell4.clone())));
        cell5
    }

    // Cell //

    #[test]
    fn test_cell_head_and_tail() {
        let cell = Cell::new(TestVal::Int(5), None);
        assert_eq!(cell.head().clone(), TestVal::Int(5));
        assert_eq!(cell.tail().clone(), None);
    }

    #[test]
    fn test_cell_is_dotted() {
        let cell = Cell::new(TestVal::Int(5), None);
        assert_eq!(cell.is_dotted(), false);

        let cell = Cell::new(
            TestVal::Int(5),
            Some(TestVal::Pair(Cell::new(TestVal::Int(9), None))),
        );
        assert_eq!(cell.is_dotted(), false);

        let cell = Cell::new(TestVal::Int(5), Some(TestVal::Int(6)));
        assert_eq!(cell.is_dotted(), true);
    }

    // Iterators //

    #[test]
    fn test_cell_iterator() {
        let cell = Cell::new(TestVal::Int(5), None);
        let cell2 = Cell::new(TestVal::Int(4), Some(TestVal::Pair(cell.clone())));
        let cell3 = Cell::new(TestVal::Int(3), Some(TestVal::Pair(cell2.clone())));
        let cell4 = Cell::new(TestVal::Int(2), Some(TestVal::Pair(cell3.clone())));
        let cell5 = Cell::new(TestVal::Int(1), Some(TestVal::Pair(cell4.clone())));

        let mut iter = cell5.cells();
        assert_eq!(iter.next(), Some(cell5));
        assert_eq!(iter.next(), Some(cell4));
        assert_eq!(iter.next(), Some(cell3));
        assert_eq!(iter.next(), Some(cell2));
        assert_eq!(iter.next(), Some(cell));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_cell_value_iterator() {
        let list = make_list_5();
        let mut iter = list.values();
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), None);

        let list = make_list_6_dotted();
        let mut iter = list.values();
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), Some(TestVal::Int(6)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_representations() {
        let list = make_list_5();
        let list2 = make_list_6_dotted();
        let list3 = TestVal::Pair(Cell::new(
            TestVal::Pair(list2.clone()),
            Some(TestVal::Pair(list.clone())),
        ));
        assert_eq!(list3.to_display(), "((1 2 3 4 5 . 6) 1 2 3 4 5)".to_owned());
        assert_eq!(
            list3.to_external(),
            "((#1 #2 #3 #4 #5 . #6) #1 #2 #3 #4 #5)".to_owned()
        );
    }
}
