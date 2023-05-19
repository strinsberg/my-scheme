//use crate::err::Error;
use crate::rep::{DisplayRep, ExternalRep};
//use std::rc::Rc;

// TODO consider implementing map, filter, fold, reverse and other list methods
// on here directly if they must be implemented in rust. Then core procs can
// just call them. I like doing things in scheme, but using core procs offers
// a lot more flexibility for arity and type checking. We will essentially do
// this for Num and have done it for Char, so why not all the types. Then Val
// just wraps them and other things are a bridge between the Val variants,
// the interpreter, and the base types.

// Cell Value Trait ///////////////////////////////////////////////////////////

pub trait CellValue<T>
where
    T: std::fmt::Debug + Clone,
{
    fn get_cell<'a>(&'a self) -> Option<&'a Cell<T>>;
}

// Cons Cell //////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
pub struct Cell<T> {
    head: T,
    tail: Option<T>,
}

impl<T> Cell<T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    // Constructors //

    pub fn new(head: T, tail: Option<T>) -> Cell<T> {
        Cell {
            head: head,
            tail: tail,
        }
    }

    // Access //

    pub fn head(&self) -> &T {
        &self.head
    }

    pub fn tail(&self) -> Option<&T> {
        match self.tail {
            Some(ref t) => Some(t),
            None => None,
        }
    }

    pub fn values(&self) -> CellValueIter<T> {
        CellValueIter::new(self)
    }

    pub fn cells(&self) -> CellIter<T> {
        CellIter::new(self)
    }

    // Information //

    // For a list type value like scheme, Empty should not appear in the tail
    // position of a Cell. It should always be None to indicate nothing follows.
    // If Empty is used this will consider it a dotted pair. If None is in the
    // tail position the cell is not considered a dotted pair, but the end of
    // a list.
    pub fn is_dotted(&self) -> bool {
        match self.tail.clone() {
            Some(val) => match val.get_cell() {
                Some(_) => false,
                None => true,
            },
            None => false,
        }
    }
}

// Traits /////////////////////////////////////////////////////////////////////

impl<T> DisplayRep for Cell<T>
where
    T: std::fmt::Debug + Clone + CellValue<T> + DisplayRep,
{
    fn to_display(&self) -> String {
        let mut strings = vec![];
        for cell in CellIter::new(self) {
            strings.push(cell.head.to_display());
            if cell.is_dotted() {
                strings.push(".".to_owned());
                strings.push(
                    cell.tail()
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
    T: std::fmt::Debug + Clone + CellValue<T> + ExternalRep,
{
    fn to_external(&self) -> String {
        let mut strings = vec![];
        for cell in CellIter::new(self) {
            strings.push(cell.head.to_external());
            if cell.is_dotted() {
                strings.push(".".to_owned());
                strings.push(
                    cell.tail()
                        .expect("dotted tail should not be none")
                        .to_external(),
                );
            }
        }
        format!("({})", strings.join(" "))
    }
}

// Cell Iterator //////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct CellIter<'a, T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    cell: Option<&'a Cell<T>>,
}

impl<'a, T> CellIter<'a, T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    pub fn new(cell: &'a Cell<T>) -> CellIter<'a, T> {
        CellIter { cell: Some(cell) }
    }
}

impl<'a, T> Iterator for CellIter<'a, T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    type Item = &'a Cell<T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cell {
            Some(cell) => {
                let current = Some(cell);
                self.cell = match cell.tail() {
                    Some(val) => val.get_cell(),
                    None => None,
                };
                current
            }
            None => None,
        }
    }
}

// CellValue Iterator /////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct CellValueIter<'a, T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    last: Option<&'a Cell<T>>,
    iter: CellIter<'a, T>,
}

impl<'a, T> CellValueIter<'a, T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    pub fn new(cell: &'a Cell<T>) -> CellValueIter<'a, T> {
        CellValueIter {
            last: None,
            iter: CellIter::new(cell),
        }
    }
}

impl<'a, T> Iterator for CellValueIter<'a, T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cell) = self.last {
            let value = cell.tail().expect("last should not have None tail");
            self.last = None;
            return Some(value);
        }

        let next = self.iter.next();
        match next {
            Some(cell) => {
                let value = cell.head();
                if cell.is_dotted() {
                    self.last = Some(cell);
                }
                Some(value)
            }
            None => None,
        }
    }
}

// Testing ////////////////////////////////////////////////////////////////////
#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    // Test CellValue //

    #[derive(Clone, Debug, PartialEq)]
    enum TestVal {
        Int(i64),
        Pair(Rc<Cell<TestVal>>),
    }

    impl CellValue<TestVal> for TestVal {
        fn get_cell<'a>(&'a self) -> Option<&'a Cell<TestVal>> {
            match self {
                TestVal::Int(_) => None,
                TestVal::Pair(cell) => Some(cell),
            }
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
        let cell2 = Cell::new(TestVal::Int(4), Some(TestVal::Pair(Rc::new(cell.clone()))));
        let cell3 = Cell::new(TestVal::Int(3), Some(TestVal::Pair(Rc::new(cell2.clone()))));
        let cell4 = Cell::new(TestVal::Int(2), Some(TestVal::Pair(Rc::new(cell3.clone()))));
        let cell5 = Cell::new(TestVal::Int(1), Some(TestVal::Pair(Rc::new(cell4.clone()))));
        cell5
    }

    fn make_list_6_dotted() -> Cell<TestVal> {
        let cell = Cell::new(TestVal::Int(5), Some(TestVal::Int(6)));
        let cell2 = Cell::new(TestVal::Int(4), Some(TestVal::Pair(Rc::new(cell.clone()))));
        let cell3 = Cell::new(TestVal::Int(3), Some(TestVal::Pair(Rc::new(cell2.clone()))));
        let cell4 = Cell::new(TestVal::Int(2), Some(TestVal::Pair(Rc::new(cell3.clone()))));
        let cell5 = Cell::new(TestVal::Int(1), Some(TestVal::Pair(Rc::new(cell4.clone()))));
        cell5
    }

    // Cell //

    #[test]
    fn test_cell_head_and_tail() {
        let cell = Cell::new(TestVal::Int(5), None);
        assert_eq!(cell.head(), &TestVal::Int(5));
        assert_eq!(cell.tail(), None);
    }

    #[test]
    fn test_cell_is_dotted() {
        let cell = Cell::new(TestVal::Int(5), None);
        assert_eq!(cell.is_dotted(), false);

        let cell = Cell::new(
            TestVal::Int(5),
            Some(TestVal::Pair(Rc::new(Cell::new(TestVal::Int(9), None)))),
        );
        assert_eq!(cell.is_dotted(), false);

        let cell = Cell::new(TestVal::Int(5), Some(TestVal::Int(6)));
        assert_eq!(cell.is_dotted(), true);
    }

    // Iterators //

    #[test]
    fn test_cell_iterator() {
        let cell = Cell::new(TestVal::Int(5), None);
        let cell2 = Cell::new(TestVal::Int(4), Some(TestVal::Pair(Rc::new(cell.clone()))));
        let cell3 = Cell::new(TestVal::Int(3), Some(TestVal::Pair(Rc::new(cell2.clone()))));
        let cell4 = Cell::new(TestVal::Int(2), Some(TestVal::Pair(Rc::new(cell3.clone()))));
        let cell5 = Cell::new(TestVal::Int(1), Some(TestVal::Pair(Rc::new(cell4.clone()))));

        let mut iter = CellIter::new(&cell5);
        assert_eq!(iter.next(), Some(&cell5));
        assert_eq!(iter.next(), Some(&cell4));
        assert_eq!(iter.next(), Some(&cell3));
        assert_eq!(iter.next(), Some(&cell2));
        assert_eq!(iter.next(), Some(&cell));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_cell_value_iterator() {
        let list = make_list_5();
        let mut iter = CellValueIter::new(&list);
        assert_eq!(iter.next(), Some(&TestVal::Int(1)));
        assert_eq!(iter.next(), Some(&TestVal::Int(2)));
        assert_eq!(iter.next(), Some(&TestVal::Int(3)));
        assert_eq!(iter.next(), Some(&TestVal::Int(4)));
        assert_eq!(iter.next(), Some(&TestVal::Int(5)));
        assert_eq!(iter.next(), None);

        let list = make_list_6_dotted();
        let mut iter = CellValueIter::new(&list);
        assert_eq!(iter.next(), Some(&TestVal::Int(1)));
        assert_eq!(iter.next(), Some(&TestVal::Int(2)));
        assert_eq!(iter.next(), Some(&TestVal::Int(3)));
        assert_eq!(iter.next(), Some(&TestVal::Int(4)));
        assert_eq!(iter.next(), Some(&TestVal::Int(5)));
        assert_eq!(iter.next(), Some(&TestVal::Int(6)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_display() {
        let list = make_list_5();
        let list2 = make_list_6_dotted();
        let list3 = TestVal::Pair(Rc::new(Cell::new(
            TestVal::Pair(Rc::new(list2.clone())),
            Some(TestVal::Pair(Rc::new(list.clone()))),
        )));
        assert_eq!(list3.to_display(), "((1 2 3 4 5 . 6) 1 2 3 4 5)".to_owned());
    }

    #[test]
    fn test_external() {
        let list = make_list_5();
        let list2 = make_list_6_dotted();
        let list3 = TestVal::Pair(Rc::new(Cell::new(
            TestVal::Pair(Rc::new(list2.clone())),
            Some(TestVal::Pair(Rc::new(list.clone()))),
        )));
        assert_eq!(
            list3.to_external(),
            "((#1 #2 #3 #4 #5 . #6) #1 #2 #3 #4 #5)".to_owned()
        );
    }
}
