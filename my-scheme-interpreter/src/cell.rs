use std::cell::{Ref, RefCell};
use std::rc::Rc;

// TODO testing

// Cell Value Trait ////////////////////////////////////////////////////////////
pub trait CellValue<T>
where
    T: Clone,
{
    fn get_cell(&self) -> Option<Rc<Cell<T>>>;
}

// Cons Cell //////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq)]
pub struct Cell<T> {
    mutable: bool,
    head: RefCell<T>,
    tail: RefCell<Option<T>>,
}

impl<T> Cell<T>
where
    T: Clone + CellValue<T>,
{
    // Constructors //

    pub fn new(head: T, tail: Option<T>) -> Cell<T> {
        Cell {
            mutable: false,
            head: RefCell::new(head),
            tail: RefCell::new(tail),
        }
    }

    pub fn new_mut(head: T, tail: Option<T>) -> Cell<T> {
        Cell {
            mutable: true,
            head: RefCell::new(head),
            tail: RefCell::new(tail),
        }
    }

    // Access //

    pub fn set_head(&self, val: T) -> Option<T> {
        if self.mutable {
            Some(self.head.replace(val))
        } else {
            None
        }
    }

    pub fn set_tail(&self, val: Option<T>) -> Option<T> {
        if self.mutable {
            self.tail.replace(val)
        } else {
            None
        }
    }

    pub fn clone_head(&self) -> T {
        self.head.borrow().clone()
    }

    pub fn clone_tail(&self) -> Option<T> {
        self.tail.borrow().clone()
    }

    pub fn borrow_head(&self) -> Ref<'_, T> {
        self.head.borrow()
    }

    pub fn borrow_tail(&self) -> Ref<'_, Option<T>> {
        self.tail.borrow()
    }

    // Information //

    pub fn is_mut(&self) -> bool {
        self.mutable
    }

    // For a list type value like scheme, Empty should not appear in the tail
    // position of a Cell. It should always be None to indicate nothing follows.
    // If Empty is used this will consider it a dotted pair. If None is in the
    // tail position the cell is not considered a dotted pair, but the end of
    // a list.
    pub fn is_dotted(&self) -> bool {
        match self.clone_tail() {
            Some(val) => match val.get_cell() {
                Some(_) => false,
                None => true,
            },
            None => false,
        }
    }
}

// Cell Iterator //////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct CellIter<T> {
    cell: Option<Rc<Cell<T>>>,
}

impl<T> CellIter<T>
where
    T: Clone + CellValue<T>,
{
    pub fn new(cell: Rc<Cell<T>>) -> CellIter<T> {
        CellIter { cell: Some(cell) }
    }
}

impl<T> Iterator for CellIter<T>
where
    T: Clone + CellValue<T>,
{
    type Item = Rc<Cell<T>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cell.clone() {
            Some(cell) => {
                let current = Some(cell.clone());
                self.cell = match cell.clone_tail() {
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
pub struct CellValueIter<T> {
    last: Option<Rc<Cell<T>>>,
    iter: CellIter<T>,
}

impl<T> CellValueIter<T>
where
    T: Clone + CellValue<T>,
{
    pub fn new(cell: Rc<Cell<T>>) -> CellValueIter<T> {
        CellValueIter {
            last: None,
            iter: CellIter::new(cell),
        }
    }
}

impl<T> Iterator for CellValueIter<T>
where
    T: Clone + CellValue<T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cell) = self.last.clone() {
            let value = cell.clone_tail();
            self.last = None;
            return value;
        }

        let next = self.iter.next();
        match next {
            Some(cell) => {
                let value = cell.clone_head();
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

    // Test CellValue //

    #[derive(Clone, Debug, PartialEq)]
    enum TestVal {
        Int(i64),
        Pair(Rc<Cell<TestVal>>),
    }

    impl TestVal {
        pub fn doot(&self) -> i64 {
            match self {
                TestVal::Int(i) => *i,
                TestVal::Pair(_) => 42,
            }
        }
    }

    impl CellValue<TestVal> for TestVal {
        fn get_cell(&self) -> Option<Rc<Cell<TestVal>>> {
            match self {
                TestVal::Int(_) => None,
                TestVal::Pair(cell) => Some(cell.clone()),
            }
        }
    }

    // Cell //

    #[test]
    fn test_cell_clone_head_and_tail() {
        let cell = Cell::new(TestVal::Int(5), None);
        assert_eq!(cell.clone_head(), TestVal::Int(5));
        assert_eq!(cell.clone_tail(), None);
    }

    #[test]
    fn test_cell_borrow_head_and_tail() {
        let cell = Cell::new(TestVal::Int(5), Some(TestVal::Int(88)));
        assert_eq!(cell.borrow_head().doot(), 5);
        assert_eq!(cell.clone_tail().unwrap().doot(), 88);
    }

    #[test]
    fn test_cell_set_head_and_tail() {
        let cell = Cell::new_mut(TestVal::Int(5), None);
        cell.set_head(TestVal::Int(56));
        cell.set_tail(Some(TestVal::Int(99)));
        assert_eq!(cell.clone_head(), TestVal::Int(56));
        assert_eq!(cell.clone_tail(), Some(TestVal::Int(99)));
    }

    #[test]
    fn test_cell_is_mutable() {
        let cell = Cell::new(TestVal::Int(5), None);
        assert_eq!(cell.is_mut(), false);

        let cell = Cell::new_mut(TestVal::Int(5), None);
        assert_eq!(cell.is_mut(), true);
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
        let cell = Rc::new(Cell::new(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new(
            TestVal::Int(3),
            Some(TestVal::Pair(cell2.clone())),
        ));
        let cell4 = Rc::new(Cell::new(
            TestVal::Int(2),
            Some(TestVal::Pair(cell3.clone())),
        ));
        let cell5 = Rc::new(Cell::new(
            TestVal::Int(1),
            Some(TestVal::Pair(cell4.clone())),
        ));

        let mut iter = CellIter::new(cell5.clone());
        assert_eq!(iter.next(), Some(cell5));
        assert_eq!(iter.next(), Some(cell4));
        assert_eq!(iter.next(), Some(cell3));
        assert_eq!(iter.next(), Some(cell2));
        assert_eq!(iter.next(), Some(cell));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_cell_value_iterator_last_is_not_dotted() {
        let cell = Rc::new(Cell::new(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new(
            TestVal::Int(3),
            Some(TestVal::Pair(cell2.clone())),
        ));
        let cell4 = Rc::new(Cell::new(
            TestVal::Int(2),
            Some(TestVal::Pair(cell3.clone())),
        ));
        let cell5 = Rc::new(Cell::new(
            TestVal::Int(1),
            Some(TestVal::Pair(cell4.clone())),
        ));

        let mut iter = CellValueIter::new(cell5.clone());
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_cell_value_iterator_last_is_dotted() {
        let cell = Rc::new(Cell::new(TestVal::Int(5), Some(TestVal::Int(6))));
        let cell2 = Rc::new(Cell::new(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new(
            TestVal::Int(3),
            Some(TestVal::Pair(cell2.clone())),
        ));
        let cell4 = Rc::new(Cell::new(
            TestVal::Int(2),
            Some(TestVal::Pair(cell3.clone())),
        ));
        let cell5 = Rc::new(Cell::new(
            TestVal::Int(1),
            Some(TestVal::Pair(cell4.clone())),
        ));

        let mut iter = CellValueIter::new(cell5.clone());
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), Some(TestVal::Int(6)));
        assert_eq!(iter.next(), None);
    }
}
