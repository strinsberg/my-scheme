use crate::err::Error;
use crate::rep::{DisplayRep, ExternalRep};
use std::rc::Rc;

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
    fn get_cell(&self) -> Option<Rc<Cell<T>>>;
    fn new_pair(cell: Cell<T>) -> T;
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

    pub fn tail(&self) -> &Option<T> {
        &self.tail
    }

    pub fn values(&self) -> CellValueIter<T> {
        CellValueIter::new(Rc::new(self.clone()))
    }

    pub fn cells(&self) -> CellIter<T> {
        CellIter::new(Rc::new(self.clone()))
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

    // Scheme List Functions //

    pub fn length(&self) -> usize {
        let mut len = 0;
        for _ in CellValueIter::new(Rc::new(self.clone())) {
            len += 1;
        }
        len
    }

    pub fn list_tail(&self, idx: usize) -> Result<Option<T>, Error> {
        if idx == 0 {
            return Err(Error::BadArg(1));
        }
        for (i, cell) in CellIter::new(Rc::new(self.clone())).enumerate() {
            if idx == i + 1 {
                return Ok(cell.tail.clone());
            }
        }
        Err(Error::OutOfRange)
    }

    pub fn list_ref(&self, idx: usize) -> Result<T, Error> {
        for (i, value) in CellValueIter::new(Rc::new(self.clone())).enumerate() {
            if idx == i {
                return Ok(value.clone());
            }
        }
        Err(Error::OutOfRange)
    }

    // appends the other onto a copy of self
    pub fn append(&self, other: T) -> Result<T, Error> {
        let rev = self.reverse()?;
        let mut result = other;

        for value in CellValueIter::new(rev.get_cell().expect("should be a cell holding value")) {
            result = T::new_pair(Cell::new(value.clone(), Some(result)));
        }
        Ok(result)
    }

    pub fn reverse(&self) -> Result<T, Error> {
        if self.is_dotted() {
            return Ok(T::new_pair(Cell::new(
                self.tail.clone().expect("dotted tail should not be none"),
                Some(self.head.clone()),
            )));
        }

        let mut res = T::new_pair(Cell::new(self.head.clone(), None));
        match self.tail.clone() {
            Some(tail) => {
                for value in CellValueIter::new(tail.get_cell().expect("tail should hold a cell")) {
                    res = T::new_pair(Cell::new(value.clone(), Some(res)));
                }
                Ok(res)
            }
            None => Ok(res),
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
        for cell in CellIter::new(Rc::new(self.clone())) {
            strings.push(cell.head.to_display());
            if cell.is_dotted() {
                strings.push(".".to_owned());
                strings.push(
                    cell.tail
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
    T: std::fmt::Debug + Clone + CellValue<T> + ExternalRep,
{
    fn to_external(&self) -> String {
        let mut strings = vec![];
        for cell in CellIter::new(Rc::new(self.clone())) {
            strings.push(cell.head.to_external());
            if cell.is_dotted() {
                strings.push(".".to_owned());
                strings.push(
                    cell.tail
                        .clone()
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
pub struct CellIter<T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    cell: Option<Rc<Cell<T>>>,
}

impl<T> CellIter<T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    pub fn new(cell: Rc<Cell<T>>) -> CellIter<T> {
        CellIter { cell: Some(cell) }
    }
}

impl<T> Iterator for CellIter<T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    type Item = Rc<Cell<T>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cell.clone() {
            Some(cell) => {
                let current = Some(cell.clone());
                self.cell = match cell.tail.clone() {
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
pub struct CellValueIter<T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    last: Option<Rc<Cell<T>>>,
    iter: CellIter<T>,
}

impl<T> CellValueIter<T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
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
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cell) = self.last.clone() {
            let value = cell.tail.clone();
            self.last = None;
            return value;
        }

        let next = self.iter.next();
        match next {
            Some(cell) => {
                let value = cell.head.clone();
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

    impl CellValue<TestVal> for TestVal {
        fn get_cell(&self) -> Option<Rc<Cell<TestVal>>> {
            match self {
                TestVal::Int(_) => None,
                TestVal::Pair(cell) => Some(cell.clone()),
            }
        }

        fn new_pair(cell: Cell<TestVal>) -> TestVal {
            TestVal::Pair(Rc::new(cell))
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

    fn make_list_5() -> Rc<Cell<TestVal>> {
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
        Rc::new(Cell::new(
            TestVal::Int(1),
            Some(TestVal::Pair(cell4.clone())),
        ))
    }

    fn make_list_6_dotted() -> Rc<Cell<TestVal>> {
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
        Rc::new(Cell::new(
            TestVal::Int(1),
            Some(TestVal::Pair(cell4.clone())),
        ))
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
    fn test_cell_value_iterator() {
        let list = make_list_5();
        let mut iter = CellValueIter::new(list);
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), None);

        let list = make_list_6_dotted();
        let mut iter = CellValueIter::new(list);
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), Some(TestVal::Int(6)));
        assert_eq!(iter.next(), None);
    }

    // Pair and List Functions //

    #[test]
    fn test_cell_list_reverse() {
        let list = make_list_5();
        let rev_list = list.reverse().unwrap().get_cell().unwrap();
        let mut iter = CellValueIter::new(rev_list);
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), None);

        let list = make_list_6_dotted();
        let rev_list = list.reverse().unwrap().get_cell().unwrap();
        let mut iter = CellValueIter::new(rev_list);
        assert_eq!(iter.next(), Some(TestVal::Int(6)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_cell_list_length() {
        let list = make_list_5();
        assert_eq!(list.length(), 5);

        let list = make_list_6_dotted();
        assert_eq!(list.length(), 6);
    }

    #[test]
    fn test_cell_list_ref() {
        let list = make_list_5();
        assert_eq!(list.list_ref(0), Ok(TestVal::Int(1)));
        assert_eq!(list.list_ref(1), Ok(TestVal::Int(2)));
        assert_eq!(list.list_ref(2), Ok(TestVal::Int(3)));
        assert_eq!(list.list_ref(3), Ok(TestVal::Int(4)));
        assert_eq!(list.list_ref(4), Ok(TestVal::Int(5)));
        assert_eq!(list.list_ref(5), Err(Error::OutOfRange));

        let list = make_list_6_dotted();
        assert_eq!(list.list_ref(0), Ok(TestVal::Int(1)));
        assert_eq!(list.list_ref(1), Ok(TestVal::Int(2)));
        assert_eq!(list.list_ref(2), Ok(TestVal::Int(3)));
        assert_eq!(list.list_ref(3), Ok(TestVal::Int(4)));
        assert_eq!(list.list_ref(4), Ok(TestVal::Int(5)));
        assert_eq!(list.list_ref(5), Ok(TestVal::Int(6)));
        assert_eq!(list.list_ref(6), Err(Error::OutOfRange));
    }

    #[test]
    fn test_cell_list_tail() {
        let list = make_list_5();
        assert_eq!(list.list_tail(0), Err(Error::BadArg(1)));
        assert_eq!(
            list.list_tail(1)
                .unwrap()
                .unwrap()
                .get_cell()
                .unwrap()
                .head
                .clone(),
            TestVal::Int(2)
        );
        assert_eq!(
            list.list_tail(4)
                .unwrap()
                .unwrap()
                .get_cell()
                .unwrap()
                .head
                .clone(),
            TestVal::Int(5)
        );
        assert_eq!(list.list_tail(5).unwrap(), None);
        assert_eq!(list.list_tail(6), Err(Error::OutOfRange));

        let list = make_list_6_dotted();
        assert_eq!(
            list.list_tail(1)
                .unwrap()
                .unwrap()
                .get_cell()
                .unwrap()
                .head
                .clone(),
            TestVal::Int(2)
        );
        assert_eq!(
            list.list_tail(4)
                .unwrap()
                .unwrap()
                .get_cell()
                .unwrap()
                .head
                .clone(),
            TestVal::Int(5)
        );
        assert_eq!(list.list_tail(5).unwrap().unwrap(), TestVal::Int(6));
        assert_eq!(list.list_tail(6), Err(Error::OutOfRange));
    }

    #[test]
    fn test_cell_list_append() {
        let list = make_list_5();
        let list2 = list
            .append(TestVal::Pair(list.clone()))
            .unwrap()
            .get_cell()
            .unwrap();
        assert_eq!(list2.list_ref(0), Ok(TestVal::Int(1)));
        assert_eq!(list2.list_ref(1), Ok(TestVal::Int(2)));
        assert_eq!(list2.list_ref(2), Ok(TestVal::Int(3)));
        assert_eq!(list2.list_ref(3), Ok(TestVal::Int(4)));
        assert_eq!(list2.list_ref(4), Ok(TestVal::Int(5)));
        assert_eq!(list2.list_ref(5), Ok(TestVal::Int(1)));
        assert_eq!(list2.list_ref(6), Ok(TestVal::Int(2)));
        assert_eq!(list2.list_ref(7), Ok(TestVal::Int(3)));
        assert_eq!(list2.list_ref(8), Ok(TestVal::Int(4)));
        assert_eq!(list2.list_ref(9), Ok(TestVal::Int(5)));
        assert_eq!(list2.list_ref(10), Err(Error::OutOfRange));

        let list = make_list_6_dotted();
        let list2 = list
            .append(TestVal::Pair(list.clone()))
            .unwrap()
            .get_cell()
            .unwrap();
        assert_eq!(list2.list_ref(0), Ok(TestVal::Int(1)));
        assert_eq!(list2.list_ref(1), Ok(TestVal::Int(2)));
        assert_eq!(list2.list_ref(2), Ok(TestVal::Int(3)));
        assert_eq!(list2.list_ref(3), Ok(TestVal::Int(4)));
        assert_eq!(list2.list_ref(4), Ok(TestVal::Int(5)));
        assert_eq!(list2.list_ref(5), Ok(TestVal::Int(6)));
        assert_eq!(list2.list_ref(6), Ok(TestVal::Int(1)));
        assert_eq!(list2.list_ref(7), Ok(TestVal::Int(2)));
        assert_eq!(list2.list_ref(8), Ok(TestVal::Int(3)));
        assert_eq!(list2.list_ref(9), Ok(TestVal::Int(4)));
        assert_eq!(list2.list_ref(10), Ok(TestVal::Int(5)));
        assert_eq!(list2.list_ref(11), Ok(TestVal::Int(6)));
        assert_eq!(list2.list_ref(12), Err(Error::OutOfRange));
    }

    #[test]
    fn test_display() {
        let list = make_list_5();
        let list2 = make_list_6_dotted();
        let list3 = TestVal::Pair(Rc::new(Cell::new(
            TestVal::Pair(list2.clone()),
            Some(TestVal::Pair(list.clone())),
        )));
        assert_eq!(list3.to_display(), "((1 2 3 4 5 . 6) 1 2 3 4 5)".to_owned());
    }

    #[test]
    fn test_external() {
        let list = make_list_5();
        let list2 = make_list_6_dotted();
        let list3 = TestVal::Pair(Rc::new(Cell::new(
            TestVal::Pair(list2.clone()),
            Some(TestVal::Pair(list.clone())),
        )));
        assert_eq!(
            list3.to_external(),
            "((#1 #2 #3 #4 #5 . #6) #1 #2 #3 #4 #5)".to_owned()
        );
    }
}
