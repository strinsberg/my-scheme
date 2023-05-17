use crate::rep::{DisplayRep, ExternalRep};
use std::cell::{Ref, RefCell};
use std::collections::HashSet;
use std::rc::Rc;

// TODO consider implementing map, filter, fold, reverse and other list methods
// on here directly if they must be implemented in rust. Then core procs can
// just call them. I like doing things in scheme, but using core procs offers
// a lot more flexibility for arity and type checking. We will essentially do
// this for Num and have done it for Char, so why not all the types. Then Val
// just wraps them and other things are a bridge between the Val variants,
// the interpreter, and the base types.

// List Errors ////////////////////////////////////////////////////////////////
#[derive(Debug, PartialEq)]
pub enum ListErr {
    Cyclic,
    Dotted,
    OutOfRange,
    NoZero,
    Immutable,
}

impl std::error::Error for ListErr {}

impl std::fmt::Display for ListErr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", "not implemented")
    }
}

// Cell Value Trait ///////////////////////////////////////////////////////////
pub trait CellValue<T>
where
    T: std::fmt::Debug + Clone,
{
    fn get_cell(&self) -> Option<Rc<Cell<T>>>;
    fn new_pair(cell: Cell<T>) -> T;
}

// Cons Cell //////////////////////////////////////////////////////////////////
#[derive(Clone, PartialEq)]
pub struct Cell<T> {
    mutable: bool,
    head: RefCell<T>,
    tail: RefCell<Option<T>>,
}

impl<T> Cell<T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
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

    pub fn set_head(&self, val: T) -> Result<T, ListErr> {
        if self.mutable {
            Ok(self.head.replace(val))
        } else {
            Err(ListErr::Immutable)
        }
    }

    pub fn set_tail(&self, val: Option<T>) -> Result<Option<T>, ListErr> {
        if self.mutable {
            Ok(self.tail.replace(val))
        } else {
            Err(ListErr::Immutable)
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

    fn id(&self) -> usize {
        self.head.as_ptr() as usize
    }

    fn check_dotted_and_cycle(
        &self,
        checker: &mut CycleChecker,
        cell: Rc<Cell<T>>,
    ) -> Result<(), ListErr> {
        if cell.is_dotted() {
            return Err(ListErr::Dotted);
        } else if checker.insert_and_check(cell.id()) {
            return Err(ListErr::Cyclic);
        } else if let Some(cell) = cell.clone_head().get_cell() {
            if checker.check(cell.id()) {
                return Err(ListErr::Cyclic);
            }
        };
        Ok(())
    }

    // Scheme List Functions //

    pub fn is_list(&self) -> Result<bool, ListErr> {
        let mut checker = CycleChecker::new(self.id());
        for cell in CellIter::new(Rc::new(self.clone())) {
            match self.check_dotted_and_cycle(&mut checker, cell) {
                Err(ListErr::Dotted) => return Ok(false),
                Err(e) => return Err(e),
                _ => (),
            }
        }
        Ok(true)
    }

    pub fn length(&self) -> Result<usize, ListErr> {
        let mut checker = CycleChecker::new(self.id());
        let mut len = 0;
        for cell in CellIter::new(Rc::new(self.clone())) {
            self.check_dotted_and_cycle(&mut checker, cell)?;
            len += 1;
        }
        Ok(len)
    }

    pub fn list_tail(&self, idx: usize) -> Result<Option<T>, ListErr> {
        if idx == 0 {
            return Err(ListErr::NoZero);
        }

        let mut checker = CycleChecker::new(self.id());
        for (i, cell) in CellIter::new(Rc::new(self.clone())).enumerate() {
            self.check_dotted_and_cycle(&mut checker, cell.clone())?;
            if idx == i + 1 {
                return Ok(cell.clone_tail());
            }
        }
        Err(ListErr::OutOfRange)
    }

    pub fn list_ref(&self, idx: usize) -> Result<T, ListErr> {
        let mut checker = CycleChecker::new(self.id());
        for (i, cell) in CellIter::new(Rc::new(self.clone())).enumerate() {
            self.check_dotted_and_cycle(&mut checker, cell.clone())?;
            if idx == i {
                return Ok(cell.clone_head());
            }
        }
        Err(ListErr::OutOfRange)
    }

    // appends the end onto a copy of self
    pub fn append(&self, end: T) -> Result<T, ListErr> {
        let mut checker = CycleChecker::new(self.id());
        let rev = self.reverse()?;
        let mut result = end;

        for cell in CellIter::new(rev.get_cell().expect("should be a cell holding value")) {
            self.check_dotted_and_cycle(&mut checker, cell.clone())?;
            // If this is a mutable list then make the new one mutable
            if self.mutable {
                result = T::new_pair(Cell::new_mut(cell.clone_head(), Some(result)));
            } else {
                result = T::new_pair(Cell::new(cell.clone_head(), Some(result)));
            }
        }
        Ok(result)
    }

    pub fn reverse(&self) -> Result<T, ListErr> {
        if self.is_dotted() {
            return Err(ListErr::Dotted);
        }

        let mut res = T::new_pair(Cell::new(self.clone_head(), None));
        match self.clone_tail() {
            Some(val) => {
                let mut checker = CycleChecker::new(self.id());
                for cell in CellIter::new(val.get_cell().expect("tail should hold a cell")) {
                    self.check_dotted_and_cycle(&mut checker, cell.clone())?;
                    // If this is a mutable list then make the new one mutable
                    if self.mutable {
                        res = T::new_pair(Cell::new_mut(cell.clone_head(), Some(res)));
                    } else {
                        res = T::new_pair(Cell::new(cell.clone_head(), Some(res)));
                    }
                }
                Ok(res)
            }
            None => Ok(res),
        }
    }
}

impl<T> DisplayRep for Cell<T>
where
    T: std::fmt::Debug + Clone + CellValue<T> + DisplayRep,
{
    fn to_display(&self) -> Result<String, Box<dyn std::error::Error>> {
        let mut checker = CycleChecker::new(self.id());
        let mut strings = vec![];
        for cell in CellIter::new(Rc::new(self.clone())) {
            strings.push(cell.clone_head().to_display()?);

            match self.check_dotted_and_cycle(&mut checker, cell.clone()) {
                Err(ListErr::Dotted) => {
                    if let Some(tail) = cell.clone_tail() {
                        strings.push(".".to_owned());
                        strings.push(tail.to_display()?);
                    }
                }
                Err(e) => return Err(e.into()),
                _ => (),
            }
        }
        Ok(format!("({})", strings.join(" ")))
    }
}

impl<T> ExternalRep for Cell<T>
where
    T: std::fmt::Debug + Clone + CellValue<T> + ExternalRep,
{
    fn to_external(&self) -> Result<String, Box<dyn std::error::Error>> {
        let mut checker = CycleChecker::new(self.id());
        let mut strings = vec![];
        for cell in CellIter::new(Rc::new(self.clone())) {
            strings.push(cell.clone_head().to_external()?);

            match self.check_dotted_and_cycle(&mut checker, cell.clone()) {
                Err(ListErr::Dotted) => {
                    if let Some(tail) = cell.clone_tail() {
                        strings.push(".".to_owned());
                        strings.push(tail.to_external()?);
                    }
                }
                Err(e) => return Err(e.into()),
                _ => (),
            }
        }
        Ok(format!("({})", strings.join(" ")))
    }
}

// This is not super helpful, but it helps eliminate cycles causing infinite
// loops when debug printing for now.
impl<T> std::fmt::Debug for Cell<T>
where
    T: std::fmt::Debug + Clone + CellValue<T>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "Cell{{ head: {:?}, tail: {}}}",
            self.head.clone(),
            match self.clone_tail() {
                Some(val) => match val.get_cell() {
                    Some(cell) => cell.id().to_string(),
                    None => "None".to_string(),
                },
                None => "None".to_string(),
            }
        )
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

// Cycle Checker //////////////////////////////////////////////////////////////

struct CycleChecker {
    ids: HashSet<usize>,
}

impl CycleChecker {
    pub fn new(id: usize) -> CycleChecker {
        let mut set = HashSet::new();
        set.insert(id);
        CycleChecker { ids: set }
    }

    pub fn insert_and_check(&mut self, id: usize) -> bool {
        if self.ids.contains(&id) {
            true
        } else {
            self.ids.insert(id);
            false
        }
    }

    pub fn check(&self, id: usize) -> bool {
        self.ids.contains(&id)
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

        fn new_pair(cell: Cell<TestVal>) -> TestVal {
            TestVal::Pair(Rc::new(cell))
        }
    }

    impl ExternalRep for TestVal {
        fn to_external(&self) -> Result<String, Box<dyn std::error::Error>> {
            match self {
                TestVal::Int(i) => Ok(format!("#{}", i)),
                TestVal::Pair(cell) => Ok(format!("{}", cell.to_external()?)),
            }
        }
    }

    impl DisplayRep for TestVal {
        fn to_display(&self) -> Result<String, Box<dyn std::error::Error>> {
            match self {
                TestVal::Int(i) => Ok(format!("{}", i)),
                TestVal::Pair(cell) => Ok(format!("{}", cell.to_display()?)),
            }
        }
    }

    // Helpers //

    fn make_list_5() -> Rc<Cell<TestVal>> {
        let cell = Rc::new(Cell::new_mut(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new_mut(
            TestVal::Int(3),
            Some(TestVal::Pair(cell2.clone())),
        ));
        let cell4 = Rc::new(Cell::new_mut(
            TestVal::Int(2),
            Some(TestVal::Pair(cell3.clone())),
        ));
        Rc::new(Cell::new_mut(
            TestVal::Int(1),
            Some(TestVal::Pair(cell4.clone())),
        ))
    }

    fn make_list_6_dotted() -> Rc<Cell<TestVal>> {
        let cell = Rc::new(Cell::new_mut(TestVal::Int(5), Some(TestVal::Int(6))));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new_mut(
            TestVal::Int(3),
            Some(TestVal::Pair(cell2.clone())),
        ));
        let cell4 = Rc::new(Cell::new_mut(
            TestVal::Int(2),
            Some(TestVal::Pair(cell3.clone())),
        ));
        Rc::new(Cell::new_mut(
            TestVal::Int(1),
            Some(TestVal::Pair(cell4.clone())),
        ))
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
        assert_eq!(cell.set_head(TestVal::Int(56)), Ok(TestVal::Int(5)));
        assert_eq!(cell.set_tail(Some(TestVal::Int(99))), Ok(None));
        assert_eq!(cell.clone_head(), TestVal::Int(56));
        assert_eq!(cell.clone_tail(), Some(TestVal::Int(99)));

        let cell = Cell::new(TestVal::Int(5), None);
        assert_eq!(cell.set_head(TestVal::Int(56)), Err(ListErr::Immutable));
        assert_eq!(
            cell.set_tail(Some(TestVal::Int(99))),
            Err(ListErr::Immutable)
        );
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
        let list = make_list_5();
        let mut iter = CellValueIter::new(list);
        assert_eq!(iter.next(), Some(TestVal::Int(1)));
        assert_eq!(iter.next(), Some(TestVal::Int(2)));
        assert_eq!(iter.next(), Some(TestVal::Int(3)));
        assert_eq!(iter.next(), Some(TestVal::Int(4)));
        assert_eq!(iter.next(), Some(TestVal::Int(5)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_cell_value_iterator_last_is_dotted() {
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

        // Dotted
        let list = make_list_6_dotted();
        assert_eq!(list.clone().reverse(), Err(ListErr::Dotted));

        // Cyclic
        let cell = Rc::new(Cell::new_mut(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell2.clone()))).unwrap();
        assert_eq!(cell2.clone().reverse(), Err(ListErr::Cyclic));
    }

    #[test]
    fn test_cell_list_length() {
        let list = make_list_5();
        assert_eq!(list.length(), Ok(5));

        // Dotted
        let list = make_list_6_dotted();
        assert_eq!(list.length(), Err(ListErr::Dotted));

        // Cyclic
        let cell = Rc::new(Cell::new_mut(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell2.clone()))).unwrap();
        assert_eq!(cell2.length(), Err(ListErr::Cyclic));
    }

    #[test]
    fn test_cell_is_list() {
        let list = make_list_5();
        assert_eq!(list.is_list(), Ok(true));

        // Dotted
        let list = make_list_6_dotted();
        assert_eq!(list.is_list(), Ok(false));

        // Cyclic
        let cell = Rc::new(Cell::new_mut(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell2.clone()))).unwrap();
        assert_eq!(cell2.is_list(), Err(ListErr::Cyclic));
    }

    #[test]
    fn test_cell_list_ref() {
        let list = make_list_5();
        assert_eq!(list.list_ref(0), Ok(TestVal::Int(1)));
        assert_eq!(list.list_ref(1), Ok(TestVal::Int(2)));
        assert_eq!(list.list_ref(2), Ok(TestVal::Int(3)));
        assert_eq!(list.list_ref(3), Ok(TestVal::Int(4)));
        assert_eq!(list.list_ref(4), Ok(TestVal::Int(5)));
        assert_eq!(list.list_ref(5), Err(ListErr::OutOfRange));

        // Dotted
        let list = make_list_6_dotted();
        assert_eq!(list.list_ref(0), Ok(TestVal::Int(1)));
        assert_eq!(list.list_ref(1), Ok(TestVal::Int(2)));
        assert_eq!(list.list_ref(2), Ok(TestVal::Int(3)));
        assert_eq!(list.list_ref(3), Ok(TestVal::Int(4)));
        assert_eq!(list.list_ref(4), Err(ListErr::Dotted));

        // Cyclic
        let cell = Rc::new(Cell::new_mut(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell2.clone()))).unwrap();
        assert_eq!(cell2.list_ref(0), Ok(TestVal::Int(4)));
        assert_eq!(cell2.list_ref(1), Ok(TestVal::Int(5)));
        assert_eq!(cell2.list_ref(2), Err(ListErr::Cyclic));
    }

    #[test]
    fn test_cell_list_tail() {
        let list = make_list_5();
        assert_eq!(list.list_tail(0), Err(ListErr::NoZero));
        assert_eq!(
            list.list_tail(1)
                .unwrap()
                .unwrap()
                .get_cell()
                .unwrap()
                .clone_head(),
            TestVal::Int(2)
        );
        assert_eq!(
            list.list_tail(4)
                .unwrap()
                .unwrap()
                .get_cell()
                .unwrap()
                .clone_head(),
            TestVal::Int(5)
        );
        assert_eq!(list.list_tail(5).unwrap(), None);
        assert_eq!(list.list_tail(6), Err(ListErr::OutOfRange));

        // Dotted
        let list = make_list_6_dotted();
        assert_eq!(list.list_ref(5), Err(ListErr::Dotted));

        // Cyclic
        let cell = Rc::new(Cell::new_mut(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell2.clone()))).unwrap();
        assert_eq!(
            cell2
                .list_tail(1)
                .unwrap()
                .unwrap()
                .get_cell()
                .unwrap()
                .clone_head(),
            TestVal::Int(5)
        );
        assert_eq!(
            cell2
                .list_tail(2)
                .unwrap()
                .unwrap()
                .get_cell()
                .unwrap()
                .clone_head(),
            TestVal::Int(4)
        );
        assert_eq!(cell2.list_tail(3), Err(ListErr::Cyclic));
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
        assert_eq!(list2.list_ref(10), Err(ListErr::OutOfRange));

        // Dotted
        let list = make_list_6_dotted();
        assert_eq!(
            list.append(TestVal::Pair(list.clone())),
            Err(ListErr::Dotted)
        );

        // Cyclic
        let cell = Rc::new(Cell::new_mut(TestVal::Int(5), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(4),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell2.clone()))).unwrap();
        assert_eq!(
            cell2.append(TestVal::Pair(cell2.clone())),
            Err(ListErr::Cyclic)
        );
    }

    #[test]
    fn test_cell_with_alternative_cycles() {
        // Cycle is not with head
        let cell = Rc::new(Cell::new_mut(TestVal::Int(3), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(2),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new_mut(
            TestVal::Int(1),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell2.clone()))).unwrap();
        assert_eq!(cell3.is_list(), Err(ListErr::Cyclic));

        // Cycle is in head not tail
        let cell = Rc::new(Cell::new_mut(TestVal::Int(3), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(2),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new_mut(
            TestVal::Int(1),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_head(TestVal::Pair(cell3.clone()));
        assert_eq!(cell3.is_list(), Err(ListErr::Cyclic));

        // Not a cycle, but the same element is pointed to by two heads.
        let list = make_list_5();
        let cell = Rc::new(Cell::new_mut(TestVal::Pair(list.clone()), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(2),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new_mut(
            TestVal::Pair(list.clone()),
            Some(TestVal::Pair(cell.clone())),
        ));
        assert_eq!(cell3.is_list(), Ok(true));
    }

    #[test]
    fn test_display() {
        let list = make_list_5();
        let list2 = make_list_6_dotted();

        list.set_head(TestVal::Pair(list2));
        assert_eq!(
            list.to_display().unwrap(),
            "((1 2 3 4 5 . 6) 2 3 4 5)".to_owned()
        );

        // Cycle
        let cell = Rc::new(Cell::new_mut(TestVal::Int(3), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(2),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new_mut(
            TestVal::Int(1),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell3.clone()))).unwrap();
        assert!(cell3.to_display().err().unwrap().is::<ListErr>());
    }

    #[test]
    fn test_external() {
        let list = make_list_5();
        let list2 = make_list_6_dotted();

        list.set_head(TestVal::Pair(list2));
        assert_eq!(
            list.to_external().unwrap(),
            "((#1 #2 #3 #4 #5 . #6) #2 #3 #4 #5)".to_owned()
        );

        // Cycle
        let cell = Rc::new(Cell::new_mut(TestVal::Int(3), None));
        let cell2 = Rc::new(Cell::new_mut(
            TestVal::Int(2),
            Some(TestVal::Pair(cell.clone())),
        ));
        let cell3 = Rc::new(Cell::new_mut(
            TestVal::Int(1),
            Some(TestVal::Pair(cell.clone())),
        ));
        cell.set_tail(Some(TestVal::Pair(cell3.clone()))).unwrap();
        assert!(cell3.to_external().err().unwrap().is::<ListErr>());
    }
}
