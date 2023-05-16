use std::cell::RefCell;
use std::rc::Rc;

// Cell Value Trait ////////////////////////////////////////////////////////////
pub trait CellValue<T>
where
    T: Clone,
{
    fn get_cell(&self) -> Option<Rc<Cell<T>>>;
}

// Cons Cell //////////////////////////////////////////////////////////////////
#[derive(Debug, Clone)]
pub struct Cell<T> {
    pub mutable: bool,
    pub head: RefCell<T>,
    pub tail: RefCell<Option<T>>,
}

impl<T> Cell<T>
where
    T: Clone + CellValue<T>,
{
    pub fn new(head: T, tail: T) -> Cell<T> {
        Cell {
            mutable: false,
            head: RefCell::new(head),
            tail: RefCell::new(Some(tail)),
        }
    }

    pub fn new_mut(head: T, tail: T) -> Cell<T> {
        Cell {
            mutable: true,
            head: RefCell::new(head),
            tail: RefCell::new(Some(tail)),
        }
    }

    pub fn set_head(&self, val: T) -> Option<T> {
        if self.mutable {
            Some(self.head.replace(val))
        } else {
            None
        }
    }

    pub fn set_tail(&self, val: T) -> Option<T> {
        if self.mutable {
            self.tail.replace(Some(val))
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
            Some(cell) => match cell.clone_tail() {
                Some(val) => {
                    let current = Some(cell);
                    self.cell = val.get_cell();
                    current
                }
                None => None,
            },
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
