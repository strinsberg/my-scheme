use crate::scm_types::error::{ScmErr, ValResult};
use crate::scm_types::scm_val::{ConsCell, Pointer, ScmVal};
use std::collections::HashMap;

// TODO garbage collection
// TODO test panics
//
// NOTE panics are because in all cases where this is used the expectation is that
// pointers were aquired from the heap and given to the user. They are not able
// to access raw pointers or the heap directly, so if a pointer is invalid it is
// my programming error and is unrecoverable.

pub struct Heap {
    current_ptr: Pointer,
    cells: HashMap<Pointer, ConsCell>,
}

impl Heap {
    pub fn new() -> Heap {
        Heap {
            current_ptr: 0,
            cells: HashMap::new(),
        }
    }

    pub fn has_ptr(&self, ptr: Pointer) -> bool {
        self.cells.contains_key(&ptr)
    }

    pub fn get_cell(&self, ptr: Pointer) -> ConsCell {
        self.cells
            .get(&ptr)
            .expect("heap pointer should be valid")
            .clone()
    }

    pub fn get_cell_mut(&mut self, ptr: Pointer) -> &mut ConsCell {
        self.cells
            .get_mut(&ptr)
            .expect("heap pointer should be valid")
    }

    pub fn next_ptr(&mut self) {
        let ptr = self.current_ptr;
        while self.has_ptr(self.current_ptr) {
            self.current_ptr = self.current_ptr.wrapping_add(1);
            // Check if we went all the way around
            //
            // TODO call garbage collection, maybe use size of the heap as we
            // will want to check after GC that we still have memory, or we
            // need to loop again to find the next ptr and if we fail again then
            // we are oom. But probably just giving a max size for the heap
            // and checking that, not capacity of course.
            if self.current_ptr == ptr {
                // Branch is untested because making this happen is a little tricky
                panic!("heap out of memory");
            }
        }
    }

    pub fn cons(&mut self, first: ScmVal, second: ScmVal) -> Pointer {
        let cell = ConsCell::new(first, second);
        let key = self.current_ptr;
        self.cells.insert(key, cell);
        self.next_ptr();
        key
    }

    pub fn car(&self, ptr: Pointer) -> ScmVal {
        self.get_cell(ptr).head
    }

    pub fn cdr(&self, ptr: Pointer) -> ScmVal {
        self.get_cell(ptr).tail
    }

    pub fn set_car(&mut self, ptr: Pointer, val: ScmVal) {
        self.get_cell_mut(ptr).head = val;
    }

    pub fn set_cdr(&mut self, ptr: Pointer, val: ScmVal) {
        self.get_cell_mut(ptr).tail = val;
    }

    pub fn nth(&self, ptr: Pointer, idx: usize) -> ValResult {
        let (i, ptr) = self.get_nth_ptr(ptr, idx);
        let cell = self.get_cell(ptr);
        if i == idx {
            Ok(cell.head)
        } else if i == idx - 1 && cell.is_dotted() {
            Ok(cell.tail)
        } else if cell.is_dotted() {
            Err(ScmErr::OutOfBounds(i + 2, idx))
        } else {
            Err(ScmErr::OutOfBounds(i + 1, idx))
        }
    }

    pub fn set_nth(&mut self, ptr: Pointer, idx: usize, val: ScmVal) -> ValResult {
        let (i, p) = self.get_nth_ptr(ptr, idx);
        let cell = self.get_cell_mut(p);
        if i == idx {
            cell.head = val;
            Ok(ScmVal::Empty)
        } else if i == idx - 1 && cell.is_dotted() {
            cell.tail = val;
            Ok(ScmVal::Empty)
        } else if cell.is_dotted() {
            // can't get this branch to run in tests, almost not sure it is a
            // possible situation, but it runs in the get nth and is the same here
            // so it should work if the situation is possible.
            Err(ScmErr::OutOfBounds(i + 2, idx))
        } else {
            Err(ScmErr::OutOfBounds(i + 1, idx))
        }
    }

    fn get_nth_ptr(&self, ptr: Pointer, idx: usize) -> (usize, Pointer) {
        let mut i = 0;
        let mut p = ptr;

        while i < idx {
            let cell = self.get_cell(p);
            match cell.tail {
                ScmVal::Pair(ptr) => p = ptr,
                _ => break,
            }

            i += 1;
        }

        (i, p)
    }
}

// Iterators //////////////////////////////////////////////////////////////////

// iterate over pointers //

pub struct ListPtrIter<'a> {
    done: bool,
    ptr: Pointer,
    heap: &'a Heap,
}

impl<'a> ListPtrIter<'a> {
    pub fn new(ptr: Pointer, heap: &'a Heap) -> ListPtrIter {
        ListPtrIter {
            done: false,
            ptr: ptr,
            heap: heap,
        }
    }
}

impl<'a> Iterator for ListPtrIter<'a> {
    type Item = Pointer;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let ptr = self.ptr;
        let cell = self.heap.get_cell(ptr);
        match cell.tail {
            ScmVal::Pair(p) => self.ptr = p,
            _ => self.done = true,
        }
        Some(ptr)
    }
}

// iterate over values //

pub struct ListValIter<'a> {
    done: bool,
    dotted: bool,
    ptr: Pointer,
    heap: &'a Heap,
}

impl<'a> ListValIter<'a> {
    pub fn new(ptr: Pointer, heap: &'a Heap) -> ListValIter {
        ListValIter {
            done: false,
            dotted: false,
            ptr: ptr,
            heap: heap,
        }
    }
}

impl<'a> Iterator for ListValIter<'a> {
    type Item = ScmVal;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        if self.dotted {
            self.done = true;
            return Some(self.heap.cdr(self.ptr));
        }

        let ptr = self.ptr;
        let cell = self.heap.get_cell(ptr);
        match cell.tail {
            ScmVal::Pair(p) => self.ptr = p,
            _ if cell.is_dotted() => self.dotted = true,
            _ => self.done = true,
        }
        Some(cell.head)
    }
}

// Tests //////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scm_types::builtin::Builtin;
    use crate::scm_types::number::ScmNumber;

    pub fn int(i: i64) -> ScmVal {
        ScmVal::Number(ScmNumber::Integer(i))
    }

    #[test]
    fn test_inserting_a_cell_into_the_heap() {
        let mut h = Heap::new();
        assert_eq!(h.cons(int(33), ScmVal::Empty), 0);
        assert_eq!(h.has_ptr(0), true);
        let cell = h.get_cell(0);
        assert_eq!(cell.head, int(33));
        assert_eq!(cell.tail, ScmVal::Empty);
    }

    #[test]
    fn test_using_car_and_cdr() {
        let mut h = Heap::new();
        assert_eq!(h.cons(int(33), ScmVal::Empty), 0);
        assert_eq!(h.car(0), int(33));
        assert_eq!(h.cdr(0), ScmVal::Empty);
    }

    #[test]
    fn test_setting_car_and_cdr() {
        let mut h = Heap::new();
        assert_eq!(h.cons(int(55), ScmVal::Empty), 0);
        h.set_car(0, int(44));
        h.set_cdr(0, int(10));
        assert_eq!(h.car(0), int(44));
        assert_eq!(h.cdr(0), int(10));
    }

    #[test]
    fn test_getting_the_nth_val_from_a_list() {
        let mut h = Heap::new();
        assert_eq!(h.cons(int(22), ScmVal::Empty), 0);
        assert_eq!(h.cons(int(77), ScmVal::Pair(0)), 1);
        assert_eq!(h.nth(1, 0), Ok(int(77)));
        assert_eq!(h.nth(1, 1), Ok(int(22)));
        assert_eq!(h.nth(1, 2), Err(ScmErr::OutOfBounds(2, 2)));
    }

    #[test]
    fn test_getting_the_nth_val_from_a_dotted_list() {
        let mut h = Heap::new();
        assert_eq!(h.cons(int(88), int(77)), 0);
        assert_eq!(h.cons(int(99), ScmVal::Pair(0)), 1);
        assert_eq!(h.nth(1, 0), Ok(int(99)));
        assert_eq!(h.nth(1, 1), Ok(int(88)));
        assert_eq!(h.nth(1, 2), Ok(int(77)));
        assert_eq!(h.nth(1, 3), Err(ScmErr::OutOfBounds(3, 3)));
    }

    #[test]
    fn test_setting_the_nth_val_in_a_list() {
        let mut h = Heap::new();
        assert_eq!(h.cons(int(88), ScmVal::Empty), 0);
        assert_eq!(h.cons(int(99), ScmVal::Pair(0)), 1);
        assert_eq!(h.set_nth(1, 0, int(22)), Ok(ScmVal::Empty));
        assert_eq!(h.set_nth(1, 1, int(33)), Ok(ScmVal::Empty));
        assert_eq!(h.set_nth(1, 2, int(2000)), Err(ScmErr::OutOfBounds(2, 2)));
        assert_eq!(h.nth(1, 0), Ok(int(22)));
        assert_eq!(h.nth(1, 1), Ok(int(33)));
        assert_eq!(h.nth(1, 2), Err(ScmErr::OutOfBounds(2, 2)));
    }

    #[test]
    fn test_setting_the_nth_val_in_a_dotted_list() {
        let mut h = Heap::new();
        assert_eq!(
            h.cons(ScmVal::Boolean(true), ScmVal::Core(Builtin::Cons)),
            0
        );
        assert_eq!(h.cons(ScmVal::Boolean(false), ScmVal::Pair(0)), 1);
        h.set_nth(1, 0, ScmVal::Pair(22));
        h.set_nth(1, 1, ScmVal::Pair(33));
        h.set_nth(1, 2, ScmVal::Boolean(true));
        assert_eq!(h.nth(1, 0), Ok(ScmVal::Pair(22)));
        assert_eq!(h.nth(1, 1), Ok(ScmVal::Pair(33)));
        assert_eq!(h.nth(1, 2), Ok(ScmVal::Boolean(true)));
        assert_eq!(h.nth(1, 3), Err(ScmErr::OutOfBounds(3, 3)));
    }

    // Iterators //

    #[test]
    fn test_iterating_pointers_of_a_list() {
        let mut h = Heap::new();
        assert_eq!(h.cons(ScmVal::Boolean(true), ScmVal::Empty), 0);
        assert_eq!(h.cons(ScmVal::Boolean(false), ScmVal::Pair(0)), 1);
        assert_eq!(h.cons(ScmVal::Boolean(true), ScmVal::Pair(1)), 2);
        let mut iter = ListPtrIter::new(2, &mut h);
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_iterating_values_of_a_list() {
        let mut h = Heap::new();
        assert_eq!(h.cons(ScmVal::Boolean(true), ScmVal::Empty), 0);
        assert_eq!(h.cons(ScmVal::Boolean(false), ScmVal::Pair(0)), 1);
        assert_eq!(h.cons(int(22), ScmVal::Pair(1)), 2);
        let mut iter = ListValIter::new(2, &mut h);
        assert_eq!(iter.next(), Some(int(22)));
        assert_eq!(iter.next(), Some(ScmVal::Boolean(false)));
        assert_eq!(iter.next(), Some(ScmVal::Boolean(true)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_iterating_pointers_of_a_dotted_list() {
        let mut h = Heap::new();
        assert_eq!(h.cons(ScmVal::Boolean(true), int(22)), 0);
        assert_eq!(h.cons(ScmVal::Boolean(false), ScmVal::Pair(0)), 1);
        assert_eq!(h.cons(ScmVal::Boolean(true), ScmVal::Pair(1)), 2);
        let mut iter = ListPtrIter::new(2, &mut h);
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_iterating_values_of_a_dotted_list() {
        let mut h = Heap::new();
        assert_eq!(h.cons(ScmVal::Boolean(true), int(33)), 0);
        assert_eq!(h.cons(ScmVal::Boolean(false), ScmVal::Pair(0)), 1);
        assert_eq!(h.cons(int(44), ScmVal::Pair(1)), 2);
        let mut iter = ListValIter::new(2, &mut h);
        assert_eq!(iter.next(), Some(int(44)));
        assert_eq!(iter.next(), Some(ScmVal::Boolean(false)));
        assert_eq!(iter.next(), Some(ScmVal::Boolean(true)));
        assert_eq!(iter.next(), Some(int(33)));
        assert_eq!(iter.next(), None);
    }

    // Panics //
    #[test]
    #[should_panic]
    fn test_get_cell_with_invalid_ptr() {
        let mut h = Heap::new();
        let cell = h.get_cell(99);
    }

    #[test]
    #[should_panic]
    fn test_get_cell_mut_with_invalid_ptr() {
        let mut h = Heap::new();
        let cell = h.get_cell_mut(33);
    }
}
