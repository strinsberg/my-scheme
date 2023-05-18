use crate::err::Error;
use crate::rep::{DisplayRep, ExternalRep};
use std::rc::Rc;

// TODO this is just an idea at this point. What is here needs to be tested.
// The major issue is that to make it more efficient in memory we need to be
// able to only copy the tail when there are multiple references to the vector.
// The difficulty is that while it is passed around the interpreter multiple
// copies will be make of the Rc even if there is only one in the environment.
// To make it efficient in speed it needs to have larger N, but this would make
// it even more innefficient memory wise. We could adjust it so that while the
// vector is smaller than say 32 we use a dynamicly sized vector for the tail.
// Once we get to 32, or grow the capacity that big, we can treat it like a
// proper tail. That would help with copies and memory before it grows.

// Nodes //////////////////////////////////////////////////////////////////////

const N: usize = 4;
const SHIFT: usize = 2;
const MASK: usize = usize::MAX << 2;

#[derive(Debug, Clone, PartialEq)]
enum VecNode<T> {
    Inner(Rc<Vec<Option<VecNode<T>>>>),
    Outer(Rc<Vec<Option<T>>>),
}

impl<T> VecNode<T>
where
    T: Clone,
{
    fn is_full(&self) -> bool {
        match self {
            VecNode::Inner(vec) => !vec[vec.len() - 1].is_none(),
            VecNode::Outer(vec) => !vec[vec.len() - 1].is_none(),
        }
    }

    fn get_right_open_inner(&self) -> Option<&VecNode<T>> {
        match self {
            VecNode::Inner(vec) => {
                for node in vec.iter().rev() {
                    if let Some(n) = node {
                        return Some(n);
                    }
                }
                None
            }
            VecNode::Outer(vec) => None,
        }
    }

    fn get_rightmost_open_inner(&self, depth: &mut usize) -> Option<&VecNode<T>> {
        let right = self.get_right_open_inner()?;
        match right {
            VecNode::Inner(_) => {
                *depth += 1;
                right.get_rightmost_open_inner(depth)
            }
            _ => None,
        }
    }

    // TODO this should modify instead of copying
    fn set_right_inner(&self, val: VecNode<T>) -> Option<VecNode<T>> {
        match self {
            VecNode::Inner(vec) => {
                let new_vec = vec![None; N];
                for node in vec.iter() {
                    match node {
                        Some(_) => new_vec.push(node.clone()),
                        None => {
                            new_vec.push(Some(val));
                            break;
                        }
                    }
                }
                Some(VecNode::Inner(Rc::new(new_vec)))
            }
            VecNode::Outer(vec) => None,
        }
    }
}

// Vector Type ////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct Vector<T> {
    length: usize,
    root: Option<VecNode<T>>,
    tail: Vec<Option<T>>,
}

impl<T> Vector<T>
where
    T: Clone,
{
    pub fn new() -> Vector<T> {
        Vector::default()
    }

    pub fn get(&self, idx: usize) -> Result<&T, Error> {
        if idx >= self.length {
            Err(Error::OutOfRange)
        } else if self.length < N {
            match self.tail[idx] {
                Some(v) => Ok(&v),
                None => panic!("tail[idx] should not be None"),
            }
        } else {
            match self.root {
                Some(VecNode::Inner(root)) => {
                    let mut loc = idx & MASK;
                    let mut idx = idx >> 2;
                    while let Some(mut node) = root[idx] {
                        loc = idx & MASK;
                        idx >>= 2;
                        match node {
                            VecNode::Inner(nd) => match nd[idx] {
                                Some(n) => node = n,
                                None => panic!("inner node should not be None"),
                            },
                            VecNode::Outer(vec) => match vec[idx] {
                                Some(val) => return Ok(&val),
                                None => panic!("outer node should not be None"),
                            },
                        }
                    }
                }
                None => panic!("root should not be None"),
            };
            panic!("if index is in bounds item should exist")
        }
    }

    pub fn conj(&self, val: T) -> Vector<T> {
        // If the tail is full
        if self.length % N == 0 {
            let mut depth = 0;
            match self.root {
                Some(root) => match root.get_rightmost_open_inner(&mut depth) {
                    Some(_) => Vector {
                        length: self.length + 1,
                        root: Some(Vector::insert_tail(&root, self.tail.clone())),
                        tail: Vec::new(),
                    },
                    None => {
                        // New root for us that points to the old root with it's first
                        // element, second element chains inners down to the current
                        // tail. So we need to generate new nodes based on the depth.
                        Vector {
                            length: self.length + 1,
                            root: Some(Vector::generate_for_new_root(
                                root,
                                self.tail.clone(),
                                depth,
                            )),
                            tail: Vec::new(),
                        }
                    }
                },
                None => {
                    let vec = vec![None; N];
                    vec[0] = Some(VecNode::Outer(Rc::new(self.tail.clone())));
                    Vector {
                        length: self.length + 1,
                        root: Some(VecNode::Inner(Rc::new(vec))),
                        tail: Vec::new(),
                    }
                }
            }

        // The tail is not full
        } else {
            let new_tail = self.tail.clone();
            new_tail[self.length % N] = Some(val);
            Vector {
                length: self.length + 1,
                root: self.root.clone(),
                tail: new_tail,
            }
        }
    }

    fn insert_tail(root: &VecNode<T>, outer: Vec<Option<T>>) -> VecNode<T> {
        let mut node = root;
        let mut stack = Vec::new();
        let mut generate = false;
        let mut level = 0;

        loop {
            let current = if generate {
                VecNode::Inner(Rc::new(Vec::new()))
            } else if node.is_full() {
                node.clone()
            } else {
                generate = true;
                node.clone()
            };

            stack.push(current);
            node = match node.get_rightmost_open_inner(&mut level) {
                Some(node) => node,
                None => break,
            };
        }

        // Chain the nodes down to the inserted node
        let mut new_node = VecNode::Outer(Rc::new(outer));
        for node in stack.iter() {
            new_node = match node.set_right_inner(new_node) {
                Some(node) => node,
                None => panic!("should only find inner nodes in stack"),
            }
        }

        new_node
    }

    fn generate_for_new_root(
        old_root: VecNode<T>,
        tail: Vec<Option<T>>,
        depth: usize,
    ) -> VecNode<T> {
        let mut node = VecNode::Outer(Rc::new(tail));
        for i in 0..depth {
            let vec = vec![None; N];
            vec[0] = Some(node);
            node = VecNode::Inner(Rc::new(vec))
        }
        let vec = vec![None; N];
        vec[0] = Some(old_root);
        VecNode::Inner(Rc::new(vec))
    }
}

// Traits /////////////////////////////////////////////////////////////////////

impl<T> Default for Vector<T>
where
    T: Clone,
{
    fn default() -> Vector<T> {
        Vector {
            length: 0,
            root: None,
            tail: vec![None; N],
        }
    }
}
