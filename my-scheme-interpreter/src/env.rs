use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

// TODO implement Display, ExternalRep, etc.
// TODO put the scope insid an Rc as well and remove the rc from the enum.

// Environment as Linked List /////////////////////////////////////////////////

#[derive(Debug, PartialEq)]
pub struct Env<K, V>
where
    K: Clone + PartialEq + Eq + Hash,
    V: Clone,
{
    scope: RefCell<HashMap<K, V>>,
    next: Option<Rc<Env<K, V>>>,
}

impl<K, V> Env<K, V>
where
    K: Clone + PartialEq + Eq + Hash,
    V: Clone,
{
    pub fn new() -> Env<K, V> {
        Env::default()
    }

    pub fn add_scope(env: Rc<Env<K, V>>) -> Rc<Env<K, V>> {
        let new_env = Env {
            scope: RefCell::new(HashMap::new()),
            next: Some(env),
        };
        Rc::new(new_env)
    }

    // Returns the value for the first time the key is found in any scope.
    pub fn lookup(&self, key: &K) -> Option<V> {
        match self.scope.borrow().get(&key) {
            Some(val) => Some(val.clone()),
            None => match self.next {
                Some(ref next) => next.lookup(key),
                None => None,
            },
        }
    }

    // Inserts a binding into the top scope of the environment.
    // If a key exists in the top scope already it will be rebound.
    pub fn insert(&self, key: K, val: V) {
        self.scope.borrow_mut().insert(key, val);
    }

    // Inserts a vector of key value pairs as bindings in the top scope.
    pub fn insert_all(&self, pairs: &[(K, V)]) {
        for (key, val) in pairs.iter() {
            self.insert(key.clone(), val.clone());
        }
    }

    // Sets a new value for the first key that matches in any scope.
    // True means the key was present and the value was set, false means no key
    pub fn set(&self, key: K, val: V) -> bool {
        let contains = self.scope.borrow().contains_key(&key);
        match contains {
            true => {
                self.insert(key, val);
                true
            }
            false => match self.next {
                Some(ref next) => next.set(key, val),
                None => false,
            },
        }
    }
}

impl<K, V> Default for Env<K, V>
where
    K: Clone + PartialEq + Eq + Hash,
    V: Clone,
{
    fn default() -> Env<K, V> {
        Env {
            scope: RefCell::new(HashMap::new()),
            next: None,
        }
    }
}

impl<K, V, const N: usize> From<[(K, V); N]> for Env<K, V>
where
    K: Clone + PartialEq + Eq + Hash,
    V: Clone,
{
    fn from(pairs: [(K, V); N]) -> Env<K, V> {
        let env = Env::new();
        env.insert_all(&pairs);
        env
    }
}

// Tests //////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_construct_env() {
        let env: Env<i64, i64> = Env::new();
        assert_eq!(env.lookup(&99), None);
    }

    #[test]
    fn test_initialize_env_with_top_level_bindings() {
        let env = Env::from([('a', 22), ('b', 99), ('c', 345)]);
        assert_eq!(env.lookup(&'a'), Some(22));
        assert_eq!(env.lookup(&'b'), Some(99));
        assert_eq!(env.lookup(&'c'), Some(345));
    }

    #[test]
    fn test_insert_top_level_bindings_one_at_a_time() {
        let env = Env::new();
        env.insert('a', 22);
        env.insert('b', 99);
        env.insert('c', 345);
        assert_eq!(env.lookup(&'a'), Some(22));
        assert_eq!(env.lookup(&'b'), Some(99));
        assert_eq!(env.lookup(&'c'), Some(345));
    }

    #[test]
    fn test_insert_a_vec_of_bindings() {
        let env = Env::new();
        env.insert_all(&[('a', 22), ('b', 99), ('c', 345)]);
        assert_eq!(env.lookup(&'a'), Some(22));
        assert_eq!(env.lookup(&'b'), Some(99));
        assert_eq!(env.lookup(&'c'), Some(345));
    }

    #[test]
    fn test_set_top_level_bindings() {
        let env = Env::from([('a', 22), ('b', 99), ('c', 345)]);
        assert!(env.set('a', 33));
        assert!(env.set('b', 88));
        assert!(env.set('c', 12));
        assert_eq!(env.set('x', 12), false);
        assert_eq!(env.lookup(&'a'), Some(33));
        assert_eq!(env.lookup(&'b'), Some(88));
        assert_eq!(env.lookup(&'c'), Some(12));
    }

    #[test]
    fn test_add_scope_insert_and_set() {
        let env = Rc::new(Env::from([('a', 22), ('b', 99), ('c', 345)]));
        let new_env = Env::add_scope(env.clone());
        new_env.insert_all(&[('d', 45), ('e', 87), ('f', 31)]);
        assert_eq!(new_env.lookup(&'a'), Some(22));
        assert_eq!(new_env.lookup(&'b'), Some(99));
        assert_eq!(new_env.lookup(&'c'), Some(345));
        assert_eq!(new_env.lookup(&'d'), Some(45));
        assert_eq!(new_env.lookup(&'e'), Some(87));
        assert_eq!(new_env.lookup(&'f'), Some(31));
        // set values in top and bottom env, but all through the top
        assert!(new_env.set('a', 4));
        assert!(new_env.set('f', 94));
        assert_eq!(new_env.lookup(&'a'), Some(4));
        assert_eq!(new_env.lookup(&'f'), Some(94));
    }

    #[test]
    fn test_add_scope_and_shadow() {
        let env = Rc::new(Env::from([('a', 22), ('b', 99), ('c', 345)]));
        let new_env = Env::add_scope(env.clone());
        new_env.insert_all(&[('a', 56), ('b', 900), ('d', 45), ('e', 87), ('f', 31)]);
        assert_eq!(new_env.lookup(&'a'), Some(56));
        assert_eq!(new_env.lookup(&'b'), Some(900));
        assert_eq!(new_env.lookup(&'c'), Some(345));
        assert_eq!(new_env.lookup(&'d'), Some(45));
        assert_eq!(new_env.lookup(&'e'), Some(87));
        assert_eq!(new_env.lookup(&'f'), Some(31));
        // from bottom level
        assert_eq!(env.lookup(&'a'), Some(22));
        assert_eq!(env.lookup(&'b'), Some(99));
        assert_eq!(env.lookup(&'c'), Some(345));
    }
}
