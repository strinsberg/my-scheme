use crate::builtin::ALL_BUILTINS;
use crate::error::{ScmErr, ScmResult, ValResult};
use crate::types::{Env, ScmVal};
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// TODO test errors for using bad keys

// Implementation and testing for Env only to avoid circular includes/moving
// more stuff into types module since Env needs ScmVal and ScmErr which are in
// different places. Same reason why the Env struct cannot be put in here with
// the implementation.
//
// The big win moving this here is that it allows putting the unit tests in here
// where they are uncluttered by other type testing.

impl Env {
    pub fn new() -> Env {
        Env {
            scope: RefCell::new(HashMap::new()),
            next: None,
        }
    }

    pub fn new_with_bindings(bindings: &[(ScmVal, ScmVal)]) -> ScmResult<Env> {
        let env = Env::new();
        env.insert_all(&bindings)?;
        Ok(env)
    }

    pub fn add_scope(env: Rc<Env>) -> Rc<Env> {
        let new_env = Env {
            scope: RefCell::new(HashMap::new()),
            next: Some(env),
        };
        Rc::new(new_env)
    }

    // Binds a list of symbols to a list of values in a new scope
    pub fn bind_in_new_env(env: Rc<Env>, params: &[ScmVal], args: &[ScmVal]) -> ScmResult<Rc<Env>> {
        let new_env = Env::add_scope(env);
        if params.len() > args.len() {
            // TODO this is not very helpful
            return Err(ScmErr::Arity("bindings".to_owned(), params.len()));
        }
        for i in 0..params.len() {
            new_env.insert(params[i].clone(), args[i].clone())?;
        }
        Ok(new_env)
    }

    // Returns the value for the first time the key is found in any scope.
    pub fn lookup(&self, key: &ScmVal) -> Option<ScmVal> {
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
    pub fn insert(&self, key: ScmVal, val: ScmVal) -> ValResult {
        match key {
            ScmVal::NewSymbol(_) => {
                self.scope.borrow_mut().insert(key, val);
                Ok(ScmVal::Empty)
            }
            _ => Err(ScmErr::BadBinding(key)),
        }
    }

    // Inserts a vector of key value pairs as bindings in the top scope.
    pub fn insert_all(&self, pairs: &[(ScmVal, ScmVal)]) -> ValResult {
        for (key, val) in pairs.iter() {
            self.insert(key.clone(), val.clone())?;
        }
        Ok(ScmVal::Empty)
    }

    // Sets a new value for the first key that matches in any scope.
    // Returns Some(ScmVal::Empty) when the value was set, None if it was not found.
    pub fn set(&self, key: ScmVal, val: ScmVal) -> ValResult {
        let contains = self.scope.borrow().contains_key(&key);
        match contains {
            true => self.insert(key, val),
            false => match self.next {
                Some(ref next) => next.set(key, val),
                None => Err(ScmErr::Undeclared(key.to_string())),
            },
        }
    }

    // Premade Environment Creation //

    pub fn new_null() -> ScmVal {
        ScmVal::Env(Env::new_null_rc())
    }

    pub fn new_null_rc() -> Rc<Env> {
        Env::new_builtin_env()
    }

    // Uses the builtin list to create pairs with symbols and core procs that
    // can be added to the env
    pub fn new_builtin_env() -> Rc<Env> {
        let pairs: Vec<(ScmVal, ScmVal)> = ALL_BUILTINS
            .iter()
            .map(|(b, n)| {
                (
                    ScmVal::new_sym(&format!("{b}")),
                    ScmVal::Core(b.clone(), *n),
                )
            })
            .collect();
        Rc::new(Env::new_with_bindings(&pairs).expect("builtin env should not have any bad keys"))
    }
}

impl Hash for Env {
    fn hash<H: Hasher>(&self, _: &mut H) {
        panic!("Env cannot be hashed");
    }
}

// Tests //////////////////////////////////////////////////////////////////////
#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefMut;

    #[test]
    fn test_construct() {
        let env = Env::new();
        assert_eq!(env.lookup(&ScmVal::new_sym("hello")), None);
    }

    #[test]
    fn test_initialize_with_top_level_bindings() {
        let env = Env::new_with_bindings(&[
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        assert_eq!(
            env.lookup(&ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            env.lookup(&ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            env.lookup(&ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
    }

    #[test]
    fn test_insert_top_level_bindings_one_at_a_time() {
        let env = Env::new();
        env.insert(ScmVal::new_sym("hello"), ScmVal::new_int(22))
            .unwrap();
        env.insert(ScmVal::new_sym("world"), ScmVal::new_int(99))
            .unwrap();
        env.insert(ScmVal::new_sym("steve"), ScmVal::new_int(345))
            .unwrap();
        assert_eq!(
            env.lookup(&ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            env.lookup(&ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            env.lookup(&ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
    }

    #[test]
    fn test_insert_a_vec_of_bindings() {
        let mut env = Env::new();
        env.insert_all(&[
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        assert_eq!(
            env.lookup(&ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            env.lookup(&ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            env.lookup(&ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
    }

    #[test]
    fn test_set_top_level_bindings() {
        let env = Env::new_with_bindings(&[
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        env.set(ScmVal::new_sym("hello"), ScmVal::new_int(0))
            .unwrap();
        env.set(ScmVal::new_sym("world"), ScmVal::new_int(100))
            .unwrap();
        env.set(ScmVal::new_sym("steve"), ScmVal::new_int(987))
            .unwrap();
        assert_eq!(
            env.lookup(&ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(0))
        );
        assert_eq!(
            env.lookup(&ScmVal::new_sym("world")),
            Some(ScmVal::new_int(100))
        );
        assert_eq!(
            env.lookup(&ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(987))
        );
    }

    #[test]
    fn test_add_scope_and_insert() {
        let env = Env::new_with_bindings(&[
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        let env_rc = Rc::new(env); // moved env
        let new_env_rc = Env::add_scope(env_rc);
        new_env_rc
            .insert_all(&[
                (ScmVal::new_sym("student"), ScmVal::new_int(45)),
                (ScmVal::new_sym("wants"), ScmVal::new_int(87)),
                (ScmVal::new_sym("cake"), ScmVal::new_int(31)),
            ])
            .unwrap();
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("student")),
            Some(ScmVal::new_int(45))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("wants")),
            Some(ScmVal::new_int(87))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("cake")),
            Some(ScmVal::new_int(31))
        );
    }

    #[test]
    fn test_add_scope_and_shadow_and_set() {
        let env = Env::new_with_bindings(&[
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        let env_rc = Rc::new(env); // moved env
        let new_env_rc = Env::add_scope(env_rc);
        new_env_rc
            .insert_all(&[
                (ScmVal::new_sym("hello"), ScmVal::new_int(555)), // shadow hello
                (ScmVal::new_sym("student"), ScmVal::new_int(45)),
                (ScmVal::new_sym("wants"), ScmVal::new_int(87)),
                (ScmVal::new_sym("cake"), ScmVal::new_int(31)),
            ])
            .unwrap();
        let good = new_env_rc.set(ScmVal::new_sym("world"), ScmVal::new_float(0.234));
        let bad = new_env_rc.set(ScmVal::new_sym("not-here"), ScmVal::new_float(0.234));
        assert_eq!(good, Ok(ScmVal::Empty));
        assert_eq!(bad, Err(ScmErr::Undeclared("not-here".to_string())));

        // Check all through the top env
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(555))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("world")),
            Some(ScmVal::new_float(0.234))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("student")),
            Some(ScmVal::new_int(45))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("wants")),
            Some(ScmVal::new_int(87))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("cake")),
            Some(ScmVal::new_int(31))
        );
        // Check shadow and set in base env
        let env_rc = new_env_rc.next.clone().unwrap();
        assert_eq!(
            env_rc.lookup(&ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            env_rc.lookup(&ScmVal::new_sym("world")),
            Some(ScmVal::new_float(0.234))
        );
    }

    #[test]
    fn test_add_scope_and_insert_using_bind_new_env() {
        let env = Env::new_with_bindings(&[
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        let new_env_rc = Env::bind_in_new_env(
            Rc::new(env), // moved env
            &[
                ScmVal::new_sym("student"),
                ScmVal::new_sym("wants"),
                ScmVal::new_sym("cake"),
            ],
            &[
                ScmVal::new_int(45),
                ScmVal::new_int(87),
                ScmVal::new_int(31),
            ],
        )
        .unwrap();

        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("student")),
            Some(ScmVal::new_int(45))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("wants")),
            Some(ScmVal::new_int(87))
        );
        assert_eq!(
            new_env_rc.lookup(&ScmVal::new_sym("cake")),
            Some(ScmVal::new_int(31))
        );
    }
}
