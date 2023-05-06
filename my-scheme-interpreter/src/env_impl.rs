use crate::error::{ScmErr, ScmResult, ValResult};
use crate::types::{Builtin, Env, Map, ScmVal};
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::iter::zip;
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
            scope: Map::default(),
            next: None,
        }
    }

    pub fn new_with_bindings(bindings: Vec<(ScmVal, ScmVal)>) -> ScmResult<Env> {
        let mut env = Env::new();
        env.insert_all(bindings)?;
        Ok(env)
    }

    pub fn add_scope(env: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        let new_env = Env {
            scope: Map::default(),
            next: Some(env),
        };
        Rc::new(RefCell::new(new_env))
    }

    // Binds a list of symbols to a list of values in a new scope
    pub fn bind_in_new_env(
        env: Rc<RefCell<Env>>,
        params: Vec<ScmVal>,
        args: Vec<ScmVal>,
    ) -> ScmResult<Rc<RefCell<Env>>> {
        let new_env = Env::add_scope(env);
        {
            new_env
                .borrow_mut()
                .insert_all(zip(params, args).collect())?;
        }
        Ok(new_env)
    }

    // Returns the value for the first time the key is found in any scope.
    pub fn lookup(&self, key: ScmVal) -> Option<ScmVal> {
        match self.scope.contents.get(&key) {
            Some(val) => Some(val.clone()),
            None => match self.next {
                Some(ref next) => next.borrow().lookup(key),
                None => None,
            },
        }
    }

    // Inserts a binding into the top scope of the environment.
    // If a key exists in the top scope already it will be rebound.
    pub fn insert(&mut self, key: ScmVal, val: ScmVal) -> ValResult {
        match key {
            ScmVal::Symbol(_) => {
                self.scope.contents.insert(key, val);
                Ok(ScmVal::Empty)
            }
            _ => Err(ScmErr::BadBinding(key)),
        }
    }

    // Inserts a vector of key value pairs as bindings in the top scope.
    pub fn insert_all(&mut self, pairs: Vec<(ScmVal, ScmVal)>) -> ValResult {
        for (key, val) in pairs.iter() {
            self.insert(key.clone(), val.clone())?;
        }
        Ok(ScmVal::Empty)
    }

    // Sets a new value for the first key that matches in any scope.
    // Returns Some(ScmVal::Empty) when the value was set, None if it was not found.
    pub fn set(&mut self, key: ScmVal, val: ScmVal) -> ValResult {
        match self.scope.contents.contains_key(&key) {
            true => self.insert(key, val),
            false => match self.next {
                Some(ref next) => next.borrow_mut().set(key, val),
                None => Err(ScmErr::Undeclared(key.to_string())),
            },
        }
    }

    // Premade Environment Creation //

    pub fn new_null() -> ScmVal {
        ScmVal::Env(Env::new_null_rc())
    }

    pub fn new_null_rc() -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(
            Env::new_with_bindings(vec![
                (ScmVal::new_sym("apply"), ScmVal::Core(Builtin::Apply)),
                (ScmVal::new_sym("eval"), ScmVal::Core(Builtin::Eval)),
                (
                    ScmVal::new_sym("null-environment"),
                    ScmVal::Core(Builtin::BaseEnv),
                ),
                // List core
                (ScmVal::new_sym("cons"), ScmVal::Core(Builtin::Cons)),
                (ScmVal::new_sym("car"), ScmVal::Core(Builtin::Car)),
                (ScmVal::new_sym("cdr"), ScmVal::Core(Builtin::Cdr)),
                // Core Arithmetic
                (ScmVal::new_sym("+"), ScmVal::Core(Builtin::Sum)),
                (ScmVal::new_sym("-"), ScmVal::Core(Builtin::Subtract)),
                (ScmVal::new_sym("*"), ScmVal::Core(Builtin::Product)),
                (ScmVal::new_sym("/"), ScmVal::Core(Builtin::Divide)),
                // Comparisson
                (ScmVal::new_sym("eqv?"), ScmVal::Core(Builtin::Eqv)),
                // Type Predicates
                (ScmVal::new_sym("boolean?"), ScmVal::Core(Builtin::IsBool)),
                (ScmVal::new_sym("char?"), ScmVal::Core(Builtin::IsChar)),
                (ScmVal::new_sym("symbol?"), ScmVal::Core(Builtin::IsSymbol)),
                (ScmVal::new_sym("number?"), ScmVal::Core(Builtin::IsNumber)),
                (ScmVal::new_sym("string?"), ScmVal::Core(Builtin::IsString)),
                (ScmVal::new_sym("pair?"), ScmVal::Core(Builtin::IsPair)),
                (ScmVal::new_sym("vector?"), ScmVal::Core(Builtin::IsVector)),
                (
                    ScmVal::new_sym("procedure?"),
                    ScmVal::Core(Builtin::IsProcedure),
                ),
                (ScmVal::new_sym("null?"), ScmVal::Core(Builtin::IsEmpty)),
            ])
            .expect("base env should not have any bad keys"),
        ))
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
        assert_eq!(env.lookup(ScmVal::new_sym("hello")), None);
    }

    #[test]
    fn test_initialize_with_top_level_bindings() {
        let env = Env::new_with_bindings(vec![
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        assert_eq!(
            env.lookup(ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            env.lookup(ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            env.lookup(ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
    }

    #[test]
    fn test_insert_top_level_bindings_one_at_a_time() {
        let mut env = Env::new();
        env.insert(ScmVal::new_sym("hello"), ScmVal::new_int(22))
            .unwrap();
        env.insert(ScmVal::new_sym("world"), ScmVal::new_int(99))
            .unwrap();
        env.insert(ScmVal::new_sym("steve"), ScmVal::new_int(345))
            .unwrap();
        assert_eq!(
            env.lookup(ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            env.lookup(ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            env.lookup(ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
    }

    #[test]
    fn test_insert_a_vec_of_bindings() {
        let mut env = Env::new();
        env.insert_all(vec![
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        assert_eq!(
            env.lookup(ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            env.lookup(ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            env.lookup(ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
    }

    #[test]
    fn test_set_top_level_bindings() {
        let mut env = Env::new_with_bindings(vec![
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
            env.lookup(ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(0))
        );
        assert_eq!(
            env.lookup(ScmVal::new_sym("world")),
            Some(ScmVal::new_int(100))
        );
        assert_eq!(
            env.lookup(ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(987))
        );
    }

    #[test]
    fn test_add_scope_and_insert() {
        let env = Env::new_with_bindings(vec![
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        let env_rc = Rc::new(RefCell::new(env)); // moved env
        let new_env_rc = Env::add_scope(env_rc);
        {
            let mut env: RefMut<_> = new_env_rc.borrow_mut();
            env.insert_all(vec![
                (ScmVal::new_sym("student"), ScmVal::new_int(45)),
                (ScmVal::new_sym("wants"), ScmVal::new_int(87)),
                (ScmVal::new_sym("cake"), ScmVal::new_int(31)),
            ])
            .unwrap();
        }
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("student")),
            Some(ScmVal::new_int(45))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("wants")),
            Some(ScmVal::new_int(87))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("cake")),
            Some(ScmVal::new_int(31))
        );
    }

    #[test]
    fn test_add_scope_and_shadow_and_set() {
        let env = Env::new_with_bindings(vec![
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        let env_rc = Rc::new(RefCell::new(env)); // moved env
        let new_env_rc = Env::add_scope(env_rc);
        {
            let mut env: RefMut<_> = new_env_rc.borrow_mut();
            env.insert_all(vec![
                (ScmVal::new_sym("hello"), ScmVal::new_int(555)), // shadow hello
                (ScmVal::new_sym("student"), ScmVal::new_int(45)),
                (ScmVal::new_sym("wants"), ScmVal::new_int(87)),
                (ScmVal::new_sym("cake"), ScmVal::new_int(31)),
            ])
            .unwrap();
            let good = env.set(ScmVal::new_sym("world"), ScmVal::new_float(0.234));
            let bad = env.set(ScmVal::new_sym("not-here"), ScmVal::new_float(0.234));
            assert_eq!(good, Ok(ScmVal::Empty));
            assert_eq!(bad, Err(ScmErr::Undeclared("not-here".to_string())));
        }
        // Check all through the top env
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(555))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("world")),
            Some(ScmVal::new_float(0.234))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("student")),
            Some(ScmVal::new_int(45))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("wants")),
            Some(ScmVal::new_int(87))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("cake")),
            Some(ScmVal::new_int(31))
        );
        // Check shadow and set in base env
        let env_rc = new_env_rc.borrow().next.clone().unwrap();
        assert_eq!(
            env_rc.borrow().lookup(ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            env_rc.borrow().lookup(ScmVal::new_sym("world")),
            Some(ScmVal::new_float(0.234))
        );
    }

    #[test]
    fn test_add_scope_and_insert_using_bind_new_env() {
        let env = Env::new_with_bindings(vec![
            (ScmVal::new_sym("hello"), ScmVal::new_int(22)),
            (ScmVal::new_sym("world"), ScmVal::new_int(99)),
            (ScmVal::new_sym("steve"), ScmVal::new_int(345)),
        ])
        .unwrap();
        let new_env_rc = Env::bind_in_new_env(
            Rc::new(RefCell::new(env)), // moved env
            vec![
                ScmVal::new_sym("student"),
                ScmVal::new_sym("wants"),
                ScmVal::new_sym("cake"),
            ],
            vec![
                ScmVal::new_int(45),
                ScmVal::new_int(87),
                ScmVal::new_int(31),
            ],
        )
        .unwrap();

        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("hello")),
            Some(ScmVal::new_int(22))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("world")),
            Some(ScmVal::new_int(99))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("steve")),
            Some(ScmVal::new_int(345))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("student")),
            Some(ScmVal::new_int(45))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("wants")),
            Some(ScmVal::new_int(87))
        );
        assert_eq!(
            new_env_rc.borrow().lookup(ScmVal::new_sym("cake")),
            Some(ScmVal::new_int(31))
        );
    }
}
