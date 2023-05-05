use my_scheme_interpreter::types::{Env, ScmVal};
use std::cell::{RefCell, RefMut};
use std::rc::Rc;

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
    ]);
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
    env.insert(ScmVal::new_sym("hello"), ScmVal::new_int(22));
    env.insert(ScmVal::new_sym("world"), ScmVal::new_int(99));
    env.insert(ScmVal::new_sym("steve"), ScmVal::new_int(345));
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
    ]);
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
    ]);
    env.set(ScmVal::new_sym("hello"), ScmVal::new_int(0));
    env.set(ScmVal::new_sym("world"), ScmVal::new_int(100));
    env.set(ScmVal::new_sym("steve"), ScmVal::new_int(987));
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
    ]);
    let env_rc = Rc::new(RefCell::new(env)); // moved env
    let new_env_rc = Env::add_scope(env_rc);
    {
        let mut env: RefMut<_> = new_env_rc.borrow_mut();
        env.insert_all(vec![
            (ScmVal::new_sym("student"), ScmVal::new_int(45)),
            (ScmVal::new_sym("wants"), ScmVal::new_int(87)),
            (ScmVal::new_sym("cake"), ScmVal::new_int(31)),
        ]);
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
    ]);
    let env_rc = Rc::new(RefCell::new(env)); // moved env
    let new_env_rc = Env::add_scope(env_rc);
    {
        let mut env: RefMut<_> = new_env_rc.borrow_mut();
        env.insert_all(vec![
            (ScmVal::new_sym("hello"), ScmVal::new_int(555)), // shadow hello
            (ScmVal::new_sym("student"), ScmVal::new_int(45)),
            (ScmVal::new_sym("wants"), ScmVal::new_int(87)),
            (ScmVal::new_sym("cake"), ScmVal::new_int(31)),
        ]);
        let good = env.set(ScmVal::new_sym("world"), ScmVal::new_float(0.234));
        let bad = env.set(ScmVal::new_sym("not-here"), ScmVal::new_float(0.234));
        assert_eq!(good, Some(ScmVal::Empty));
        assert_eq!(bad, None);
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
    ]);
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
    );

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
