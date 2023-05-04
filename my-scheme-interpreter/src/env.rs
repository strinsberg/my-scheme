use crate::heap::{Heap, ListPtrIter, ListValIter};
use crate::scm_types::builtin::Builtin;
use crate::scm_types::scm_val::{Pointer, ScmVal};
use crate::utils;
use std::iter::zip;

// Creates a new environment from with an initial set of key value tuples.
// Only fails with panic as passing bad args, out of memory, and pointer errors
// here are my problem, not the users.
pub fn new(pairs: Vec<(ScmVal, ScmVal)>, heap: &mut Heap) -> Pointer {
    let scope = utils::empty_box_map();
    let env = heap.cons(scope, ScmVal::Empty);
    insert_pairs(pairs, env, heap);
    env
}

// Adds a new empty scope to the environment.
pub fn new_scope(env: Pointer, heap: &mut Heap) -> Pointer {
    let scope = utils::empty_box_map();
    heap.cons(scope, ScmVal::Pair(env))
}

// Returns the value for the first time the key is found in any scope.
pub fn lookup(key: ScmVal, env: Pointer, heap: &mut Heap) -> Option<ScmVal> {
    for scope in ListValIter::new(env, heap) {
        match scope {
            ScmVal::HashMapBox(map) => match map.hash_map.get(&key).map(|x| x.clone()) {
                Some(val) => return Some(val),
                None => continue,
            },
            _ => panic!(
                "env can only have ScmVal::HashMapBox for scopes: {:?}",
                scope
            ),
        };
    }
    None
}

// Inserts a binding into the top scope of the environment.
// If a key exists in the top scope already it will be rebound.
pub fn insert(key: ScmVal, val: ScmVal, env: Pointer, heap: &mut Heap) {
    let env_cell = heap.get_cell_mut(env);
    match &mut env_cell.head {
        ScmVal::HashMapBox(ref mut map) => map.hash_map.insert(key, val),
        _ => panic!(
            "env can only have ScmVal::HashMapBox for scopes: {:?}",
            env_cell.head
        ),
    };
}

// Inserts a vector of key value pairs as bindings in the top scope.
pub fn insert_pairs(pairs: Vec<(ScmVal, ScmVal)>, env: Pointer, heap: &mut Heap) {
    for (key, val) in pairs.iter() {
        insert(key.clone(), val.clone(), env, heap);
    }
}

// Binds a list of symbols to a list of values in a new scope
pub fn bind(params: Vec<ScmVal>, args: Vec<ScmVal>, env: Pointer, heap: &mut Heap) -> Pointer {
    let new_env = new_scope(env, heap);
    insert_pairs(zip(params, args).collect(), new_env, heap);
    new_env
}

// Sets a new value for the first key that matches in any scope.
// Returns Some(ScmVal::Empty) when the value was set, None if it was not found.
pub fn set(key: ScmVal, val: ScmVal, env: Pointer, heap: &mut Heap) -> Option<ScmVal> {
    let ptrs: Vec<Pointer> = ListPtrIter::new(env, heap).collect();
    for ptr in ptrs.iter() {
        let cell = heap.get_cell_mut(*ptr);
        match &mut cell.head {
            ScmVal::HashMapBox(ref mut map) => {
                if map.hash_map.contains_key(&key) {
                    map.hash_map.insert(key.clone(), val.clone());
                    return Some(ScmVal::Empty);
                }
            }
            _ => panic!(
                "env can only have ScmVal::HashMapBox for scopes: {:?}",
                cell.head
            ),
        }
    }
    None
}

// Base with all builtins /////////////////////////////////////////////////////

pub fn new_base_env(heap: &mut Heap) -> Pointer {
    new(
        vec![
            (utils::new_sym("apply"), ScmVal::Core(Builtin::Apply)),
            (utils::new_sym("eval"), ScmVal::Core(Builtin::Eval)),
            (
                utils::new_sym("null-environment"),
                ScmVal::Core(Builtin::BaseEnv),
            ),
            // List core
            (utils::new_sym("cons"), ScmVal::Core(Builtin::Cons)),
            (utils::new_sym("car"), ScmVal::Core(Builtin::Car)),
            (utils::new_sym("cdr"), ScmVal::Core(Builtin::Cdr)),
            // Core Arithmetic
            (utils::new_sym("+"), ScmVal::Core(Builtin::Sum)),
            (utils::new_sym("-"), ScmVal::Core(Builtin::Subtract)),
            (utils::new_sym("*"), ScmVal::Core(Builtin::Product)),
            (utils::new_sym("/"), ScmVal::Core(Builtin::Divide)),
            // Comparisson
            (utils::new_sym("eqv?"), ScmVal::Core(Builtin::Eqv)),
            // Type Predicates
        ],
        heap,
    )
}

// Tests //////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scm_types::string::ScmString;

    #[test]
    fn test_creating_new_env_and_looking_up_elements() {
        let mut h = Heap::new();
        let sym_t = utils::new_sym("t");
        let sym_f = utils::new_sym("f");
        let sym_e = utils::new_sym("e");
        let sym_b = utils::new_sym("b");
        let init = vec![
            (sym_t.clone(), utils::new_int(5)),
            (sym_f.clone(), utils::new_int(99)),
            (sym_e.clone(), ScmVal::Empty),
        ];
        let env = new(init, &mut h);
        assert_eq!(lookup(sym_t, env, &mut h), Some(utils::new_int(5)));
        assert_eq!(lookup(sym_f, env, &mut h), Some(utils::new_int(99)));
        assert_eq!(lookup(sym_e, env, &mut h), Some(ScmVal::Empty));
        assert_eq!(lookup(sym_b, env, &mut h), None);
    }

    #[test]
    fn test_inserting_new_elements() {
        let mut h = Heap::new();
        let sym_t = utils::new_sym("t");
        let sym_f = utils::new_sym("f");
        let sym_e = utils::new_sym("e");
        let sym_b = utils::new_sym("b");
        let init = vec![
            (sym_t.clone(), utils::new_int(5)),
            (sym_f.clone(), utils::new_int(99)),
            (sym_e.clone(), ScmVal::Empty),
        ];
        let env = new(init, &mut h);
        insert(sym_t.clone(), utils::new_int(33), env, &mut h);
        insert(sym_b.clone(), utils::new_int(66), env, &mut h);
        assert_eq!(lookup(sym_b, env, &mut h), Some(utils::new_int(66)));
        assert_eq!(lookup(sym_t, env, &mut h), Some(utils::new_int(33)));
        assert_eq!(lookup(sym_f, env, &mut h), Some(utils::new_int(99)));
        assert_eq!(lookup(sym_e, env, &mut h), Some(ScmVal::Empty));
    }

    #[test]
    fn test_adding_a_new_scope_and_inserting_elements() {
        let mut h = Heap::new();
        let sym_t = utils::new_sym("t");
        let sym_f = utils::new_sym("f");
        let sym_e = utils::new_sym("e");
        let sym_b = utils::new_sym("b");
        let init = vec![
            (sym_t.clone(), utils::new_int(5)),
            (sym_f.clone(), utils::new_int(99)),
            (sym_e.clone(), ScmVal::Empty),
        ];
        let env = new(init, &mut h);
        let next = new_scope(env, &mut h);
        insert(sym_t.clone(), utils::new_int(33), next, &mut h);
        insert(sym_b.clone(), utils::new_int(66), next, &mut h);
        // find from top scope
        assert_eq!(
            lookup(sym_b.clone(), next, &mut h),
            Some(utils::new_int(66))
        );
        assert_eq!(
            lookup(sym_t.clone(), next, &mut h),
            Some(utils::new_int(33))
        );
        assert_eq!(lookup(sym_e.clone(), next, &mut h), Some(ScmVal::Empty));
        // find in original scope that should be unaltered
        assert_eq!(lookup(sym_t, env, &mut h), Some(utils::new_int(5)));
        assert_eq!(lookup(sym_f, env, &mut h), Some(utils::new_int(99)));
        assert_eq!(lookup(sym_e, env, &mut h), Some(ScmVal::Empty));
    }

    #[test]
    fn test_setting_values_in_different_scopes() {
        let mut h = Heap::new();
        let sym_t = utils::new_sym("t");
        let sym_f = utils::new_sym("f");
        let sym_e = utils::new_sym("e");
        let sym_b = utils::new_sym("b");
        let init = vec![
            (sym_t.clone(), utils::new_int(5)),
            (sym_f.clone(), utils::new_int(99)),
        ];
        let env = new(init, &mut h);
        let next = new_scope(env, &mut h);
        insert(sym_e.clone(), utils::new_int(33), next, &mut h);
        insert(sym_b.clone(), utils::new_int(66), next, &mut h);
        // ensure correct initial values
        assert_eq!(lookup(sym_t.clone(), next, &mut h), Some(utils::new_int(5)));
        assert_eq!(
            lookup(sym_f.clone(), next, &mut h),
            Some(utils::new_int(99))
        );
        assert_eq!(
            lookup(sym_e.clone(), next, &mut h),
            Some(utils::new_int(33))
        );
        assert_eq!(
            lookup(sym_b.clone(), next, &mut h),
            Some(utils::new_int(66))
        );
        // Set new values
        set(sym_f.clone(), utils::new_int(88), next, &mut h);
        set(sym_e.clone(), utils::new_int(77), next, &mut h);
        // ensure correct subsequent values
        assert_eq!(lookup(sym_t.clone(), next, &mut h), Some(utils::new_int(5)));
        assert_eq!(
            lookup(sym_f.clone(), next, &mut h),
            Some(utils::new_int(88))
        );
        assert_eq!(
            lookup(sym_e.clone(), next, &mut h),
            Some(utils::new_int(77))
        );
        assert_eq!(
            lookup(sym_b.clone(), next, &mut h),
            Some(utils::new_int(66))
        );
    }
}
