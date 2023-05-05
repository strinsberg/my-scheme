use crate::eval_tco::eval_forms;
use crate::reader::StringReader;
use crate::scheme_libs::std::SCM_LIB_STD;
use crate::types::{Env, ScmVal};
use std::cell::RefCell;
use std::rc::Rc;

// TODO test some errors in here for eval_string.
//
// TODO other libraries need to be loadable with require or something. Since some
// are supposed to be built in, but not loaded at startup, there will have to
// be a builtin called Builtin::Require or something and it will have to explicitly
// import the rust file, read it, and eval it with the current env and heap.
// like define this will only be callable at the top level I guess.

pub struct Interpreter {
    env: Rc<RefCell<Env>>,
    ready: bool,
}

impl Interpreter {
    // Create a new interpreter with an empty environment
    pub fn new() -> Interpreter {
        Interpreter {
            env: ScmVal::null_env_rc(),
            ready: false,
        }
    }

    // Initialize the interpreter.
    // Loads the base environment and the standard library.
    pub fn init(mut self) -> Interpreter {
        self.ready = true;
        self.setup_env();
        self
    }

    pub fn eval_string(&mut self, text: &str) -> String {
        if !self.ready {
            panic!("not initialized");
        }

        let result = match StringReader::new(text).read_forms() {
            Ok(forms) => eval_forms(forms, Rc::clone(&self.env)),
            Err(e) => panic!("{e}"),
        };

        match result {
            Ok(val) => val.to_string(),
            Err(e) => format!("{e}"),
        }
    }

    fn setup_env(&mut self) {
        let lib_std_str = match StringReader::new(SCM_LIB_STD).read_forms() {
            Ok(s) => s,
            Err(e) => panic!("failed to read SCM_LIB_STD: Err: {e}"),
        };

        // Eval scheme standard lib with the base env to add all defines to interpreter env
        match eval_forms(lib_std_str, Rc::clone(&self.env)) {
            Err(e) => panic!("failed to eval SCM_LIB_STD: Err: {e}"),
            _ => (),
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_evaluating_simple_forms_with_definitions() {
        let mut int = Interpreter::new().init();

        // Basic forms
        assert_eq!(int.eval_string(""), "()".to_string());
        assert_eq!(int.eval_string("1"), "1".to_string());

        // Define and use
        assert_eq!(int.eval_string("(define a 5)"), "()".to_string());
        assert_eq!(int.eval_string("a"), "5".to_string());
        assert_eq!(int.eval_string("'a"), "a".to_string());
        assert_eq!(int.eval_string("(+ a 10)"), "15".to_string());

        // Define and call a lambda
        assert_eq!(
            int.eval_string("(define f (lambda (x) (+ x 1)))"),
            "()".to_string()
        );
        assert_eq!(int.eval_string("(f a)"), "6".to_string());

        // Redefine a
        assert_eq!(int.eval_string("(define a 33)"), "()".to_string());
        assert_eq!(int.eval_string("a"), "33".to_string());

        // Multiple forms at once
        assert_eq!(
            int.eval_string("(define b 6)\n\n(+ b a)\n\n(- a b)\n"),
            "27".to_string()
        );

        // Check that the stdlib was infact loaded. MIGHT BREAK LATER.
        assert_eq!(int.eval_string("(equal? a 33)"), "#t".to_string());
    }
}
