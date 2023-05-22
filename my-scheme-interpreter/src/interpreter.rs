use crate::reader::StringReader;
use crate::rep::ExternalRep;
//use crate::scheme_libs::std::SCM_LIB_STD;
use crate::builtin::null_env;
use crate::vm::Vm;

// TODO other libraries need to be loadable with require or something. Since some
// are supposed to be built in, but not loaded at startup, there will have to
// be a builtin called Builtin::Require or something and it will have to explicitly
// import the rust file, read it, and eval it with the current env and heap.
// like define this will only be callable at the top level I guess.
//
// TODO when you implement a load, it would be cool to be able to add custom
// extensions to the language that are writtin in rust, but are not included
// unless an extension lib is loaded. Say a function like vector-push. It is
// not part of the standard and cannot be added with scheme, unless I implemented
// a dynamic vector with scheme. Other extensions might be able to be written
// in scheme though and might benefit from using the extension functions. So
// i will need an function that adds the builtin extensions to the environments
// and have to catch a load call for a specific lib that will both add the
// builtins to the env and will read and load the scheme file as well.

pub struct Interpreter {
    ready: bool,
    vm: Vm,
}

impl Interpreter {
    // Create a new interpreter with an empty environment
    pub fn new() -> Interpreter {
        Interpreter {
            ready: false,
            vm: Vm::new(null_env()),
        }
    }

    // Initialize the interpreter.
    pub fn init(mut self) -> Interpreter {
        self.ready = true;
        self.load_std();
        self
    }

    pub fn eval_string(&mut self, text: &str) -> String {
        if !self.ready {
            panic!("not initialized");
        }

        match StringReader::new(text).read_forms() {
            Ok(forms) => match self.vm.eval_forms(&forms) {
                Ok(val) => val.to_external(),
                Err(e) => format!("{e}"),
            },
            Err(e) => panic!("{e}"),
        }
    }

    fn load_std(&mut self) {
        /*
        let lib_std_str = StringReader::new(SCM_LIB_STD)
            .read_forms()
            .expect("failed to read SCM_LIB_STD: Err: {e}");

        // Eval scheme standard lib with the base env to add all defines to interpreter env
        self.vm
            .eval_forms(&lib_std_str)
            .expect("failed to eval SCM_LIB_STD: Err: {e}");
            */
    }
}

// Testing ////////////////////////////////////////////////////////////////////

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
        /* calling closures is broken right now
        assert_eq!(
            int.eval_string("(define f (lambda (x) (+ x 1)))"),
            "()".to_string()
        );
        assert_eq!(int.eval_string("(f a)"), "6".to_string());
        */

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
