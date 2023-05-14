use my_scheme_interpreter::reader::StringReader;
use my_scheme_interpreter::types::Env;
use my_scheme_interpreter::vm::Vm;

// This is not necessary in the longrun as this is a library and not an
// application. However, some errors cannot be found with the test suite
// easily so this can be used to identify issues. Remove it when things are
// working again.
pub fn main() {
    let env = Env::new_null_rc();
    let mut vm = Vm::new(env);

    // TODO test something that would blow the stack if it was not tail recursive
    // and prove we have done tail call optimization.
    let code = "(define (f) 13) (f)";

    let forms = StringReader::new(code).read_forms().unwrap();
    match vm.eval_forms(&forms) {
        Ok(val) => println!("{}", val),
        Err(e) => println!("{}", e),
    }
}
