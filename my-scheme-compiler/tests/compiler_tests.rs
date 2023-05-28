use my_scheme_compiler::module_compiler::ModuleCompiler;
use std::io::Write;

// Helper for testing that creates a compiled version of the program and executes
// it.
fn test(input: &str, expected: &str) {
    let mod_code = ModuleCompiler::new(input)
        .compile_mod()
        .expect("program should compile");
    let code = format!(
        "{mod_code}

fn main() {{
    let env = new_env();
    match module(env) {{
        Ok(val) => println!(\"{{val}}\"),
        Err(e) => println!(\"{{:?}}\", e),
   }}
}}"
    );

    // Create paths and write the program to the main file
    // NOTE tests are run from the crate root, not from the tests directory
    let main_path = std::fs::canonicalize("./tests/test_program/src/main.rs").unwrap();
    let dir_path = std::fs::canonicalize("./tests/test_program").unwrap();
    let mut file = std::fs::File::create(main_path).expect("file should be opened");
    file.write_all(code.as_bytes())
        .expect("program string should be written to file");

    // Cd into the program and call cargo run to exectue the program
    let output = std::process::Command::new("cargo")
        .arg("run")
        .current_dir(dir_path)
        .output()
        .expect("cargo run should have run in test_program directory");

    // print std::err and assert the output is equal to the expected
    println!("{}", std::str::from_utf8(&output.stderr).unwrap());
    assert_eq!(std::str::from_utf8(&output.stdout).unwrap(), expected);
}

// Testing ////////////////////////////////////////////////////////////////////

// NOTE currently the test function uses the same file and folder for each test.
// This means that it overwites the contents for each test. If the tests are called
// in different #[test] functions they are run in parallel and will not work
// correctly because the cargo run might be called when the main file has a
// different tests code in it.
#[test]
fn test_simple_compilation() {
    test(
        "(let ((len 10))
           (do ((v (make-vector len))
                (i 0 (+ i 1)))
               ((= i len) v)
             (vector-set! v i i)))",
        "#(0 1 2 3 4 5 6 7 8 9)\n",
    );
    test(
        "(letrec ((is-even?
                    (lambda (x)
                      (if (= x 0)
                        #t
                        (is-odd? (- x 1)))))
                  (is-odd?
                    (lambda (x)
                      (if (= x 0)
                        #f
                        (is-even? (- x 1))))))
           (is-even? 1000))",
        "#t\n",
    );
    // NOTE without the guarantee of tco the previous example can stack overflow
    // in testing at 10,000
    // Obviously, this makes using scheme a real bummer without a lazy sequence
    // or using do everywhere we would blow the stack quite easily with letrec
    // for even simple functions. Possibly on full optimization this might be
    // optimized, but without the guarantee it is not exactly great to compile
    // to rust. Would need to do like closure and decompose all recursion into
    // do (like loop/recur) or build in a lazy sequence to allow for stackless
    // recursion.

    /*
    test("(cons 1 2)", "(1 . 2)\n");
    test(
        "(string-append \"hello\" \"\\n\" \"world!\")",
        "hello\nworld!\n",
    );
    test(
        "(string #\\h #\\e #\\l #\\l #\\o #\\, #\\space #\\w #\\o #\\r #\\l #\\d #\\!)",
        "hello, world!\n",
    );
    test("(if #t 1 2)", "1\n");
    test("'(1 (+ a 4) 3)", "(1 (+ a 4) 3)\n");
    test("'#(1 (+ a 4) 3)", "#(1 (+ a 4) 3)\n");
    */
}
