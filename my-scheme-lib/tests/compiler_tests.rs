use my_scheme_lib::compile::compiler::compile_program_with_output;
use std::io::Write;

// Helper for testing that creates a compiled version of the program and executes
// it.
fn test(input: &str, expected: &str) {
    // Compile the program
    let code =
        compile_program_with_output("my_scheme_lib", input).expect("program should have compiled");

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
    test("(cons 1 2)", "(1 . 2)\n");
    test(
        "(letrec ((f (lambda (x) (g x)))
                   (g (lambda (x) (+ 1 x))))
           (f 6))",
        "7\n",
    );
    test("(do ((i 0 (+ i 1))) ((= i 10) i) 1 2 3)", "10\n")
}
