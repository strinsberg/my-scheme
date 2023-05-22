use my_scheme_interpreter::interpreter::Interpreter;

pub fn main() {
    let mut interp = Interpreter::new().init();

    let code = "(define f (lambda (x) x)) (f 5)";
    let expect = "5";
    let actual = interp.eval_string(code);

    println!("\n\n=== Interpreter Test ===\n");
    println!("Evaluated: {code}");
    println!("----------------------");
    println!("Expected:  {expect}");
    println!("Actual:    {actual}");
    println!("----------------------\n");
    assert_eq!(actual, expect);
}
