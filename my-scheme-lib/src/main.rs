use my_scheme_lib::compile::compiler::compile_program;

fn main() {
    println!(
        "{}",
        compile_program("my_scheme_lib", "(let ((a 5) (b 6)) (+ a b))").unwrap()
    );
}
