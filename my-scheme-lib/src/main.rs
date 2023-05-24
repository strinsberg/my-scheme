use my_scheme_lib::compile::compiler::Compiler;

fn main() {
    let mut c = Compiler::new();
    println!(
        "{}",
        c.compile_program("my_scheme_lib", "(let ((a 5) (b 6)) (+ a b))")
            .unwrap()
    );
}
