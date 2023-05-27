use crate::error::CompileError;
use crate::module_compiler::ModuleCompiler;
use std::collections::HashMap;
use std::fs;
use std::io::Write;

pub fn compile_program(input: &str, output: &str) -> Result<(), CompileError> {
    // Compile the code
    let code = fs::read_to_string(input).or(Err(CompileError::Agh))?;
    let mut compiler = ModuleCompiler::new(&code);
    let compiled_module = compiler.compile_code()?;
    let compiled_program = vec![
        format!("{compiled_module}"),
        "fn main() {".to_string(),
        "let env = new_env();".to_string(),
        "    match mod(env) {".to_string(),
        "        Ok(_) => ()".to_string(),
        "        Err(e) => println!(\"{e}\"),".to_string(),
        "    }".to_string(),
        "}".to_string(),
    ]
    .join("\n");

    // create a folder with output name
    let out_path = fs::canonicalize(format!("./{output}")).expect("path should exist");
    let src_path = fs::canonicalize(format!("./{output}/src")).expect("path should exist");
    fs::create_dir(out_path).expect("");
    // create a cargo.toml
    create_toml(output)?;
    // create a src folder
    fs::create_dir(src_path).expect("");
    // create a main.rs
    let main_path = fs::canonicalize(format!("./{output}/src/main.rs")).expect("path should exist");
    let mut prog_file = fs::File::create(main_path).expect("some junk");
    prog_file.write_all(compiled_program.as_bytes()).expect("");
    // if there are any loads in compiler after compilation
    create_modules(compiler.modules, output)?;
    Ok(())
}

pub fn create_toml(output: &str) -> Result<(), CompileError> {
    let lines = vec![
        "[package]".to_string(),
        "name = \"output\"".to_string(),
        "version = \"0.1.0\"".to_string(),
        "edition = \"2021\"".to_string(),
        "".to_string(),
        "[dependencies]".to_string(),
        // TODO change this to either accept a dir for the scheme lib or use github
        "my-scheme-lib = { path=\"../my-scheme-lib\" }".to_string(),
    ];
    let path = fs::canonicalize(format!("./{output}/Cargo.toml")).expect("path should exist");
    let mut file = fs::File::create(path).expect("some junk");
    file.write_all(lines.join("\n").as_bytes()).expect("");
    Ok(())
}

pub fn create_modules(modules: HashMap<String, String>, output: &str) -> Result<(), CompileError> {
    let mut libs = Vec::new();
    for (name, code) in modules.iter() {
        // add file name to libs
        libs.push(format!("pub mod {};", name));
        // create a file for all the compiled modules
        let path = fs::canonicalize(format!("./{output}/src/{}.rs", name)).expect("");
        let mut mod_file = fs::File::create(path).expect("some junk");
        mod_file.write_all(code.as_bytes()).expect("");
    }

    let lib_path = fs::canonicalize(format!("./{output}/src/lib.rs")).expect("");
    let mut mod_file = fs::File::create(lib_path).expect("some junk");
    mod_file.write_all(libs.join("\n").as_bytes()).expect("");
    Ok(())
}
