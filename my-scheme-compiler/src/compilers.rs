use crate::compile;
use crate::error::CompileError;

pub struct ProgramCompiler {
    in_path: String,
    out_path: String,
    loads: Vec<String>,
    symbols: Vec<String>,
}

impl ProgramCompiler {
    pub fn new(in_path: &str, out_path: &str) -> ProgramCompiler {
        ProgramCompiler {
            in_path: in_path.to_string(),
            out_path: out_path.to_string(),
            loads: Vec::new(),
            symbols: Vec::new(),
        }
    }

    pub fn compile(&self) -> Result<(), CompileError> {
        // load in the file
        // compile the contents
        // write the new file
        Ok(())
    }
}

pub struct ModuleCompiler {
    in_path: String,
    out_path: String,
    loads: Vec<String>,
    symbols: Vec<String>,
}

impl ModuleCompiler {
    pub fn new(in_path: &str, out_path: &str) -> ModuleCompiler {
        ModuleCompiler {
            in_path: in_path.to_string(),
            out_path: out_path.to_string(),
            loads: Vec::new(),
            symbols: Vec::new(),
        }
    }

    pub fn compile(&self) -> Result<(), CompileError> {
        // load in the file
        // compile the contents
        // write the new file
        Ok(())
    }
}
