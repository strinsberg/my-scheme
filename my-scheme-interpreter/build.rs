use std::fs;
use std::path::Path;

pub fn main() {
    // TODO set a way to build excluding librairies

    let in_dir = "scheme_libs";
    let out_dir = "src/scheme_libs/";

    let lib_ext = ".scm";
    let src_ext = ".rs";

    let files = vec!["std"];

    for f in files.iter() {
        // open the file and write a new file with the contents
        let lib_path = Path::new(in_dir).join(format!("{f}{lib_ext}"));
        let contents = match fs::read_to_string(lib_path) {
            Err(e) => panic!("failed to read scheme librairy: Error: {e}"),
            Ok(s) => s,
        };

        let source_path = Path::new(out_dir).join(format!("{f}{src_ext}"));
        let var_name = format!("scm_lib_{f}").to_uppercase();

        match fs::write(
            &source_path,
            format!("pub const {var_name}: &str = \"{contents}\";"),
        ) {
            Err(e) => panic!("failed to write scheme source: {f}, Error: {e}"),
            _ => (),
        }
    }
}
