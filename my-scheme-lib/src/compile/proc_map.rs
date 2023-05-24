use std::collections::HashMap;

pub fn make_proc_map() -> HashMap<&'static str, &'static str> {
    let mut map = HashMap::new();
    map.insert("+", "proc::add");
    map
}
