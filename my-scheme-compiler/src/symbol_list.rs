use std::collections::hash_map::Iter;
use std::collections::HashMap;

// Symbol List ////////////////////////////////////////////////////////////////

pub struct SymbolList {
    pub map: HashMap<String, u64>,
    idx: u64,
}

impl SymbolList {
    pub fn new() -> SymbolList {
        SymbolList {
            map: HashMap::new(),
            idx: 0,
        }
    }

    pub fn lookup(&self, sym: &String) -> Option<String> {
        if self.map.contains_key(sym) {
            Some(format!("__{}", self.map[sym]))
        } else {
            None
        }
    }

    pub fn add(&mut self, sym: &String) -> String {
        self.idx += 1;
        self.map.insert(sym.clone(), self.idx);
        format!("__{}", self.idx)
    }

    pub fn iter(&self) -> Iter<'_, String, u64> {
        self.map.iter()
    }
}
