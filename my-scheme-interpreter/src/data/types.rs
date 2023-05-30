#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Any,
    Bool,
    Char,
    Number,
    Int,
    UInt,
    Float,
    Symbol,
    Procedure(Box<Arity>),
    Proc,
    String,
    Pair,
    Empty,
    Array,
    Port,
    Env,
    ListOf(Box<Type>),
    Opt(Box<Type>),
    Dots(Box<Type>),
    Fail,
}

impl Type {
    pub fn proc(arity: Arity) -> Type {
        Type::Procedure(Box::new(arity))
    }

    pub fn list(t: Type) -> Type {
        Type::ListOf(Box::new(t))
    }

    pub fn opt(t: Type) -> Type {
        Type::Opt(Box::new(t))
    }

    pub fn dots(t: Type) -> Type {
        Type::Dots(Box::new(t))
    }
}

// TODO make this a proper display
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let string = match self {
            Type::UInt => "exact non-negative integer".to_string(),
            _ => format!("{:?}", self),
        };
        write!(f, "{string}")
    }
}

// Arity //////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Arity {
    Collect(Type),
    Fixed(Vec<Type>),
    Rest(Vec<Type>, Type),
}

impl Arity {
    pub fn get(&self, i: usize) -> Type {
        match self {
            Arity::Collect(t) => t.clone(),
            Arity::Fixed(vec) => {
                if i < vec.len() {
                    vec[i].clone()
                } else {
                    Type::Fail
                }
            }
            Arity::Rest(vec, t) => {
                if i >= vec.len() {
                    t.clone()
                } else {
                    vec[i].clone()
                }
            }
        }
    }
}
