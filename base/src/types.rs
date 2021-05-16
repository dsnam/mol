use std::mem::discriminant;

#[derive(Debug, Clone)]
pub enum MolType {
    Str,
    Int,
    Float,
    Bool,
    Var(String),
    Named(String),
    Func {
        args: Vec<MolType>,
        return_type: Box<MolType>,
    },
}
