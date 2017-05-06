#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Ty {
    Int,
    Bool,
    Byte,
    Ref(Box<Ty>),
    Sum(Vec<Ty>),
    Prod(Vec<Ty>),
    Func(Box<(Ty, Ty)>),
    Array(Box<Ty>, usize),
}
