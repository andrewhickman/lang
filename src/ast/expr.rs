#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Plus,
    Minus,
    PostIncr,
    PostDecr,
    PreIncr,
    PreDecr,
    Not,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    // Listed in order of precedence.
    Mul, Div, Rem,
    Add, Sub,
    Shr, Shl,
    Gt, Ge, Lt, Le,
    Eq, Neq,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
    Assign, AssignMul, AssignDiv, AssignRem, AssignAdd, AssignSub, AssignShr, AssignShl, 
    AssignBitAnd, AssignBitXor, AssignBitOr,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Term<'a> {
    Literal(u32),
    Ident(&'a str),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr<'a> {
    Term(Term<'a>),
    Unary {
        op: UnaryOp,
        arg: Box<Expr<'a>>,
    },
    Binary {
        op: BinaryOp,
        args: Box<(Expr<'a>, Expr<'a>)>,
    },
}
