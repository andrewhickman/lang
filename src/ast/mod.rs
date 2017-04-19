mod print;

#[derive(Debug, Eq, PartialEq)]
pub struct Ast<'a> {
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Term<'a> {
    Literal(u32),
    Ident(&'a str),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement<'a> {
    Expression(Expr<'a>),
    Declaration(Decl<'a>),
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

#[derive(Debug, Eq, PartialEq)]
pub struct Decl<'a> {
    pub name: &'a str,
    pub expr: Option<Expr<'a>>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Plus,
    Minus,
    PostIncr,
    PostDecr,
    PreIncr,
    PreDecr,
    Not,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOp {
    // Listed in order of precedence.
    Mul, Div, Rem,
    Add, Sub,
    Shl, Shr,
    Gt, Ge, Lt, Le,
    Eq, Neq,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
    Assign,
}
