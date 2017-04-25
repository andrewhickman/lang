mod print;

use typeck;

#[derive(Debug, Eq, PartialEq)]
pub struct Ast<'a> {
    pub main: Scope<'a>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Scope<'a> {
    pub symbols: typeck::SymbolTable<'a>,
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
    Scope(Scope<'a>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExprKind<'a> {
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
pub struct Expr<'a> {
    pub ty: typeck::Ty,
    pub kind: ExprKind<'a>,
} 

impl<'a> Expr<'a> {
    pub fn term(term: Term<'a>) -> Self {
        Expr { ty: typeck::Ty::Unknown, kind: ExprKind::Term(term) }
    }

    pub fn unary(op: UnaryOp, arg: Expr<'a>) -> Self {
        Expr { 
            ty: typeck::Ty::Unknown, 
            kind: ExprKind::Unary { op, arg: Box::new(arg) } 
        }
    }

    pub fn binary(op: BinaryOp, lhs: Expr<'a>, rhs: Expr<'a>) -> Self {
        Expr { 
            ty: typeck::Ty::Unknown, 
            kind: ExprKind::Binary { op, args: Box::new((lhs, rhs)) } 
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Decl<'a> {
    pub name: &'a str,
    pub ty: typeck::Ty,
    pub expr: Option<Expr<'a>>,
}

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
