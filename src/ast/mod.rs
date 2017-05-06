mod print;
mod ty;
mod expr;

pub use self::expr::*;
pub use self::ty::*;

use typeck;

#[derive(Debug, Eq, PartialEq)]
pub struct Ast<'src> {
    pub main: Scope<'src>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Scope<'src> {
    pub symbols: typeck::SymbolTable<'src>,
    pub statements: Vec<Statement<'src>>,
}

#[derive(Debug, Eq, PartialEq)]
<<<<<<< HEAD
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
=======
pub enum Statement<'src> {
    Expression(Expr<'src>),
    Declaration(Decl<'src>),
    Scope(Scope<'src>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Decl<'src> {
    pub name: &'src str,
    pub ty: Ty,
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
}
