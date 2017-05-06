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
pub enum Statement<'src> {
    Expression(Expr<'src>),
    Declaration(Decl<'src>),
    Scope(Scope<'src>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Decl<'src> {
    pub name: &'src str,
    pub ty: Ty,
}
