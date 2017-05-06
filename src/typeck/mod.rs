mod ops;

use std::collections::HashMap;

use ast;

pub type SymbolTable<'src> = HashMap<&'src str, ast::Ty>;

<<<<<<< HEAD
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Ty {
    Unknown,
    Int,
    Byte,
    Bool,
=======
pub struct TyChecker<'sym, 'src: 'sym> {
    symbols: Vec<&'sym mut SymbolTable<'src>>,
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
}

impl<'sym, 'src: 'sym> TyChecker<'sym, 'src> {
    pub fn new() -> Self {
        TyChecker { symbols: Vec::new() }
    }

    pub fn ast(&mut self, ast: &'sym mut ast::Ast<'src>) -> Result<(), String> {
        self.scope(&mut ast.main)
    }

    fn scope(&mut self, scope: &'sym mut ast::Scope<'src>) -> Result<(), String> {
        use ast::Statement::*;
        self.symbols.push(&mut scope.symbols);
        for statement in &mut scope.statements {
            match *statement {
                Declaration(ref decl) => {
                    if self.symbols.last_mut().unwrap().insert(decl.name, decl.ty.clone()).is_some() {
                        return Err(format!("variable with name {} already declared", decl.name));
                    }
                }
                Expression(ref mut expr) => {
                    self.expr(expr)?;
                },
                Scope(ref mut scope) => {
                    self.scope(scope)?;
                },
            }
        }
        self.symbols.pop();
        Ok(())
    }

    fn expr(&mut self, expr: &ast::Expr<'src>) -> Result<ast::Ty, String> {
        use ast::Expr::*;
        use ast::Term::*;
        match *expr {
            Term(Literal(_)) => Ok(ast::Ty::Int),
            Term(Ident(ident)) => self.lookup(ident),
            Binary { op, ref args } => ops::ty_of(op, self.expr(&args.0)?, self.expr(&args.1)?),
            Unary { ref arg, .. } => self.expr(&**arg),
        }
    }
    
    fn lookup(&self, key: &'src str) -> Result<ast::Ty, String> {
        for scope in self.symbols.iter().rev() {
            if let Some(ty) = scope.get(key) {
                return Ok(ty.clone());
            }
        }
        Err(format!("variable not found: {}", key))
    }
}

