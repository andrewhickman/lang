mod ops;

use std::collections::HashMap;

use ast;

pub type SymbolTable<'a> = HashMap<&'a str, Ty>;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Ty {
    Unknown,
    Int,
    Byte,
    Bool,
}

pub struct TyChecker<'b, 'a: 'b> {
    symbols: Vec<&'b mut SymbolTable<'a>>,
}

impl<'b, 'a: 'b> TyChecker<'b, 'a> {
    pub fn new() -> Self {
        TyChecker { symbols: Vec::new() }
    }

    pub fn ast(&mut self, ast: &'b mut ast::Ast<'a>) -> Result<(), String> {
        self.scope(&mut ast.main)
    }

    fn scope(&mut self, scope: &'b mut ast::Scope<'a>) -> Result<(), String> {
        use ast::Statement::*;
        self.symbols.push(&mut scope.symbols);
        for statement in &mut scope.statements {
            match *statement {
                Declaration(ref mut decl) => {
                    if self.symbols.last_mut().unwrap().insert(decl.name, decl.ty).is_some() {
                        return Err(format!("variable with name {} already declared", decl.name));
                    }
                    if let Some(ref mut expr) = decl.expr {
                        let ty = self.expr(expr)?;
                        if ty != decl.ty {
                            return Err(format!("type mismatch: variable {} has type {:?} but \
                                                expression {} has type {:?}", decl.name, decl.ty, 
                                                expr, ty));
                        }
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

    fn expr(&mut self, expr: &mut ast::Expr<'a>) -> Result<Ty, String> {
        use ast::ExprKind::*;
        use ast::Term::*;
        expr.ty = match expr.kind {
            Term(Literal(_)) => Ty::Int,
            Term(Ident(ident)) => self.lookup(ident)?,
            Binary { op, ref mut args } => ops::ty_of(op, self.expr(&mut args.0)?, 
                                                      self.expr(&mut args.1)?)?,
            Unary { ref mut arg, .. } => self.expr(&mut **arg)?,
        };
        Ok(expr.ty)
    }
    
    fn lookup(&self, key: &'a str) -> Result<Ty, String> {
        for scope in self.symbols.iter().rev() {
            if let Some(ty) = scope.get(key) {
                return Ok(*ty);
            }
        }
        Err(format!("variable not found: {}", key))
    }
}

