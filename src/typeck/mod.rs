use std::collections::HashMap;

use ast;

#[derive(Default, PartialEq, Clone, Debug, Eq)]
pub struct SymbolTable<'a> {
    names: HashMap<&'a str, Ty>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Ty {
    Int,
    Byte,
    Bool,
}

pub fn typeck(ast: &mut ast::Ast) -> Result<(), String> {
    typeck_scope(&mut ast.main)
}

fn typeck_scope<'a>(scope: &mut ast::Scope<'a>) -> Result<(), String> {
    use ast::Statement::*;
    for statement in &mut scope.statements {
        match *statement {
            Declaration(ref decl) => {
                if scope.symbols.names.insert(decl.name, decl.ty).is_some() {
                    return Err(format!("variable with name {} already declared", decl.name));
                }
                if let Some(ref expr) = decl.expr {
                    let ty = ty_of(expr)?;
                    if ty != decl.ty {
                        return Err(format!("type mismatch: variable {} has type {:?} but expression \
                                            {} has type {:?}", decl.name, decl.ty, expr, ty));
                    }
                }
            }
            Expression(ref expr) => {
                typeck_expr(expr, &mut scope.symbols)?;
            },
            Scope(ref mut scope) => {
                typeck_scope(scope)?;
            },
        }
    }
    Ok(())
}

fn typeck_expr<'a>(expr: &ast::Expr<'a>, symbols: &mut SymbolTable<'a>) -> Result<(), String> {
    unimplemented!()
}

fn ty_of<'a>(expr: &ast::Expr<'a>) -> Result<Ty, String> {
    unimplemented!()
}