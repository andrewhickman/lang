mod error;

use std::result;

use {ast, typeck};
use self::error::{err, Error};
use super::{Peekable, lex, Stream, Token};

pub type Result<'src, T> = result::Result<T, Error<'src>>;

pub struct Parser<'src> {
    lexer: Peekable<lex::Lexer<'src>>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Associativity {
    Left, Right
}

#[derive(Debug, Clone, Copy)]
struct BinaryOp {
    op: ast::BinaryOp,
    lvl: u8,
    assoc: Associativity,
}

impl<'src> Parser<'src> {
    pub fn new(data: &'src str) -> Self {
        Parser {
            lexer: lex::Lexer::new(data).peekable(),
        }
    }

    fn expect(&mut self, tok: Token<'src>) -> Result<'src, ()> {
        if self.lexer.eat(tok) {
            Ok(())
        } else {
            err(tok, self.lexer.peek)
        }
    }

    fn unary_ty(&mut self) -> Result<'src, ast::Ty> {
        let mut ty = match self.lexer.next() {
            Token::Byte => ast::Ty::Byte,
            Token::Bool => ast::Ty::Bool,
            Token::Int => ast::Ty::Int,
            Token::And => ast::Ty::Ref(Box::new(self.ty()?)),
            Token::AndAnd => ast::Ty::Ref(Box::new(ast::Ty::Ref(Box::new(self.ty()?)))),
            Token::OpenParen => {
                let ty = self.ty()?;
                self.expect(Token::CloseParen)?;
                ty
            },
            tok => return err("type", tok),
        };
        while self.lexer.eat(Token::Tilde) {
            match self.lexer.next() {
                Token::Num(size) => {
                    ty = ast::Ty::Array(Box::new(ty), size as usize);
                }
                tok => return err("integer literal", tok),
            }
        }
        Ok(ty)
    }

    fn sum_ty(&mut self) -> Result<'src, ast::Ty> {
        let ty = self.unary_ty()?;
        if self.lexer.peek == Token::Or {
            let mut args = vec![ty];
            while self.lexer.eat(Token::Or) {
                args.push(self.unary_ty()?);
            }
            Ok(ast::Ty::Sum(args))
        } else {
            Ok(ty)
        }
    }

    fn prod_ty(&mut self) -> Result<'src, ast::Ty> {
        let ty = self.sum_ty()?;
        if self.lexer.peek == Token::Comma {
            let mut args = vec![ty];
            while self.lexer.eat(Token::Comma) {
                args.push(self.sum_ty()?);
            }
            Ok(ast::Ty::Prod(args))
        } else {
            Ok(ty)
        }
    }

    fn func_ty(&mut self) -> Result<'src, ast::Ty> {
        let lhs = self.prod_ty()?;
        if self.lexer.eat(Token::Arrow) {
            Ok(ast::Ty::Func(Box::new((lhs, self.func_ty()?))))            
        } else {
            Ok(lhs)
        }
    }

    fn ty(&mut self) -> Result<'src, ast::Ty> {
        self.func_ty()
    }
    
<<<<<<< HEAD
    fn postfix_expr(&mut self, term: ast::Term<'a>) -> Result<'a, ast::Expr<'a>> {
        let mut arg = ast::Expr::term(term);
=======
    fn postfix_expr(&mut self, term: ast::Term<'src>) -> Result<'src, ast::Expr<'src>> {
        let mut arg = ast::Expr::Term(term);
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
        loop {
            let op = match self.lexer.peek {
                Token::PlusPlus => ast::UnaryOp::PostIncr,
                Token::MinusMinus => ast::UnaryOp::PostDecr,
                _ => return Ok(arg),
            };
<<<<<<< HEAD
            self.lexer.bump();
            arg = ast::Expr::unary(op, arg);
        }
    }

    fn unary_expr(&mut self) -> Result<'a, ast::Expr<'a>> {
=======
            self.lexer.next();
            arg = ast::Expr::Unary { op, arg: Box::new(arg) };
        }
    }

    fn unary_expr(&mut self) -> Result<'src, ast::Expr<'src>> {
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
        enum OpOrTerm<'b> { Op(ast::UnaryOp), Term(ast::Term<'b>) }

        let next = match self.lexer.next() {
            Token::Not => OpOrTerm::Op(ast::UnaryOp::Not),
            Token::Plus => OpOrTerm::Op(ast::UnaryOp::Plus),
            Token::Minus => OpOrTerm::Op(ast::UnaryOp::Minus),
            Token::PlusPlus => OpOrTerm::Op(ast::UnaryOp::PreIncr),
            Token::MinusMinus => OpOrTerm::Op(ast::UnaryOp::PreDecr),
            Token::Ident(ident) => OpOrTerm::Term(ast::Term::Ident(ident)),
            Token::Num(num) => OpOrTerm::Term(ast::Term::Literal(num)),
            Token::OpenParen => {
                let expr = self.expr()?;
                self.expect(Token::CloseParen)?;
                return Ok(expr)
            },
            tok => return err("prefix operator, identifier or literal", tok),
        };
        match next {
            OpOrTerm::Op(op) => Ok(ast::Expr::unary(op, self.unary_expr()?)),
            OpOrTerm::Term(term) => self.postfix_expr(term),
        }
    }

<<<<<<< HEAD
    fn expr_lvl(&mut self, min_lvl: u8) -> Result<'a, ast::Expr<'a>> {
=======
    fn expr_lvl(&mut self, min_lvl: u8) -> Result<'src, ast::Expr<'src>> {
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
        let mut lhs = self.unary_expr()?; 
        while let Ok(BinaryOp { op, lvl, assoc }) = self.peek_expr_op() {
            if min_lvl < lvl {
                self.lexer.next();
                let rhs = match assoc {
                    Associativity::Left => self.expr_lvl(lvl),
                    Associativity::Right => self.expr_lvl(lvl - 1),
                }?;
                lhs = ast::Expr::binary(op, lhs, rhs);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

<<<<<<< HEAD
    fn peek_op(&mut self) -> Result<'a, BinaryOp> {
        use self::Associativity::{Left, Right};
        use ast::BinaryOp::*;
        Ok(match self.lexer.peek() {
            Token::Star => BinaryOp { op: Mul, lvl: 11, assoc: Left },
            Token::Slash => BinaryOp { op: Div, lvl: 11, assoc: Left },
            Token::Percent => BinaryOp { op: Rem, lvl: 11, assoc: Left },
            Token::Plus => BinaryOp { op: Add, lvl: 10, assoc: Left },
            Token::Minus => BinaryOp { op: Sub, lvl: 10, assoc: Left },
            Token::Shr => BinaryOp { op: Shr, lvl: 9, assoc: Left },
            Token::Shl => BinaryOp { op: Shl, lvl: 9, assoc: Left },
            Token::And => BinaryOp { op: BitAnd, lvl: 8, assoc: Left },
            Token::Caret => BinaryOp { op: BitXor, lvl: 7, assoc: Left },
            Token::Or => BinaryOp { op: BitOr, lvl: 6, assoc: Left },
            Token::Lt => BinaryOp { op: Lt, lvl: 5, assoc: Left },
            Token::Le => BinaryOp { op: Le, lvl: 5, assoc: Left },
            Token::Gt => BinaryOp { op: Gt, lvl: 5, assoc: Left },
            Token::Ge => BinaryOp { op: Ge, lvl: 5, assoc: Left },
            Token::EqEq => BinaryOp { op: Eq, lvl: 4, assoc: Left },
            Token::NotEq => BinaryOp { op: Neq, lvl: 4, assoc: Left },
=======
    fn peek_expr_op(&mut self) -> Result<'src, BinaryOp> {
        use self::Associativity::{Left, Right};
        use ast::BinaryOp::*;
        Ok(match self.lexer.peek {
            Token::Star => BinaryOp { op: Mul, lvl: 10, assoc: Left },
            Token::Slash => BinaryOp { op: Div, lvl: 10, assoc: Left },
            Token::Percent => BinaryOp { op: Rem, lvl: 10, assoc: Left },
            Token::Plus => BinaryOp { op: Add, lvl: 9, assoc: Left },
            Token::Minus => BinaryOp { op: Sub, lvl: 9, assoc: Left },
            Token::Shr => BinaryOp { op: Shr, lvl: 8, assoc: Left },
            Token::Shl => BinaryOp { op: Shl, lvl: 8, assoc: Left },
            Token::Lt => BinaryOp { op: Lt, lvl: 7, assoc: Left },
            Token::Le => BinaryOp { op: Le, lvl: 7, assoc: Left },
            Token::Gt => BinaryOp { op: Gt, lvl: 7, assoc: Left },
            Token::Ge => BinaryOp { op: Ge, lvl: 7, assoc: Left },
            Token::EqEq => BinaryOp { op: Eq, lvl: 6, assoc: Left },
            Token::NotEq => BinaryOp { op: Neq, lvl: 6, assoc: Left },
            Token::And => BinaryOp { op: BitAnd, lvl: 5, assoc: Left },
            Token::Caret => BinaryOp { op: BitXor, lvl: 4, assoc: Left },
            Token::Or => BinaryOp { op: BitOr, lvl: 3, assoc: Left },
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
            Token::AndAnd => BinaryOp { op: And, lvl: 3, assoc: Left },
            Token::OrOr => BinaryOp { op: Or, lvl: 2, assoc: Left },
            Token::Eq => BinaryOp { op: Assign, lvl: 1, assoc: Right },
            Token::StarEq => BinaryOp { op: AssignMul, lvl: 1, assoc: Right },
            Token::SlashEq => BinaryOp { op: AssignDiv, lvl: 1, assoc: Right },
            Token::PercentEq => BinaryOp { op: AssignRem, lvl: 1, assoc: Right },
            Token::PlusEq => BinaryOp { op: AssignAdd, lvl: 1, assoc: Right },
            Token::MinusEq => BinaryOp { op: AssignSub, lvl: 1, assoc: Right },
            Token::ShrEq => BinaryOp { op: AssignShr, lvl: 1, assoc: Right },
            Token::ShlEq => BinaryOp { op: AssignShl, lvl: 1, assoc: Right },
            Token::AndEq => BinaryOp { op: AssignBitAnd, lvl: 1, assoc: Right },
            Token::CaretEq => BinaryOp { op: AssignBitXor, lvl: 1, assoc: Right },
            Token::OrEq => BinaryOp { op: AssignBitOr, lvl: 1, assoc: Right },
            tok => return err("operator", tok),
        })
    }

<<<<<<< HEAD
    fn expr(&mut self) -> Result<'a, ast::Expr<'a>> {
        self.expr_lvl(0)
    }

    fn decl(&mut self) -> Result<'a, ast::Decl<'a>> {
=======
    fn expr(&mut self) -> Result<'src, ast::Expr<'src>> {
        self.expr_lvl(0)
    }

    fn decl(&mut self) -> Result<'src, ast::Decl<'src>> {
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
        self.expect(Token::Let)?;
        let name = match self.lexer.next() {
            Token::Ident(name) => name,
            tok => return err("identifier", tok),
        };
        self.expect(Token::Colon)?;
        let ty = self.ty()?;
        Ok(ast::Decl { name, ty })
    }

<<<<<<< HEAD
    fn statement(&mut self) -> Result<'a, ast::Statement<'a>> {
        Ok(match self.lexer.peek() {
=======
    fn statement(&mut self) -> Result<'src, ast::Statement<'src>> {
        Ok(match self.lexer.peek {
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
            Token::Let => ast::Statement::Declaration(self.decl()?),
            Token::OpenBrace => ast::Statement::Scope(self.scope()?),
            _ => ast::Statement::Expression(self.expr()?),
        })
    }

<<<<<<< HEAD
    fn scope_body(&mut self) -> Result<'a, ast::Scope<'a>> {
=======
    fn scope_body(&mut self) -> Result<'src, ast::Scope<'src>> {
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
        let mut statements = Vec::new();
        while self.lexer.peek != Token::CloseBrace && self.lexer.peek != Token::Eof {
            statements.push(self.statement()?);
            self.expect(Token::Semicolon)?;
        }
        Ok(ast::Scope { statements, symbols: typeck::SymbolTable::default() })
    }
    
<<<<<<< HEAD
    fn scope(&mut self) -> Result<'a, ast::Scope<'a>> {
=======
    fn scope(&mut self) -> Result<'src, ast::Scope<'src>> {
>>>>>>> ae928ebbbb97ee5f428e5e033531f1d7e61d3c5c
        self.expect(Token::OpenBrace)?;
        let body = self.scope_body()?;
        self.expect(Token::CloseBrace)?;
        Ok(body)
    }

    pub fn parse(&mut self) -> Result<'src, ast::Ast<'src>> {
        let main = self.scope_body()?;
        self.expect(Token::Eof)?;
        Ok(ast::Ast { main })
    }
}

#[cfg(test)]
fn assert_parse_eq(left: &str, right: &str) {
    assert_eq!(&format!("{}", Parser::new(left).parse().unwrap()), right);
}

#[test]
fn test_unary_expr() {
    assert_parse_eq("++--1++--;", "(++ (-- ((1 ++) --)));\n");
}

#[test]
fn test_ops() {
    assert_parse_eq("a = b += c | d ^ e & f + g == h * i / j <= k;", 
                    "(a = (b += ((c | (d ^ (e & (f + g)))) == (((h * i) / j) <= k))));\n");
}

#[bench]
fn bench_expr(b: &mut ::test::Bencher) {
    b.iter(|| {
        let _ = Parser::new("a = b += c | d ^ e & f + g == h * i / j <= k;").parse();
    })
}
