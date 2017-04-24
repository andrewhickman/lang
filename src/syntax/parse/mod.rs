mod error;

use std::result;

use {ast, typeck};
use self::error::{err, Error};
use super::{Peekable, PeekStream, lex, Stream, Token};

pub type Result<'a, T> = result::Result<T, Error<'a>>;

pub struct Parser<'a> {
    lexer: PeekStream<lex::Lexer<'a>>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Associativity {
    Left, Right
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryOp {
    op: ast::BinaryOp,
    lvl: u8,
    assoc: Associativity,
}

impl<'a> Parser<'a> {
    pub fn new(data: &'a str) -> Self {
        Parser {
            lexer: PeekStream::new(lex::Lexer::new(data)),
        }
    }

    fn expect(&mut self, tok: Token<'a>) -> Result<'a, ()> {
        if self.lexer.eat(tok) {
            Ok(())
        } else {
            err(tok, self.lexer.peek())
        }
    }
    
    fn ty(&mut self) -> Result<'a, typeck::Ty> {
        unimplemented!()
    }
    
    pub fn postfix_expr(&mut self, term: ast::Term<'a>) -> Result<'a, ast::Expr<'a>> {
        let mut arg = ast::Expr::Term(term);
        loop {
            let op = match self.lexer.peek() {
                Token::PlusPlus => ast::UnaryOp::PostIncr,
                Token::MinusMinus => ast::UnaryOp::PostDecr,
                _ => return Ok(arg),
            };
            self.lexer.bump();
            arg = ast::Expr::Unary { op, arg: Box::new(arg) };
        }
    }

    pub fn unary_expr(&mut self) -> Result<'a, ast::Expr<'a>> {
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
            OpOrTerm::Op(op) => Ok(ast::Expr::Unary { op, arg: Box::new(self.unary_expr()?) }),
            OpOrTerm::Term(term) => self.postfix_expr(term),
        }
    }

    pub fn expr_lvl(&mut self, min_lvl: u8) -> Result<'a, ast::Expr<'a>> {
        let mut lhs = self.unary_expr()?; 
        while let Ok(BinaryOp { op, lvl, assoc }) = self.peek_op() {
            if min_lvl < lvl {
                self.lexer.bump();
                let rhs = match assoc {
                    Associativity::Left => self.expr_lvl(lvl),
                    Associativity::Right => self.expr_lvl(lvl - 1),
                }?;
                lhs = ast::Expr::Binary { op, args: Box::new((lhs, rhs)) }
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    pub fn peek_op(&mut self) -> Result<'a, BinaryOp> {
        use self::Associativity::{Left, Right};
        use ast::BinaryOp::*;
        Ok(match self.lexer.peek() {
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
            Token::AndAnd => BinaryOp { op: And, lvl: 3, assoc: Left },
            Token::OrOr => BinaryOp { op: Or, lvl: 2, assoc: Left },
            Token::Eq => BinaryOp { op: Assign, lvl: 1, assoc: Right },
            tok => return err("operator", tok),
        })
    }

    pub fn expr(&mut self) -> Result<'a, ast::Expr<'a>> {
        self.expr_lvl(0)
    }

    pub fn decl(&mut self) -> Result<'a, ast::Decl<'a>> {
        self.expect(Token::Let)?;
        let name = match self.lexer.next() {
            Token::Ident(name) => name,
            tok => return err("identifier", tok),
        };
        self.expect(Token::Colon)?;
        let ty = self.ty()?;
        let expr = if self.lexer.eat(Token::Eq) {
            Some(self.expr()?)
        } else {
            None
        };
        Ok(ast::Decl { name, ty, expr })
    }

    pub fn statement(&mut self) -> Result<'a, ast::Statement<'a>> {
        Ok(match self.lexer.peek() {
            Token::Let => ast::Statement::Declaration(self.decl()?),
            Token::OpenBrace => ast::Statement::Scope(self.scope()?),
            _ => ast::Statement::Expression(self.expr()?),
        })
    }

    pub fn scope_body(&mut self) -> Result<'a, ast::Scope<'a>> {
        let mut statements = Vec::new();
        while self.lexer.peek() != Token::CloseBrace && self.lexer.peek() != Token::Eof {
            statements.push(self.statement()?);
            self.expect(Token::Semicolon)?;
        }
        Ok(ast::Scope { statements, symbols: typeck::SymbolTable::default() })
    }
    
    pub fn scope(&mut self) -> Result<'a, ast::Scope<'a>> {
        self.expect(Token::OpenBrace)?;
        let body = self.scope_body()?;
        self.expect(Token::CloseBrace)?;
        Ok(body)
    }

    pub fn parse(&mut self) -> Result<'a, ast::Ast<'a>> {
        let main = self.scope_body()?;
        self.expect(Token::Eof)?;
        Ok(ast::Ast { main })
    }
}

#[cfg(test)]
fn assert_parse_eq(left: &str, right: &str) {
    assert_eq!(Parser::new(left).parse().unwrap(), Parser::new(right).parse().unwrap());
}

#[test]
fn test_unary_expr() {
    assert_parse_eq("+-+1;", "(+ (- (+ 1)));");
}

#[test]
fn test_sum_expr() {
    assert_parse_eq("-1 + 2 * +4;", "((- 1) + (2 * (+ 4)));");
}

#[test]
fn test_factor_expr() {
    assert_parse_eq("1 * 2 / 4;", "((1 * 2) / 4);");
}

#[test]
fn test_assign_expr() {
    assert_parse_eq("x = -2 + +4;", "(x = ((- 2) + (+ 4)));");
}

#[test]
fn test_parens() {
    assert_parse_eq("(2 * (x + 5));", "(2 * (x + 5));");
}

#[test]
fn test_left_associativity() {
    assert_parse_eq("2 * a * 5 * b;", "(((2 * a) * 5) * b);");
}

#[bench]
fn bench_term(b: &mut ::test::Bencher) {
    let mut data = String::new();
    for _ in 0..100 {
        data.push_str("x;");
    }
    b.iter(|| {
        let _ = Parser::new(&data).parse();
    })
}

#[bench]
fn bench_decl(b: &mut ::test::Bencher) {
    let mut data = String::new();
    for _ in 0..100 {
        data.push_str("let x;");
    }
    b.iter(|| {
        let _ = Parser::new(&data).parse();
    })
}

#[bench]
fn bench_lex(b: &mut ::test::Bencher) {
    let mut data = String::new();
    for _ in 0..100 {
        data.push_str("let x;");
    }
    b.iter(|| {
        let mut lexer = PeekStream::new(lex::Lexer::new(&data));
        while lexer.next() != Token::Eof {}
    })
}