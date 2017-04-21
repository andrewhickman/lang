mod error;

use std::result;

use ast;
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

    pub fn term(&mut self) -> Result<'a, ast::Expr<'a>> {
        match self.lexer.next() {
            Token::Ident(ident) => Ok(ast::Expr::Term(ast::Term::Ident(ident))),
            Token::Num(num) => Ok(ast::Expr::Term(ast::Term::Literal(num))),
            Token::OpenParen => {
                let expr = self.expr()?;
                self.expect(Token::CloseParen)?;
                Ok(expr)
            },
            tok => err("identifier or literal", tok),
        }
    }

    pub fn peek_op(&mut self) -> Result<'a, BinaryOp> {
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
            Token::Lt => BinaryOp { op: Lt, lvl: 8, assoc: Left },
            Token::Le => BinaryOp { op: Le, lvl: 8, assoc: Left },
            Token::Gt => BinaryOp { op: Gt, lvl: 8, assoc: Left },
            Token::Ge => BinaryOp { op: Ge, lvl: 8, assoc: Left },
            Token::EqEq => BinaryOp { op: Eq, lvl: 7, assoc: Left },
            Token::NotEq => BinaryOp { op: Neq, lvl: 7, assoc: Left },
            Token::And => BinaryOp { op: BitAnd, lvl: 6, assoc: Left },
            Token::Caret => BinaryOp { op: BitXor, lvl: 5, assoc: Left },
            Token::Or => BinaryOp { op: BitOr, lvl: 4, assoc: Left },
            Token::AndAnd => BinaryOp { op: And, lvl: 3, assoc: Left },
            Token::OrOr => BinaryOp { op: Or, lvl: 2, assoc: Left },
            Token::Eq => BinaryOp { op: Assign, lvl: 1, assoc: Right },
            tok => return err("operator", tok),
        })
    }

    // postfix unary operators
    pub fn expr1(&mut self) -> Result<'a, ast::Expr<'a>> {
        let mut arg = self.term()?;
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

    // prefix unary operators.
    pub fn expr2(&mut self) -> Result<'a, ast::Expr<'a>> {
        let op = match self.lexer.peek() {
            Token::Not => ast::UnaryOp::Not,
            Token::Plus => ast::UnaryOp::Plus,
            Token::Minus => ast::UnaryOp::Minus,
            Token::PlusPlus => ast::UnaryOp::PreIncr,
            Token::MinusMinus => ast::UnaryOp::PreDecr,
            _ => return self.expr1().map_err(|e| e.exp("prefix operator, identifier or literal")),
        };
        self.lexer.bump();
        Ok(ast::Expr::Unary { op, arg: Box::new(self.expr2()?) })
    }

    pub fn expr_lvl(&mut self, min_lvl: u8) -> Result<'a, ast::Expr<'a>> {
        let mut lhs = self.expr2()?; 
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

    pub fn expr(&mut self) -> Result<'a, ast::Expr<'a>> {
        self.expr_lvl(0)
    }

    pub fn decl(&mut self) -> Result<'a, ast::Decl<'a>> {
        //self.token(Token::Let)?;
        let name = match self.lexer.next() {
            Token::Ident(name) => name,
            tok => return err("identifier", tok),
        };
        let expr = if self.lexer.eat(Token::Eq) {
            Some(self.expr()?)
        } else {
            None
        };
        Ok(ast::Decl { name, expr })
    }

    pub fn statement(&mut self) -> Result<'a, ast::Statement<'a>> {
        // dodgy hack
        if self.lexer.eat(Token::Let) {
            Ok(ast::Statement::Declaration(self.decl()?))
        } else {
            Ok(ast::Statement::Expression(self.expr()?))
        }
    }

    pub fn parse(&mut self) -> Result<'a, ast::Ast<'a>> {
        let mut statements = Vec::new();
        while !self.lexer.eat(Token::Eof) {
            statements.push(self.statement()?);
            self.expect(Token::Semicolon)?;
        }
        Ok(ast::Ast { statements })
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
