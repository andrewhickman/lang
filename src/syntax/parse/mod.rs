mod buf;
mod error;

use std::result;

use ast;
use self::error::err;
use super::{Peekable, lex, Stream, Token};

pub type Result<T> = result::Result<T, error::Error>;

macro_rules! try_parse {
    ( $this:ident . $func:ident ) => {
        try_parse!($this, $this.$func())
    };
    ( $this:ident , $expr:expr) => {{
        let save = $this.lexer.save();
        match $expr {
            Ok(v) => { $this.lexer.commit(save); Ok(v) }
            Err(e) => { $this.lexer.restore(save); Err(e) }
        }}
    }
}

pub struct Parser<'a> {
    lexer: buf::StreamBuf<lex::Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(data: &'a str) -> Self {
        Parser {
            lexer: buf::StreamBuf::new(lex::Lexer::new(data)),
        }
    }

    fn expect(&mut self, tok: Token<'a>) -> Result<()> {
        if self.lexer.eat(tok) {
            Ok(())
        } else {
            err(tok, self.lexer.peek())
        }
    }

    pub fn term(&mut self) -> Result<ast::Expr<'a>> {
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

    // postfix unary operators
    pub fn expr1(&mut self) -> Result<ast::Expr<'a>> {
        let mut arg = self.term()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::PlusPlus => Ok(ast::UnaryOp::PostIncr),
            Token::MinusMinus => Ok(ast::UnaryOp::PostDecr),
            _ => Err(()),
        }) {
            arg = ast::Expr::Unary { op, arg: Box::new(arg) };
        }
        Ok(arg)
    }

    // prefix unary operators.
    pub fn expr2(&mut self) -> Result<ast::Expr<'a>> {
        if let Ok(term) = try_parse!(self.expr1) {
            return Ok(term);
        }
        let op = match self.lexer.next() {
            Token::Not => ast::UnaryOp::Not,
            Token::Plus => ast::UnaryOp::Plus,
            Token::Minus => ast::UnaryOp::Minus,
            Token::PlusPlus => ast::UnaryOp::PreIncr,
            Token::MinusMinus => ast::UnaryOp::PreDecr,
            tok => return err("operator", tok),
        };
        Ok(ast::Expr::Unary { op, arg: Box::new(self.expr2()?) })
    }

    // *, /, % operators. Left associative.
    pub fn expr3(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr2()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::Star => Ok(ast::BinaryOp::Mul),
            Token::Slash => Ok(ast::BinaryOp::Div),
            Token::Percent => Ok(ast::BinaryOp::Rem),
            _ => Err(()),
        }) {
            let right = self.expr2()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // +, - operators. Left associative.
    pub fn expr4(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr3()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::Plus => Ok(ast::BinaryOp::Add),
            Token::Minus => Ok(ast::BinaryOp::Sub),
            _ => Err(()),
        }) {
            let right = self.expr3()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // <<, >> operators. Left associative.
    pub fn expr5(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr4()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::Shr => Ok(ast::BinaryOp::Shr),
            Token::Shl => Ok(ast::BinaryOp::Shl),
            _ => Err(()),
        }) {
            let right = self.expr4()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // <, <=, >, >= operators. Left associative.
    pub fn expr6(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr5()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::Lt => Ok(ast::BinaryOp::Lt),
            Token::Le => Ok(ast::BinaryOp::Le),
            Token::Gt => Ok(ast::BinaryOp::Gt),
            Token::Ge => Ok(ast::BinaryOp::Ge),
            _ => Err(()),
        }) {
            let right = self.expr5()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // ==, != operators. Left associative.
    pub fn expr7(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr6()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::EqEq => Ok(ast::BinaryOp::Eq),
            Token::Neq => Ok(ast::BinaryOp::Neq),
            _ => Err(()),
        }) {
            let right = self.expr6()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // & operator. Left associative.
    pub fn expr8(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr7()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::And => Ok(ast::BinaryOp::BitAnd),
            _ => Err(()),
        }) {
            let right = self.expr7()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // ^ operator. Left associative.
    pub fn expr9(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr8()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::Caret => Ok(ast::BinaryOp::BitXor),
            _ => Err(()),
        }) {
            let right = self.expr8()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // | operator. Left associative.
    pub fn expr10(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr9()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::Or => Ok(ast::BinaryOp::BitOr),
            _ => Err(()),
        }) {
            let right = self.expr9()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // && operator. Left associative.
    pub fn expr11(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr10()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::AndAnd => Ok(ast::BinaryOp::And),
            _ => Err(()),
        }) {
            let right = self.expr10()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }
    
    // || operator. Left associative.
    pub fn expr12(&mut self) -> Result<ast::Expr<'a>> {
        let mut left = self.expr11()?;
        while let Ok(op) = try_parse!(self, match self.lexer.next() {
            Token::OrOr => Ok(ast::BinaryOp::Or),
            _ => Err(()),
        }) {
            let right = self.expr11()?;
            left = ast::Expr::Binary { op, args: Box::new((left, right)) };
        }
        Ok(left)
    }

    // = operator. Right associative.
    pub fn expr13(&mut self) -> Result<ast::Expr<'a>> {
        let left = self.expr12()?;
        let op = match try_parse!(self, match self.lexer.next() {
            Token::Eq => Ok(ast::BinaryOp::Assign),
            _ => Err(()),
        }) {
            Ok(op) => op,
            Err(_) => return Ok(left),
        };
        let right = self.expr13()?;
        Ok(ast::Expr::Binary { op, args: Box::new((left, right)) })
    }

    pub fn expr(&mut self) -> Result<ast::Expr<'a>> {
        self.expr13()
    }

    pub fn decl(&mut self) -> Result<ast::Decl<'a>> {
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

    pub fn statement(&mut self) -> Result<ast::Statement<'a>> {
        // dodgy hack
        if self.lexer.eat(Token::Let) {
            Ok(ast::Statement::Declaration(self.decl()?))
        } else {
            Ok(ast::Statement::Expression(self.expr()?))
        }
    }

    pub fn parse(&mut self) -> Result<ast::Ast<'a>> {
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
        data.push_str("x;");
    }
    b.iter(|| {
        let mut lexer = buf::StreamBuf::new(lex::Lexer::new(&data));
        let _save = lexer.save();
        while lexer.next() != Token::Eof {}
    })
}
