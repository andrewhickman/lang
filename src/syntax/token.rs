use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Ident(&'a str),
    Num(u32),
    // Binary Operators 
    Star,
    Slash,
    Percent,
    Plus,
    Minus,
    Shr,
    Shl,
    Le,
    Lt,
    Ge,
    Gt,
    EqEq,
    NotEq,
    And,
    Caret,
    Or,
    AndAnd,
    OrOr,
    Eq,
    // (strict) Unary operators
    PlusPlus,
    MinusMinus,
    Not,
    // Keywords
    Let,
    // Delimiters
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    // Separators
    Semicolon,
    // Other
    Colon,
    Eof,
    Unexpected(char),
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match *self {
            Ident(ident) => write!(f, "{}", ident),
            Num(num) => write!(f, "{}", num),
            Plus => write!(f, "+"),
            PlusPlus => write!(f, "++"),
            Minus => write!(f, "-"),
            MinusMinus => write!(f, "--"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Percent => write!(f, "%"),
            OpenParen => write!(f, "("),
            CloseParen => write!(f, ")"),
            OpenBrace => write!(f, "{{"),
            CloseBrace => write!(f, "}}"),
            Eq => write!(f, "="),
            EqEq => write!(f, "=="),
            Lt => write!(f, "<"),
            Le => write!(f, "<="),
            Gt => write!(f, ">"),
            Ge => write!(f, ">="),
            Caret => write!(f, "^"),
            And => write!(f, "&"),
            AndAnd => write!(f, "&&"),
            Or => write!(f, "|"),
            OrOr => write!(f, "||"),
            Not => write!(f, "!"),
            NotEq => write!(f, "!="),
            Shl => write!(f, "<<"),
            Shr => write!(f, ">>"),
            Semicolon => write!(f, ";"),
            Colon => write!(f, ":"),
            Let => write!(f, "let"),
            Eof => write!(f, "<EOF>"),
            Unexpected(ch) => write!(f, "{}", ch),
        }
    }
}