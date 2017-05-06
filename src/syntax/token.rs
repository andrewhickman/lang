use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token<'src> {
    Ident(&'src str),
    Num(u32),
    // Binary Operators 
    Star, Slash, Percent, Plus, Minus, Shr, Shl, Le, Lt, Ge, Gt, EqEq, NotEq, And, Caret, Or,
    AndAnd,
    OrOr,
    // Assignment
    Eq, StarEq, SlashEq, PercentEq, PlusEq, MinusEq, ShrEq, ShlEq, AndEq, CaretEq, OrEq,
    // (strict) Unary operators
    PlusPlus, MinusMinus, Not,
    // Keywords
    Let, Int, Bool, Byte,
    // Delimiters
    OpenParen, CloseParen, OpenBrace, CloseBrace,
    // Separators
    Semicolon, Comma,
    // Other
    Colon, Eof, Arrow, Tilde,
    Unexpected(char),
}

impl<'src> fmt::Display for Token<'src> {
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
            StarEq => write!(f, "*="),
            SlashEq => write!(f, "/="),
            PercentEq => write!(f, "%="),
            PlusEq => write!(f, "+="),
            MinusEq => write!(f, "-="),
            ShrEq => write!(f, ">>="),
            ShlEq => write!(f, "<<="),
            AndEq => write!(f, "&="),
            CaretEq => write!(f, "^="),
            OrEq => write!(f, "|="),
            Semicolon => write!(f, ";"),
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            Arrow => write!(f, "->"),
            Tilde => write!(f, "~"),
            Let => write!(f, "let"),
            Bool => write!(f, "Bool"),
            Int => write!(f, "Int"),
            Byte => write!(f, "Byte"),
            Eof => write!(f, "<EOF>"),
            Unexpected(ch) => write!(f, "{}", ch),
        }
    }
}