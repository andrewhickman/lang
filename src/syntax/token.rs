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
    Invalid(char),
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;

        match *self {
            Ident(ident) => write!(f, "identifier: '{}'", ident),
            Num(num) => write!(f, "integer literal: '{}'", num),
            Invalid(ch) => write!(f, "invalid character: '{}'", ch),
            tok => write!(f, "token: '{}'", match tok {
                Plus => "+",
                PlusPlus => "++",
                Minus => "-",
                MinusMinus => "--",
                Star => "*",
                Slash => "/",
                Percent => "%",
                OpenParen => "(",
                CloseParen => ")",
                OpenBrace => "{{",
                CloseBrace => "}}",
                Eq => "=",
                EqEq => "==",
                Lt => "<",
                Le => "<=",
                Gt => ">",
                Ge => ">=",
                Caret => "^",
                And => "&",
                AndAnd => "&&",
                Or => "|",
                OrOr => "||",
                Not => "!",
                NotEq => "!=",
                Shl => "<<",
                Shr => ">>",
                StarEq => "*=",
                SlashEq => "/=",
                PercentEq => "%=",
                PlusEq => "+=",
                MinusEq => "-=",
                ShrEq => ">>=",
                ShlEq => "<<=",
                AndEq => "&=",
                CaretEq => "^=",
                OrEq => "|=",
                Semicolon => ";",
                Comma => ",",
                Colon => ":",
                Arrow => "->",
                Tilde => "~",
                Let => "let",
                Bool => "Bool",
                Int => "Int",
                Byte => "Byte",
                Eof => "<EOF>",
                Ident(_) | Num(_) | Invalid(_) => unreachable!(),
            }),
        }
    }
}