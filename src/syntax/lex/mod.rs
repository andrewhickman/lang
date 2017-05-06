use std::mem::size_of;
use std::str::Chars;

use super::{Peekable, Stream, Token};

fn ptr_sub<T>(start: *const T, end: *const T) -> usize {
    (end as usize - start as usize) / size_of::<T>()
}

// An iterator over tokens of data.
pub struct Lexer<'src> {
    data: &'src str,
    chars: Peekable<Chars<'src>>,
}

impl<'src> Lexer<'src> {
    pub fn new(data: &'src str) -> Self {
        Lexer {
            data: data,
            chars: <Chars as Stream>::peekable(data.chars()),
        }
    }

    fn index(&self) -> usize {
        ptr_sub(self.data.as_ptr(), self.chars.stream.as_str().as_ptr())
        - self.chars.peek.map(char::len_utf8).unwrap_or(0)
    }

    // Take an index returned by index() and return the span from it to the current position.
    fn index_data(&self, start: usize) -> &'src str {
        &self.data[start..self.index()]
    }

    // Remove all characters satisfying pred from the stream.
    fn eat_while<F: Fn(char) -> bool>(&mut self, pred: F) {
        self.chars.eat_while(|ch| ch.map(&pred).unwrap_or(false));
    }

    fn eat(&mut self, ch: char) -> bool {
        self.chars.eat(Some(ch))
    }
}

impl<'src> Stream for Lexer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Self::Item {
        use super::Token::*;

        self.eat_while(char::is_whitespace);
        let start = self.index();
        let ch = match self.chars.next() {
            Some(ch) => ch,
            None => return Eof,
        };

        match ch {
            ch if ch.is_alphabetic() => {
                self.eat_while(char::is_alphanumeric);
                match self.index_data(start) {
                    "let" => Let,
                    "Bool" => Bool,
                    "Int" => Int,
                    "Byte" => Byte,
                    ident => Ident(ident),
                }
            },
            ch if ch.is_numeric() => {
                self.eat_while(char::is_numeric);
                Num(self.index_data(start).parse().unwrap())
            }
            '+' => if self.eat('=') { PlusEq } else 
                   if self.eat('+') { PlusPlus } else { Plus },
            '-' => if self.eat('=') { MinusEq } else 
                   if self.eat('-') { MinusMinus } else 
                   if self.eat('>') { Arrow } else { Minus },
            '&' => if self.eat('=') { AndEq } else 
                   if self.eat('&') { AndAnd } else { And },
            '|' => if self.eat('=') { OrEq } else 
                   if self.eat('|') { OrOr } else { Or },
            '^' => if self.eat('=') { CaretEq } else { Caret },
            '*' => if self.eat('=') { StarEq } else { Star },
            '/' => if self.eat('=') { SlashEq } else { Slash },
            '%' => if self.eat('=') { PercentEq } else { Percent },
            '=' => if self.eat('=') { EqEq } else { Eq },
            '!' => if self.eat('=') { NotEq } else { Not },
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '<' => if self.eat('=') { Le } else 
                   if self.eat('<') { 
                       if self.eat('=') { ShlEq } else { Shl }
                   } else { Lt },
            '>' => if self.eat('=') { Ge } else 
                   if self.eat('>') { 
                       if self.eat('=') { ShrEq } else { Shr }
                   } else { Gt },
            ';' => Semicolon,
            ',' => Comma,
            ':' => Colon,
            '~' => Tilde,
            ch => Unexpected(ch),
        }
    }
}

#[test]
fn test_parse_tok() {
    use super::Token::*;

    let mut lexer = Lexer::new("abc ᚠᛇᚻ Laȝamon γλῶσσα ಸಂಭವಿಸು 123 * / % + - >> << <= < >= > == != 
                                & ^ | && || = *= /= %= += -= >>= <<= &= ^= |= ++ -- let Int Bool 
                                Byte ( ) { } ; : £");
    assert_eq!(lexer.next(), Ident("abc"));
    assert_eq!(lexer.next(), Ident("ᚠᛇᚻ"));
    assert_eq!(lexer.next(), Ident("Laȝamon"));
    assert_eq!(lexer.next(), Ident("γλῶσσα"));
    assert_eq!(lexer.next(), Ident("ಸಂಭವಿಸು"));
    assert_eq!(lexer.next(), Num(123));
    assert_eq!(lexer.next(), Star);
    assert_eq!(lexer.next(), Slash);
    assert_eq!(lexer.next(), Percent);
    assert_eq!(lexer.next(), Plus);
    assert_eq!(lexer.next(), Minus);
    assert_eq!(lexer.next(), Shr);
    assert_eq!(lexer.next(), Shl);
    assert_eq!(lexer.next(), Le);
    assert_eq!(lexer.next(), Lt);
    assert_eq!(lexer.next(), Ge);
    assert_eq!(lexer.next(), Gt);
    assert_eq!(lexer.next(), EqEq);
    assert_eq!(lexer.next(), NotEq);
    assert_eq!(lexer.next(), And);
    assert_eq!(lexer.next(), Caret);
    assert_eq!(lexer.next(), Or);
    assert_eq!(lexer.next(), AndAnd);
    assert_eq!(lexer.next(), OrOr);
    assert_eq!(lexer.next(), Eq);
    assert_eq!(lexer.next(), StarEq);
    assert_eq!(lexer.next(), SlashEq);
    assert_eq!(lexer.next(), PercentEq);
    assert_eq!(lexer.next(), PlusEq);
    assert_eq!(lexer.next(), MinusEq);
    assert_eq!(lexer.next(), ShrEq);
    assert_eq!(lexer.next(), ShlEq);
    assert_eq!(lexer.next(), AndEq);
    assert_eq!(lexer.next(), CaretEq);
    assert_eq!(lexer.next(), OrEq);
    assert_eq!(lexer.next(), PlusPlus);
    assert_eq!(lexer.next(), MinusMinus);
    assert_eq!(lexer.next(), Let);
    assert_eq!(lexer.next(), Int);
    assert_eq!(lexer.next(), Bool);
    assert_eq!(lexer.next(), Byte);
    assert_eq!(lexer.next(), OpenParen);
    assert_eq!(lexer.next(), CloseParen);
    assert_eq!(lexer.next(), OpenBrace);
    assert_eq!(lexer.next(), CloseBrace);
    assert_eq!(lexer.next(), Semicolon);
    assert_eq!(lexer.next(), Colon);
    assert_eq!(lexer.next(), Unexpected('£'));
    assert_eq!(lexer.next(), Eof);
}
