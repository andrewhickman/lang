use std::mem::size_of;
use std::str::Chars;

use super::{Peekable, PeekIter, Stream, Token};

fn ptr_sub<T>(start: *const T, end: *const T) -> usize {
    (end as usize - start as usize) / size_of::<T>()
}

// An iterator over tokens of data.
pub struct Lexer<'a> {
    data: &'a str,
    chars: PeekIter<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a str) -> Self {
        Lexer {
            data: data,
            chars: PeekIter::new(data.chars()),
        }
    }

    fn index(&self) -> usize {
        ptr_sub(self.data.as_ptr(), self.chars.iter.as_str().as_ptr()) 
            - self.chars.peek().map(char::len_utf8).unwrap_or(0)
    }

    // Take an index returned by index() and return the span from it to the current position.
    fn index_data(&self, start: usize) -> &'a str {
        &self.data[start..self.index()]
    }

    // Remove all characters satisfying pred from the stream.
    fn eat_while<F: Fn(char) -> bool>(&mut self, pred: F) {
        self.chars.eat_while(|ch| ch.map(&pred).unwrap_or(false));
    }

    fn eat_char(&mut self, ch: char) -> bool {
        self.chars.eat(Some(ch))
    }
}

impl<'a> Stream for Lexer<'a> {
    type Item = Token<'a>;

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
                self.eat_while(char::is_alphabetic);
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
            '+' => if self.eat_char('=') { PlusEq } else if self.eat_char('+') { PlusPlus } else { Plus },
            '-' => if self.eat_char('=') { MinusEq } else if self.eat_char('-') { MinusMinus } else { Minus },
            '&' => if self.eat_char('=') { AndEq } else if self.eat_char('&') { AndAnd } else { And },
            '|' => if self.eat_char('=') { OrEq } else if self.eat_char('|') { OrOr } else { Or },
            '^' => if self.eat_char('=') { CaretEq } else { Caret },
            '*' => if self.eat_char('=') { StarEq } else { Star },
            '/' => if self.eat_char('=') { SlashEq } else { Slash },
            '%' => if self.eat_char('=') { PercentEq } else { Percent },
            '=' => if self.eat_char('=') { EqEq } else { Eq },
            '!' => if self.eat_char('=') { NotEq } else { Not },
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '<' => if self.eat_char('=') { Le } else if self.eat_char('<') { 
                if self.eat_char('=') { ShlEq } else { Shl }
            } else { 
                Lt 
            },
            '>' => if self.eat_char('=') { Ge } else if self.eat_char('>') { 
                if self.eat_char('=') { ShrEq } else { Shr }
            } else { 
                Gt 
            },
            ';' => Semicolon,
            ':' => Colon,
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
