mod peek;

use self::peek::PeekChars;
use super::{Peekable, Stream, Token};

// An iterator over tokens of data.
pub struct Lexer<'a> {
    data: &'a str,
    chars: PeekChars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a str) -> Self {
        Lexer {
            data: data,
            chars: PeekChars::new(data),
        }
    }

    // Take an index returned by chars.index() and return the span from it to the current position.
    fn index_data(&self, start: usize) -> &'a str {
        &self.data[start..self.chars.index()]
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
        let start = self.chars.index();
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

#[test]
fn test_index() {
    let mut lexer = Lexer::new("sup");

    assert_eq!(lexer.chars.index(), 0);
    assert_eq!(&lexer.data[lexer.chars.index()..], "sup");
    assert_eq!(lexer.chars.next(), Some('s'));

    assert_eq!(lexer.chars.index(), 1);
    assert_eq!(&lexer.data[lexer.chars.index()..], "up");
    assert_eq!(lexer.chars.next(), Some('u'));

    assert_eq!(lexer.chars.index(), 2);
    assert_eq!(&lexer.data[lexer.chars.index()..], "p");
    assert_eq!(lexer.chars.next(), Some('p'));

    assert_eq!(lexer.chars.index(), 3);
    assert_eq!(&lexer.data[lexer.chars.index()..], "");
    assert_eq!(lexer.chars.next(), None);
}
