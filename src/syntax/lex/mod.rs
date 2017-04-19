mod peek;

use self::peek::PeekChars;
use super::{Stream, Token};

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

    fn bump(&mut self) {
        self.chars.next();
    }

    // Take an index returned by chars.index() and return the span from it to the current position.
    fn index_data(&self, start: usize) -> &'a str {
        &self.data[start..self.chars.index()]
    }

    // Remove all characters satisfying pref from the stream.
    fn eat_while<F: Fn(char) -> bool>(&mut self, pred: F) {
        while let Some(ch) = self.chars.peek() {
            if !pred(ch) {
                break
            }
            self.bump();
        }
    }

    fn eat_char(&mut self, ch: char) -> bool {
        if self.chars.peek() == Some(ch) {
            self.bump();
            true
        } else {
            false
        }
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
                    ident => Ident(ident),
                }
            },
            ch if ch.is_numeric() => {
                self.eat_while(char::is_numeric);
                Num(self.index_data(start).parse().unwrap())
            }
            '+' => if self.eat_char('+') { PlusPlus } else { Plus },
            '-' => if self.eat_char('-') { MinusMinus } else { Minus },
            '=' => if self.eat_char('=') { EqEq } else { Eq },
            '&' => if self.eat_char('&') { AndAnd } else { And },
            '|' => if self.eat_char('|') { OrOr } else { Or },
            '<' => if self.eat_char('=') { Le } else if self.eat_char('<') { Shl } else { Lt },
            '>' => if self.eat_char('=') { Ge } else if self.eat_char('>') { Shr } else { Gt },
            '!' => if self.eat_char('=') { Neq } else { Not },
            '^' => Caret,
            '*' => Star,
            '/' => Slash,
            '%' => Percent,
            '(' => OpenParen,
            ')' => CloseParen,
            ';' => Semicolon,
            ch => Unexpected(ch),
        }
    }
}

#[test]
fn test_parse_tok() {
    use super::Token::*;

    let mut lexer = Lexer::new("pow hello + 123 - 
    goodbye--          			   +ghe1242232");
    assert_eq!(lexer.next(), Ident("pow"));
    assert_eq!(lexer.next(), Ident("hello"));
    assert_eq!(lexer.next(), Plus);
    assert_eq!(lexer.next(), Num(123));
    assert_eq!(lexer.next(), Minus);
    assert_eq!(lexer.next(), Ident("goodbye"));
    assert_eq!(lexer.next(), MinusMinus);
    assert_eq!(lexer.next(), Plus);
    assert_eq!(lexer.next(), Ident("ghe"));
    assert_eq!(lexer.next(), Num(1242232));
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