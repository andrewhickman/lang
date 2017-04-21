use std::str::Chars;

use super::Peekable;

// A char iterator with a one-item buffer.
pub struct PeekChars<'a> {
    iter: Chars<'a>,
    peeked: Option<char>,
    index: usize,
}

impl<'a> PeekChars<'a> {
    pub fn new(data: &'a str) -> Self {
        let mut iter = data.chars();
        let peeked = iter.next();
        PeekChars { iter, peeked, index: 0 }
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

impl<'a> Peekable for PeekChars<'a> {
    type Item = Option<char>;

    fn peek(&self) -> Self::Item {
        self.peeked
    }

    fn bump(&mut self) {
        if let Some(ch) = self.peeked {
            self.index += ch.len_utf8();
        }
        self.peeked = self.iter.next();
    }
}

#[test]
fn test_peek() {
    use super::Stream;

    let data = "hi";
    let mut iter = PeekChars::new(data);

    assert_eq!(iter.peek(), Some('h'));
    let start = iter.index;
    assert_eq!(iter.index, 0);
    assert_eq!(iter.next(), Some('h'));

    assert_eq!(iter.peek(), Some('i'));
    assert_eq!(iter.index, 1);
    assert_eq!(iter.next(), Some('i'));

    assert_eq!(iter.peek(), None);
    assert_eq!(iter.index, 2);
    assert_eq!(iter.next(), None);

    assert_eq!(iter.peek(), None);
    let end = iter.index;
    assert_eq!(iter.index, 2);
    assert_eq!(iter.next(), None);

    assert_eq!(data, &data[start..end]);
}