use std::str::Chars;

// A char iterator with a one-item buffer.
pub struct PeekChars<'a> {
    iter: Chars<'a>,
    peek: Option<char>,
    index: usize,
}

impl<'a> PeekChars<'a> {
    pub fn new(data: &'a str) -> Self {
        let mut iter = data.chars();
        let peek = iter.next();
        PeekChars {
            iter: iter,
            peek: peek,
            index: 0,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn peek(&self) -> Option<char> {
        self.peek
    }
}

impl<'a> Iterator for PeekChars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.peek;
        self.peek = self.iter.next();
        if let Some(ch) = ret {
            self.index += ch.len_utf8();
        }
        ret
    }
}

#[test]
fn test_peek() {
    let data = "hi";
    let mut iter = PeekChars::new(data);

    assert_eq!(iter.peek, Some('h'));
    let start = iter.index;
    assert_eq!(iter.index, 0);
    assert_eq!(iter.next(), Some('h'));

    assert_eq!(iter.peek, Some('i'));
    assert_eq!(iter.index, 1);
    assert_eq!(iter.next(), Some('i'));

    assert_eq!(iter.peek, None);
    assert_eq!(iter.index, 2);
    assert_eq!(iter.next(), None);

    assert_eq!(iter.peek, None);
    let end = iter.index;
    assert_eq!(iter.index, 2);
    assert_eq!(iter.next(), None);

    assert_eq!(data, &data[start..end]);
}