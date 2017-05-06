use std::mem;

pub trait Stream {
    type Item;

    fn next(&mut self) -> Self::Item;

    fn peekable(mut self) -> Peekable<Self> where Self: Sized {
        let peek = self.next();
        Peekable { stream: self, peek }
    }
}

impl<I: Iterator> Stream for I {
    type Item = Option<I::Item>;

    fn next(&mut self) -> Self::Item {
        <Self as Iterator>::next(self)
    }
}

pub struct Peekable<S: Stream> {
    pub stream: S,
    pub peek: S::Item,
}

impl<S: Stream> Peekable<S> {
    pub fn eat(&mut self, item: S::Item) -> bool
        where S::Item: PartialEq
    {
        if self.peek == item {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn eat_while<F: FnMut(S::Item) -> bool>(&mut self, mut pred: F) 
        where S::Item: Copy
    {
        while pred(self.peek) {
            self.next();
        }
    }
}

impl<S: Stream> Stream for Peekable<S> {
    type Item = S::Item;

    fn next(&mut self) -> Self::Item {
        mem::replace(&mut self.peek, self.stream.next())
    }
}