pub trait Stream {
    type Item: Copy + PartialEq;

    fn next(&mut self) -> Self::Item;
}

/// An iterator with one-token lookahead.
pub trait Peekable {
    type Item: Copy + PartialEq;

    fn peek(&self) -> Self::Item;
    fn bump(&mut self);

    fn eat(&mut self, item: Self::Item) -> bool {
        if self.peek() == item {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_while<F: FnMut(Self::Item) -> bool>(&mut self, mut pred: F) -> usize {
        let mut count = 0;
        while pred(self.peek()) {
            self.bump();
            count += 1;
        }
        count
    }
}

impl<P: Peekable> Stream for P {
    type Item = P::Item;

    fn next(&mut self) -> Self::Item {
        let item = self.peek();
        self.bump();
        item
    }
}

pub struct PeekStream<S: Stream> {
    pub stream: S,
    peeked: S::Item,
}

impl<S: Stream> PeekStream<S> {
    pub fn new(mut stream: S) -> Self {
        let peeked = stream.next();
        PeekStream { stream, peeked }
    }
}

impl<S: Stream> Peekable for PeekStream<S> {
    type Item = S::Item;

    fn peek(&self) -> Self::Item {
        self.peeked
    }

    fn bump(&mut self) {
        self.peeked = self.stream.next();
    }
}

pub struct PeekIter<I: Iterator>
    where I::Item: Copy + PartialEq
{
    pub iter: I,
    peeked: Option<I::Item>,
}

impl<I: Iterator> PeekIter<I> 
    where I::Item: Copy + PartialEq
{
    pub fn new(mut iter: I) -> Self {
        let peeked = iter.next();
        PeekIter { iter, peeked }
    }
}

impl<I: Iterator> Peekable for PeekIter<I>
    where I::Item: Copy + PartialEq
{
    type Item = Option<I::Item>;

    fn peek(&self) -> Self::Item {
        self.peeked
    }

    fn bump(&mut self) {
        self.peeked = self.iter.next();
    }
}