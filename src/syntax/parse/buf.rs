use std::collections::VecDeque;

use super::{Peekable, Stream};

/// An stream with limited back-tracking capabilities. Note that save(), commit() and restore()
/// should be called in pairs in FILO order.
pub struct StreamBuf<I: Stream> where I::Item: Copy {
    stream: I,
    buf: VecDeque<I::Item>,
    // The index of the current item, relative to buf.
    index: usize,
    // The number of saved indices.
    save_count: usize,
}

#[must_use]
pub struct Save(usize);

impl<S: Stream> StreamBuf<S> {
    pub fn new(mut stream: S) -> Self {
        let mut buf = VecDeque::new();
        buf.push_back(stream.next());
        StreamBuf { stream, buf, index: 0, save_count: 0 }
    }
    
    // Adds a new save-point.
    pub fn save(&mut self) -> Save {
        self.save_count += 1;
        Save(self.index)
    }
    
    // Backtrack the stream to the last save-point.
    pub fn restore(&mut self, Save(index): Save) {
        self.save_count -= 1;
        self.index = index;
    }
    
    // Remove the last save-point.
    pub fn commit(&mut self, _: Save) {
        self.save_count -= 1;
        if self.save_count == 0 {
            while self.index != 0 {
                self.buf.pop_front().unwrap();
                self.index -= 1;
            }
        }
    }

    #[cfg(test)]
    fn assert_invariant(&self) {
        assert!(self.index < self.buf.len());
        assert!(self.save_count != 0 || self.index == 0);
    }
}

impl<S: Stream> Peekable for StreamBuf<S> {
    type Item = S::Item;

    fn peek(&self) -> Self::Item {
        self.buf[self.index]
    }
    
    fn bump(&mut self) {
        self.index += 1;
        if self.index != self.buf.len() {
            debug_assert!(self.index < self.buf.len());
            if self.save_count == 0 {
                // buf.len() cannot be 0 because it is greater than index.
                self.index -= 1;
                self.buf.pop_front().unwrap();
            }
        } else {
            // Only push to the buffer if we have active save-points.
            self.buf.push_back(self.stream.next());
        }
    }
}

#[test]
fn test_iter_buf() {
    struct Range(::std::ops::Range<u32>);

    impl Stream for Range {
        type Item = Option<u32>;

        fn next(&mut self) -> Self::Item {
            self.0.next()
        }
    }

    let mut iter = StreamBuf::new(Range(0..6));

    iter.assert_invariant();
    {
        let save = iter.save();
        iter.assert_invariant();
        assert_eq!(iter.next(), Some(0));
        iter.assert_invariant();
        {
            let save = iter.save();
            iter.assert_invariant();
            assert_eq!(iter.next(), Some(1));
            iter.assert_invariant();
            iter.restore(save);
            iter.assert_invariant();
        }
        iter.commit(save);
        iter.assert_invariant();
    }
    {
        let save = iter.save();
        iter.assert_invariant();
        assert_eq!(iter.next(), Some(1));
        iter.assert_invariant();
        iter.restore(save);
        iter.assert_invariant();
    }
}