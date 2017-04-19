/// Similar to iterator but with the end condition contained within Item.
pub trait Stream {
    type Item;

    fn next(&mut self) -> Self::Item;
}