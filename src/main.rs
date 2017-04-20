#![feature(test)]
extern crate test;

mod ast;
mod syntax;

use std::env::args;

fn main() {
    for arg in args().skip(1) {
        print!("{}", syntax::Parser::new(&arg).parse().unwrap());
    }
}