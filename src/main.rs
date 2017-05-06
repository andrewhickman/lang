#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

mod ast;
mod syntax;
mod typeck;

use std::env::args;

fn main() {
    for arg in args().skip(1) {
        match syntax::Parser::new(&arg).parse() {
            Ok(mut ast) => {
                print!("{}", ast);
                match typeck::TyChecker::new().ast(&mut ast) {
                    Ok(_) => (),
                    Err(msg) => println!("Error: {}.", msg),
                }
            }
            Err(msg) => print!("{}", msg),
        }
    }
}