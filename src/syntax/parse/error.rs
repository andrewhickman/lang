use std::error::Error as StdError;
use std::fmt;

use super::Token;

#[derive(Debug)]
pub struct Error {
    message: String,
}

pub fn err<'a, E: fmt::Display, T>(expected: E, found: Token<'a>) -> Result<T, Error> {
    Err(Error { 
        message: format!("expected {}, found token '{}'", expected, found) 
    })
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}.", self.description())
    }
}

impl StdError for Error {
    fn description(&self) -> &str {
        &self.message
    }
}