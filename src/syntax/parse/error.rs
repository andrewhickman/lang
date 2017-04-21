use std::error::Error as StdError;
use std::fmt;

use super::Token;

#[derive(Debug)]
pub struct Error<'a> {
    message: String,
    found: Token<'a>,
}

impl<'a> Error<'a> {
    pub fn new<E: fmt::Display>(expected: E, found: Token<'a>) -> Self {
        Error { 
            message: format!("expected {}, found token {}", expected, found), found
        }
    }

    pub fn exp<E: fmt::Display>(self, expected: E) -> Self {
        Error::new(expected, self.found)
    }
}

pub fn err<'a, E: fmt::Display, T>(expected: E, found: Token<'a>) -> Result<T, Error<'a>> {
    Err(Error::new(expected, found))
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}.", self.description())
    }
}

impl<'a> StdError for Error<'a> {
    fn description(&self) -> &str {
        &self.message
    }
}