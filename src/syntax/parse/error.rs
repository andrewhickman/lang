use std::error::Error as StdError;
use std::fmt;

use super::Token;

#[derive(Debug)]
pub struct Error<'src> {
    message: String,
    found: Token<'src>,
}

impl<'src> Error<'src> {
    pub fn new<E: fmt::Display>(expected: E, found: Token<'src>) -> Self {
        Error { 
            message: format!("expected {}, found {}", expected, found), found
        }
    }
}

pub fn err<'src, E: fmt::Display, T>(expected: E, found: Token<'src>) -> Result<T, Error<'src>> {
    Err(Error::new(expected, found))
}

impl<'src> fmt::Display for Error<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}.", self.description())
    }
}

impl<'src> StdError for Error<'src> {
    fn description(&self) -> &str {
        &self.message
    }
}