use core::fmt;
use serde::Serialize;
use std::error::Error;

#[derive(Debug, Serialize)]
pub struct LoxError {
    line: usize,
    details: String,
}

impl LoxError {
    pub fn new(line: usize, msg: impl Into<String>) -> Self {
        Self {
            line,
            details: msg.into(),
        }
    }
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.details)
    }
}

impl Error for LoxError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format() {
        assert_eq!(
            format!("{}", LoxError::new(10, "cool error")),
            "[line 10] Error: cool error"
        );
    }
}
