use scanner::Scanner;
use std::error::Error;

#[derive(Default)]
pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn run(&mut self, input: &str) -> Result<(), Box<dyn Error>> {
        println!("Received input:\n {}", input);
        let tokens = Scanner::new(input).scan_tokens();
        println!("Tokens: {:?}", tokens);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
