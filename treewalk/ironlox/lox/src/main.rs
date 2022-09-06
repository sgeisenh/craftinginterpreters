use clap::Parser;
use interpreter::Interpreter;
use std::error::Error;
use std::fs;
use std::io::{stdin, Write};

#[derive(Parser, Debug)]
#[clap(name = "ironlox")]
#[clap(author = "Samuel Eisenhandler <sgeisenhandler@gmail.com>")]
#[clap(version = "0.1")]
#[clap(about = "Interprets lox programs", long_about = None)]
struct Cli {
    #[clap(value_parser)]
    script: Option<String>,
}

fn run_file(filename: &str) -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string(filename)?;
    Interpreter::new().run(&input)?;
    Ok(())
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut buffer = String::new();
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        std::io::stdout().flush()?;
        if stdin().read_line(&mut buffer)? == 0 {
            break;
        }
        if let Err(e) = interpreter.run(&buffer) {
            println!("{e}");
        }
        buffer.clear();
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();
    match cli.script {
        Some(filename) => run_file(&filename),
        None => run_prompt(),
    }
}
