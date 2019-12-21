extern crate nom;

pub mod ast;
pub mod eval;
pub mod parser;
pub mod print;
pub mod read;

pub fn repl() -> Result<(), String> {
    loop {
        let loop_result = read::read("user> ")
            .and_then(|read_val| parser::parse(&read_val))
            .and_then(eval::eval)
            .and_then(print::print);
        if let Err(e) = loop_result {
            print::print(e)?;
        }
    }
}
