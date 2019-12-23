extern crate nom;

pub mod ast;
pub mod eval;
mod init;
pub mod parser;
pub mod print;
pub mod read;
use std::cell::RefCell;
use std::rc::Rc;

pub fn repl() -> Result<(), String> {
    let env = Rc::new(RefCell::new(ast::Env::new()));
    loop {
        let loop_result = read::read("user> ")
            .and_then(|read_val| parser::parse(&read_val))
            .and_then(|parsed_val| eval::eval(env.clone(), parsed_val))
            .and_then(print::print);
        if let Err(e) = loop_result {
            print::print(e)?;
        }
    }
}
