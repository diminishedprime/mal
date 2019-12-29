extern crate nom;

pub mod ast;
pub mod eval;
pub mod parser;
pub mod print;
pub mod read;
mod val;

use eval::env;
use eval::EvalResult;

pub fn repl() -> EvalResult<()> {
    let env = env::Env::new()?;
    eval::eval(
        env.clone(),
        parser::parse(
            "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))",
        )?,
    )?;
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
