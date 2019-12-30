extern crate nom;

pub mod eval;
pub mod parser;
pub mod print;
pub mod read;
pub mod val;

use val::EvalResult;

pub fn repl() -> EvalResult<()> {
    let env = crate::val::m_env(None);
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
