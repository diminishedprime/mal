extern crate nom;

pub mod ast;
pub mod eval;
pub mod parser;
pub mod print;
pub mod read;

fn eval_loop() -> Result<(), String> {
    let read_val = read::read("user> ")?;
    let parsed = parser::parse(&read_val)?;
    let _evaled = eval::eval(parsed)?;
    Ok(())
}

pub fn main() -> Result<(), String> {
    loop {
        eval_loop()?;
    }
}
