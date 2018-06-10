#![feature(slice_patterns)]
#![feature(slice_concat_ext)]

#[macro_use]
extern crate nom;

#[macro_use]
extern crate im;

pub mod eval;
pub mod lisp_val;
pub mod parser;
pub mod print;
pub mod read;

use lisp_val::Environment;
use lisp_val::ExecyBoi;
use lisp_val::LispError;

fn eval_loop(incoming_env: &Environment) -> Result<ExecyBoi, LispError> {
    let read_val = read::read("user> ")?;
    let parsed = parser::parse(&read_val)?;
    let evaled = eval::eval(ExecyBoi {
        val: parsed,
        env: incoming_env.clone(),
    });
    print::print(&evaled);
    evaled
}

pub fn main() {
    let mut env = lisp_val::Environment::new();
    loop {
        match eval_loop(&env) {
            Ok(execy_boi) => env = execy_boi.env,
            Err(e) => println!("Ignoring these errors for now?\n{:?}", e),
        };
    }
}
