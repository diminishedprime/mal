#![feature(slice_patterns)]
#![feature(slice_concat_ext)]

#[macro_use]
extern crate nom;

extern crate regex;

macro_rules! regex {
    ($e:expr) => {
        ::regex::Regex::new($e).unwrap()
    };
}

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
use std::sync::Arc;

fn eval_loop(incoming_env: Arc<Environment>) -> Result<ExecyBoi, LispError> {
    let read_val = read::read("user> ")?;
    let parsed = parser::parse(&read_val)?;
    let evaled = eval::eval(ExecyBoi {
        val: parsed,
        env: incoming_env,
    });
    print::print(&evaled);
    evaled
}

pub fn main() {
    let mut env = Arc::new(lisp_val::Environment::prelude().unwrap());
    loop {
        let evaled = eval_loop(Arc::clone(&env));
        match evaled {
            Ok(execy_boi) => env = execy_boi.env,
            Err(e) => println!("Error:\n{}", e),
        }
    }
}
