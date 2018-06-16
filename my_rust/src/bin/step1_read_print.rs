extern crate mal;

// use mal::eval;
use mal::lisp_val::{Environment, ExecyBoi};
use mal::parser;
use mal::print;
use mal::read;
use std::sync::Arc;

fn main() {
    // let mut env = lisp_val::Environment::new();
    loop {
        read::read("user> ")
            .map(|input| parser::parse(&input))
        // .map(|parsed| eval::eval(&mut env, parsed))
            .map(|s| {
                match s {
                    Ok(s) => {
                        print::print(&Ok(ExecyBoi {
                            val: s,
                            env: Arc::new(Environment::new()),
                        }))
                    },
                    Err(e) => {
                        print::print(&Err(e))
                    },
                }
            })
            .expect("This shouldn't happen");
    }
}
