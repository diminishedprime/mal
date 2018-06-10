extern crate mal;

use mal::eval;
use mal::lisp_val;
use mal::parser;
use mal::print;
use mal::read;

fn main() {
    let mut env = lisp_val::Environment::new();
    loop {
        read::read("user> ")
            .map(|input| parser::parse(&input))
            .map(|parsed| eval::eval(&mut env, parsed))
            .map(print::print)
            .expect("This shouldn't happen");
    }
}
