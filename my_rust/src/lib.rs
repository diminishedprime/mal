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

pub fn main() {
    let mut env = lisp_val::Environment::new();
    loop {
        read::read("user> ")
            .map(|input| parser::parse(&input))
            .map(|parsed| {
                eval::eval(lisp_val::ExecyBoi {
                    val: parsed.unwrap(),
                    env: env.clone(),
                })
            })
            .map(|last| {
                let last = last.unwrap();
                env = last.env;
                last.val
            })
            .map(print::print)
            .expect("This shouldn't happen");
    }
}
