#![feature(slice_patterns)]
#![feature(slice_concat_ext)]
#[macro_use]
extern crate nom;

pub mod parser;
pub mod lisp_val;
pub mod eval;
pub mod print;
pub mod read;

pub fn main() {
    loop {
        read::read("user> ")
            .map(parser::parse)
            .map(eval::eval)
            .map(print::print)
            .expect("This should not happen");
    }
}
