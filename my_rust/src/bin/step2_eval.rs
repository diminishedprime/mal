extern crate mal;

fn main() {
    loop {
        mal::read::read("user> ")
            .map(mal::parser::parse)
            .map(mal::eval::eval)
            .map(mal::print::print)
            .expect("This should not happen");
        // .map(mal::print::print)
        // .expect("This should not happen");
    }
}