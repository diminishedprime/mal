extern crate mal;

fn main() {
    loop {
        mal::read::read("user> ")
            .map(|s| mal::parser::parse(&s))
            .map(mal::print::print)
            .expect("This should not happen");
        // .map(mal::print::print)
        // .expect("This should not happen");
    }
}
