extern crate mal;

fn main() {
    loop {
        mal::read::read("user> ")
            .map(mal::parser::parse)
            .map(mal::print::print)
            .map(|_| println!())
            .expect("This should not happen");
        // .map(mal::print::print)
        // .expect("This should not happen");
    }
}
