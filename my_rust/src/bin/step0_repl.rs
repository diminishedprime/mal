extern crate mal;

fn main() {
    loop {
        mal::read::read("user> ")
            .map(|s| print!("{}", s))
            .expect("This should not happen");
        // .map(mal::print::print)
        // .expect("This should not happen");
    }
}
