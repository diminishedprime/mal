extern crate mal;

fn main() {
    loop {
        mal::read::read("user> ")
            .map(Ok::<String, String>)
            .map(mal::print::print)
            .expect("This should not happen");
        // .map(mal::print::print)
        // .expect("This should not happen");
    }
}
