extern crate mal;

fn main() {
    let mut env = mal::lisp_val::Environment::new();
    loop {
        let input = mal::read::read("user> ").unwrap();
        print!("{}", input);
    }
}
