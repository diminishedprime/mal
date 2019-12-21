extern crate mal;

use mal::read;

fn main() -> Result<(), String> {
    loop {
        read::read("user> ").map(|s| print!("{}", s))?;
    }
}
