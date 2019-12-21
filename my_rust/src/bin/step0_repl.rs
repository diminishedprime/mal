extern crate mal;

use mal::print;
use mal::read;

fn main() -> Result<(), String> {
    loop {
        read::read("user> ").and_then(print::print)?;
    }
}
