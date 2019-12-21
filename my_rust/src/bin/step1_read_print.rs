extern crate mal;

use mal::parser;
use mal::print;
use mal::read;

fn main() -> Result<(), String> {
    loop {
        let read_value = read::read("user> ")?;
        let parsed_value = parser::parse(&read_value)?;
        print::print(&parsed_value)?;
    }
}
