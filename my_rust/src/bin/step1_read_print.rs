extern crate mal;

use mal::parser;
use mal::print;
use mal::read;

use mal::eval::EvalResult;

fn main() -> EvalResult<()> {
    loop {
        let read_value = read::read("user> ")?;
        let parsed_value = parser::parse(&read_value);
        match parsed_value {
            Ok(p) => print::print(&p)?,
            Err(_) => print::print("frown")?,
        };
    }
}
