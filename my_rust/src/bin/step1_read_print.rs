extern crate mal;

use mal::parser;
use mal::read;

use mal::eval::EvalResult;

fn main() -> EvalResult<()> {
    loop {
        let read_value = read::read("user> ")?;
        let parsed_value = parser::parse(&read_value);
        match parsed_value {
            Ok(parsed_value) => println!("{}", parsed_value.pr_str(true)),
            Err(_) => println!("{:?}", parsed_value),
        }
    }
}
