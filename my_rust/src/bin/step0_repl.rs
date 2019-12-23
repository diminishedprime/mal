extern crate mal;

use mal::print;
use mal::read;

use mal::eval::EvalResult;

fn main() -> EvalResult<()> {
    loop {
        read::read("user> ").and_then(print::print)?;
    }
}
