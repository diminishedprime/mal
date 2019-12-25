extern crate mal;

use mal::eval::EvalResult;

fn main() -> EvalResult<()> {
    mal::repl()
}
