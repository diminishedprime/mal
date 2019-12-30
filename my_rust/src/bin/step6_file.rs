extern crate mal;

use mal::val::EvalResult;

fn main() -> EvalResult<()> {
    mal::repl()
}
