use crate::eval::EvalResult;
use core::fmt::Display;

pub fn print(a: impl Display) -> EvalResult<()> {
    println!("{}", a);
    Ok(())
}
