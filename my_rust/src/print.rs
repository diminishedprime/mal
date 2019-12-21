use core::fmt::Display;

pub fn print(a: impl Display) -> Result<(), String> {
    println!("{}", a);
    Ok(())
}
