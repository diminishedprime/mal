use std::fmt::Display;

pub fn print<T>(t: T) -> ()
where
    T: Display,
{
    println!("{}", t)
}

#[cfg(test)]
mod tests {}
