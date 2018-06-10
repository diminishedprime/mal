use std::fmt::Display;

pub fn print<T, U>(result: &Result<T, U>) -> ()
where
    T: Display,
    U: Display,
{
    match result {
        Ok(a) => println!("{}", a),
        Err(e) => eprintln!("{}", e),
    }
}

#[cfg(test)]
mod tests {}
