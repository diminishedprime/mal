use std::fmt::Display;

pub fn print<T, U>(result: Result<T, U>) -> ()
where
    T: Display,
    U: Display,
{
    match result {
        Ok(lisp_val) => println!("{}", lisp_val),
        Err(e) => println!("ERR => {}", e),
    }
}

#[cfg(test)]
mod tests {
}
