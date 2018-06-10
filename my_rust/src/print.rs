use std::fmt::Display;

pub fn print<T, U>(result: Result<T, U>) -> ()
where
    T: Display,
    U: Display,
{
    match result {
        Ok(lisp_val) => print!("{}", lisp_val),
        Err(e) => print!("ERR => {}", e),
    }
}

#[cfg(test)]
mod tests {
}
