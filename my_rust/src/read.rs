use std::io;
use std::io::Write;

pub fn read(prompt: &str) -> Result<String, String> {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    let mut input = String::new();
    if let Ok(_) = io::stdin().read_line(&mut input) {
        // Input of empty string means C-D, empty input is a newline.
        if input == "" {
            ::std::process::exit(0);
        }
        Ok(input)
    } else {
        // What exactly can go wrong here?
        Err(String::from("Something went wrong"))
    }
}

#[cfg(test)]
mod tests {}
