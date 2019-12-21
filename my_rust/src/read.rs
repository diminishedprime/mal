use std::io;
use std::io::Write;
use std::process;

pub fn read(prompt: &str) -> Result<String, String> {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_ok() {
        // Input of empty string means C-D, empty input is a newline.
        if input == "" {
            process::exit(0);
        }
        Ok(input)
    } else {
        Err("could not read from stdin for some reason".to_string())
    }
}
