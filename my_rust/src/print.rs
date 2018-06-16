use lisp_val::LispResult;

pub fn print(result: &LispResult) -> () {
    match result {
        Ok(e) => println!("{}", e.val.pr_str(true)),
        Err(e) => eprintln!("{}", e),
    }
}

#[cfg(test)]
mod tests {}
