use crate::ast::Listy;
use crate::ast::AST;
use crate::eval::EvalResult;
use std::fmt;
use std::fmt::Display;

pub fn print(a: impl Display) -> EvalResult<()> {
    println!("{}", a);
    Ok(())
}

pub fn pr_seq<'a>(
    seq: Vec<AST>,
    print_readably: bool,
    start: &str,
    end: &str,
    join: &str,
) -> String {
    let strs: Vec<String> = seq.iter().map(|x| x.pr_str(print_readably)).collect();
    format!("{}{}{}", start, strs.join(join), end)
}

fn escape_str(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '"' => r#"\""#.to_string(),
            '\n' => r#"\\n"#.to_string(),
            '\\' => r#"\"#.to_string(),
            _ => c.to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

impl Listy {
    pub fn pr_str(&self, print_readably: bool) -> String {
        match self {
            Listy::List(l) => pr_seq(l.clone(), print_readably, "(", ")", " "),
            Listy::Vector(l) => pr_seq(l.clone(), print_readably, "[", "]", " "),
        }
    }
}

impl AST {
    pub fn pr_str(&self, print_readably: bool) -> String {
        match self {
            AST::Nil => String::from("nil"),
            AST::Boolean(true) => String::from("true"),
            AST::Boolean(false) => String::from("false"),
            AST::Double(i) => format!("{}", i),
            AST::LString(s) => {
                if s.starts_with("\u{29e}") {
                    format!(":{}", &s[2..])
                } else if print_readably {
                    format!("\"{}\"", escape_str(s))
                } else {
                    s.clone()
                }
            }
            AST::Symbol(s) => s.clone(),
            AST::ListLike(l) => l.pr_str(print_readably),
            AST::Map(contents) => pr_seq(contents.clone(), print_readably, "{", "}", " "),
            AST::Keyword(kw) => format!(":{}", kw),
            AST::Closure(f) => format!("#<fn {:p}>", f.0),
            AST::Lambda(_) => format!("#<lambda>"),
        }
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.pr_str(true))
    }
}
