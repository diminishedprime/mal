use crate::ast::Listy;
use crate::ast::AST;
use crate::ast::AST::Boolean;
use crate::ast::AST::Closure;
use crate::ast::AST::Double;
use crate::ast::AST::Keyword;
use crate::ast::AST::LString;
use crate::ast::AST::Lambda;
use crate::ast::AST::ListLike;
use crate::ast::AST::Map;
use crate::ast::AST::Nil;
use crate::ast::AST::Symbol;
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
            Nil => String::from("nil"),
            Boolean(true) => String::from("true"),
            Boolean(false) => String::from("false"),
            Double(i) => format!("{}", i),
            LString(s) => {
                if s.starts_with("\u{29e}") {
                    format!(":{}", &s[2..])
                } else if print_readably {
                    format!("\"{}\"", escape_str(s))
                } else {
                    s.clone()
                }
            }
            Symbol(s) => s.clone(),
            ListLike(l) => l.pr_str(print_readably),
            Map(contents) => pr_seq(contents.clone(), print_readably, "{", "}", " "),
            Keyword(kw) => format!(":{}", kw),
            Closure(f) => format!("#<fn {:p}>", f.0),
            Lambda(_) => format!("#<lambda>"),
        }
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.pr_str(true))
    }
}
