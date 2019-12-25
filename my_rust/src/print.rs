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

pub fn pr_seq(
    seq: impl Iterator<Item = AST>,
    print_readably: bool,
    start: &str,
    end: &str,
    join: &str,
) -> String {
    let strs: Vec<String> = seq.map(|x| x.pr_str(print_readably)).collect();
    format!("{}{}{}", start, strs.join(join), end)
}

fn escape_str(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\\' => "\\\\".to_string(),
            _ => c.to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

impl AST {
    pub fn pr_str(self, print_readably: bool) -> String {
        match self {
            Nil => String::from("nil"),
            Boolean(true) => String::from("true"),
            Boolean(false) => String::from("false"),
            Double(i) => format!("{}", i),
            LString(s) => {
                if print_readably {
                    format!("\"{}\"", escape_str(&s))
                } else {
                    s.clone()
                }
            }
            Symbol(s) => s.clone(),
            ListLike(l) => match l {
                Listy::List(l) => pr_seq(l.into_iter(), print_readably, "(", ")", " "),
                Listy::Vector(l) => pr_seq(l.into_iter(), print_readably, "[", "]", " "),
            },
            Map(_contents) => String::from("not implemented"),
            Keyword(kw) => format!(":{}", kw),
            Closure(f) => format!("#<fn {:p}>", f.0),
            Lambda(_) => format!("#<lambda>"),
        }
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Double(a) => write!(f, "{}", a),
            Symbol(a) => write!(f, "{}", a),
            LString(a) => write!(f, r#""{}""#, a),
            Keyword(a) => write!(f, ":{}", a),
            Boolean(a) => write!(f, "{}", a),
            ListLike(l) => write!(f, "{}", l),
            Map(contents) => {
                write!(f, "{{")?;
                let mut contents = contents.iter().peekable();
                while let Some(val) = contents.next() {
                    if contents.peek().is_some() {
                        write!(f, "{} ", val)?;
                    } else {
                        write!(f, "{}", val)?;
                    }
                }
                write!(f, "}}")
            }
            // List(contents) => {
            //     write!(f, "(")?;
            //     let mut contents = contents.iter().peekable();
            //     while let Some(val) = contents.next() {
            //         if contents.peek().is_some() {
            //             write!(f, "{} ", val)?;
            //         } else {
            //             write!(f, "{}", val)?;
            //         }
            //     }
            //     write!(f, ")")
            // }
            Closure(closure_val) => write!(f, "fn @{:p}", &closure_val),
            Lambda(_) => write!(f, "#<lambda>"),
        }
    }
}
