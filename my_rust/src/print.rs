use crate::val::EvalError;
use crate::val::EvalResult;
use crate::val::MalType;
use crate::val::MalVal;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

pub fn print(a: impl Display) -> EvalResult<()> {
    println!("{}", a);
    Ok(())
}

pub fn pr_seq<'a>(
    seq: &Vec<MalVal>,
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

impl MalType {
    pub fn pr_str(&self, print_readably: bool) -> String {
        match self {
            MalType::Nil => String::from("nil"),
            MalType::Boolean(true) => String::from("true"),
            MalType::Boolean(false) => String::from("false"),
            MalType::Double(i) => format!("{}", i),
            MalType::LString(s) => {
                if s.starts_with("\u{29e}") {
                    format!(":{}", &s[2..])
                } else if print_readably {
                    format!("\"{}\"", escape_str(s))
                } else {
                    s.clone()
                }
            }
            MalType::Symbol(s) => s.clone(),
            MalType::List(l) => pr_seq(l, print_readably, "(", ")", " "),
            MalType::Vector(l) => pr_seq(l, print_readably, "[", "]", " "),
            MalType::Map(l) => pr_seq(l, print_readably, "{", "}", " "),
            MalType::Keyword(kw) => format!(":{}", kw),
            MalType::Closure(f) => format!("#<fn {:p}>", f.0),
            MalType::Lambda(_) => format!("#<lambda>"),
            // MalType::Atom(a) => format!("(atom {})", a),
        }
    }
}

impl Display for MalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.pr_str(true))
    }
}

impl Debug for MalType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::UnevenNumberOfForms => write!(f, "Uneven number of forms"),
            EvalError::ParseError(s) => write!(f, "Parse error: {}", s),
            EvalError::CannotEvaluate(val) => write!(f, "Cannot evaluate form: {}", val),
            EvalError::ReadError => write!(f, "Cannot read input."),
            EvalError::TwoManyArgs => write!(f, "too many args"),
            EvalError::TwoFewArgs => write!(f, "too few args"),
            EvalError::WrongNumberOfArgs => write!(f, "wrong number of args"),
            EvalError::NotDefined(symbol) => {
                write!(f, "{} was not found in the enviroment.", symbol)
            }
            EvalError::CannotUnwrap(t, form) => {
                write!(f, "Cannot unwrap form: {} as a {}", form, t)
            }
        }
    }
}
