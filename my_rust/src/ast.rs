use core::fmt::Debug;
use im::hashmap;
use im::HashMap;
use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use AST::Closure;
use AST::Double;
use AST::Keyword;
use AST::LString;
use AST::List;
use AST::Map;
use AST::Symbol;
use AST::Vector;

type SymbolVal = String;

#[derive(Clone)]
pub struct ClosureVal(Rc<dyn Fn(Box<dyn Iterator<Item = AST>>) -> Result<AST, String>>);

#[derive(Clone, PartialEq)]
pub enum AST {
    List(Vec<AST>),
    Vector(Vec<AST>),
    Map(Vec<AST>),
    Symbol(SymbolVal),
    Keyword(String),
    Double(f64),
    LString(String),
    Closure(ClosureVal),
    // Int(i64),
}

pub struct Env {
    functions: HashMap<SymbolVal, ClosureVal>,
}

impl PartialEq for ClosureVal {
    fn eq(&self, _other: &Self) -> bool {
        // TODO - figure out if there's a good way to compare closures
        return false;
    }
}

impl Debug for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Double(a) => write!(f, "{}", a),
            Symbol(a) => write!(f, "{}", a),
            LString(a) => write!(f, r#""{}""#, a),
            Keyword(a) => write!(f, ":{}", a),
            Vector(contents) => {
                write!(f, "[")?;
                let mut contents = contents.iter().peekable();
                while let Some(val) = contents.next() {
                    if contents.peek().is_some() {
                        write!(f, "{} ", val)?;
                    } else {
                        write!(f, "{}", val)?;
                    }
                }
                write!(f, "]")
            }
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
            List(contents) => {
                write!(f, "(")?;
                let mut contents = contents.iter().peekable();
                while let Some(val) = contents.next() {
                    if contents.peek().is_some() {
                        write!(f, "{} ", val)?;
                    } else {
                        write!(f, "{}", val)?;
                    }
                }
                write!(f, ")")
            }
            Closure(closure_val) => write!(f, "fn @{:p}", &closure_val),
        }
    }
}

impl Env {
    pub fn new() -> Self {
        Env {
            functions: hashmap![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_double() {
        let actual = Double(1.23);
        assert_eq!(format!("{}", actual), String::from("1.23"));
    }

    #[test]
    fn display_symbol() {
        let actual = Symbol(String::from("abc"));
        assert_eq!(format!("{}", actual), String::from("abc"));
    }

    #[test]
    fn display_list() {
        let actual = List(vec![Symbol(String::from("abc")), Double(1.23)]);
        assert_eq!(format!("{}", actual), String::from("(abc 1.23)"));
    }

    #[test]
    fn display_keyword() {
        let actual = Keyword(String::from("abc"));
        assert_eq!(format!("{}", actual), String::from(":abc"));
    }

    #[test]
    fn display_closure() {
        let actual = Closure(ClosureVal(Rc::new(|mut a| Ok(a.next().unwrap()))));
        assert_eq!(format!("{}", actual).contains("fn @0x"), true)
    }
}
