use crate::env::Env;
use core::fmt::Debug;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use AST::Boolean;
use AST::Closure;
use AST::Double;
use AST::Keyword;
use AST::LString;
use AST::ListLike;
use AST::Map;
use AST::Nil;
use AST::Symbol;

pub type SymbolVal = String;

#[derive(Clone)]
pub struct ClosureVal(
    pub Rc<dyn Fn(Rc<RefCell<Env>>, Box<dyn Iterator<Item = AST>>) -> Result<AST, String>>,
);

#[derive(Clone)]
pub enum Listy {
    List(Vec<AST>),
    Vector(Vec<AST>),
}

// TODO - pull out primitives into their own variant.
#[derive(Clone, PartialEq)]
pub enum AST {
    ListLike(Listy),
    Map(Vec<AST>),
    Symbol(SymbolVal),
    Keyword(String),
    Double(f64),
    LString(String),
    Closure(ClosureVal),
    Boolean(bool),
    Nil, // Int(i64),
}

pub fn list_of(v: Vec<AST>) -> AST {
    AST::ListLike(Listy::List(v))
}

pub fn vec_of(v: Vec<AST>) -> AST {
    AST::ListLike(Listy::Vector(v))
}

impl AST {
    pub fn unwrap_double(self) -> Result<f64, String> {
        match self {
            Double(d) => Ok(d),
            a => Err(format!("{} is not a double", a)),
        }
    }

    pub fn unwrap_symbol(self) -> Result<String, String> {
        match self {
            Symbol(s) => Ok(s),
            a => Err(format!("{} is not a symbol", a)),
        }
    }

    pub fn unwrap_list_like(self) -> Result<Vec<AST>, String> {
        match self {
            ListLike(s) => Ok(match s {
                Listy::List(l) | Listy::Vector(l) => l,
            }),
            a => Err(format!("{} is not a List or Vector", a)),
        }
    }
}

impl PartialEq for Listy {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Listy::List(f), Listy::List(s)) => f == s,
            (Listy::Vector(f), Listy::List(s)) => f == s,
            (Listy::Vector(f), Listy::Vector(s)) => f == s,
            (Listy::List(f), Listy::Vector(s)) => f == s,
        }
    }
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

impl Display for Listy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Listy::Vector(contents) => {
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
            Listy::List(contents) => {
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
        let actual = ListLike(Listy::List(vec![Symbol(String::from("abc")), Double(1.23)]));
        assert_eq!(format!("{}", actual), String::from("(abc 1.23)"));
    }

    #[test]
    fn display_keyword() {
        let actual = Keyword(String::from("abc"));
        assert_eq!(format!("{}", actual), String::from(":abc"));
    }

    #[test]
    fn display_closure() {
        let actual = Closure(ClosureVal(Rc::new(|_, mut a| Ok(a.next().unwrap()))));
        assert_eq!(format!("{}", actual).contains("fn @0x"), true)
    }
}
