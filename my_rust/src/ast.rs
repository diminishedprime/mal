use crate::eval::env::Env;
use crate::eval::EvalResult;
use core::fmt::Debug;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::rc::Rc;

pub type SymbolVal = String;

#[derive(Clone)]
pub struct ClosureVal(
    pub Rc<dyn Fn(Rc<RefCell<Env>>, Box<dyn Iterator<Item = AST>>) -> EvalResult<AST>>,
);

#[derive(Clone)]
pub struct LambdaVal {
    pub body: Box<AST>,
    pub env: Rc<RefCell<Env>>,
    pub params: Vec<String>,
}

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
    Lambda(LambdaVal),
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
    pub fn m_lambda(env: Rc<RefCell<Env>>, params: Vec<String>, body: Box<AST>) -> AST {
        AST::Lambda(LambdaVal { env, params, body })
    }
    pub fn m_string(s: &str) -> AST {
        AST::LString(s.to_string())
    }
    pub fn m_boolean(b: bool) -> AST {
        AST::Boolean(b)
    }
    pub fn m_false() -> AST {
        AST::Boolean(false)
    }

    pub fn m_double(d: f64) -> AST {
        AST::Double(d)
    }
    pub fn m_map(c: Vec<AST>) -> AST {
        AST::Map(c)
    }

    pub fn m_list(d: Vec<AST>) -> AST {
        AST::ListLike(Listy::List(d))
    }

    pub fn m_symbol(s: &str) -> AST {
        AST::Symbol(s.to_string())
    }

    pub fn nil() -> AST {
        AST::Nil
    }

    pub fn typee(&self) -> String {
        match self {
            AST::ListLike(_) => String::from("list"),
            AST::Map(_) => String::from("map"),
            AST::Symbol(_) => String::from("symbol"),
            AST::Keyword(_) => String::from("keyword"),
            AST::Double(_) => String::from("double"),
            AST::LString(_) => String::from("string"),
            AST::Closure(_) => String::from("closure"),
            AST::Lambda(_) => String::from("lambda"),
            AST::Boolean(_) => String::from("boolean"),
            AST::Nil => String::from("nil"),
        }
    }

    pub fn unwrap_bool(self) -> EvalResult<bool> {
        match self {
            AST::Boolean(b) => Ok(b),
            a => Err(format!("{} is not a boolean", a)),
        }
    }

    pub fn unwrap_double(self) -> EvalResult<f64> {
        match self {
            AST::Double(d) => Ok(d),
            a => Err(format!("{} is not a double", a)),
        }
    }

    pub fn unwrap_symbol(self) -> EvalResult<String> {
        match self {
            AST::Symbol(s) => Ok(s),
            a => Err(format!("{} is not a symbol", a)),
        }
    }

    pub fn unwrap_string(self) -> EvalResult<String> {
        match self {
            AST::LString(s) => Ok(s),
            a => Err(format!("{} is not a string", a)),
        }
    }

    pub fn unwrap_list(self) -> EvalResult<Vec<AST>> {
        match self {
            AST::ListLike(s) => match s {
                Listy::List(l) => Ok(l),
                a => Err(format!("{} is not a List or Vector", a)),
            },
            a => Err(format!("{} is not a List or Vector", a)),
        }
    }

    pub fn unwrap_list_like(self) -> EvalResult<Vec<AST>> {
        match self {
            AST::ListLike(s) => Ok(match s {
                Listy::List(l) | Listy::Vector(l) => l,
            }),
            a => Err(format!("{} is not a List or Vector", a)),
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            AST::Nil => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            AST::ListLike(Listy::List(_)) => true,
            _ => false,
        }
    }

    pub fn assert_list(&self) -> EvalResult<()> {
        if self.is_list() {
            Ok(())
        } else {
            Err(format!("{} is not a list", self.typee()))
        }
    }

    pub fn is_lambda(&self) -> bool {
        match self {
            AST::Lambda(_) => true,
            _ => false,
        }
    }

    pub fn is_falsy(&self) -> bool {
        match self {
            AST::Nil | AST::Boolean(false) => true,
            _ => false,
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

impl PartialEq for LambdaVal {
    fn eq(&self, _other: &Self) -> bool {
        // TODO - figure out if there's a good way to compare closures
        return false;
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

#[cfg(test)]
mod tests {
    use super::*;

    use AST::*;

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
        let actual = list_of(vec![Symbol(String::from("abc")), Double(1.23)]);
        assert_eq!(format!("{}", actual), String::from("(abc 1.23)"));
    }

    #[test]
    fn display_keyword() {
        let actual = Keyword(String::from("abc"));
        assert_eq!(format!("{}", actual), String::from(":abc"));
    }
}
