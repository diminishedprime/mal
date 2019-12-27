mod is;
mod make;
#[cfg(test)]
mod tests;

use crate::eval::env::Env;
use crate::eval::EvalResult;
use core::fmt::Debug;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::iter;
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
    Atom(Box<AST>),
    Nil, // Int(i64),
}

impl AST {
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
            AST::Atom(_) => String::from("atom"),
            AST::Nil => String::from("nil"),
        }
    }

    pub fn unwrap_bool(self) -> EvalResult<bool> {
        match self {
            AST::Boolean(b) => Ok(b),
            a => Err(format!("{} is not a boolean", a)),
        }
    }

    pub fn unwrap_atom(self) -> EvalResult<Box<AST>> {
        match self {
            AST::Atom(a) => Ok(a),
            a => Err(format!("{} is not a double", a)),
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
    pub fn assert_list(&self) -> EvalResult<()> {
        if self.is_list() {
            Ok(())
        } else {
            Err(format!("{} is not a list", self.typee()))
        }
    }
    pub fn set_atom(&mut self, new: AST) -> EvalResult<()> {
        match self {
            AST::Atom(ref mut a) => {
                **a = new;
                Ok(())
            }
            _ => Err(format!("{} is not an atom.", self)),
        }
    }
    pub fn update_atom(
        &mut self,
        env: Rc<RefCell<Env>>,
        update_fn: AST,
        rest: impl Iterator<Item = AST>,
    ) -> EvalResult<()> {
        let current_self = self.clone().unwrap_atom()?;
        match self {
            AST::Atom(_) => {
                let new_val = crate::eval::eval(
                    env.clone(),
                    AST::m_list(
                        iter::once(update_fn)
                            .chain(iter::once(*current_self))
                            .chain(rest)
                            .collect::<Vec<_>>(),
                    ),
                )?;
                self.set_atom(new_val)?;
                Ok(())
            }
            _ => Err(format!("{} is not an atom.", self)),
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
