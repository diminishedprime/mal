use core::fmt::Debug;
use im::hashmap;
use im::HashMap;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::rc::Rc;
use AST::Closure;
use AST::Double;
use AST::Keyword;
use AST::LString;
use AST::List;
use AST::Map;
use AST::Nil;
use AST::Symbol;
use AST::Vector;

type SymbolVal = String;

#[derive(Clone)]
pub struct ClosureVal(
    pub Rc<dyn Fn(Rc<RefCell<Env>>, Box<dyn Iterator<Item = AST>>) -> Result<AST, String>>,
);

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
    Nil, // Int(i64),
}

pub struct Env {
    pub envs: Vec<HashMap<SymbolVal, AST>>,
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
            Nil => write!(f, "nil"),
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
    pub fn get(&self, key: &SymbolVal) -> Result<AST, String> {
        let mut envs = self.envs.iter().rev();
        while let Some(env) = envs.next() {
            match env.get(key) {
                Some(val) => return Ok(val.clone()),
                None => continue,
            }
        }
        return Err(format!("Key: {} is not in the enviroment.", key));
    }

    pub fn new_local(&mut self) {
        self.envs.push(hashmap![])
    }

    pub fn clear_local(&mut self) {
        if self.envs.len() == 1 {
            panic!("cannot clear the last environment. You probably forgot to call new_local before calling this method.")
        }
        self.envs.remove(self.envs.len() - 1);
    }

    pub fn set(&mut self, key: SymbolVal, value: AST) -> Result<AST, String> {
        let len = self.envs.len();
        // TODO - is there a way to avoid this unsafe?
        unsafe {
            let env = self.envs.get_unchecked_mut(len - 1);
            env.insert(key.clone(), value.clone());
        }
        Ok(value)
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
        let actual = Closure(ClosureVal(Rc::new(|_, mut a| Ok(a.next().unwrap()))));
        assert_eq!(format!("{}", actual).contains("fn @0x"), true)
    }
}
