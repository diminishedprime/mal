use super::LambdaVal;
use super::Listy;
use super::AST;
use crate::env::Env;
use std::cell::RefCell;
use std::rc::Rc;

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
    pub fn m_keyword(k: &str) -> AST {
        AST::Keyword(k.to_string())
    }
    pub fn m_atom(contents: AST) -> AST {
        AST::Atom(Box::new(contents))
    }
    pub fn m_double(d: f64) -> AST {
        AST::Double(d)
    }
    pub fn m_map(c: Vec<AST>) -> AST {
        AST::Map(c)
    }
    pub fn m_vec(d: Vec<AST>) -> AST {
        AST::ListLike(Listy::Vector(d))
    }
    pub fn m_list(d: Vec<AST>) -> AST {
        AST::ListLike(Listy::List(d))
    }
    pub fn m_symbol(s: &str) -> AST {
        AST::Symbol(s.to_string())
    }
    pub fn m_nil() -> AST {
        AST::Nil
    }
}
