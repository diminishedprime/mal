use std::cell::RefCell;
use std::rc::Rc;

#[derive(PartialEq, Debug)]
pub enum MalType {
    Vector(Vec<MalVal>),
    Symbol(String),
    Boolean(bool),
}

pub type MalVal = Rc<MalType>;

#[derive(Debug)]
pub enum EvalError {
    NotImplemented,
}

pub struct EnvType {}

pub type EvalResult<T> = Result<T, EvalError>;

pub type Env = Rc<RefCell<EnvType>>;

pub fn m_bool(b: bool) -> MalVal {
    Rc::new(MalType::Boolean(b))
}

pub fn m_vector(contents: Vec<MalVal>) -> MalVal {
    Rc::new(MalType::Vector(contents))
}

pub fn m_symbol<S: Into<String>>(s: S) -> MalVal {
    Rc::new(MalType::Symbol(s.into()))
}

pub fn m_env() -> Env {
    Rc::new(RefCell::new(EnvType {}))
}
