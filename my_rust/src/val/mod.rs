use im::hashmap;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
pub mod malval;

#[derive(Clone)]
pub struct ClosureVal(pub Rc<dyn Fn(Env, Box<dyn Iterator<Item = MalVal>>) -> EvalResult<MalVal>>);

impl PartialEq for ClosureVal {
    fn eq(&self, _other: &Self) -> bool {
        // TODO - figure out if there's a good way to compare closures
        return false;
    }
}

#[derive(Clone)]
pub struct LambdaVal {
    pub env: Env,
    pub params: Vec<String>,
    pub body: MalVal,
}

impl PartialEq for LambdaVal {
    fn eq(&self, _other: &Self) -> bool {
        // TODO - figure out if there's a good way to compare lambdas
        return false;
    }
}

#[derive(PartialEq, Clone)]
pub enum MalType {
    Vector(Vec<MalVal>),
    List(Vec<MalVal>),
    Map(Vec<MalVal>),
    Symbol(String),
    Keyword(String),
    Boolean(bool),
    Double(f64),
    LString(String),
    Nil,
    Closure(ClosureVal),
    Lambda(LambdaVal),
}

pub type MalVal = Rc<MalType>;

#[derive(Debug)]
pub enum EvalError {
    UnevenNumberOfForms,
    ReadError,
    CannotUnwrap(String, MalVal),
    ParseError(String),
    TwoManyArgs,
    TwoFewArgs,
    WrongNumberOfArgs,
    NotDefined(String),
    CannotEvaluate(MalVal),
    InvalidAmp,
}

pub struct EnvType {
    pub env: HashMap<String, MalVal>,
    pub parent: Option<Env>,
}

pub type EvalResult<T> = Result<T, EvalError>;

pub type Env = Rc<RefCell<EnvType>>;

pub fn m_bool(b: bool) -> MalVal {
    Rc::new(MalType::Boolean(b))
}

pub fn m_vector(contents: Vec<MalVal>) -> MalVal {
    Rc::new(MalType::Vector(contents))
}

pub fn m_map(contents: Vec<MalVal>) -> MalVal {
    Rc::new(MalType::Map(contents))
}

pub fn m_nil() -> MalVal {
    Rc::new(MalType::Nil)
}

pub fn m_double(d: f64) -> MalVal {
    Rc::new(MalType::Double(d))
}

pub fn m_list(contents: Vec<MalVal>) -> MalVal {
    Rc::new(MalType::List(contents))
}

pub fn m_symbol<S: Into<String>>(s: S) -> MalVal {
    Rc::new(MalType::Symbol(s.into()))
}

pub fn m_string<S: Into<String>>(s: S) -> MalVal {
    Rc::new(MalType::LString(s.into()))
}

pub fn m_keyword<S: Into<String>>(s: S) -> MalVal {
    Rc::new(MalType::Keyword(s.into()))
}

pub fn m_lambda(env: Env, params: Vec<String>, body: MalVal) -> MalVal {
    Rc::new(MalType::Lambda(LambdaVal { env, params, body }))
}

pub fn m_env(parent: Option<Env>) -> Env {
    // TODO if parent is None, add in the stdlib.
    let env = if parent.is_some() {
        hashmap![]
    } else {
        crate::eval::env::standard_library::with_standard_library()
    };
    Rc::new(RefCell::new(EnvType { env, parent }))
}
