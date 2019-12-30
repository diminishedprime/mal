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

impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        use MalType::{
            Atom, Boolean, Closure, Double, Keyword, LString, Lambda, List, Map, Nil, Symbol,
            Vector,
        };
        match (self, other) {
            (Nil, Nil) => true,
            (Double(a), Double(b)) => a == b,
            (Boolean(a), Boolean(b)) => a == b,
            (LString(a), LString(b)) => a == b,
            (Symbol(a), Symbol(b)) => a == b,
            (Keyword(a), Keyword(b)) => a == b,

            (Vector(a), Vector(b)) => a == b,
            (Vector(a), List(b)) => a == b,
            (List(a), Vector(b)) => a == b,
            (List(a), List(b)) => a == b,

            (Map(a), Map(b)) => a == b,

            (Atom(a), Atom(b)) => a == b,
            (Closure(a), Closure(b)) => a == b,
            (Lambda(a), Lambda(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub enum MalType {
    Nil,
    Double(f64),
    Boolean(bool),
    LString(String),
    Symbol(String),
    Keyword(String),

    Vector(Vec<MalVal>),
    List(Vec<MalVal>),

    Map(Vec<MalVal>),

    Atom(RefCell<MalVal>),
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
    IOError(String),
    WrongType(String),
}

pub struct EnvType {
    pub env: HashMap<String, MalVal>,
    pub parent: Option<Env>,
}

pub type EvalResult<T> = Result<T, EvalError>;

pub struct Env(Rc<RefCell<EnvType>>);

impl Clone for Env {
    fn clone(&self) -> Self {
        Env(self.0.clone())
    }
}

impl Env {
    pub fn keys(&self) -> Vec<String> {
        self.0.borrow().keys()
    }

    pub fn get(&self, key: &str) -> Option<MalVal> {
        self.0.borrow().get(key)
    }

    pub fn set(&mut self, key: String, value: MalVal) -> EvalResult<MalVal> {
        self.0.borrow_mut().set(key, value)
    }

    pub fn empty() -> Self {
        let parent = None;
        let env = hashmap![];
        Env(Rc::new(RefCell::new(EnvType { env, parent })))
    }

    pub fn with_standard_library() -> Self {
        let parent = None;
        let env = crate::eval::env::standard_library::with_standard_library();
        Env(Rc::new(RefCell::new(EnvType { env, parent })))
    }

    pub fn push(&self) -> Self {
        let parent = Some(self.clone());
        let env = hashmap![];
        Env(Rc::new(RefCell::new(EnvType { env, parent })))
    }
}

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

pub fn m_atom(val: MalVal) -> MalVal {
    Rc::new(MalType::Atom(RefCell::new(val)))
}

pub fn m_lambda(env: Env, params: Vec<String>, body: MalVal) -> MalVal {
    Rc::new(MalType::Lambda(LambdaVal { env, params, body }))
}
