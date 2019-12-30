use crate::val::EvalError;
use crate::val::EvalResult;
use crate::val::MalType;
use crate::val::MalVal;

pub fn unwrap_atom(val: &MalVal) -> EvalResult<MalVal> {
    match &**val {
        MalType::Atom(a) => Ok(a.borrow().clone()),
        _ => Err(EvalError::CannotUnwrap("atom".to_string(), val.clone())),
    }
}

pub fn unwrap_double(val: MalVal) -> EvalResult<f64> {
    match &*val {
        MalType::Double(d) => Ok(*d),
        _ => Err(EvalError::CannotUnwrap("double".to_string(), val.clone())),
    }
}

pub fn unwrap_string(val: MalVal) -> EvalResult<String> {
    match &*val {
        MalType::LString(d) => Ok(d.to_string()),
        _ => Err(EvalError::CannotUnwrap("string".to_string(), val.clone())),
    }
}

pub fn unwrap_symbol(val: MalVal) -> EvalResult<String> {
    match &*val {
        MalType::Symbol(s) => Ok(s.to_string()),
        _ => Err(EvalError::CannotUnwrap("double".to_string(), val.clone())),
    }
}

pub fn unwrap_list(val: MalVal) -> EvalResult<Vec<MalVal>> {
    match &*val {
        MalType::List(s) => Ok(s.clone()),
        MalType::Vector(s) => Ok(s.clone()),
        _ => Err(EvalError::CannotUnwrap("double".to_string(), val.clone())),
    }
}

pub fn assert_list(val: &MalVal) -> EvalResult<()> {
    match &**val {
        MalType::List(_) => Ok(()),
        _ => Err(EvalError::CannotUnwrap("list".to_string(), val.clone())),
    }
}

impl MalType {
    pub fn is_list(&self) -> bool {
        match self {
            MalType::List(_) => true,
            _ => false,
        }
    }
    pub fn is_nil(&self) -> bool {
        match self {
            MalType::Nil => true,
            _ => false,
        }
    }
    pub fn is_lambda(&self) -> bool {
        match self {
            MalType::Lambda(_) => true,
            _ => false,
        }
    }
    pub fn is_atom(&self) -> bool {
        match self {
            MalType::Atom(_) => true,
            _ => false,
        }
    }
    pub fn is_falsy(&self) -> bool {
        match self {
            MalType::Nil => true,
            MalType::Boolean(false) => true,
            _ => false,
        }
    }
    pub fn set_atom(&self, new: MalVal) -> EvalResult<MalVal> {
        match self {
            MalType::Atom(cell) => {
                *cell.borrow_mut() = new;
                Ok(cell.borrow().clone())
            }
            _ => Err(EvalError::WrongType("atom".to_string())),
        }
    }
}
