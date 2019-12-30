use crate::val::EvalError;
use crate::val::EvalResult;
use crate::val::MalType;
use crate::val::MalVal;

pub fn unwrap_double(val: MalVal) -> EvalResult<f64> {
    match &*val {
        MalType::Double(d) => Ok(*d),
        _ => Err(EvalError::CannotUnwrap("double".to_string(), val.clone())),
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
}
