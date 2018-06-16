use lisp_val::LispError;
use lisp_val::LispVal;
use lisp_val::LispVal::{List, Vector};

type ValErr = Result<LispVal, LispError>;

pub fn add(a: i32, b: i32) -> LispVal {
    LispVal::from(a + b)
}

pub fn subtract(a: i32, b: i32) -> LispVal {
    LispVal::from(a - b)
}

pub fn multiply(a: i32, b: i32) -> LispVal {
    LispVal::from(a * b)
}

pub fn divide(a: i32, b: i32) -> LispVal {
    LispVal::from(a / b)
}

pub fn lt(a: i32, b: i32) -> LispVal {
    LispVal::from(a < b)
}

pub fn lte(a: i32, b: i32) -> LispVal {
    LispVal::from(a <= b)
}

pub fn gt(a: i32, b: i32) -> LispVal {
    LispVal::from(a > b)
}

pub fn gte(a: i32, b: i32) -> LispVal {
    LispVal::from(a >= b)
}

pub fn eq(a: LispVal, b: LispVal) -> bool {
    a == b
}

pub fn is_list(arg: LispVal) -> ValErr {
    Ok(LispVal::from(match arg {
        List(_) => true,
        _ => false,
    }))
}

pub fn is_empty(arg: LispVal) -> ValErr {
    Ok(LispVal::from(match arg {
        Vector(vc) => vc.is_empty(),
        List(lc) => lc.is_empty(),
        _ => false,
    }))
}

pub fn count(arg: LispVal) -> ValErr {
    Ok(LispVal::from(match arg {
        Vector(vc) => vc.len(),
        List(lc) => lc.len(),
        _ => return Err(LispError::TypeMismatch(String::from("countable"), arg)),
    }))
}
