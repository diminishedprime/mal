use lisp_val::LispError;
use lisp_val::LispVal;
use lisp_val::LispVal::{List, Number, Vector};

type ValErr = Result<LispVal, LispError>;

pub fn add(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(match (&a, &b) {
        (Number(a), Number(b)) => a + b,
        _ => {
            return Err(LispError::TypeMismatch(
                String::from("number"),
                a.clone().clone(),
            ))
        }
    }))
}

pub fn subtract(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(match (&a, &b) {
        (Number(a), Number(b)) => a - b,
        _ => return Err(LispError::TypeMismatch(String::from("number"), a.clone())),
    }))
}

pub fn multiply(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(match (&a, &b) {
        (Number(a), Number(b)) => a * b,
        _ => return Err(LispError::TypeMismatch(String::from("number"), a.clone())),
    }))
}

pub fn divide(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(match (&a, &b) {
        (Number(a), Number(b)) => a / b,
        _ => return Err(LispError::TypeMismatch(String::from("number"), a.clone())),
    }))
}

pub fn lt(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(match (&a, &b) {
        (Number(a), Number(b)) => a < b,
        _ => return Err(LispError::TypeMismatch(String::from("number"), a.clone())),
    }))
}

pub fn lte(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(match (&a, &b) {
        (Number(a), Number(b)) => a <= b,
        _ => return Err(LispError::TypeMismatch(String::from("number"), a.clone())),
    }))
}

pub fn gt(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(match (&a, &b) {
        (Number(a), Number(b)) => a > b,
        _ => return Err(LispError::TypeMismatch(String::from("number"), a.clone())),
    }))
}

pub fn gte(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(match (&a, &b) {
        (Number(a), Number(b)) => a >= b,
        _ => return Err(LispError::TypeMismatch(String::from("number"), a.clone())),
    }))
}

pub fn eq(a: LispVal, b: LispVal) -> ValErr {
    Ok(LispVal::from(a == b))
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
