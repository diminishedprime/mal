use lisp_val;
use lisp_val::LispError;
use lisp_val::LispVal;
use lisp_val::LispVal::{LString, List, Nil, Number, Vector};

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
        Nil => 0,
        _ => return Err(LispError::TypeMismatch(String::from("countable"), arg)),
    }))
}

pub fn pr_str(args: Vec<LispVal>) -> ValErr {
    let s = lisp_val::print_list(&args, true, "", "", " ");
    Ok(LString(s))
}

pub fn str(args: Vec<LispVal>) -> ValErr {
    let s = lisp_val::print_list(&args, false, "", "", "");
    Ok(LString(s))
}

pub fn prn(args: Vec<LispVal>) -> ValErr {
    println!("{}", lisp_val::print_list(&args, true, "", "", " "));
    Ok(Nil)
}

pub fn println(args: Vec<LispVal>) -> ValErr {
    println!("{}", lisp_val::print_list(&args, false, "", "", " "));
    Ok(Nil)
}
