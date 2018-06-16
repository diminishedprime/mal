use lisp_val::LispError;
use lisp_val::LispVal;
use lisp_val::LispVal::{
    Atom, Closure, False, Keyword, LString, List, Map, Nil, Number, True, Vector,
};
use std::slice::SliceConcatExt;

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
    let s = args
        .into_iter()
        .map(|a| pr_str_impl(a, true))
        .collect::<Vec<_>>()
        .join(" ");
    Ok(LString(s))
}

pub fn println(args: Vec<LispVal>) -> ValErr {
    let s = args
        .into_iter()
        .map(|a| pr_str_impl(a, true))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{}", s);
    Ok(Nil)
}

pub fn prn(args: Vec<LispVal>) -> ValErr {
    let s = args
        .into_iter()
        .map(|a| pr_str_impl(a, true))
        .collect::<Vec<_>>()
        .join(" ");
    print!("{}", s);
    Ok(Nil)
}

pub fn str(args: Vec<LispVal>) -> ValErr {
    let s = args
        .into_iter()
        .map(|a| pr_str_impl(a, false))
        .collect::<Vec<_>>()
        .join("");
    Ok(LString(s))
}

fn escape_str(s: &str) -> String {
    let mut escaped = String::new();
    escaped.push_str("\"");
    for c in s.chars() {
        match c {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(c),
        }
    }
    escaped.push_str("\"");
    escaped
}

fn pr_str_impl(arg: LispVal, print_readably: bool) -> String {
    match arg {
        LString(s) => format!("{}", {
            if print_readably {
                escape_str(&s)
            } else {
                s
            }
        }),
        Nil => format!("nil"),
        True => format!("true"),
        False => format!("false"),
        Number(n) => format!("{}", n),
        Atom(n) => format!("{}", n),
        Keyword(k) => format!("{}", k),
        Map(m) => format!(
            "{{{}}}",
            m.into_iter()
                .map(|(k, v)| format!(
                    "{} {}",
                    pr_str_impl((*k).clone(), print_readably),
                    pr_str_impl((*v).clone(), print_readably)
                ))
                .collect::<Vec<String>>()
                .join(" ")
        ),
        List(l) => format!(
            "({})",
            l.into_iter()
                .map(|i| pr_str_impl(i, print_readably))
                .collect::<Vec<String>>()
                .join(" ")
        ),
        Vector(v) => format!(
            "[{}]",
            v.into_iter()
                .map(|i| pr_str_impl(i, print_readably))
                .collect::<Vec<String>>()
                .join(" ")
        ),
        Closure(_) => format!("fn"),
    }
}
