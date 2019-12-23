use crate::ast::ClosureVal;
use crate::ast::Env;
use crate::ast::AST;
use crate::ast::AST::Closure;
use crate::ast::AST::Double;
use crate::ast::AST::Symbol;
use im::hashmap;
use std::rc::Rc;

fn plus() -> AST {
    Closure(ClosureVal(Rc::new(|_, a| {
        a.fold(Ok(Double(0.0)), |acc, arg| {
            let acc = acc?;
            if let (Double(acc), Double(arg)) = (acc, &arg) {
                Ok(Double(acc + arg))
            } else {
                Err(format!("Arg: {} was not a number", arg))
            }
        })
    })))
}

fn multiply() -> AST {
    Closure(ClosureVal(Rc::new(|_, a| {
        a.fold(Ok(Double(1.0)), |acc, arg| {
            let acc = acc?;
            if let (Double(acc), Double(arg)) = (acc, &arg) {
                Ok(Double(acc * arg))
            } else {
                Err(format!("Arg: {} was not a number", arg))
            }
        })
    })))
}

fn divide() -> AST {
    Closure(ClosureVal(Rc::new(|_, a| {
        let mut a = a.peekable();
        let first = a
            .next()
            .ok_or(String::from("- requires at least 1 argument"))?;
        if let Double(d) = first {
            if a.peek().is_none() {
                Ok(Double(1.0 / d))
            } else {
                a.fold(Ok(first), |acc, arg| {
                    let acc = acc?;
                    if let (Double(acc), Double(arg)) = (acc, &arg) {
                        Ok(Double(acc / arg))
                    } else {
                        Err(format!("Arg: {} was not a number", arg))
                    }
                })
            }
        } else {
            Err(format!("Arg: {} was not a number", first))
        }
    })))
}

fn subtract() -> AST {
    Closure(ClosureVal(Rc::new(|_, a| {
        let mut a = a.peekable();
        let first = a
            .next()
            .ok_or(String::from("- requires at least 1 argument"))?;
        if let Double(d) = first {
            if a.peek().is_none() {
                Ok(Double(-d))
            } else {
                a.fold(Ok(first), |acc, arg| {
                    let acc = acc?;
                    if let (Double(acc), Double(arg)) = (acc, &arg) {
                        Ok(Double(acc - arg))
                    } else {
                        Err(format!("Arg: {} was not a number", arg))
                    }
                })
            }
        } else {
            Err(format!("Arg: {} was not a number", first))
        }
    })))
}

fn define() -> AST {
    Closure(ClosureVal(Rc::new(|env, mut a| {
        let first = a.next();
        let second = a.next();
        let rest = a.next();
        match first {
            Some(Symbol(s)) => match second {
                Some(val) => match rest {
                    None => env.borrow_mut().set(s, val),
                    _ => Err(String::from("def! can only have two arguments.")),
                },
                _ => Err(String::from("def! requires a second argument")),
            },
            _ => Err(format!("first argument to def! must be a symbol.")),
        }
    })))
}

impl Env {
    pub fn new() -> Self {
        Env(hashmap! {
            String::from("+") => plus(),
            String::from("*") => multiply(),
            String::from("/") => divide(),
            String::from("-") => subtract(),
            String::from("def!") => define()
        })
    }
}
