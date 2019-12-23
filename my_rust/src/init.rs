use crate::ast::ClosureVal;
use crate::ast::Env;
use crate::ast::AST;
use crate::ast::AST::Boolean;
use crate::ast::AST::Closure;
use crate::ast::AST::Double;
use crate::ast::AST::List;
use crate::ast::AST::Symbol;
use crate::ast::AST::Vector;
use crate::eval::eval;
use core::cell::RefCell;
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

fn eval_let_star(env: Rc<RefCell<Env>>, l: Vec<AST>, expr: Option<AST>) -> Result<AST, String> {
    let mut l = l.into_iter();
    // TODO - make a new function called new_local & add functions
    // for setting to local_env instead of global.
    env.borrow_mut().new_local();
    let local_env = env.clone();
    loop {
        let name = l.next();
        let binding = l.next();
        match (name, binding) {
            (None, None) => break,
            (Some(Symbol(name)), Some(expr)) => {
                let value = eval(local_env.clone(), expr)?;
                local_env.borrow_mut().set(name.to_string(), value)?;
            }
            (Some(_), Some(_)) => return Err(String::from("let bindings must be symbols")),
            (Some(_), None) => {
                return Err(String::from(
                    "let bindings must be an even number of forms.",
                ))
            }
            (None, Some(_)) => panic!("this shouldn't be able to happen"),
        }
    }
    let result = match expr {
        Some(expr) => eval(local_env, expr),
        None => Ok(AST::Nil),
    };
    env.borrow_mut().clear_local();
    result
}

fn let_star() -> AST {
    Closure(ClosureVal(Rc::new(|env, mut a| {
        let bindings = a.next();
        let expr = a.next();
        match (bindings, expr) {
            (Some(List(l)), expr) => eval_let_star(env.clone(), l, expr),
            (Some(Vector(l)), expr) => eval_let_star(env.clone(), l, expr),
            _ => Err(format!("first argument to def! must be a symbol.")),
        }
    })))
}

fn eq() -> AST {
    Closure(ClosureVal(Rc::new(|_, mut a| {
        // TODO - make a helper function for asserting at least 1 arg.
        let first = a
            .next()
            .ok_or(String::from("= requires at least 1 argument."))?;
        Ok(Boolean(a.fold(true, |acc, next: AST| {
            if acc == false {
                false
            } else {
                next == first
            }
        })))
    })))
}

impl Env {
    pub fn new() -> Self {
        Env {
            envs: vec![hashmap! {
                String::from("+") => plus(),
                String::from("*") => multiply(),
                String::from("/") => divide(),
                String::from("-") => subtract(),
                String::from("def!") => define(),
                String::from("let*") => let_star(),
                String::from("=") => eq()
            }],
        }
    }
}
