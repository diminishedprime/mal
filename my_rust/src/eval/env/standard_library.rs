use crate::ast::list_of;
use crate::ast::ClosureVal;
use crate::ast::SymbolVal;
use crate::ast::AST;
use crate::ast::AST::Boolean;
use crate::ast::AST::Closure;
use crate::ast::AST::Double;
use crate::eval::env::util;
use crate::eval::env::Env;
use crate::eval::eval;
use crate::eval::EvalResult;
use core::cell::RefCell;
use im::hashmap;
use im::HashMap;
use std::rc::Rc;

pub fn with_standard_library() -> Vec<HashMap<SymbolVal, AST>> {
    vec![hashmap! {
        String::from("+") => Closure(ClosureVal(Rc::new(plus))),
        String::from("*") => Closure(ClosureVal(Rc::new(multiply))),
        String::from("/") => Closure(ClosureVal(Rc::new(divide))),
        String::from("-") => Closure(ClosureVal(Rc::new(subtract))),
        String::from("def!") => Closure(ClosureVal(Rc::new(define))),
        String::from("let*") => Closure(ClosureVal(Rc::new(let_star))),
        String::from("=") => Closure(ClosureVal(Rc::new(eq))),
        String::from("list") => Closure(ClosureVal(Rc::new(list))),
        String::from("list?") => Closure(ClosureVal(Rc::new(is_list))),
        String::from("empty?") => Closure(ClosureVal(Rc::new(is_empty))),
        String::from("count") => Closure(ClosureVal(Rc::new(count))),
        String::from("do") => Closure(ClosureVal(Rc::new(doo))),
    }]
}

pub fn plus(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    Ok(Double(
        args.map(AST::unwrap_double)
            .collect::<EvalResult<Vec<_>>>()?
            .into_iter()
            .fold(0.0, |acc, arg| acc + arg),
    ))
}

pub fn multiply(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    Ok(Double(
        args.map(AST::unwrap_double)
            .collect::<EvalResult<Vec<_>>>()?
            .into_iter()
            .fold(1.0, |acc, arg| acc * arg),
    ))
}

pub fn divide(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args("/", args)?;
    let first = first.unwrap_double()?;
    let mut rest = rest
        .map(AST::unwrap_double)
        .collect::<EvalResult<Vec<_>>>()?
        .into_iter()
        .peekable();
    if rest.peek().is_none() {
        Ok(Double(1.0 / first))
    } else {
        Ok(Double(rest.fold(first, |acc, arg| acc / arg)))
    }
}

pub fn subtract(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args("-", args)?;
    let first = first.unwrap_double()?;
    let mut rest = rest
        .map(AST::unwrap_double)
        .collect::<EvalResult<Vec<_>>>()?
        .into_iter()
        .peekable();
    if rest.peek().is_none() {
        Ok(Double(-first))
    } else {
        Ok(Double(rest.fold(first, |acc, arg| acc - arg)))
    }
}

pub fn define(env: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, second) = util::two_args("def!", args)?;
    let first = first.unwrap_symbol()?;
    let second = eval(env.clone(), second)?;
    env.borrow_mut().set(first, second)
}

pub fn let_star_helper(
    env: Rc<RefCell<Env>>,
    mut bindings: impl Iterator<Item = AST>,
    exprs: impl Iterator<Item = AST>,
) -> EvalResult<AST> {
    loop {
        let name = bindings.next();
        let binding = bindings.next();
        match (name, binding) {
            (None, None) => break,
            (Some(name), Some(expr)) => {
                let name = name.unwrap_symbol()?;
                let value = eval(env.clone(), expr)?;
                env.borrow_mut().set(name.to_string(), value)?;
            }
            (_, None) => {
                return Err(String::from(
                    "let bindings must be an even number of forms.",
                ))
            }
            (None, Some(_)) => panic!("this shouldn't be able to happen"),
        }
    }
    exprs.fold(Ok(AST::Nil), |last, next| {
        last?;
        eval(env.clone(), next)
    })
}

pub fn let_star(env: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (bindings, exprs) = util::one_or_more_args("let*", args)?;
    let bindings = bindings.unwrap_list_like()?.into_iter();
    env.borrow_mut().new_local();
    let result = let_star_helper(env.clone(), bindings, exprs);
    env.borrow_mut().clear_local();
    result
}

pub fn eq(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args("=", args)?;
    Ok(Boolean(rest.fold(true, |acc, next: AST| {
        if acc == false {
            false
        } else {
            next == first
        }
    })))
}

pub fn list(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    Ok(list_of(args.collect::<Vec<_>>()))
}

pub fn is_list(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("list?", args)?;
    Ok(Boolean(arg.is_list()))
}

pub fn is_empty(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("empty?", args)?;
    let contents = arg.unwrap_list_like()?;
    Ok(Boolean(contents.is_empty()))
}

pub fn count(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("empty?", args)?;
    let arg = arg.unwrap_list_like()?;
    Ok(Double(arg.len() as f64))
}

pub fn doo(env: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    args.fold(Ok(AST::Nil), |last, expr| {
        last?;
        eval(env.clone(), expr)
    })
}
