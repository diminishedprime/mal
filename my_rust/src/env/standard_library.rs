use crate::ast::list_of;
use crate::ast::AST;
use crate::ast::AST::Boolean;
use crate::ast::AST::Double;
use crate::env::util;
use crate::env::Env;
use crate::eval::eval;
use core::cell::RefCell;
use std::rc::Rc;

pub fn plus(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    Ok(Double(
        args.map(AST::unwrap_double)
            .collect::<Result<Vec<_>, String>>()?
            .into_iter()
            .fold(0.0, |acc, arg| acc + arg),
    ))
}

pub fn multiply(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    Ok(Double(
        args.map(AST::unwrap_double)
            .collect::<Result<Vec<_>, String>>()?
            .into_iter()
            .fold(1.0, |acc, arg| acc * arg),
    ))
}

pub fn divide(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    let (first, rest) = util::one_or_more_args("/", args)?;
    let first = first.unwrap_double()?;
    let mut rest = rest
        .map(AST::unwrap_double)
        .collect::<Result<Vec<_>, String>>()?
        .into_iter()
        .peekable();
    if rest.peek().is_none() {
        Ok(Double(1.0 / first))
    } else {
        Ok(Double(rest.fold(first, |acc, arg| acc / arg)))
    }
}

pub fn subtract(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    let (first, rest) = util::one_or_more_args("-", args)?;
    let first = first.unwrap_double()?;
    let mut rest = rest
        .map(AST::unwrap_double)
        .collect::<Result<Vec<_>, String>>()?
        .into_iter()
        .peekable();
    if rest.peek().is_none() {
        Ok(Double(-first))
    } else {
        Ok(Double(rest.fold(first, |acc, arg| acc - arg)))
    }
}

pub fn define(env: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    let (first, second) = util::two_args("def!", args)?;
    let first = first.unwrap_symbol()?;
    let second = eval(env.clone(), second)?;
    env.borrow_mut().set(first, second)
}

pub fn let_star_helper(
    env: Rc<RefCell<Env>>,
    mut bindings: impl Iterator<Item = AST>,
    exprs: impl Iterator<Item = AST>,
) -> Result<AST, String> {
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

pub fn let_star(env: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    let (bindings, exprs) = util::one_or_more_args("let*", args)?;
    let bindings = bindings.unwrap_list_like()?.into_iter();
    env.borrow_mut().new_local();
    let result = let_star_helper(env.clone(), bindings, exprs);
    env.borrow_mut().clear_local();
    result
}

pub fn eq(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    let (first, rest) = util::one_or_more_args("=", args)?;
    Ok(Boolean(rest.fold(true, |acc, next: AST| {
        if acc == false {
            false
        } else {
            next == first
        }
    })))
}

pub fn list(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    Ok(list_of(args.collect::<Vec<_>>()))
}

pub fn is_list(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    let arg = util::one_arg("list?", args)?;
    arg.unwrap_list().map(|_| Boolean(true))
}

pub fn is_empty(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> Result<AST, String> {
    let arg = util::one_arg("empty?", args)?;
    let first = arg.unwrap_list_like();
    let first = first.iter().next();
    Ok(match first {
        Some(_) => Boolean(true),
        _ => Boolean(false),
    })
}
