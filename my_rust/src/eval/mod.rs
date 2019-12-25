pub mod env;
#[cfg(test)]
mod tests;

use crate::ast::list_of;
use crate::ast::vec_of;
use crate::ast::LambdaVal;
use crate::ast::Listy;
use crate::ast::AST;
use crate::ast::AST::ListLike;
use env::util;
use env::Env;
use std::cell::RefCell;
use std::rc::Rc;
use AST::Closure;
use AST::Lambda;
use AST::Map;
use AST::Symbol;

pub type EvalResult<T> = Result<T, String>;

fn split_fn_and_arg(program: Vec<AST>) -> EvalResult<(AST, impl Iterator<Item = AST>)> {
    let mut program_iter = program.into_iter();
    Ok(if let Some(first) = program_iter.next() {
        (first, program_iter)
    } else {
        return Err(format!("cannot split program"));
    })
}

fn eval_fn_star(env: Rc<RefCell<Env>>, forms: impl Iterator<Item = AST>) -> EvalResult<AST> {
    // (fn* [a] (+ a 2))
    let (params, body) = util::two_args("fn*", forms)?;
    let params = params
        .unwrap_list_like()?
        .into_iter()
        .map(|item| item.unwrap_symbol())
        .collect::<EvalResult<Vec<String>>>()?;
    let body = Box::new(body);
    Ok(Lambda(LambdaVal {
        env: env.clone(),
        params,
        body,
    }))
}

fn eval_apply_fn(c: LambdaVal, args: Vec<AST>) -> EvalResult<AST> {
    let LambdaVal { body, env, params } = c;
    if params.len() != args.len() {
        return Err(format!(
            "wrong number of args. Expected: {}, Actual: {}",
            params.len(),
            args.len(),
        ));
    }
    let mut params = params.into_iter();
    let mut args = args
        .into_iter()
        .map(|arg| eval(env.clone(), arg))
        .collect::<EvalResult<Vec<AST>>>()?
        .into_iter();
    env.borrow_mut().new_local();
    while let (Some(param), Some(arg)) = (params.next(), args.next()) {
        env.borrow_mut().set(param, arg)?;
    }
    let result = eval(env.clone(), *body);
    env.borrow_mut().clear_local();
    result
}

pub fn eval_list(env: Rc<RefCell<Env>>, l: Vec<AST>) -> EvalResult<AST> {
    if l.len() == 0 {
        return Ok(list_of(vec![]));
    }
    let (first, rest) = split_fn_and_arg(l)?;
    match first {
        Symbol(s) => {
            let evaled = match s.as_ref() {
                // TODO - this probably shouldn't be necessary to fix the types up.
                "if" | "let*" | "def!" => rest.collect::<Vec<_>>().into_iter(),
                "fn*" => return eval_fn_star(env.clone(), rest),
                _ => rest
                    .map(|part| eval(env.clone(), part))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter(),
            };
            let thing = env.borrow().get(&s)?;
            match thing {
                Closure(val) => val.0(env.clone(), Box::new(evaled)),
                Lambda(l) => eval_apply_fn(l, evaled.collect::<Vec<_>>()),
                _ => return Err(format!("Env value: {} is not a closure", thing)),
            }
        }
        list @ ListLike(Listy::List(_)) => {
            let first = eval(env.clone(), list)?;
            let contents = std::iter::once(first).chain(rest).collect::<Vec<_>>();
            eval(env.clone(), ListLike(Listy::List(contents)))
        }
        Lambda(l) => return eval_apply_fn(l, rest.collect::<Vec<_>>()),
        a => return Err(format!("not implemented: {}", a.typee())),
    }
}

pub fn eval(env: Rc<RefCell<Env>>, program: AST) -> EvalResult<AST> {
    Ok(match program {
        Symbol(s) => return env.borrow().get(&s),
        Map(m) => Map(m
            .into_iter()
            .map(|part| eval(env.clone(), part))
            .collect::<Result<_, _>>()?),
        ListLike(l) => match l {
            Listy::List(l) => eval_list(env.clone(), l)?,
            Listy::Vector(v) => vec_of(
                v.into_iter()
                    .map(|part| eval(env.clone(), part))
                    .collect::<Result<_, _>>()?,
            ),
        },
        Closure(_) => return Err(String::from("not implemented")),
        otherwise => otherwise,
    })
}
