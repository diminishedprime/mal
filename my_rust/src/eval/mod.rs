pub mod env;
#[cfg(test)]
mod tests;

use crate::ast::LambdaVal;
use crate::ast::Listy;
use crate::ast::AST;
use env::util;
use env::Env;
use std::cell::RefCell;
use std::rc::Rc;

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
    Ok(AST::m_lambda(env.clone(), params, body))
}

pub fn eval(env: Rc<RefCell<Env>>, program: AST) -> EvalResult<AST> {
    let mut program = program;
    let mut env = env;
    'eval_loop: loop {
        match program {
            AST::Symbol(s) => return env.borrow().get(&s),
            AST::Map(m) => {
                return Ok(AST::Map(
                    m.into_iter()
                        .map(|part| eval(env.clone(), part))
                        .collect::<Result<_, _>>()?,
                ))
            }
            AST::Closure(_) => return Err(String::from("not implemented")),
            AST::ListLike(l) => match l {
                Listy::List(l) => {
                    if l.len() == 0 {
                        return Ok(AST::m_list(vec![]));
                    }
                    let (first, rest) = split_fn_and_arg(l)?;
                    match first {
                        AST::Symbol(s) => {
                            let evaled =
                                match s.as_ref() {
                                    "if" => {
                                        let (first, second, third) =
                                            util::two_or_three_args("if", rest)?;
                                        let first_evaled = eval(env.clone(), first)?;
                                        if first_evaled.is_falsy() {
                                            program = third.unwrap_or(AST::Nil);
                                        } else {
                                            program = second;
                                        };
                                        env = env.clone();
                                        continue 'eval_loop;
                                    }
                                    "do" => {
                                        let mut peekable = rest.peekable();
                                        loop {
                                            let current = peekable.next();
                                            let next = peekable.peek();
                                            match (current, next) {
                                                (Some(current), Some(_)) => {
                                                    eval(env.clone(), current)?;
                                                }
                                                (Some(current), None) => {
                                                    program = current;
                                                    env = env;
                                                    continue 'eval_loop;
                                                }
                                                (None, _) => return Ok(AST::Nil),
                                            }
                                        }
                                    }
                                    "let*" => {
                                        let (bindings, expr) = util::two_args("let*", rest)?;
                                        let mut bindings = bindings.unwrap_list_like()?.into_iter();
                                        let let_star_env = Env::with_scope(env.clone());
                                        loop {
                                            let name = bindings.next();
                                            let binding = bindings.next();
                                            match (name, binding) {
                                                (None, None) => break,
                                                (Some(name), Some(expr)) => {
                                                    let name = name.unwrap_symbol()?;
                                                    let value = eval(let_star_env.clone(), expr)?;
                                                    let_star_env
                                                        .borrow_mut()
                                                        .set(name.to_string(), value)?;
                                                }
                                                (_, _) => return Err(String::from(
                                                    "let bindings must be an even number of forms.",
                                                )),
                                            }
                                        }
                                        program = expr;
                                        env = let_star_env;
                                        continue;
                                    }
                                    // TODO - this probably shouldn't be necessary to fix the types up.
                                    "def!" => rest.collect::<Vec<_>>().into_iter(),
                                    "fn*" => return eval_fn_star(env.clone(), rest),
                                    _ => rest
                                        .map(|part| eval(env.clone(), part))
                                        .collect::<Result<Vec<_>, _>>()?
                                        .into_iter(),
                                };
                            let thing = env.borrow().get(&s)?;
                            match thing {
                                AST::Closure(val) => return val.0(env.clone(), Box::new(evaled)),
                                AST::Lambda(l) => {
                                    let LambdaVal {
                                        body,
                                        env: lambda_env,
                                        params,
                                    } = l;
                                    let has_rest = params.iter().find(|p| *p == "&").is_some();
                                    let args = evaled.collect::<Vec<_>>();
                                    if !has_rest && params.len() != args.len() {
                                        return Err(format!("wrong number of args"));
                                    }
                                    if has_rest && params.len() - 2 > args.len() {
                                        return Err(format!("wrong number of args rest"));
                                    }
                                    let mut params = params.into_iter();
                                    let mut args = args
                                        .into_iter()
                                        .map(|arg| eval(lambda_env.clone(), arg))
                                        .collect::<EvalResult<Vec<AST>>>()?
                                        .into_iter();
                                    while let Some(param) = params.next() {
                                        if param == "&" {
                                            let param = params.next();
                                            if param.is_none() {
                                                return Err(String::from("name required after &"));
                                            }
                                            if params.next().is_some() {
                                                return Err(String::from(
                                                    "Only one param can be bound as rest",
                                                ));
                                            }
                                            lambda_env.borrow_mut().set(
                                                param.unwrap(),
                                                AST::ListLike(Listy::List(
                                                    args.collect::<Vec<_>>(),
                                                )),
                                            )?;
                                            break;
                                        }
                                        // unwrap is safe because we know there's <= params than args
                                        let arg = args.next().unwrap();
                                        lambda_env.borrow_mut().set(param, arg)?;
                                    }
                                    program = *body;
                                    env = lambda_env.clone();
                                    continue 'eval_loop;
                                }
                                _ => return Err(format!("Env value: {} is not a closure", thing)),
                            }
                        }
                        list @ AST::ListLike(Listy::List(_)) => {
                            let first = eval(env.clone(), list)?;
                            let contents = std::iter::once(first).chain(rest).collect::<Vec<_>>();
                            return eval(env.clone(), AST::ListLike(Listy::List(contents)));
                        }
                        AST::Lambda(l) => {
                            let LambdaVal {
                                body,
                                env: lambda_env,
                                params,
                            } = l;
                            let has_rest = params.iter().find(|p| *p == "&").is_some();
                            let args = rest.collect::<Vec<_>>();
                            if !has_rest && params.len() != args.len() {
                                return Err(format!("wrong number of args"));
                            }
                            if has_rest && params.len() - 2 > args.len() {
                                return Err(format!("wrong number of args rest"));
                            }
                            let mut params = params.into_iter();
                            let mut args = args
                                .into_iter()
                                .map(|arg| eval(lambda_env.clone(), arg))
                                .collect::<EvalResult<Vec<AST>>>()?
                                .into_iter();
                            while let Some(param) = params.next() {
                                if param == "&" {
                                    let param = params.next();
                                    if param.is_none() {
                                        return Err(String::from("name required after &"));
                                    }
                                    if params.next().is_some() {
                                        return Err(String::from(
                                            "Only one param can be bound as rest",
                                        ));
                                    }
                                    lambda_env.borrow_mut().set(
                                        param.unwrap(),
                                        AST::ListLike(Listy::List(args.collect::<Vec<_>>())),
                                    )?;
                                    break;
                                }
                                // unwrap is safe because we know there's <= params than args
                                let arg = args.next().unwrap();
                                lambda_env.borrow_mut().set(param, arg)?;
                            }
                            program = *body;
                            env = lambda_env.clone();
                            continue 'eval_loop;
                        }
                        a => return Err(format!("not implemented: {}", a.typee())),
                    }
                }
                Listy::Vector(v) => {
                    return Ok(AST::m_vec(
                        v.into_iter()
                            .map(|part| eval(env.clone(), part))
                            .collect::<Result<_, _>>()?,
                    ))
                }
            },
            otherwise => return Ok(otherwise),
        }
    }
}

pub fn eval_2(
    _env: crate::val::Env,
    program: crate::val::MalVal,
) -> crate::val::EvalResult<crate::val::MalVal> {
    let evaluated = match *program {
        crate::val::MalType::Vector(ref a) => {
            let new_contents = a
                .iter()
                .map(|val| eval_2(_env.clone(), val.clone()))
                .collect::<crate::val::EvalResult<Vec<_>>>()?;
            crate::val::m_vector(new_contents)
        }
        _ => program.clone(),
    };
    Ok(evaluated)
}
