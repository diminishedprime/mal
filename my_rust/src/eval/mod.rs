pub mod env;
#[cfg(test)]
mod tests;

use crate::eval::env::util;
use crate::val;
use crate::val::malval::{unwrap_list, unwrap_symbol};
use crate::val::Env;
use crate::val::EvalError;
use crate::val::EvalResult;
use crate::val::LambdaVal;
use crate::val::MalType;
use crate::val::MalVal;

pub fn eval(env: Env, program: MalVal) -> EvalResult<MalVal> {
    let mut program = program;
    let mut env = env;
    'eval_loop: loop {
        match *program {
            MalType::Symbol(ref a) => {
                return Ok(env
                    .borrow()
                    .get(a)
                    .ok_or(EvalError::NotDefined(a.to_string()))?)
            }
            MalType::Vector(ref a) => {
                let new_contents = a
                    .iter()
                    .map(|val| eval(env.clone(), val.clone()))
                    .collect::<EvalResult<Vec<_>>>()?;
                return Ok(val::m_vector(new_contents));
            }
            MalType::Map(ref a) => {
                let new_contents = a
                    .iter()
                    .map(|val| eval(env.clone(), val.clone()))
                    .collect::<EvalResult<Vec<_>>>()?;
                return Ok(val::m_map(new_contents));
            }
            MalType::List(ref a) => {
                let mut contents = a.iter();
                let first = contents.next();
                let rest = contents;
                match first {
                    None => return Ok(program.clone()),
                    Some(first) => match &**first {
                        MalType::List(_) => {
                            let first = eval(env.clone(), first.clone())?;
                            let contents = std::iter::once(first)
                                .chain(rest.map(|m| m.clone()))
                                .collect::<Vec<_>>();
                            program = val::m_list(contents);
                            continue 'eval_loop;
                        }
                        MalType::Lambda(lambda_val) => {
                            let LambdaVal {
                                params,
                                body,
                                env: lambda_env,
                            } = lambda_val;
                            let lambda_env = val::m_env(Some(lambda_env.clone()));
                            let has_rest = params.iter().find(|p| *p == "&").is_some();
                            let args = rest.collect::<Vec<_>>();
                            if !has_rest && params.len() != args.len() {
                                return Err(EvalError::WrongNumberOfArgs);
                            }
                            if has_rest && params.len() - 2 > args.len() {
                                return Err(EvalError::WrongNumberOfArgs);
                            }
                            let mut params = params.iter();
                            let mut args = args
                                .into_iter()
                                .map(|arg| eval(lambda_env.clone(), arg.clone()))
                                .collect::<EvalResult<Vec<_>>>()?
                                .into_iter();
                            while let Some(param) = params.next() {
                                if param == "&" {
                                    let param = params.next();
                                    if param.is_none() {
                                        return Err(EvalError::InvalidAmp);
                                    }
                                    if params.next().is_some() {
                                        return Err(EvalError::InvalidAmp);
                                    }
                                    lambda_env.borrow_mut().set(
                                        param.unwrap().to_string(),
                                        val::m_list(args.collect::<Vec<_>>()),
                                    )?;
                                    break;
                                }
                                // unwrap is safe because we know there's <= params than args
                                let arg = args.next().unwrap();
                                lambda_env.borrow_mut().set(param.clone(), arg)?;
                            }
                            program = body.clone();
                            env = lambda_env.clone();
                            continue 'eval_loop;
                        }
                        MalType::Symbol(symbol) => {
                            match symbol.as_ref() {
                                "if" => {
                                    let (first, second, third) =
                                        util::two_or_three_args("if", rest.map(|n| n.clone()))?;
                                    let first_evaled = eval(env.clone(), first)?;
                                    if first_evaled.is_falsy() {
                                        program = third.unwrap_or(val::m_nil());
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
                                                eval(env.clone(), current.clone())?;
                                            }
                                            (Some(current), None) => {
                                                program = current.clone();
                                                env = env;
                                                continue 'eval_loop;
                                            }
                                            (None, _) => return Ok(val::m_nil()),
                                        }
                                    }
                                }
                                "def!" => {
                                    let (first, second) =
                                        util::two_args("def", rest.map(|r| r.clone()))?;
                                    let first = unwrap_symbol(first)?;
                                    let second = eval(env.clone(), second)?;
                                    return env.borrow_mut().set(first, second);
                                }
                                "fn*" => {
                                    let (params, body) =
                                        util::two_args("fn*", rest.map(|r| r.clone()))?;
                                    let params = unwrap_list(params)?
                                        .iter()
                                        .map(|item| unwrap_symbol(item.clone()))
                                        .collect::<EvalResult<Vec<_>>>()?;
                                    return Ok(val::m_lambda(env.clone(), params, body));
                                }
                                "let*" => {
                                    let (bindings, expr) =
                                        util::two_args("let*", rest.map(|r| r.clone()))?;
                                    let bindings = unwrap_list(bindings)?;
                                    let mut bindings = bindings.iter();
                                    let let_star_env = val::m_env(Some(env));
                                    loop {
                                        let name = bindings.next();
                                        let binding = bindings.next();
                                        match (name, binding) {
                                            (None, None) => break,
                                            (Some(name), Some(expr)) => {
                                                let name = unwrap_symbol(name.clone())?;
                                                let value =
                                                    eval(let_star_env.clone(), expr.clone())?;
                                                let_star_env
                                                    .borrow_mut()
                                                    .set(name.to_string(), value)?;
                                            }
                                            (_, _) => return Err(EvalError::UnevenNumberOfForms),
                                        }
                                    }
                                    program = expr;
                                    env = let_star_env;
                                    continue;
                                }
                                _ => (),
                            };
                            let value = env.borrow().get(symbol);
                            match value {
                                None => return Err(EvalError::NotDefined(symbol.to_string())),
                                Some(value) => match &*value {
                                    MalType::Closure(closure) => {
                                        return closure.0(
                                            env.clone(),
                                            Box::new(
                                                rest.map(|arg| eval(env.clone(), arg.clone()))
                                                    .collect::<EvalResult<Vec<_>>>()?
                                                    .into_iter(),
                                            ),
                                        )
                                    }
                                    MalType::Lambda(_) => {
                                        program = val::m_list(
                                            std::iter::once(value.clone())
                                                .chain(rest.map(|r| r.clone()))
                                                .collect::<Vec<_>>(),
                                        );
                                        continue 'eval_loop;
                                    }
                                    _ => return Err(EvalError::CannotEvaluate(value.clone())),
                                },
                            }
                        }

                        _ => return Err(EvalError::CannotEvaluate(first.clone())),
                    },
                }
            }
            _ => return Ok(program.clone()),
        }
    }
}
