pub mod env;
#[cfg(test)]
mod tests;

use crate::eval::env::util;
use crate::val;
use crate::val::malval::{unwrap_list, unwrap_symbol};
use crate::val::Env;
use crate::val::EvalError;
use crate::val::EvalResult;
use crate::val::MalType;
use crate::val::MalVal;

// fn split_fn_and_arg(program: Vec<AST>) -> EvalResult<(AST, impl Iterator<Item = AST>)> {
//     let mut program_iter = program.into_iter();
//     Ok(if let Some(first) = program_iter.next() {
//         (first, program_iter)
//     } else {
//         return Err(format!("cannot split program"));
//     })
// }

// pub fn eval(env: Rc<RefCell<Env>>, program: AST) -> EvalResult<AST> {
//     let mut program = program;
//     let mut env = env;
//     'eval_loop: loop {
//         match program {
//             AST::Symbol(s) => return env.borrow().get(&s),
//             AST::Map(m) => {
//                 return Ok(AST::Map(
//                     m.into_iter()
//                         .map(|part| eval(env.clone(), part))
//                         .collect::<Result<_, _>>()?,
//                 ))
//             }
//             AST::Closure(_) => return Err(String::from("not implemented")),
//             AST::ListLike(l) => match l {
//                 Listy::List(l) => {
//                     if l.len() == 0 {
//                         return Ok(AST::m_list(vec![]));
//                     }
//                     let (first, rest) = split_fn_and_arg(l)?;
//                     match first {
//                         AST::Symbol(s) => {
//                             let evaled =
//                                 match s.as_ref() {
//                                     "if" => {
//                                         let (first, second, third) =
//                                             util::two_or_three_args("if", rest)?;
//                                         let first_evaled = eval(env.clone(), first)?;
//                                         if first_evaled.is_falsy() {
//                                             program = third.unwrap_or(AST::Nil);
//                                         } else {
//                                             program = second;
//                                         };
//                                         env = env.clone();
//                                         continue 'eval_loop;
//                                     }
//                                     "do" => {
//                                         let mut peekable = rest.peekable();
//                                         loop {
//                                             let current = peekable.next();
//                                             let next = peekable.peek();
//                                             match (current, next) {
//                                                 (Some(current), Some(_)) => {
//                                                     eval(env.clone(), current)?;
//                                                 }
//                                                 (Some(current), None) => {
//                                                     program = current;
//                                                     env = env;
//                                                     continue 'eval_loop;
//                                                 }
//                                                 (None, _) => return Ok(AST::Nil),
//                                             }
//                                         }
//                                     }
//                                     "let*" => {
//                                         let (bindings, expr) = util::two_args("let*", rest)?;
//                                         let mut bindings = bindings.unwrap_list_like()?.into_iter();
//                                         let let_star_env = Env::with_scope(env.clone());
//                                         loop {
//                                             let name = bindings.next();
//                                             let binding = bindings.next();
//                                             match (name, binding) {
//                                                 (None, None) => break,
//                                                 (Some(name), Some(expr)) => {
//                                                     let name = name.unwrap_symbol()?;
//                                                     let value = eval(let_star_env.clone(), expr)?;
//                                                     let_star_env
//                                                         .borrow_mut()
//                                                         .set(name.to_string(), value)?;
//                                                 }
//                                                 (_, _) => return Err(String::from(
//                                                     "let bindings must be an even number of forms.",
//                                                 )),
//                                             }
//                                         }
//                                         program = expr;
//                                         env = let_star_env;
//                                         continue;
//                                     }
//                                     // TODO - this probably shouldn't be necessary to fix the types up.
//                                     "def!" => rest.collect::<Vec<_>>().into_iter(),
//                                     "fn*" => return eval_fn_star(env.clone(), rest),
//                                     _ => rest
//                                         .map(|part| eval(env.clone(), part))
//                                         .collect::<Result<Vec<_>, _>>()?
//                                         .into_iter(),
//                                 };
//                             let thing = env.borrow().get(&s)?;
//                             match thing {
//                                 AST::Closure(val) => return val.0(env.clone(), Box::new(evaled)),
//                                 AST::Lambda(l) => {
//                                     let LambdaVal {
//                                         body,
//                                         env: lambda_env,
//                                         params,
//                                     } = l;
//                                     let has_rest = params.iter().find(|p| *p == "&").is_some();
//                                     let args = evaled.collect::<Vec<_>>();
//                                     if !has_rest && params.len() != args.len() {
//                                         return Err(format!("wrong number of args"));
//                                     }
//                                     if has_rest && params.len() - 2 > args.len() {
//                                         return Err(format!("wrong number of args rest"));
//                                     }
//                                     let mut params = params.into_iter();
//                                     let mut args = args
//                                         .into_iter()
//                                         .map(|arg| eval(lambda_env.clone(), arg))
//                                         .collect::<EvalResult<Vec<AST>>>()?
//                                         .into_iter();
//                                     while let Some(param) = params.next() {
//                                         if param == "&" {
//                                             let param = params.next();
//                                             if param.is_none() {
//                                                 return Err(String::from("name required after &"));
//                                             }
//                                             if params.next().is_some() {
//                                                 return Err(String::from(
//                                                     "Only one param can be bound as rest",
//                                                 ));
//                                             }
//                                             lambda_env.borrow_mut().set(
//                                                 param.unwrap(),
//                                                 AST::ListLike(Listy::List(
//                                                     args.collect::<Vec<_>>(),
//                                                 )),
//                                             )?;
//                                             break;
//                                         }
//                                         // unwrap is safe because we know there's <= params than args
//                                         let arg = args.next().unwrap();
//                                         lambda_env.borrow_mut().set(param, arg)?;
//                                     }
//                                     program = *body;
//                                     env = lambda_env.clone();
//                                     continue 'eval_loop;
//                                 }
//                                 _ => return Err(format!("Env value: {} is not a closure", thing)),
//                             }
//                         }
//                         list @ AST::ListLike(Listy::List(_)) => {
//                             let first = eval(env.clone(), list)?;
//                             let contents = std::iter::once(first).chain(rest).collect::<Vec<_>>();
//                             return eval(env.clone(), AST::ListLike(Listy::List(contents)));
//                         }
//                         AST::Lambda(l) => {
//                             let LambdaVal {
//                                 body,
//                                 env: lambda_env,
//                                 params,
//                             } = l;
//                             let has_rest = params.iter().find(|p| *p == "&").is_some();
//                             let args = rest.collect::<Vec<_>>();
//                             if !has_rest && params.len() != args.len() {
//                                 return Err(format!("wrong number of args"));
//                             }
//                             if has_rest && params.len() - 2 > args.len() {
//                                 return Err(format!("wrong number of args rest"));
//                             }
//                             let mut params = params.into_iter();
//                             let mut args = args
//                                 .into_iter()
//                                 .map(|arg| eval(lambda_env.clone(), arg))
//                                 .collect::<EvalResult<Vec<AST>>>()?
//                                 .into_iter();
//                             while let Some(param) = params.next() {
//                                 if param == "&" {
//                                     let param = params.next();
//                                     if param.is_none() {
//                                         return Err(String::from("name required after &"));
//                                     }
//                                     if params.next().is_some() {
//                                         return Err(String::from(
//                                             "Only one param can be bound as rest",
//                                         ));
//                                     }
//                                     lambda_env.borrow_mut().set(
//                                         param.unwrap(),
//                                         AST::ListLike(Listy::List(args.collect::<Vec<_>>())),
//                                     )?;
//                                     break;
//                                 }
//                                 // unwrap is safe because we know there's <= params than args
//                                 let arg = args.next().unwrap();
//                                 lambda_env.borrow_mut().set(param, arg)?;
//                             }
//                             program = *body;
//                             env = lambda_env.clone();
//                             continue 'eval_loop;
//                         }
//                         a => return Err(format!("not implemented: {}", a.typee())),
//                     }
//                 }
//                 Listy::Vector(v) => {
//                     return Ok(AST::m_vec(
//                         v.into_iter()
//                             .map(|part| eval(env.clone(), part))
//                             .collect::<Result<_, _>>()?,
//                     ))
//                 }
//             },
//             otherwise => return Ok(otherwise),
//         }
//     }
// }

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
                        MalType::Symbol(symbol) => {
                            match symbol.as_ref() {
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
