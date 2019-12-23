pub mod env;
#[cfg(test)]
mod tests;

use crate::ast::list_of;
use crate::ast::vec_of;
use crate::ast::Listy;
use crate::ast::AST;
use crate::ast::AST::ListLike;
use env::Env;
use std::cell::RefCell;
use std::rc::Rc;
use AST::Closure;
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

pub fn eval(env: Rc<RefCell<Env>>, program: AST) -> EvalResult<AST> {
    Ok(match program {
        Symbol(s) => return env.borrow().get(&s),
        Map(m) => Map(m
            .into_iter()
            .map(|part| eval(env.clone(), part))
            .collect::<Result<_, _>>()?),
        ListLike(l) => match l {
            Listy::List(l) => {
                if l.len() == 0 {
                    list_of(vec![])
                } else {
                    let (first, rest) = split_fn_and_arg(l)?;
                    match first {
                        Symbol(s) => {
                            let thing = env.borrow().get(&s)?;
                            let evaled = match s.as_ref() {
                                // TODO - this probably shouldn't be necessary to fix the types up.
                                "let*" | "def!" => rest.collect::<Vec<_>>().into_iter(),
                                _ => rest
                                    .map(|part| eval(env.clone(), part))
                                    .collect::<Result<Vec<_>, _>>()?
                                    .into_iter(),
                            };
                            match thing {
                                Closure(val) => val.0(env.clone(), Box::new(evaled))?,
                                _ => return Err(format!("Env value: {} is not a closure", thing)),
                            }
                        }
                        _ => return Err(String::from("not implemented")),
                    }
                }
            }
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
