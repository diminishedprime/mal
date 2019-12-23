use crate::ast::Env;
use crate::ast::AST;
use std::cell::RefCell;
use std::rc::Rc;
use AST::Closure;
use AST::List;
use AST::Map;
use AST::Symbol;
use AST::Vector;

fn split_fn_and_arg(program: Vec<AST>) -> Result<(AST, impl Iterator<Item = AST>), String> {
    let mut program_iter = program.into_iter();
    Ok(if let Some(first) = program_iter.next() {
        (first, program_iter)
    } else {
        return Err(format!("cannot split program"));
    })
}

pub fn eval(env: Rc<RefCell<Env>>, program: AST) -> Result<AST, String> {
    Ok(match program {
        Symbol(s) => return env.borrow().get(&s),
        Map(m) => Map(m
            .into_iter()
            .map(|part| eval(env.clone(), part))
            .collect::<Result<_, _>>()?),
        Vector(v) => Vector(
            v.into_iter()
                .map(|part| eval(env.clone(), part))
                .collect::<Result<_, _>>()?,
        ),
        List(l) => {
            if l.len() == 0 {
                List(vec![])
            } else {
                let (first, rest) = split_fn_and_arg(l)?;
                match first {
                    Symbol(s) => {
                        let thing = env.borrow().get(&s)?;
                        let evaled = match s.as_ref() {
                            "def!" => rest
                                .enumerate()
                                .map(|(i, val)| match i {
                                    0 => Ok(val),
                                    1 => eval(env.clone(), val),
                                    _ => Err(String::from("def! requires exactly 2 arguments")),
                                })
                                .collect::<Result<Vec<_>, String>>()?
                                .into_iter(),
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
        Closure(_) => return Err(String::from("not implemented")),
        otherwise => otherwise,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use AST::Double;

    #[test]
    fn parse_symbol() {
        let program = parse("(def! a 3)").unwrap();
        let actual = eval(Rc::new(RefCell::new(Env::new())), program).unwrap();
        assert_eq!(actual, Double(3.0));
    }
}
