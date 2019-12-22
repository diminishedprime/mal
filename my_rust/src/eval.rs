use crate::ast::Env;
use crate::ast::AST;
use AST::Closure;
use AST::Double;
use AST::Keyword;
use AST::LString;
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

pub fn eval(env: &mut Env, program: AST) -> Result<AST, String> {
    Ok(match program {
        d @ Double(_) => d,
        k @ Keyword(_) => k,
        s @ Symbol(_) => s,
        s @ LString(_) => s,
        Map(m) => Map(m
            .into_iter()
            .map(|part| eval(env, part))
            .collect::<Result<_, _>>()?),
        Vector(v) => Vector(
            v.into_iter()
                .map(|part| eval(env, part))
                .collect::<Result<_, _>>()?,
        ),
        List(l) => {
            if l.len() == 0 {
                List(vec![])
            } else {
                let (first, rest) = split_fn_and_arg(l)?;
                let rest = rest
                    .map(|part| eval(env, part))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter();
                match first {
                    Symbol(s) => {
                        let thing = env
                            .functions
                            .get(&s)
                            .ok_or(format!("function: {} is not defined", s))?;
                        thing.0(Box::new(rest))?
                    }
                    _ => return Err(String::from("not implemented")),
                }
            }
        }
        Closure(_) => return Err(String::from("not implemented")),
    })
}
