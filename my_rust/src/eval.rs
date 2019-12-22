use crate::ast::AST;
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

fn apply(
    args: impl Iterator<Item = AST>,
    f: impl Fn(AST, AST) -> Result<AST, String>,
) -> Result<AST, String> {
    let mut evaled = args.map(eval);
    if let (Some(first), rest) = (evaled.next(), evaled) {
        rest.fold(
            first,
            |acc: Result<AST, String>, evaled: Result<AST, String>| {
                let acc = acc?;
                let evaled = evaled?;
                f(acc, evaled)
            },
        )
    } else {
        Err(String::from("must have at least one argument"))
    }
}

fn add(a: AST, b: AST) -> Result<AST, String> {
    if let (Double(a), Double(b)) = (a, b) {
        Ok(Double(a + b))
    } else {
        Err(String::from("All arguments must be numbers"))
    }
}

fn sub(a: AST, b: AST) -> Result<AST, String> {
    if let (Double(a), Double(b)) = (a, b) {
        Ok(Double(a - b))
    } else {
        Err(String::from("All arguments must be numbers"))
    }
}

fn multiply(a: AST, b: AST) -> Result<AST, String> {
    if let (Double(a), Double(b)) = (a, b) {
        Ok(Double(a * b))
    } else {
        Err(String::from("All arguments must be numbers"))
    }
}

fn divide(a: AST, b: AST) -> Result<AST, String> {
    if let (Double(a), Double(b)) = (a, b) {
        Ok(Double(a / b))
    } else {
        Err(String::from("All arguments must be numbers"))
    }
}

pub fn eval(program: AST) -> Result<AST, String> {
    Ok(match program {
        d @ Double(_) => d,
        k @ Keyword(_) => k,
        s @ Symbol(_) => s,
        s @ LString(_) => s,
        Map(m) => Map(m.into_iter().map(eval).collect::<Result<_, _>>()?),
        Vector(v) => Vector(v.into_iter().map(eval).collect::<Result<_, _>>()?),
        List(l) => {
            if l.len() == 0 {
                List(vec![])
            } else {
                let (first, rest) = split_fn_and_arg(l)?;
                match first {
                    Symbol(s) => match s.as_ref() {
                        "+" => apply(rest, add)?,
                        "-" => apply(rest, sub)?,
                        "*" => apply(rest, multiply)?,
                        "/" => apply(rest, divide)?,
                        _ => return Err(String::from("not implemented")),
                    },
                    _ => return Err(String::from("not implemented")),
                }
            }
        }
    })
}
