use crate::ast::AST;
use AST::Double;
use AST::Keyword;
use AST::LString;
use AST::List;
use AST::Symbol;

fn split_fn_and_arg(program: AST) -> Result<(AST, impl Iterator<Item = AST>), String> {
    Ok(if let List(l) = program {
        let mut program_iter = l.into_iter();
        if let Some(first) = program_iter.next() {
            (first, program_iter)
        } else {
            return Err(format!("cannot split program"));
        }
    } else {
        return Err(format!("Invalid program: {}", program));
    })
}

pub fn eval(program: AST) -> Result<AST, String> {
    Ok(match program {
        d @ Double(_) => d,
        k @ Keyword(_) => k,
        s @ Symbol(_) => s,
        s @ LString(_) => s,
        List(l) => match &l[0] {
            Symbol(s) => match s.as_ref() {
                "+" => LString(String::from("do a plus")),
                _ => return Err(String::from("not implemented")),
            },
            _ => return Err(String::from("not implemented")),
        },
        otherwise => otherwise,
    })
}
