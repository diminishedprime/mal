use crate::ast::AST;
use crate::eval::EvalResult;

pub fn one_or_more_args(
    fn_name: &str,
    mut args: impl Iterator<Item = AST>,
) -> EvalResult<(AST, impl Iterator<Item = AST>)> {
    let first = args
        .next()
        .ok_or(format!("{} requires at least one argument.", fn_name))?;
    Ok((first, args))
}

pub fn two_args(fn_name: &str, mut args: impl Iterator<Item = AST>) -> EvalResult<(AST, AST)> {
    let first = args.next();
    let second = args.next();
    let third = args.next();
    match (first, second, third) {
        (Some(first), Some(second), None) => Ok((first, second)),
        (Some(_), None, _) => Err(format!("{} requires a second argument", fn_name)),
        (None, None, _) => Err(format!("{}, requires two more arguments", fn_name)),
        _ => Err(String::from("{} can only have two arguments")),
    }
}

pub fn one_arg(fn_name: &str, mut args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let first = args.next();
    let second = args.next();
    match (first, second) {
        (Some(first), None) => Ok(first),
        _ => Err(format!("{} can only have one argument", fn_name)),
    }
}
