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

pub fn two_or_three_args(
    fn_name: &str,
    mut args: impl Iterator<Item = AST>,
) -> EvalResult<(AST, AST, Option<AST>)> {
    let first = args.next();
    let second = args.next();
    let third = args.next();
    let no_forth = args.next();
    match (first, second, third, no_forth) {
        (Some(first), Some(second), third, None) => Ok((first, second, third)),
        (_, _, _, Some(_)) => Err(format!("{} can have at most 3 arguments.", fn_name)),
        _ => Err(format!("{} requires 2 or three arguments", fn_name)),
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
