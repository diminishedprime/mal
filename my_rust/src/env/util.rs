use crate::ast::AST;

pub fn one_or_more_args(
    fn_name: &str,
    mut args: impl Iterator<Item = AST>,
) -> Result<(AST, impl Iterator<Item = AST>), String> {
    let first = args
        .next()
        .ok_or(format!("{} requires at least one argument.", fn_name))?;
    Ok((first, args))
}

pub fn two_args(fn_name: &str, mut args: impl Iterator<Item = AST>) -> Result<(AST, AST), String> {
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
