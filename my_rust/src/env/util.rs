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
