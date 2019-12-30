use crate::val::EvalError;
use crate::val::EvalResult;
use crate::val::MalVal;

pub fn one_or_more_args(
    _fn_name: &str,
    mut args: impl Iterator<Item = MalVal>,
) -> EvalResult<(MalVal, impl Iterator<Item = MalVal>)> {
    let first = args.next().ok_or(EvalError::TwoFewArgs)?;
    Ok((first, args))
}

pub fn two_or_more_args(
    _fn_name: &str,
    mut args: impl Iterator<Item = MalVal>,
) -> EvalResult<(MalVal, MalVal, impl Iterator<Item = MalVal>)> {
    let first = args.next();
    let second = args.next();
    match (first, second) {
        (Some(first), Some(second)) => Ok((first, second, args)),
        _ => Err(EvalError::WrongNumberOfArgs),
    }
}

pub fn two_args(
    _fn_name: &str,
    mut args: impl Iterator<Item = MalVal>,
) -> EvalResult<(MalVal, MalVal)> {
    let first = args.next();
    let second = args.next();
    let third = args.next();
    match (first, second, third) {
        (Some(first), Some(second), None) => Ok((first, second)),
        (Some(_), None, _) => Err(EvalError::TwoFewArgs),
        (None, None, _) => Err(EvalError::TwoFewArgs),
        _ => Err(EvalError::WrongNumberOfArgs),
    }
}

pub fn two_or_three_args(
    _fn_name: &str,
    mut args: impl Iterator<Item = MalVal>,
) -> EvalResult<(MalVal, MalVal, Option<MalVal>)> {
    let first = args.next();
    let second = args.next();
    let third = args.next();
    let no_forth = args.next();
    match (first, second, third, no_forth) {
        (Some(first), Some(second), third, None) => Ok((first, second, third)),
        (_, _, _, Some(_)) => Err(EvalError::TwoManyArgs),
        _ => Err(EvalError::WrongNumberOfArgs),
    }
}

pub fn one_arg(_fn_name: &str, mut args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let first = args.next();
    let second = args.next();
    match (first, second) {
        (Some(first), None) => Ok(first),
        _ => Err(EvalError::WrongNumberOfArgs),
    }
}
