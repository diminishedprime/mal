use im::HashMap;
use lisp_val::DottedListContents;
use lisp_val::Environment;
use lisp_val::LispError;
use lisp_val::LispError::BadSpecialForm;
use lisp_val::LispVal;
use lisp_val::{ListContents, MapContents, VecContents};

#[cfg(test)]
mod tests {

    fn easy_atom(v: &str) -> LispVal {
        Atom(String::from(v))
    }

    use self::LispError::NumArgs;
    use self::LispVal::Atom;
    use self::LispVal::False;
    use self::LispVal::List;
    use self::LispVal::Number;
    use self::LispVal::True;
    use super::*;
    use parser;

    fn parse_eval(s: &str) -> Result<LispVal, LispError> {
        eval_start(parser::parse(s))
    }

    #[test]
    fn evals_to_self() {
        let inputs = vec![r#""my string""#, "3", "#t", "#f"];
        for input in inputs {
            let parsed = parser::parse(input);
            assert_eq!(eval_start(parsed.clone()), parsed.clone());
        }
    }

    #[test]
    fn maths() {
        let inputs = vec![
            ("(+)", "0"),
            ("(+ -3)", "-3"),
            ("(+ 1 7)", "8"),
            ("(+ 1 2 3)", "6"),
            ("(*)", "1"),
            ("(* -3)", "-3"),
            ("(* 1 7)", "7"),
            ("(* 1 2 3)", "6"),
            ("(- 9)", "-9"),
            ("(- 9 8)", "1"),
            ("(- 9 8 7)", "-6"),
            ("(/ -1)", "-1"),
            ("(/ -6, 3)", "-2"),
            ("(/ 24 2 2 1)", "6"),
        ];
        for (i, e) in inputs {
            let input = parse_eval(i);
            let expected = parse_eval(e);
            assert_eq!(input, expected, "\nInput : {}\nOutput: {}\n", i, e);
        }
    }

    #[test]
    fn lists() {
        let inputs = vec![("(quote 3)", "3")];
        for (input, expected) in inputs {
            let input = parse_eval(input);
            let expected = parse_eval(expected);
            assert_eq!(input, expected);
        }
    }

    #[test]
    fn bad_addition() {
        let expr = List(vec![easy_atom("+"), Number(2), easy_atom("hi")]);
        if let Ok(_) = eval_start(Ok(expr)) {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn subtraction_no_args() {
        let expr = List(vec![easy_atom("-")]);
        if let Err(NumArgs(num, _)) = eval_start(Ok(expr)) {
            assert_eq!(num, 1)
        } else {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn division_no_args() {
        let expr = List(vec![easy_atom("/")]);
        if let Err(NumArgs(num, _)) = eval_start(Ok(expr)) {
            assert_eq!(num, 1)
        } else {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn equality_two_args_same() {
        let expr = List(vec![
            easy_atom("="),
            List(vec![easy_atom("+"), Number(2), Number(3)]),
            List(vec![easy_atom("+"), Number(3), Number(2)]),
        ]);
        assert_eq!(eval_start(Ok(expr)), Ok(True));
    }

    fn easy_str(s: &str) -> LispVal {
        LispVal::String(String::from(s))
    }

    #[test]
    fn if_works_truthy() {
        let expr = parser::parse(r#"(if "truthy" "hi" "there")"#);
        assert_eq!(eval_start(expr), Ok(easy_str("hi")))
    }

    #[test]
    fn if_works_false() {
        let expr = parser::parse(r#"(if #f "hi" "there")"#);
        assert_eq!(eval_start(expr), Ok(easy_str("there")))
    }

    #[test]
    fn car_test() {
        let expr = List(vec![easy_atom("car"), List(vec![True, False])]);
        assert_eq!(eval_start(Ok(expr)), Ok(True));
    }

}

// fn unpack_atom(lisp_val: &LispVal) -> Option<String> {
//     if let LispVal::Atom(s) = lisp_val {
//         Some(s.to_string())
//     } else {
//         None
//     }
// }

fn unpack_num(val: LispVal) -> Result<i32, LispError> {
    match val {
        LispVal::Number(n) => Ok(n),
        _ => Err(LispError::TypeMismatch(String::from("number"), val)),
    }
}

fn unpack_bool(val: LispVal) -> Result<bool, LispError> {
    match val {
        LispVal::True => Ok(true),
        LispVal::False => Ok(false),
        _ => Err(LispError::TypeMismatch(String::from("bool"), val)),
    }
}

fn unpack_string(val: LispVal) -> Result<String, LispError> {
    match val {
        LispVal::String(n) => Ok(n),
        _ => Err(LispError::TypeMismatch(String::from("string"), val)),
    }
}

fn generic_binop<A>(
    env: Environment,
    unpacker: fn(LispVal) -> Result<A, LispError>,
    op: fn(A, A) -> LispVal,
    args: Vec<LispVal>,
) -> Result<LispVal, LispError> {
    if args.len() != 2 {
        return Err(LispError::NumArgs(2, LispVal::List(args)));
    }
    let unpacked_args = args
        .into_iter()
        .map(Ok)
        .map(|a| eval(env.clone(), a))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(unpacker)
        .collect::<Result<Vec<_>, _>>()?;
    let mut iter = unpacked_args.into_iter();
    let left = iter.next().unwrap();
    let right = iter.next().unwrap();
    let result = op(left, right);
    Ok(result)
}

fn apply_zero<A>(
    env: Environment,
    identity: A,
    pure_: fn(A) -> LispVal,
    unwraper: fn(LispVal) -> Result<A, LispError>,
    op: fn(A, A) -> A,
    args: Vec<LispVal>,
) -> Result<LispVal, LispError>
where
    A: ::std::fmt::Debug,
{
    let mut left = identity;
    let mut iter = args.into_iter();
    while let Some(right) = iter.next() {
        let right = unwraper(eval(env.clone(), Ok(right))?)?;
        left = op(left, right);
    }
    Ok(pure_(left))
}

fn apply_one<A>(
    env: Environment,
    pure_: fn(A) -> LispVal,
    unpack: fn(LispVal) -> Result<A, LispError>,
    uniop: fn(A) -> A,
    op: fn(A, A) -> A,
    args: Vec<LispVal>,
) -> Result<LispVal, LispError> {
    if args.len() < 1 {
        return Err(LispError::NumArgs(1, LispVal::List(args)));
    } else if args.len() == 1 {
        return Ok(pure_(uniop(unpack(args.into_iter().next().unwrap())?)));
    }
    let mut iter = args.into_iter();
    let left = eval(env.clone(), Ok(iter.next().unwrap()))?;
    let mut left = unpack(left)?;
    while let Some(right) = iter.next() {
        let right = unpack(eval(env.clone(), Ok(right))?)?;
        left = op(left, right);
    }
    Ok(pure_(left))
}

fn eval_car(env: Environment, args: Vec<LispVal>) -> Result<LispVal, LispError> {
    if args.len() != 1 {
        return Err(LispError::NumArgs(1, LispVal::List(args)));
    }
    Ok(match args.into_iter().nth(0).unwrap() {
        LispVal::List(v) => {
            if v.len() < 1 {
                return Err(LispError::TypeMismatch(
                    String::from("pair"),
                    LispVal::String(String::from("need to figure this out")),
                ));
            }
            eval(env, Ok(v.into_iter().nth(0).unwrap()))?
        }
        LispVal::DottedList(DottedListContents { head, .. }) => {
            if head.len() < 1 {
                return Err(LispError::TypeMismatch(
                    String::from("pair"),
                    LispVal::String(String::from("need to figure this out")),
                ));
            }
            eval(env, Ok(head.into_iter().nth(0).unwrap()))?
        }
        _ => {
            return Err(LispError::TypeMismatch(
                String::from("pair"),
                LispVal::String(String::from("need to figure this out")),
            ));
        }
    })
}

fn eval_primatives(
    env: Environment,
    func: &str,
    args: Vec<LispVal>,
) -> Result<Option<LispVal>, LispError> {
    match func.as_ref() {
        // TODO(me) - is there a way I can get this to work? I might have to do
        // something fancy with the parser...
        // "+1" => Ok(),
        "+" => Ok(Some(apply_zero(
            env,
            0,
            LispVal::Number,
            unpack_num,
            |a, b| a + b,
            args,
        )?)),
        "-" => Ok(Some(apply_one(
            env,
            LispVal::Number,
            unpack_num,
            |a| -a,
            |a, b| a - b,
            args,
        )?)),
        "*" => Ok(Some(apply_zero(
            env,
            1,
            LispVal::Number,
            unpack_num,
            |a, b| a * b,
            args,
        )?)),
        "/" => Ok(Some(apply_one(
            env,
            LispVal::Number,
            unpack_num,
            |a| 1 / a,
            |a, b| a / b,
            args,
        )?)),
        // TODO(me) - when I feel like it...
        // "mod" => Some(Box::new(|a, b| { a / b})),
        // "quotient" => Some(Box::new(|a, b| { a / b})),
        // "remainder" => Some(Box::new(|a, b| { a / b})),
        "=" => Ok(Some(generic_binop(
            env,
            unpack_num,
            |a, b| LispVal::from(a == b),
            args,
        )?)),
        "<" => Ok(Some(generic_binop(
            env,
            unpack_num,
            |a, b| LispVal::from(a < b),
            args,
        )?)),
        ">" => Ok(Some(generic_binop(
            env,
            unpack_num,
            |a, b| LispVal::from(a > b),
            args,
        )?)),
        "/=" => Ok(Some(generic_binop(
            env,
            unpack_num,
            |a, b| LispVal::from(a != b),
            args,
        )?)),
        ">=" => Ok(Some(generic_binop(
            env,
            unpack_num,
            |a, b| LispVal::from(a >= b),
            args,
        )?)),
        "<=" => Ok(Some(generic_binop(
            env,
            unpack_num,
            |a, b| LispVal::from(a <= b),
            args,
        )?)),
        "&&" => Ok(Some(generic_binop(
            env,
            unpack_bool,
            |a, b| LispVal::from(a && b),
            args,
        )?)),
        "||" => Ok(Some(generic_binop(
            env,
            unpack_bool,
            |a, b| LispVal::from(a || b),
            args,
        )?)),
        "string=?" => Ok(Some(generic_binop(
            env,
            unpack_string,
            |a, b| LispVal::from(a == b),
            args,
        )?)),
        "string<?" => Ok(Some(generic_binop(
            env,
            unpack_string,
            |a, b| LispVal::from(a <= b),
            args,
        )?)),
        "string>?" => Ok(Some(generic_binop(
            env,
            unpack_string,
            |a, b| LispVal::from(a >= b),
            args,
        )?)),
        "string<=?" => Ok(Some(generic_binop(
            env,
            unpack_string,
            |a, b| LispVal::from(a <= b),
            args,
        )?)),
        "string>=?" => Ok(Some(generic_binop(
            env,
            unpack_string,
            |a, b| LispVal::from(a >= b),
            args,
        )?)),
        "car" => Ok(Some(eval_car(env, args)?)),
        _ => Ok(None),
    }
}

use lisp_val::SpecialForm::{If, Quote};

fn eval_list(env: Environment, lisp_val: ListContents) -> Result<LispVal, LispError> {
    match &lisp_val[..] {
        &[] => Ok(LispVal::from(vec![])),
        &[LispVal::SpecialForm(If), ref pred, ref conseq, ref alt] => {
            // TODO(me) - Do I really need to clone pred, conseq, and alt?
            if let LispVal::False = eval(env.clone(), Ok(pred.clone()))? {
                eval(env.clone(), Ok(alt.clone()))
            } else {
                eval(env, Ok(conseq.clone()))
            }
        }
        &[LispVal::SpecialForm(Quote), ref val] => Ok(val.clone()),
        &[LispVal::SpecialForm(_)] => Err(BadSpecialForm(
            String::from("Unrecognized special form"),
            LispVal::List(lisp_val.clone()),
        )),
        &[LispVal::Atom(ref s), ref _rest..] => {
            let args = lisp_val.clone()
                .into_iter()
            // We don't want the first since it's the function.
                .skip(1)
                .collect();
            if let Some(result) = eval_primatives(env, &s, args)? {
                Ok(result)
            } else {
                return Err(LispError::NotFunction(
                    String::from("Not a function"),
                    s.clone(),
                ));
            }
        }
        _ => Err(BadSpecialForm(
            String::from("Unrecognized special form"),
            LispVal::List(lisp_val.clone()),
        )),
    }
}

fn eval_vector(env: Environment, lisp_val: VecContents) -> Result<LispVal, LispError> {
    Ok(LispVal::Vector(
        lisp_val
            .into_iter()
            .map(Ok)
            .map(|s| eval(env.clone(), s))
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn eval_hash_map(env: Environment, lisp_val: MapContents) -> Result<LispVal, LispError> {
    let initial: Result<HashMap<LispVal, LispVal>, LispError> = Ok(hashmap!());
    Ok(LispVal::Map(lisp_val.into_iter().fold(
        initial,
        |acc, (k, v)| {
            if let Ok(acc) = acc {
                Ok(acc.insert(
                    eval(env.clone(), Ok((*k).clone()))?,
                    eval(env.clone(), Ok((*v).clone()))?,
                ))
            } else {
                acc
            }
        },
    )?))
}

fn eval(env: Environment, lisp_val: Result<LispVal, LispError>) -> Result<LispVal, LispError> {
    let lisp_val = match lisp_val? {
        val @ LispVal::Atom(_) => val,
        val @ LispVal::DottedList(_) => val,
        val @ LispVal::String(_) => val,
        val @ LispVal::Number(_) => val,
        val @ LispVal::True => val,
        val @ LispVal::False => val,
        val @ LispVal::Keyword(_) => val,
        LispVal::List(lc) => eval_list(env, lc)?,
        LispVal::Vector(vc) => eval_vector(env, vc)?,
        LispVal::Map(mc) => eval_hash_map(env, mc)?,
        val @ LispVal::SpecialForm(_) => {
            return Err(LispError::BadSpecialForm(
                String::from("Bad special form"),
                val,
            ))
        }
    };
    Ok(lisp_val)
}

pub fn eval_start(lisp_val: Result<LispVal, LispError>) -> Result<LispVal, LispError> {
    eval(Environment::new(), lisp_val)
}
