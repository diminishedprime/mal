use im::HashMap;
use lisp_val::Environment;
use lisp_val::LispError;
use lisp_val::LispError::BadSpecialForm;
use lisp_val::LispResult;
use lisp_val::LispVal;
use lisp_val::SpecialForm::{DefBang, If, LetStar, Quote};
use lisp_val::{AtomContents, ListContents, MapContents, VecContents};
use std::ops::Deref;

#[cfg(test)]
mod tests {

    fn easy_atom(v: &str) -> LispVal {
        Atom(String::from(v))
    }

    use self::LispError::NumArgs;
    use self::LispVal::Atom;
    use self::LispVal::List;
    use self::LispVal::Number;
    use super::*;
    use parser;

    fn parse_eval(s: &str) -> LispResult {
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
    fn def_bang_test() {
        let expr1 = parser::parse(r#"(def! a "hi there")"#);
        let expr2 = parser::parse("a");
        let mut env = Environment::new();
        let actual = eval(&mut env, expr1).and_then(|_| eval(&mut env, expr2));
        assert_eq!(actual, Ok(LispVal::string_from("hi there")))
    }

    #[test]
    fn let_star_test() {
        let input = "(let* (q (+ 1 2)) q)";
        let parsed = parser::parse(input);
        let actual = eval_start(parsed).unwrap();
        assert_eq!(actual, LispVal::from(3))
    }

}

fn unpack_atom(val: LispVal) -> Result<String, LispError> {
    match val {
        LispVal::Atom(a) => Ok(a),
        _ => Err(LispError::TypeMismatch(String::from("list"), val)),
    }
}

fn unpack_list_or_vec(val: LispVal) -> Result<Vec<LispVal>, LispError> {
    match val {
        LispVal::List(lc) => Ok(lc),
        LispVal::Vector(vc) => Ok(vc),
        _ => Err(LispError::TypeMismatch(String::from("list"), val)),
    }
}

fn unpack_num(val: LispVal) -> Result<i32, LispError> {
    match val {
        LispVal::Number(n) => Ok(n),
        _ => Err(LispError::TypeMismatch(String::from("number"), val)),
    }
}

fn apply_zero<A>(
    env: &mut Environment,
    identity: A,
    pure_: fn(A) -> LispVal,
    unwraper: fn(LispVal) -> Result<A, LispError>,
    op: fn(A, A) -> A,
    args: Vec<LispVal>,
) -> LispResult
where
    A: ::std::fmt::Debug,
{
    let mut left = identity;
    let mut iter = args.into_iter();
    while let Some(right) = iter.next() {
        let right = unwraper(eval(env, Ok(right))?)?;
        left = op(left, right);
    }
    Ok(pure_(left))
}

fn apply_one<A>(
    env: &mut Environment,
    pure_: fn(A) -> LispVal,
    unpack: fn(LispVal) -> Result<A, LispError>,
    uniop: fn(A) -> A,
    op: fn(A, A) -> A,
    args: Vec<LispVal>,
) -> LispResult {
    if args.len() < 1 {
        return Err(LispError::NumArgs(1, LispVal::List(args)));
    } else if args.len() == 1 {
        return Ok(pure_(uniop(unpack(args.into_iter().next().unwrap())?)));
    }
    let mut iter = args.into_iter();
    let left = eval(env, Ok(iter.next().unwrap()))?;
    let mut left = unpack(left)?;
    while let Some(right) = iter.next() {
        let right = unpack(eval(env, Ok(right))?)?;
        left = op(left, right);
    }
    Ok(pure_(left))
}

fn eval_primatives(
    env: &mut Environment,
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
        _ => Ok(None),
    }
}

fn eval_list(env: &mut Environment, lisp_val: ListContents) -> LispResult {
    match &lisp_val[..] {
        &[] => Ok(LispVal::from(vec![])),
        &[LispVal::SpecialForm(LetStar), ref bindings, ref body] => {
            // bindings must be a list or vec of even elements
            let bindings = unpack_list_or_vec(bindings.clone())?;
            // This could be a LispError instead of a panic.
            assert!(bindings.len() % 2 == 0);
            let mut let_star_env = bindings.chunks(2).fold(
                Ok(Environment::shadowing(env.clone())),
                |env, chunk| {
                    let mut env = env?;
                    let name = chunk[0].clone();
                    let expr = Ok(chunk[1].clone());
                    let name = unpack_atom(name)?;
                    let evaled = eval(&mut env, expr)?;
                    Ok(env.set(name, evaled))
                },
            )?;
            eval(&mut let_star_env, Ok(body.clone()))
        }
        &[LispVal::SpecialForm(DefBang), LispVal::Atom(ref name), ref val] => {
            let evaled = eval(env, Ok(val.clone()))?;
            env.set_mut(name.clone(), evaled.clone());
            Ok(evaled)
        }
        &[LispVal::SpecialForm(If), ref pred, ref conseq, ref alt] => {
            // TODO(me) - Do I really need to clone pred, conseq, and alt?
            if let LispVal::False = eval(env, Ok(pred.clone()))? {
                eval(env, Ok(alt.clone()))
            } else {
                eval(env, Ok(conseq.clone()))
            }
        }
        &[LispVal::SpecialForm(Quote), ref val] => Ok(val.clone()),
        &[LispVal::Atom(ref name), ref _rest..] => {
            let args = lisp_val.clone()
                .into_iter()
            // We don't want the first since it's the function.
                .skip(1)
                .collect();
            if let Some(result) = eval_primatives(env, &name, args)? {
                Ok(result)
            } else {
                Err(LispError::NotFunction(name.clone()))
            }
        }
        _ => Err(BadSpecialForm(
            String::from("Unrecognized special form"),
            LispVal::List(lisp_val.clone()),
        )),
    }
}

fn eval_vector(env: &mut Environment, lisp_val: VecContents) -> LispResult {
    Ok(LispVal::Vector(
        lisp_val
            .into_iter()
            .map(Ok)
            .map(|s| eval(env, s))
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

fn eval_hash_map(env: &mut Environment, lisp_val: MapContents) -> LispResult {
    let initial: Result<HashMap<LispVal, LispVal>, LispError> = Ok(hashmap!());
    Ok(LispVal::Map(lisp_val.into_iter().fold(
        initial,
        |acc, (k, v)| {
            if let Ok(acc) = acc {
                Ok(acc.insert(eval(env, Ok((*k).clone()))?, eval(env, Ok((*v).clone()))?))
            } else {
                acc
            }
        },
    )?))
}

fn eval_atom(env: &mut Environment, name: AtomContents) -> LispResult {
    if let Some(val) = env.get(&name) {
        // TODO(me) - Is this how Arcs work?
        Ok(val.deref().clone())
    } else {
        return Err(LispError::UnboundVar(name.clone()));
    }
}

pub fn eval(env: &mut Environment, lisp_val: LispResult) -> LispResult {
    let lisp_val = match lisp_val? {
        val @ LispVal::DottedList(_) => val,
        val @ LispVal::String(_) => val,
        val @ LispVal::Number(_) => val,
        val @ LispVal::True => val,
        val @ LispVal::False => val,
        val @ LispVal::Keyword(_) => val,
        LispVal::Atom(ac) => eval_atom(env, ac)?,
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

pub fn eval_start(lisp_val: LispResult) -> LispResult {
    eval(&mut Environment::new(), lisp_val)
}
