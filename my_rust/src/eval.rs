use lisp_val::LispVal;
use lisp_val::DottedListContents;
use lisp_val::LispError;
use lisp_val::LispError::BadSpecialForm;
use im::HashMap;

#[cfg(test)]
mod tests {

    fn easy_atom(v: &str) -> LispVal {
        Atom(String::from(v))
    }

    use super::*;
    use self::LispVal::List;
    use self::LispVal::Atom;
    use self::LispVal::Number;
    use self::LispVal::True;
    use self::LispVal::False;
    use self::LispError::NumArgs;

    #[test]
    fn eval_string() {
        let input = Ok(LispVal::String(String::from("my string")));
        let expected = Ok(LispVal::String(String::from("my string")));
        let output = eval(input);
        assert_eq!(output, expected);
    }

    #[test]
    fn eval_number() {
        let input = Ok(LispVal::Number(13));
        let expected = Ok(LispVal::Number(13));
        let output = eval(input);
        assert_eq!(output, expected);
    }

    #[test]
    fn eval_bool() {
        let input = Ok(LispVal::True);
        let expected = Ok(LispVal::True);
        let output = eval(input);
        assert_eq!(output, expected);
    }

    #[test]
    fn eval_quoted_list() {
        let quote = LispVal::Atom(String::from("quote"));
        let three = LispVal::Number(3);
        let input = Ok(LispVal::List(vec!(quote, three)));
        let expected = Ok(LispVal::Number(3));
        let output = eval(input);
        assert_eq!(output, expected);
    }

    #[test]
    fn bad_addition() {
        let expr = List(vec!(easy_atom("+"), Number(2), easy_atom("hi")));
        if let Ok(_) = eval(Ok(expr)) {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn addition_no_args() {
        let expr = List(vec!(easy_atom("+")));
        assert_eq!(eval(Ok(expr)), Ok(Number(0)))
    }

    #[test]
    fn addition_one_args() {
        let expr = List(vec!(easy_atom("+"), Number(-3)));
        assert_eq!(eval(Ok(expr)), Ok(Number(-3)))
    }

    #[test]
    fn addition_two_args() {
        let expr = List(vec!(easy_atom("+"), Number(-3), Number(3)));
        assert_eq!(eval(Ok(expr)), Ok(Number(0)))
    }

    #[test]
    fn addition_many_args() {
        let expr = List(vec!(easy_atom("+"), Number(-3), Number(3), Number(2)));
        assert_eq!(eval(Ok(expr)), Ok(Number(2)))
    }

    #[test]
    fn subtraction_no_args() {
        let expr = List(vec!(easy_atom("-")));
        if let Err(NumArgs(num, _)) = eval(Ok(expr)) {
            assert_eq!(num, 1)
        } else {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn subtraction_one_args() {
        let expr = List(vec!(easy_atom("-"), Number(-3)));
        assert_eq!(eval(Ok(expr)), Ok(Number(3)))
    }

    #[test]
    fn subtraction_two_args() {
        let expr = List(vec!(easy_atom("-"), Number(-3), Number(-3)));
        assert_eq!(eval(Ok(expr)), Ok(Number(0)))
    }

    #[test]
    fn subtraction_many_args() {
        let expr = List(vec!(easy_atom("-"), Number(100), Number(10), Number(40)));
        assert_eq!(eval(Ok(expr)), Ok(Number(50)))
    }

    #[test]
    fn multiplication_no_args() {
        let expr = List(vec!(easy_atom("*")));
        assert_eq!(eval(Ok(expr)), Ok(Number(1)))
    }

    #[test]
    fn multiplication_one_args() {
        let expr = List(vec!(easy_atom("*"), Number(-3)));
        assert_eq!(eval(Ok(expr)), Ok(Number(-3)))
    }

    #[test]
    fn multiplication_two_args() {
        let expr = List(vec!(easy_atom("*"), Number(-3), Number(2)));
        assert_eq!(eval(Ok(expr)), Ok(Number(-6)))
    }

    #[test]
    fn multiplication_many_args() {
        let expr = List(vec!(easy_atom("*"), Number(4), Number(3), Number(2), Number(1)));
        assert_eq!(eval(Ok(expr)), Ok(Number(24)))
    }

    #[test]
    fn division_no_args() {
        let expr = List(vec!(easy_atom("/")));
        if let Err(NumArgs(num, _)) = eval(Ok(expr)) {
            assert_eq!(num, 1)
        } else {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn division_one_args() {
        let expr = List(vec!(easy_atom("/"), Number(-1)));
        assert_eq!(eval(Ok(expr)), Ok(Number(-1)))
    }

    #[test]
    fn division_two_args() {
        let expr = List(vec!(easy_atom("/"), Number(-6), Number(3)));
        assert_eq!(eval(Ok(expr)), Ok(Number(-2)))
    }

    #[test]
    fn division_many_args() {
        let expr = List(vec!(easy_atom("/"), Number(24), Number(2), Number(2), Number(1)));
        assert_eq!(eval(Ok(expr)), Ok(Number(6)))
    }

    // #[test]
    // fn equality_one_arg() {
    //     let expr = List(vec!(easy_atom("="), Number(3)));
    //     assert_eq!(eval(Ok(expr)), Ok(True));
    // }

    #[test]
    fn equality_two_args_same() {
        let expr = List(vec!(easy_atom("="),
                             List(vec!(easy_atom("+"),
                                       Number(2), Number(3))),
                             List(vec!(easy_atom("+"),
                                       Number(3), Number(2))),
        ));
        assert_eq!(eval(Ok(expr)), Ok(True));
    }

    fn easy_bool(b: bool) -> LispVal {
        LispVal::from(b)
    }

    fn easy_str(s: &str) -> LispVal {
        LispVal::String(String::from(s))
    }

    #[test]
    fn if_works_true() {
        let expr = List(vec!(easy_atom("if"),
                             easy_str("truthy"),
                             easy_str("hi"),
                             easy_str("there")));
        assert_eq!(eval(Ok(expr)), Ok(easy_str("hi")))
    }

    #[test]
    fn if_works_false() {
        let expr = List(vec!(easy_atom("if"),
                             easy_bool(false),
                             easy_str("hi"),
                             easy_str("there")));
        assert_eq!(eval(Ok(expr)), Ok(easy_str("there")))
    }

    // #[test]
    // fn equality_many_args_same() {
    //     let expr = List(vec!(easy_atom("="),
    //                          List(vec!(easy_atom("+"),
    //                                    Number(2), Number(3))),
    //                          List(vec!(easy_atom("+"),
    //                                    Number(3), Number(2))),
    //                          Number(5)
    //     ));
    //     assert_eq!(eval(Ok(expr)), Ok(True));
    // }

    // #[test]
    // fn equality_many_args_different() {
    //     let expr = List(vec!(easy_atom("="),
    //                          List(vec!(easy_atom("+"),
    //                                    Number(2), Number(3))),
    //                          List(vec!(easy_atom("+"),
    //                                    Number(3), Number(2))),
    //                          False
    //     ));
    //     assert_eq!(eval(Ok(expr)), Ok(False));
    // }

    #[test]
    fn car_test() {
        let expr = List(vec!(
            easy_atom("car"),
            List(vec!(
                True,
                False
            ))
        ));
        assert_eq!(eval(Ok(expr)), Ok(True));
    }

}

fn unpack_atom(lisp_val: &LispVal) -> Option<String> {
    if let LispVal::Atom(s) = lisp_val {
        Some(s.to_string())
    } else {
        None
    }
}

fn unpack_num(val: LispVal) -> Result<i32, LispError> {
    match val {
        LispVal::Number(n) => Ok(n),
        _ => Err(LispError::TypeMismatch(String::from("number"), val))
    }
}

fn unpack_bool(val: LispVal) -> Result<bool, LispError> {
    match val {
        LispVal::True => Ok(true),
        LispVal::False => Ok(false),
        _ => Err(LispError::TypeMismatch(String::from("bool"), val))
    }
}

fn unpack_string(val: LispVal) -> Result<String, LispError> {
    match val {
        LispVal::String(n) => Ok(n),
        _ => Err(LispError::TypeMismatch(String::from("string"), val))
    }
}

fn generic_binop<A>(
    unpacker: fn(LispVal) -> Result<A, LispError>,
    op: fn(A, A) -> LispVal,
    args: Vec<LispVal>
) -> Result<LispVal, LispError> {
    if args.len() != 2 {
        return Err(LispError::NumArgs(2, LispVal::List(args)))
    }
    let unpacked_args = args.into_iter()
        .map(Ok)
        .map(eval)
        .collect::<Result<Vec<_>, _>>()?.into_iter()
        .map(unpacker).collect::<Result<Vec<_>, _>>()?;
    let mut iter = unpacked_args.into_iter();
    let left = iter.next().unwrap();
    let right = iter.next().unwrap();
    let result = op(left, right);
    Ok(result)
}

fn apply_zero<A>(
    identity: A,
    pure_: fn(A) -> LispVal,
    unwraper: fn(LispVal) -> Result<A, LispError>,
    op: fn(A, A) -> A,
    args: Vec<LispVal>
) -> Result<LispVal, LispError>
where
    A: ::std::fmt::Debug,
{
    let mut left = identity;
    let mut iter = args.into_iter();
    while let Some(right) = iter.next() {
        let right = unwraper(eval(Ok(right))?)?;
        left = op(left, right);
    }
    Ok(pure_(left))
}

fn apply_one<A>(
    pure_: fn(A) -> LispVal,
    unpack: fn(LispVal) -> Result<A, LispError>,
    uniop: fn(A) -> A,
    op: fn(A, A) -> A,
    args: Vec<LispVal>
) -> Result<LispVal, LispError>
{
    if args.len() < 1 {
        return Err(LispError::NumArgs(1, LispVal::List(args)));
    } else if args.len() == 1 {
        return Ok(pure_(uniop(unpack(args.into_iter().next().unwrap())?)));
    }
    let mut iter = args.into_iter();
    let left = eval(Ok(iter.next().unwrap()))?;
    let mut left = unpack(left)?;
    while let Some(right) = iter.next() {
        let right = unpack(eval(Ok(right))?)?;
        left = op(left, right);
    }
    Ok(pure_(left))
}

fn eval_car(args: Vec<LispVal>) -> Result<LispVal, LispError> {
    if args.len() != 1 {
        return Err(LispError::NumArgs(1, LispVal::List(args)));
    }
    Ok(match args.into_iter().nth(0).unwrap() {
        LispVal::List(v) => {
            if v.len() < 1 {
                return Err(LispError::TypeMismatch(String::from("pair"), LispVal::String(String::from("need to figure this out"))));
            }
            eval(Ok(v.into_iter().nth(0).unwrap()))?
        }
        LispVal::DottedList(DottedListContents{head, ..}) => {
            if head.len() < 1 {
                return Err(LispError::TypeMismatch(String::from("pair"), LispVal::String(String::from("need to figure this out"))));
            }
            eval(Ok(head.into_iter().nth(0).unwrap()))?
        },
        _ => {
            return Err(LispError::TypeMismatch(String::from("pair"), LispVal::String(String::from("need to figure this out"))));
        }
    })
}


fn eval_primatives(func: &str, args: Vec<LispVal>) -> Result<Option<LispVal>, LispError> {
    match func.as_ref() {
        // TODO(me) - is there a way I can get this to work? I might have to do
        // something fancy with the parser...
        // "+1" => Ok(),
        "+" => Ok(Some(
            apply_zero(0, LispVal::Number, unpack_num, |a, b| a + b, args)?
        )),
        "-" => Ok(Some(
            apply_one(LispVal::Number, unpack_num, |a| -a, |a, b| a - b, args)?
        )),
        "*" => Ok(Some(
            apply_zero(1, LispVal::Number, unpack_num, |a, b| a * b, args)?
        )),
        "/" => Ok(Some(
            apply_one(LispVal::Number, unpack_num, |a| 1 / a, |a, b| a / b, args)?
        )),
        // TODO(me) - when I feel like it...
        // "mod" => Some(Box::new(|a, b| { a / b})),
        // "quotient" => Some(Box::new(|a, b| { a / b})),
        // "remainder" => Some(Box::new(|a, b| { a / b})),
        "=" => Ok(Some(
            generic_binop(unpack_num, |a, b| LispVal::from(a == b), args)?
        )),
        "<" => Ok(Some(generic_binop(unpack_num, |a, b| LispVal::from(a < b), args)?)),
        ">" => Ok(Some(generic_binop(unpack_num, |a, b| LispVal::from(a > b), args)?)),
        "/=" => Ok(Some(generic_binop(unpack_num, |a, b| LispVal::from(a != b), args)?)),
        ">=" => Ok(Some(generic_binop(unpack_num, |a, b| LispVal::from(a >= b), args)?)),
        "<=" => Ok(Some(generic_binop(unpack_num, |a, b| LispVal::from(a <= b), args)?)),
        "&&" => Ok(Some(generic_binop(unpack_bool, |a, b| LispVal::from(a && b), args)?)),
        "||" => Ok(Some(generic_binop(unpack_bool, |a, b| LispVal::from(a || b), args)?)),
        "string=?" => Ok(Some(generic_binop(unpack_string, |a, b| LispVal::from(a == b), args)?)),
        "string<?" => Ok(Some(generic_binop(unpack_string, |a, b| LispVal::from(a <= b), args)?)),
        "string>?" => Ok(Some(generic_binop(unpack_string, |a, b| LispVal::from(a >= b), args)?)),
        "string<=?" => Ok(Some(generic_binop(unpack_string, |a, b| LispVal::from(a <= b), args)?)),
        "string>=?" => Ok(Some(generic_binop(unpack_string, |a, b| LispVal::from(a >= b), args)?)),
        "car" => Ok(Some(eval_car(args)?)),
        _ => Ok(None)
    }
}

pub fn eval_list(lisp_val: LispVal) -> Result<LispVal, LispError> {
    if let LispVal::List(l) = lisp_val {
        if l.len() ==  0 {
            Ok(LispVal::List(vec!()))
        } else if l.len() == 4 && l[0] == LispVal::Atom(String::from("if")) {
            let mut l = l.into_iter();
            // TODO(me) - There's sure to be a better way of doing this. At
            // this point I know there are 4 entries, but I can't get them
            // via index because that counts as moving into borrowed
            // context.
            let pred = Ok(l.clone().nth(1).unwrap());
            let conseq = Ok(l.clone().nth(2).unwrap());
            let alt = Ok(l.clone().nth(3).unwrap());
            if let LispVal::False = eval(pred)? {
                eval(alt)
            } else {
                eval(conseq)
            }
        } else if l.len() == 2 && l[0] == LispVal::Atom(String::from("quote")) {
            Ok(l.into_iter().nth(1).expect("This cannot happen"))
        } else if let Some(s) = unpack_atom(&l[0]) {
            let args = l.into_iter()
            // We don't want the first since it's the function.
                .skip(1)
                .collect();
            if let Some(result) = eval_primatives(&s, args)? {
                Ok(result)
            } else {
                return Err(LispError::NotFunction(String::from("Not a function"), s))
            }
        } else {
            return Err(BadSpecialForm(
                String::from("Unrecognized special form"),
                LispVal::List(l)));
        }
    } else {
        panic!("cannot happen");
    }
}

pub fn eval_vector(lisp_val: LispVal) -> Result<LispVal, LispError> {
    if let LispVal::Vector(v) = lisp_val {
        Ok(LispVal::Vector(v.into_iter()
                           .map(Ok)
                           .map(eval)
                           .collect::<Result<Vec<_>, _>>()?))
    } else {
        panic!("cannot happen");
    }
}

pub fn eval_hash_map(lisp_val: LispVal) -> Result<LispVal, LispError> {
    if let LispVal::Map(m) = lisp_val {
        let initial: Result<HashMap<LispVal, LispVal>, LispError>
            = Ok(hashmap!());
        let h = m.into_iter()
            .fold(initial, |acc, (k, v)| {
                if let Ok(acc) = acc {
                    Ok(acc.insert(
                        eval(Ok((*k).clone()))?,
                        eval(Ok((*v).clone()))?)
                    )
                } else {
                    acc
                }
            });
        Ok(LispVal::Map(h?))
    } else {
        panic!("cannot happen");
    }
}


pub fn eval(lisp_val: Result<LispVal, LispError>) -> Result<LispVal, LispError> {
    let lisp_val = match lisp_val? {
        val @ LispVal::Atom(_) => val,
        val @ LispVal::DottedList(_) => val,
        val @ LispVal::String(_) => val,
        val @ LispVal::Number(_) => val,
        val @ LispVal::True => val,
        val @ LispVal::False => val,
        val @ LispVal::Keyword(_) => val,
        val @ LispVal::List(_) => eval_list(val)?,
        val @ LispVal::Vector(_) => eval_vector(val)?,
        val @ LispVal::Map(_) => eval_hash_map(val)?,
    };
    Ok(lisp_val)
}
