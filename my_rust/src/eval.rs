use im::HashMap;
use lisp_val::Environment;
use lisp_val::ExecyBoi;
use lisp_val::LispError;
use lisp_val::LispResult;
use lisp_val::LispVal;
use lisp_val::SpecialForm::{DefBang, If, LetStar, Quote};
use lisp_val::{AtomContents, ListContents, MapContents, VecContents};
use std::ops::Deref;

#[cfg(test)]
mod tests {

    use self::LispError::NumArgs;
    use super::*;
    use parser;

    fn parse_eval(s: &str) -> LispResult {
        eval_start(Ok(ExecyBoi {
            val: parser::parse(s).unwrap(),
            env: Environment::new(),
        }))
    }

    #[test]
    fn evals_to_self() {
        let inputs = vec![r#""my string""#, "3", "#t", "#f"];
        for input in inputs {
            let parsed = Ok(ExecyBoi {
                val: parser::parse(input).unwrap(),
                env: Environment::new(),
            });
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
        let expr = Ok(ExecyBoi {
            val: parser::parse(r#"(+ 2 "hi")"#).unwrap(),
            env: Environment::new(),
        });
        if let Ok(_) = eval_start(expr) {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn subtraction_no_args() {
        let expr = Ok(ExecyBoi {
            val: parser::parse(r#"(-)"#).unwrap(),
            env: Environment::new(),
        });
        if let Err(NumArgs(num, _)) = eval_start(expr) {
            assert_eq!(num, 1)
        } else {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn division_no_args() {
        let expr = Ok(ExecyBoi {
            val: parser::parse(r#"(/)"#).unwrap(),
            env: Environment::new(),
        });
        if let Err(NumArgs(num, _)) = eval_start(expr) {
            assert_eq!(num, 1)
        } else {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn if_works_truthy() {
        let expr = Ok(ExecyBoi {
            val: parser::parse(r#"(if "truthy" "hi" "there")"#).unwrap(),
            env: Environment::new(),
        });
        let actual = eval_start(expr).unwrap().val;
        let expected = LispVal::string_from("hi");
        assert_eq!(actual, expected);
    }

    #[test]
    fn if_works_false() {
        let expr = Ok(ExecyBoi {
            val: parser::parse(r#"(if #f "hi" "there")"#).unwrap(),
            env: Environment::new(),
        });
        let actual = eval_start(expr).unwrap().val;
        let expected = LispVal::string_from("there");
        assert_eq!(actual, expected);
    }

    #[test]
    fn def_bang_test() {
        let expr1 = ExecyBoi {
            val: parser::parse(r#"(def! a "hi there")"#).unwrap(),
            env: Environment::new(),
        };
        let expr2 = parser::parse("a").unwrap();

        let ExecyBoi { env: new_env, .. } = eval(expr1).unwrap();
        let actual = eval(ExecyBoi {
            val: expr2,
            env: new_env,
        }).unwrap()
            .val;
        let expected = LispVal::string_from("hi there");
        assert_eq!(actual, expected);
    }

    #[test]
    fn let_star_test() {
        let input = "(let* (q (+ 1 2)) q)";
        let parsed = Ok(ExecyBoi {
            val: parser::parse(input).unwrap(),
            env: Environment::new(),
        });
        let actual = eval_start(parsed).unwrap().val;
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
    env: &Environment,
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
    let mut env = env.clone();
    while let Some(right) = iter.next() {
        let ExecyBoi {
            val: new_val,
            env: new_env,
        } = eval(ExecyBoi {
            val: right,
            env: env.clone(),
        })?;
        env = new_env;
        let right = unwraper(new_val)?;
        left = op(left, right);
    }
    Ok(ExecyBoi {
        val: pure_(left),
        env: env,
    })
}

fn apply_one<A>(
    env: &Environment,
    pure_: fn(A) -> LispVal,
    unpack: fn(LispVal) -> Result<A, LispError>,
    uniop: fn(A) -> A,
    op: fn(A, A) -> A,
    args: Vec<LispVal>,
) -> LispResult {
    if args.len() < 1 {
        Err(LispError::NumArgs(1, LispVal::List(args)))
    } else if args.len() == 1 {
        let ExecyBoi {
            val: evaled,
            env: new_env,
        } = eval(ExecyBoi {
            val: args.into_iter().next().unwrap(),
            env: env.clone(),
        })?;
        let unpacked = unpack(evaled)?;
        let opped = uniop(unpacked);
        let valued = pure_(opped);
        Ok(ExecyBoi {
            val: valued,
            env: new_env,
        })
    } else {
        let mut env = env.clone();
        let mut iter = args.into_iter();
        let ExecyBoi {
            val: next_val,
            env: next_env,
        } = eval(ExecyBoi {
            val: iter.next().unwrap(),
            env: env.clone(),
        })?;
        env = next_env;
        let mut left = unpack(next_val)?;
        while let Some(right) = iter.next() {
            let ExecyBoi {
                env: next_env,
                val: next_val,
            } = eval(ExecyBoi {
                val: right,
                env: env,
            })?;
            let right = unpack(next_val)?;
            left = op(left, right);
            env = next_env;
        }
        Ok(ExecyBoi {
            val: pure_(left),
            env: env,
        })
    }
}

fn eval_primatives(
    env: &Environment,
    func: &str,
    args: Vec<LispVal>,
) -> Result<Option<LispResult>, LispError> {
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
        ))),
        "-" => Ok(Some(apply_one(
            env,
            LispVal::Number,
            unpack_num,
            |a| -a,
            |a, b| a - b,
            args,
        ))),
        "*" => Ok(Some(apply_zero(
            env,
            1,
            LispVal::Number,
            unpack_num,
            |a, b| a * b,
            args,
        ))),
        "/" => Ok(Some(apply_one(
            env,
            LispVal::Number,
            unpack_num,
            |a| 1 / a,
            |a, b| a / b,
            args,
        ))),
        _ => Ok(None),
    }
}

fn eval_list(env: &Environment, lisp_val: ListContents) -> LispResult {
    match &lisp_val[..] {
        &[] => Ok(ExecyBoi {
            val: LispVal::from(vec![]),
            env: env.clone(),
        }),
        &[LispVal::SpecialForm(LetStar), ref bindings, ref body] => {
            // bindings must be a list or vec of even elements
            let bindings = unpack_list_or_vec(bindings.clone())?;
            // This could be a LispError instead of a panic.
            assert!(bindings.len() % 2 == 0);
            let mut let_star_env = bindings.chunks(2).fold(
                Ok(Environment::shadowing(env.clone())),
                |env, chunk| {
                    let env = env?;
                    let name = chunk[0].clone();
                    let execy_boi = ExecyBoi {
                        val: chunk[1].clone(),
                        env: env.clone(),
                    };
                    let name = unpack_atom(name)?;
                    let evaled = eval(execy_boi)?;
                    Ok(env.set(name, evaled.val))
                },
            )?;
            let ExecyBoi {
                val: evaled_body, ..
            } = eval(ExecyBoi {
                env: let_star_env,
                val: body.clone(),
            })?;
            Ok(ExecyBoi {
                env: env.clone(),
                val: evaled_body,
            })
        }
        &[LispVal::SpecialForm(DefBang), LispVal::Atom(ref name), ref val] => {
            let evaled = eval(ExecyBoi {
                env: env.clone(),
                val: val.clone(),
            })?;
            let new_env = env.set(name.clone(), evaled.val.clone());
            Ok(ExecyBoi {
                env: new_env,
                val: evaled.val,
            })
        }
        &[LispVal::SpecialForm(If), ref pred, ref conseq, ref alt] => {
            let ExecyBoi {
                env: new_env,
                val: pred_val,
            } = eval(ExecyBoi {
                env: env.clone(),
                val: pred.clone(),
            })?;
            // TODO(me) - Do I really need to clone pred, conseq, and alt?
            if let LispVal::False = pred_val {
                eval(ExecyBoi {
                    env: new_env,
                    val: alt.clone(),
                })
            } else {
                eval(ExecyBoi {
                    env: new_env,
                    val: conseq.clone(),
                })
            }
        }
        &[LispVal::SpecialForm(Quote), ref val] => Ok(ExecyBoi {
            val: val.clone(),
            env: env.clone(),
        }),
        &[LispVal::Atom(ref name), ref _rest..] => {
            let args = lisp_val.clone()
                .into_iter()
            // We don't want the first since it's the function.
                .skip(1)
                .collect();
            if let Some(result) = eval_primatives(env, &name, args)? {
                result
            } else {
                Err(LispError::NotFunction(name.clone()))
            }
        }
        _ => Err(LispError::BadSpecialForm(
            String::from("Unrecognized special form"),
            LispVal::List(lisp_val.clone()),
        )),
    }
}

fn eval_vector(env: &Environment, lisp_val: VecContents) -> LispResult {
    let mut env = env.clone();
    Ok(ExecyBoi {
        val: LispVal::Vector(
            lisp_val
                .into_iter()
                .map(|next_val| {
                    let ExecyBoi {
                        val: new_val,
                        env: new_env,
                    } = eval(ExecyBoi {
                        val: next_val,
                        env: env.clone(),
                    })?;
                    env = new_env;
                    Ok(new_val)
                })
                .collect::<Result<Vec<_>, _>>()?,
        ),
        env: env.clone(),
    })
}

fn eval_hash_map(env: &Environment, lisp_val: MapContents) -> LispResult {
    let mut env = env.clone();
    let initial: Result<HashMap<LispVal, LispVal>, LispError> = Ok(hashmap!());
    Ok(ExecyBoi {
        val: LispVal::Map(lisp_val.into_iter().fold(initial, |acc, (k, v)| {
            if let Ok(acc) = acc {
                let ExecyBoi {
                    env: next_env,
                    val: evaled_key,
                } = eval(ExecyBoi {
                    env: env.clone(),
                    val: (*k).clone(),
                })?;
                let ExecyBoi {
                    env: next_env,
                    val: evaled_value,
                } = eval(ExecyBoi {
                    env: next_env,
                    val: (*v).clone(),
                })?;
                env = next_env;
                Ok(acc.insert(evaled_key, evaled_value))
            } else {
                acc
            }
        })?),
        env: env,
    })
}

fn eval_atom(env: &Environment, name: AtomContents) -> LispResult {
    if let Some(val) = env.get(&name) {
        // TODO(me) - Is this how Arcs work?
        Ok(ExecyBoi {
            val: val.deref().clone(),
            env: env.clone(),
        })
    } else {
        return Err(LispError::UnboundVar(name.clone()));
    }
}

pub fn eval(lisp_val: ExecyBoi) -> LispResult {
    match lisp_val.val {
        LispVal::DottedList(_) => Ok(lisp_val),
        LispVal::String(_) => Ok(lisp_val),
        LispVal::Number(_) => Ok(lisp_val),
        LispVal::True => Ok(lisp_val),
        LispVal::False => Ok(lisp_val),
        LispVal::Keyword(_) => Ok(lisp_val),
        LispVal::Atom(ac) => eval_atom(&lisp_val.env, ac),
        LispVal::List(lc) => eval_list(&lisp_val.env, lc),
        LispVal::Vector(vc) => eval_vector(&lisp_val.env, vc),
        LispVal::Map(mc) => eval_hash_map(&lisp_val.env, mc),
        val @ LispVal::SpecialForm(_) => {
            return Err(LispError::BadSpecialForm(
                String::from("Bad special form"),
                val,
            ))
        }
    }
}

pub fn eval_start(val: LispResult) -> LispResult {
    eval(val?)
}
