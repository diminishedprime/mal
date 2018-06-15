use im::HashMap;
use lisp_val::Environment;
use lisp_val::ExecyBoi;
use lisp_val::LispError;
use lisp_val::LispResult;
use lisp_val::LispVal;
use lisp_val::SpecialForm::{DefBang, Do, FnStar, If, LetStar, Quote};
use lisp_val::{AtomContents, Binding, ClosureData, ListContents, MapContents, VecContents};
use std::sync::Arc;

#[cfg(test)]
mod tests {

    use self::LispError::NumArgs;
    use super::*;
    use parser;

    fn parse_eval(s: &str) -> LispResult {
        eval_start(Ok(ExecyBoi::from(parser::parse(s).unwrap())))
    }

    #[test]
    fn evals_to_self() {
        let inputs = vec![r#""my string""#, "3", "#t", "#f"];
        for input in inputs {
            let parsed = Ok(ExecyBoi::from(parser::parse(input).unwrap()));
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
        let expr = Ok(ExecyBoi::from(parser::parse(r#"(+ 2 "hi")"#).unwrap()));
        if let Ok(_) = eval_start(expr) {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn subtraction_no_args() {
        let expr = Ok(ExecyBoi::from(parser::parse(r#"(-)"#).unwrap()));
        if let Err(NumArgs(num, _)) = eval_start(expr) {
            assert_eq!(num, 1)
        } else {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn division_no_args() {
        let expr = Ok(ExecyBoi::from(parser::parse(r#"(/)"#).unwrap()));
        if let Err(NumArgs(num, _)) = eval_start(expr) {
            assert_eq!(num, 1)
        } else {
            panic!("This should not eval successfully.")
        }
    }

    #[test]
    fn if_works_truthy() {
        let expr = Ok(ExecyBoi::from(
            parser::parse(r#"(if "truthy" "hi" "there")"#).unwrap(),
        ));
        let actual = eval_start(expr).unwrap().val;
        let expected = LispVal::string_from("hi");
        assert_eq!(actual, expected);
    }

    #[test]
    fn if_works_false() {
        let expr = Ok(ExecyBoi::from(
            parser::parse(r#"(if #f "hi" "there")"#).unwrap(),
        ));
        let actual = eval_start(expr).unwrap().val;
        let expected = LispVal::string_from("there");
        assert_eq!(actual, expected);
    }

    #[test]
    fn def_bang_test() {
        let expr1 = ExecyBoi::from(parser::parse(r#"(def! a "hi there")"#).unwrap());
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
        let parsed = Ok(ExecyBoi::from(parser::parse(input).unwrap()));
        let actual = eval_start(parsed).unwrap().val;
        assert_eq!(actual, LispVal::from(3));
    }

    #[test]
    fn let_star_fancy_test() {
        let input = "(let* [p (+ 2 3) q (+ 2 p)] (+ p q))";
        let parsed = Ok(ExecyBoi::from(parser::parse(input).unwrap()));
        let actual = eval_start(parsed).unwrap().val;
        assert_eq!(actual, LispVal::from(12));
    }

    #[test]
    fn fn_star_works() {
        let input = "((fn* [] 4))";
        let parsed = Ok(ExecyBoi::from(parser::parse(input).unwrap()));
        let actual = eval_start(parsed).unwrap().val;
        assert_eq!(actual, LispVal::from(4));
    }

    #[test]
    fn fn_star_pass_a_function() {
        let input = "( (fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)";
        let parsed = Ok(ExecyBoi::from(parser::parse(input).unwrap()));
        let actual = eval_start(parsed).unwrap().val;
        assert_eq!(actual, LispVal::from(8));
    }

}

fn unpack_atom(val: LispVal) -> Result<String, LispError> {
    match val {
        LispVal::Atom(a) => Ok(a),
        _ => Err(LispError::TypeMismatch(String::from("atom"), val)),
    }
}

fn unpack_closure(val: LispVal) -> Result<ClosureData, LispError> {
    match val {
        LispVal::Closure(cd) => Ok(cd),
        _ => Err(LispError::TypeMismatch(String::from("closure"), val)),
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
    env: Arc<Environment>,
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
    env: Arc<Environment>,
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
    env: Arc<Environment>,
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
        "=" => {
            let mut iter = args.into_iter();
            let left = iter.next().unwrap();
            let left = eval(ExecyBoi {
                val: left,
                env: Arc::clone(&env),
            })?.val;
            let right = iter.next().unwrap();
            let right = eval(ExecyBoi {
                val: right,
                env: Arc::clone(&env),
            })?.val;
            Ok(Some(Ok(ExecyBoi {
                val: if left == right {
                    LispVal::True
                } else {
                    LispVal::False
                },
                env: Arc::clone(&env),
            })))
        }
        _ => Ok(None),
    }
}

fn eval_let_star(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let bindings = &lisp_val[1];
    let body = &lisp_val[2];
    // bindings must be a list or vec of even elements
    let bindings = unpack_list_or_vec(bindings.clone())?;
    // This could be a LispError instead of a panic.
    assert!(bindings.len() % 2 == 0);

    let mut let_star_env = Arc::clone(&env);
    for chunk in bindings.chunks(2) {
        let name = chunk[0].clone();
        let val = chunk[1].clone();
        let execy_boi = ExecyBoi {
            val,
            env: Arc::clone(&let_star_env),
        };
        let name = unpack_atom(name)?;
        let evaled = eval(execy_boi)?;
        let_star_env = Arc::new(let_star_env.set(name, evaled.val));
    }
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

fn eval_do(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let forms = &lisp_val[1..];
    let mut val = None;
    let env = forms.clone().iter().fold(Ok(env.clone()), |env, form| {
        let env = env?;
        let ExecyBoi {
            env: next_env,
            val: last_val,
        } = eval(ExecyBoi {
            env: env,
            val: form.clone(),
        })?;
        val = Some(last_val);
        Ok(next_env)
    })?;
    Ok(ExecyBoi {
        env: env,
        val: val.unwrap(),
    })
}

fn eval_def_bang(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let name = unpack_atom(lisp_val[1].clone())?;
    let val = &lisp_val[2];
    let evaled = eval(ExecyBoi {
        env: env.clone(),
        val: val.clone(),
    })?;
    let new_env = env.set(name.clone(), evaled.val.clone());
    Ok(ExecyBoi {
        env: Arc::new(new_env),
        val: evaled.val,
    })
}

fn eval_if(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let pred = &lisp_val[1];
    let conseq = &lisp_val[2];
    let ExecyBoi {
        env: new_env,
        val: pred_val,
    } = eval(ExecyBoi {
        env: env.clone(),
        val: pred.clone(),
    })?;
    // TODO(me) - Do I really need to clone pred, conseq, and alt?
    match pred_val {
        LispVal::False | LispVal::Nil if lisp_val.len() == 3 => Ok(ExecyBoi {
            env: new_env,
            val: LispVal::Nil,
        }),
        LispVal::False | LispVal::Nil if lisp_val.len() == 4 => eval(ExecyBoi {
            env: new_env,
            val: lisp_val[3].clone(),
        }),
        _ => eval(ExecyBoi {
            env: new_env,
            val: conseq.clone(),
        }),
    }
}

fn eval_quote(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let val = &lisp_val[1];
    Ok(ExecyBoi {
        val: val.clone(),
        env: env.clone(),
    })
}

fn eval_bound(
    env: Arc<Environment>,
    name: &str,
    args: ListContents,
) -> Result<Option<LispResult>, LispError> {
    if let Some(bound) = env.get(&String::from(name)) {
        let mut args = args.clone();
        args.insert(0, (*bound).clone());
        Ok(Some(eval(ExecyBoi {
            env: Arc::clone(&env),
            val: LispVal::List(args),
        })))
    } else {
        Ok(None)
    }
}

fn eval_function(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let name = unpack_atom(lisp_val[0].clone())?;
    let args = lisp_val.clone()
        .into_iter()
    // We don't want the first since it's the function.
        .skip(1)
        .collect::<Vec<LispVal>>();
    if let Some(result) = eval_primatives(Arc::clone(&env), &name, args.clone())? {
        result
    } else if let Some(result) = eval_bound(env, &name, args)? {
        result
    } else {
        Err(LispError::NotFunction(name.clone()))
    }
}

fn unpack_name_bindings(vals: Vec<LispVal>) -> Result<Vec<AtomContents>, LispError> {
    vals.into_iter()
        .map(|val| unpack_atom(val))
        .collect::<Result<Vec<String>, LispError>>()
}

// (fn* (a b) (+ a b))
fn eval_fn_star(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let outer_env = Arc::clone(&env);
    let bindings = unpack_list_or_vec(lisp_val[1].clone())?;
    let name_bindings = unpack_name_bindings(bindings)?;
    let body = Arc::new(lisp_val[2].clone());
    Ok(ExecyBoi {
        val: LispVal::Closure(ClosureData {
            name_bindings,
            body,
            env: outer_env,
        }),
        env: env,
    })
}

fn eval_closure(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let mut iter = lisp_val.into_iter();
    let first_entry = eval(ExecyBoi {
        env: Arc::clone(&env),
        val: iter.next().unwrap(),
    })?;
    let ClosureData {
        name_bindings,
        body,
        env,
    } = unpack_closure(first_entry.val)?;
    let evaled_bois =
        iter.map(|arg| {
            eval(ExecyBoi {
                env: Arc::clone(&env),
                val: arg,
            })
        }).collect::<Result<Vec<ExecyBoi>, _>>()?;
    let evaled_args = evaled_bois.into_iter().map(|s| s.val);
    let bindings = name_bindings
        .into_iter()
        .zip(evaled_args)
        .collect::<Vec<Binding>>();
    let full_env = env.with_bindings(bindings);
    eval(ExecyBoi {
        env: Arc::new(full_env),
        // Change val in execy boy to be arc...
        val: (*body).clone(),
    })
}

fn eval_closure_2(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    let mut iter = lisp_val.into_iter();
    let first_entry = iter.next().unwrap();
    // TODO(mjhamrick) - I'm pretty sure I need to combine the closure env with
    // the current env?
    let ClosureData {
        name_bindings,
        body,
        env: _closure_env,
    } = unpack_closure(first_entry)?;
    let evaled_bois = iter;
    let evaled_args = evaled_bois;
    let bindings = name_bindings
        .into_iter()
        .zip(evaled_args)
        .collect::<Vec<Binding>>();
    let full_env = env.with_bindings(bindings);
    eval(ExecyBoi {
        env: Arc::new(full_env),
        // Change val in execy boy to be arc...
        val: (*body).clone(),
    })
}

fn eval_list(env: Arc<Environment>, lisp_val: ListContents) -> LispResult {
    if lisp_val.len() == 0 {
        return Ok(ExecyBoi {
            val: LispVal::from(vec![]),
            env: env.clone(),
        });
    }
    let first_entry = lisp_val[0].clone();
    match first_entry {
        LispVal::SpecialForm(LetStar) => eval_let_star(env, lisp_val),
        LispVal::SpecialForm(Do) => eval_do(env, lisp_val),
        LispVal::SpecialForm(DefBang) => eval_def_bang(env, lisp_val),
        LispVal::SpecialForm(If) => eval_if(env, lisp_val),
        LispVal::SpecialForm(Quote) => eval_quote(env, lisp_val),
        LispVal::SpecialForm(FnStar) => eval_fn_star(env, lisp_val),
        LispVal::Atom(_) => eval_function(env, lisp_val),
        LispVal::List(_) => eval_closure(env, lisp_val),
        LispVal::Closure(_) => eval_closure_2(env, lisp_val),
        _ => Err(LispError::BadSpecialForm(
            String::from("Unrecognized special form"),
            LispVal::List(lisp_val),
        )),
    }
}

fn eval_vector(env: Arc<Environment>, lisp_val: VecContents) -> LispResult {
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

fn eval_hash_map(env: Arc<Environment>, lisp_val: MapContents) -> LispResult {
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

fn eval_atom(env: Arc<Environment>, name: AtomContents) -> LispResult {
    if let Some(val) = env.get(&name) {
        let val = (*val).clone();
        if let LispVal::Atom(name) = val {
            eval_atom(env, name)
        } else {
            // TODO(me) - Is this how Arcs work?
            Ok(ExecyBoi {
                val: val,
                env: env.clone(),
            })
        }
    } else {
        return Err(LispError::UnboundVar(name.clone()));
    }
}

pub fn eval(lisp_val: ExecyBoi) -> LispResult {
    match lisp_val.val {
        LispVal::Nil => Ok(lisp_val),
        LispVal::DottedList(_) => Ok(lisp_val),
        LispVal::LString(_) => Ok(lisp_val),
        LispVal::Number(_) => Ok(lisp_val),
        LispVal::True => Ok(lisp_val),
        LispVal::False => Ok(lisp_val),
        LispVal::Keyword(_) => Ok(lisp_val),
        LispVal::Closure(_) => Ok(lisp_val),
        LispVal::Atom(ac) => eval_atom(lisp_val.env, ac),
        LispVal::List(lc) => eval_list(lisp_val.env, lc),
        LispVal::Vector(vc) => eval_vector(lisp_val.env, vc),
        LispVal::Map(mc) => eval_hash_map(lisp_val.env, mc),
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
