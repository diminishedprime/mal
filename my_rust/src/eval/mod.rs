use lisp_val;
use lisp_val::LispError;
use lisp_val::LispVal::{
    Atom, Closure, False, Keyword, LString, List, Map, Nil, Number, True, Vector,
};
use lisp_val::{AtomContents, Environment, LispVal, ListContents};
use lisp_val::{ClosureData, ExecyBoi, LispResult};
use std::mem;
use std::sync::Arc;

pub mod prelude;
mod stdlib;

/// Evaluates an atom
///
/// An atom is evaluated by looking it up in the environment. If it is bound,
/// return the value it is bound to. Otherwise return an UnboundVar error.
fn eval_atom(execy_boi: ExecyBoi) -> LispResult {
    let atom = lisp_val::unpack_atom(&execy_boi.val)?;
    let bound_val = execy_boi.env.get(&atom);
    if let Some(bound_val) = bound_val {
        Ok(execy_boi.with_value((*bound_val).clone()))
    } else {
        Err(LispError::UnboundVar(atom))
    }
}

fn val_with_env(val: LispVal, env: Arc<Environment>) -> ExecyBoi {
    ExecyBoi { val, env }
}

fn eval_let_star(env: Arc<Environment>, args: Vec<LispVal>) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::NumArgs(2, LispVal::List(args)));
    }
    let bindings = args[0].clone();
    let expression = args[1].clone();
    let bindings = lisp_val::unpack_list_or_vec(bindings)?;
    // check bindings is even
    let mut let_star_env = Arc::clone(&env);
    bindings
        .chunks(2)
        .map(|chunk| {
            let name = lisp_val::unpack_atom(&chunk[0])?;
            let eb = eval(val_with_env(chunk[1].clone(), Arc::clone(&let_star_env)))?;
            let_star_env = Arc::new(let_star_env.with_bindings(vec![(name, eb.val)]));
            Ok(())
        })
        .collect::<Result<Vec<_>, _>>()?;
    let val = eval(val_with_env(expression, let_star_env))?.val;
    Ok(val_with_env(val, Arc::clone(&env)))
}

fn eval_def_bang(env: Arc<Environment>, args: Vec<LispVal>) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::NumArgs(2, LispVal::List(args)));
    }
    let name = args[0].clone();
    let expression = args[1].clone();
    let name = lisp_val::unpack_atom(&name)?;
    let val = eval(val_with_env(expression, Arc::clone(&env)))?.val;
    let env = env.with_binding((name, val.clone()));
    Ok(val_with_env(val, Arc::new(env)))
}

fn eval_make_list(env: Arc<Environment>, elements: Vec<LispVal>) -> LispResult {
    Ok(val_with_env(
        List(eval_vec_or_list_contents(Arc::clone(&env), elements)?),
        env,
    ))
}

fn eval_do(env: Arc<Environment>, elements: Vec<LispVal>) -> LispResult {
    let acc = Ok((env, Nil));
    let evaled = elements.into_iter().fold(acc, |acc, expr| {
        let env = acc?.0;
        let evaled = eval(val_with_env(expr, Arc::clone(&env)))?;
        Ok((evaled.env, evaled.val))
    });
    let evaled = evaled?;
    Ok(val_with_env(evaled.1, evaled.0))
}

fn eval_if(env: Arc<Environment>, op: &AtomContents, exprs: ListContents) -> LispResult {
    if exprs.len() > 3 {
        Err(LispError::NotImplemented(List({
            let mut list = vec![LispVal::atom_from(op)];
            list.append(&mut exprs.to_vec());
            list
        })))
    } else {
        let pred = exprs[0].clone();
        let evaled_pred = eval(val_with_env(pred, Arc::clone(&env)))?;
        match evaled_pred.val {
            Nil | False => eval(val_with_env(
                exprs.get(2).unwrap_or(&Nil).clone(),
                Arc::clone(&evaled_pred.env),
            )),
            _ => eval(val_with_env(exprs[1].clone(), Arc::clone(&evaled_pred.env))),
        }
    }
}

fn ampersand_pos(b: &Vec<String>) -> Option<usize> {
    for (idx, item) in b.iter().enumerate() {
        if item == "&" {
            return Some(idx);
        }
    }
    None
}

fn eval_fn_star(env: Arc<Environment>, args: Vec<LispVal>) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::NumArgs(2, LispVal::List(args)));
    }
    let bindings = args[0].clone();
    let expression = args[1].clone();
    let bindings = lisp_val::unpack_list_or_vec(bindings)?;
    let mut bindings = bindings
        .iter()
        .map(lisp_val::unpack_atom)
        .collect::<Result<Vec<_>, _>>()?;
    let mut vararg = None;
    if let Some(p) = ampersand_pos(&bindings) {
        let split = bindings.split_off(p);
        vararg = Some(split[1].clone())
    }
    Ok(val_with_env(
        Closure(ClosureData {
            env: Arc::clone(&env),
            body: Arc::new(expression),
            params: bindings,
            vararg,
        }),
        Arc::clone(&env),
    ))
}

fn eval_unary_op(env: Arc<Environment>, op: String, args: Vec<LispVal>) -> LispResult {
    if args.len() != 1 {
        return Err(LispError::NumArgs(1, LispVal::List(args)));
    }
    let arg = args[0].clone();
    let evaled = eval(val_with_env(arg, Arc::clone(&env)))?;
    let f = match &op[..] {
        "list?" => stdlib::is_list,
        "empty?" => stdlib::is_empty,
        "count" => stdlib::count,
        _ => return Err(LispError::NotImplemented(LispVal::Atom(op))),
    };
    Ok(val_with_env(f(evaled.val)?, env))
}

fn eval_binary_op(env: Arc<Environment>, op: String, args: Vec<LispVal>) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::NumArgs(2, LispVal::List(args)));
    }
    let first = args[0].clone();
    let second = args[1].clone();
    let f = match &op[..] {
        "+" => stdlib::add,
        "-" => stdlib::subtract,
        "*" => stdlib::multiply,
        "/" => stdlib::divide,
        "<" => stdlib::lt,
        "<=" => stdlib::lte,
        ">" => stdlib::gt,
        ">=" => stdlib::gte,
        "=" => stdlib::eq,
        _ => return Err(LispError::NotImplemented(LispVal::Atom(op))),
    };
    let first = eval(val_with_env(first, Arc::clone(&env)))?;
    let second = eval(val_with_env(second, Arc::clone(&env)))?;
    Ok(val_with_env(f(first.val, second.val)?, env))
}

fn eval_pr_str(env: Arc<Environment>, args: Vec<LispVal>) -> LispResult {
    let results = eval_vec_or_list_contents(Arc::clone(&env), args)?;
    Ok(val_with_env(stdlib::pr_str(results)?, env))
}

fn eval_println(env: Arc<Environment>, args: Vec<LispVal>) -> LispResult {
    let results = eval_vec_or_list_contents(Arc::clone(&env), args)?;
    Ok(val_with_env(stdlib::println(results)?, env))
}

fn eval_prn(env: Arc<Environment>, args: Vec<LispVal>) -> LispResult {
    let results = eval_vec_or_list_contents(Arc::clone(&env), args)?;
    Ok(val_with_env(stdlib::prn(results)?, env))
}

fn eval_str(env: Arc<Environment>, args: Vec<LispVal>) -> LispResult {
    let results = eval_vec_or_list_contents(Arc::clone(&env), args)?;
    Ok(val_with_env(stdlib::str(results)?, env))
}

fn apply_closure(env: Arc<Environment>, list_contents: Vec<LispVal>) -> LispResult {
    let ClosureData {
        params,
        body,
        env: closure_env,
        vararg,
    } = lisp_val::unpack_closure(list_contents[0].clone())?;
    let evaled_params = list_contents[1..]
        .to_vec()
        .into_iter()
        .map(|expr| eval(val_with_env(expr, Arc::clone(&env))))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|eb| eb.val)
        .collect::<Vec<_>>();
    let param_len = params.len();
    let mut bindings = params
        .into_iter()
        .zip(evaled_params.clone())
        .collect::<Vec<(_, _)>>();
    if let Some(n) = vararg {
        let rest = evaled_params
            .into_iter()
            .skip(param_len)
            .collect::<Vec<_>>();
        bindings.push((n, List(rest)));
    }
    let env = env.with_bindings(bindings).over_env(&closure_env);
    eval(val_with_env((*body).clone(), Arc::new(env)))
}

fn eval_vec_or_list_contents(
    env: Arc<Environment>,
    contents: Vec<LispVal>,
) -> Result<Vec<LispVal>, LispError> {
    Ok(contents
        .into_iter()
        .map(|vc| eval(val_with_env(vc, Arc::clone(&env))))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|eb| eb.val)
        .collect::<Vec<LispVal>>())
}

fn eval_vector(execy_boi: ExecyBoi) -> LispResult {
    let env = execy_boi.env;
    let vec_contents = lisp_val::unpack_vec(execy_boi.val)?;
    let vec_contents = eval_vec_or_list_contents(Arc::clone(&env), vec_contents)?;
    Ok(val_with_env(Vector(vec_contents), Arc::clone(&env)))
}

fn eval_hashmap(execy_boi: ExecyBoi) -> LispResult {
    let env = execy_boi.env;
    let hash_contents = lisp_val::unpack_hash_map(execy_boi.val)?;
    let hash_contents = hash_contents
        .into_iter()
        .fold(Ok(hashmap!()), |map, (k, v)| {
            let map = map?;
            let k = eval(val_with_env((*k).clone(), Arc::clone(&env)))?.val;
            let v = eval(val_with_env((*v).clone(), Arc::clone(&env)))?.val;
            Ok(map.insert(k, v))
        })?;
    Ok(val_with_env(Map(hash_contents), Arc::clone(&env)))
}

fn eval_ast(execy_boi: ExecyBoi) -> LispResult {
    let env = execy_boi.env;
    let val = execy_boi.val;
    match val {
        Nil | LString(_) | Number(_) | Keyword(_) | True | False | Closure(_) => {
            Ok(val_with_env(val, env))
        }
        Atom(_) => eval_atom(val_with_env(val, env)),
        Vector(_) => eval_vector(val_with_env(val, env)),
        Map(_) => eval_hashmap(val_with_env(val, env)),
        _ => Err(LispError::BadSpecialForm(val)),
    }
}

fn apply(
    f: Box<Fn(Arc<Environment>, Vec<LispVal>) -> LispResult>,
    env: Arc<Environment>,
    args: Vec<LispVal>,
) -> LispResult {
    f(env, args)
}

fn f_map(
    env: Arc<Environment>,
    f_name: String,
) -> Result<Box<Fn(Arc<Environment>, Vec<LispVal>) -> LispResult>, LispError> {
    match &f_name[..] {
        "list" => Ok(Box::new(eval_make_list)),
        "pr-str" => Ok(Box::new(eval_pr_str)),
        "str" => Ok(Box::new(eval_str)),
        "prn" => Ok(Box::new(eval_prn)),
        "println" => Ok(Box::new(eval_println)),
        "list?" => Ok(Box::new(move |env, rest| {
            eval_unary_op(env, f_name.to_string(), rest)
        })),
        "empty?" => Ok(Box::new(move |env, rest| {
            eval_unary_op(env, f_name.to_string(), rest)
        })),
        "count" => Ok(Box::new(move |env, rest| {
            eval_unary_op(env, f_name.to_string(), rest)
        })),
        "+" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        "-" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        "*" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        "/" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        "<" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        "<=" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        ">" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        ">=" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        "=" => Ok(Box::new(move |env, rest| {
            eval_binary_op(env, f_name.to_string(), rest)
        })),
        _ if env.get(&f_name).is_some() => Ok(Box::new(move |env, rest| {
            let mut with_fn_first = rest.clone();
            let f = env.get(&f_name).unwrap();
            with_fn_first.insert(0, (*f).clone());
            let as_list = List(with_fn_first);
            eval(val_with_env(as_list, env))
        })),
        _ => Err(LispError::NotFunction(f_name.to_string())),
    }
}

pub fn eval(execy_boi: ExecyBoi) -> LispResult {
    let env = execy_boi.env;
    let val = execy_boi.val;
    match val {
        List(list_contents) => match &list_contents[..] {
            [] => Ok(val_with_env(List(list_contents.to_vec()), env)),
            [Atom(op), rest..] => {
                let rest = rest.to_vec();
                let l = rest.len();
                match &op[..] {
                    "def!" => eval_def_bang(env, rest),
                    "let*" => eval_let_star(env, rest),
                    "do" => eval_do(env, rest),
                    "if" if l == 2 || l == 3 => eval_if(env, op, rest),
                    "fn*" => eval_fn_star(env, rest),
                    f_name => {
                        let f = f_map(Arc::clone(&env), f_name.to_string())?;
                        apply(f, env, rest)
                    }
                }
            }
            list @ [Closure(_), _..] => apply_closure(env, list.to_vec()),
            list @ [List(_), _..] => {
                let mut list_contents = list.to_vec();
                let eb = val_with_env(list_contents[0].clone(), Arc::clone(&env));
                let evaled = eval(eb)?;
                let val = evaled.val;
                let env = evaled.env;
                mem::replace(&mut list_contents[0], val);
                eval(val_with_env(List(list_contents), Arc::clone(&env)))
            }
            _ => Err(LispError::BadSpecialForm(List(list_contents.clone()))),
        },
        _ => eval_ast(val_with_env(val, env)),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use lisp_val::LispVal;
    use lisp_val::{ClosureData, Environment, ExecyBoi};
    use parser;
    use std::sync::Arc;

    fn parse(s: &str) -> ExecyBoi {
        parse_with_env(s, Environment::new())
    }

    fn parse_with_env(s: &str, env: Environment) -> ExecyBoi {
        ExecyBoi {
            env: Arc::new(env),
            val: parser::parse(s).unwrap(),
        }
    }

    fn env_for_bindings(bindings: Vec<(&str, LispVal)>) -> Environment {
        Environment::new().with_bindings(
            bindings
                .into_iter()
                .map(|(s, l)| (String::from(s), l))
                .collect::<Vec<_>>(),
        )
    }

    #[test]
    fn bunch_of_tests() {
        let test_data = vec![
            (r#" "hi there"  "#, LispVal::string_from("hi there")),
            ("3", LispVal::from(3)),
            ("true", True),
            ("false", False),
            ("nil", Nil),
            (":hi", LispVal::keyword_from("hi")),
            ("()", LispVal::from(vec![])),
            ("(+ 1 2)", LispVal::from(3)),
            ("(- 3 2)", LispVal::from(1)),
            ("(* -3 2)", LispVal::from(-6)),
            ("(/ 8 2)", LispVal::from(4)),
            (
                "[1 2 (+ 1 2)]",
                Vector(vec![Number(1), Number(2), Number(3)]),
            ),
            ("{ (+ 1 2) (+ 2 2) }", Map(hashmap!(Number(3) => Number(4)))),
            ("(let* [a 2] (let* [b a] b))", Number(2)),
            ("(let* [a (+ 1 2) a (+ a a)] a)", Number(6)),
            ("(= 1 2)", False),
            ("(= 1 1)", True),
            ("(= [(+ 1 2) 2] [3 (+ 1 1)] )", True),
            ("(list 1 2)", List(vec![Number(1), Number(2)])),
            ("(list 1 2 3)", List(vec![Number(1), Number(2), Number(3)])),
            ("(< 1 2)", True),
            ("(< 2 1)", False),
            ("(<= 1 1)", True),
            ("(<= 1 2)", True),
            ("(<= 2 1)", False),
            ("(> 1 2)", False),
            ("(> 2 1)", True),
            ("(>= 1 1)", True),
            ("(>= 1 2)", False),
            ("(>= 2 1)", True),
            ("(do (def! a 3) a)", Number(3)),
            ("(if true 3 a)", Number(3)),
            ("(if false a 3)", Number(3)),
            ("(if (def! a true) a false)", True),
            (
                "(fn* [] 3)",
                Closure(ClosureData {
                    vararg: None,
                    params: vec![],
                    body: Arc::new(Number(3)),
                    env: Arc::new(Environment::new()),
                }),
            ),
            ("((fn* [] 3))", Number(3)),
            ("(do (def! a (fn* [] 3)) (a))", Number(3)),
            ("(do (def! a (fn* [a] a)) (a 3))", Number(3)),
            ("(list? (list 1 2 3))", True),
            ("(empty? (list))", True),
            ("(empty? (list 1))", False),
            ("(count (list 1))", Number(1)),
            ("((fn* (& more) (list? more)))", True),
            // // (pr-str "") => "\\"\\""
            // (r#"(pr-str "")"#, LispVal::from(r#"\\"\\""#)),
        ];
        for (input, expected) in test_data.into_iter() {
            let input = parse(input);
            let actual = eval(input).unwrap_or_else(|e| panic!("{}", e)).val;
            assert_eq!(actual, expected);
        }
    }

    // #[test]
    // fn bunch_of_tests_prelude() {
    //     let test_data = vec![("(not true)", False), ("(not false)", True)];
    //     for (input, expected) in test_data.into_iter() {
    //         let input = parse_with_env(input, Environment::prelude().unwrap());
    //         let actual = eval(input).unwrap_or_else(|e| panic!("{}", e)).val;
    //         assert_eq!(actual, expected);
    //     }
    // }

    #[test]
    fn bunch_of_tests_with_env() {
        let test_data = vec![
            (
                "a",
                env_for_bindings(vec![("a", LispVal::from(3))]),
                LispVal::from(3),
            ),
            (
                "do",
                env_for_bindings(vec![("do", LispVal::from(13))]),
                LispVal::from(13),
            ),
        ];
        for (input, env, expected) in test_data.into_iter() {
            let input = parse_with_env(input, env);
            let actual = eval(input).unwrap_or_else(|e| panic!("{}", e)).val;
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn multi_step_tests_with_env() {
        let test_data = vec![
            (vec!["(def! a 3)", "a"], LispVal::from(3)),
            (vec!["(def! a 3)", "(let* [a 4] a)"], LispVal::from(4)),
            (vec!["(def! a 3)", "(let* [a 4] a)", "a"], LispVal::from(3)),
        ];
        for (inputs, expected) in test_data.into_iter() {
            let mut env = Environment::new();
            let mut last_val = None;
            for input in inputs {
                let input = parse_with_env(input, env);
                let last = eval(input).unwrap_or_else(|e| panic!("{}", e));
                env = (*last.env).clone();
                last_val = Some(last.val);
            }
            assert_eq!(last_val.unwrap(), expected);
        }
    }

    #[test]
    fn eval_special_form_unbound() {
        let input = parse("do");
        let actual = eval(input).unwrap_err();
        let expected = LispError::UnboundVar(String::from("do"));
        assert_eq!(actual, expected);
    }

    #[test]
    fn eval_atom_unbound() {
        let input = parse("a");
        let actual = eval(input).unwrap_err();
        let expected = LispError::UnboundVar(String::from("a"));
        assert_eq!(actual, expected);
    }

    #[test]
    fn test() {
        // (pr-str "")\r\n"\\"\\""'
    }
}
