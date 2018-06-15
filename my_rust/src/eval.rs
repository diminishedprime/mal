use lisp_val::LispError;
use lisp_val::LispError::NotImplemented;
use lisp_val::LispVal::{
    Atom, Closure, False, Keyword, LString, List, Map, Nil, Number, True, Vector,
};
use lisp_val::{AtomContents, Environment, LispVal, ListContents, MapContents};
use lisp_val::{ExecyBoi, LispResult};
use std::sync::Arc;

fn unpack_num(l: &LispVal) -> Result<i32, LispError> {
    match l {
        Number(ac) => Ok(*ac),
        _ => Err(LispError::TypeMismatch(String::from("number"), l.clone())),
    }
}

fn unpack_atom(l: &LispVal) -> Result<AtomContents, LispError> {
    match l {
        Atom(ac) => Ok(ac.clone()),
        _ => Err(LispError::TypeMismatch(String::from("Atom"), l.clone())),
    }
}

fn unpack_list(l: LispVal) -> Result<ListContents, LispError> {
    match l {
        List(lc) => Ok(lc),
        _ => Err(LispError::TypeMismatch(String::from("List"), l.clone())),
    }
}

fn unpack_vec(l: LispVal) -> Result<ListContents, LispError> {
    match l {
        Vector(vc) => Ok(vc),
        _ => Err(LispError::TypeMismatch(String::from("Vector"), l.clone())),
    }
}

fn unpack_list_or_vec(l: LispVal) -> Result<Vec<LispVal>, LispError> {
    match l {
        Vector(c) | List(c) => Ok(c),
        _ => Err(LispError::TypeMismatch(
            String::from("Vector or List"),
            l.clone(),
        )),
    }
}

fn unpack_hash_map(l: LispVal) -> Result<MapContents, LispError> {
    match l {
        Map(mc) => Ok(mc),
        _ => Err(LispError::TypeMismatch(String::from("Map"), l.clone())),
    }
}

/// Evaluates an atom
///
/// An atom is evaluated by looking it up in the environment. If it is bound,
/// return the value it is bound to. Otherwise return an UnboundVar error.
fn eval_atom(execy_boi: ExecyBoi) -> LispResult {
    let atom = unpack_atom(&execy_boi.val)?;
    let bound_val = execy_boi.env.get(&atom);
    if let Some(bound_val) = bound_val {
        Ok(execy_boi.with_value((*bound_val).clone()))
    } else {
        Err(LispError::UnboundVar(atom))
    }
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn subtract(a: i32, b: i32) -> i32 {
    a - b
}

fn multiply(a: i32, b: i32) -> i32 {
    a * b
}

fn divide(a: i32, b: i32) -> i32 {
    a / b
}

fn val_with_env(val: LispVal, env: Arc<Environment>) -> ExecyBoi {
    ExecyBoi { val, env }
}

fn eval_binary_num_op(
    env: Arc<Environment>,
    op: &str,
    first: LispVal,
    second: LispVal,
) -> LispResult {
    let first = eval(val_with_env(first, Arc::clone(&env)))?.val;
    let second = eval(val_with_env(second, Arc::clone(&env)))?.val;
    let op = match &op[..] {
        "+" => add,
        "-" => subtract,
        "*" => multiply,
        "/" => divide,
        _ => return Err(LispError::NotFunction(op.to_string())),
    };
    let result = op(unpack_num(&first)?, unpack_num(&second)?);
    Ok(val_with_env(LispVal::from(result), env))
}

fn eval_let_star(env: Arc<Environment>, bindings: LispVal, expression: LispVal) -> LispResult {
    let bindings = unpack_list_or_vec(bindings)?;
    // check bindings is even
    let mut let_star_env = Arc::clone(&env);
    bindings
        .chunks(2)
        .map(|chunk| {
            let name = unpack_atom(&chunk[0])?;
            let eb = eval(val_with_env(chunk[1].clone(), Arc::clone(&let_star_env)))?;
            let_star_env = Arc::new(let_star_env.with_bindings(vec![(name, eb.val)]));
            Ok(())
        })
        .collect::<Result<Vec<_>, _>>()?;
    let val = eval(val_with_env(expression, let_star_env))?.val;
    Ok(val_with_env(val, Arc::clone(&env)))
}

fn eval_def_bang(env: Arc<Environment>, name: LispVal, expression: LispVal) -> LispResult {
    let name = unpack_atom(&name)?;
    let val = eval(val_with_env(expression, Arc::clone(&env)))?.val;
    let env = env.with_binding((name, val.clone()));
    Ok(val_with_env(val, Arc::new(env)))
}

fn eval_list_first_is_atom(list_contents: ListContents, env: Arc<Environment>) -> LispResult {
    match &list_contents[..] {
        [Atom(op), first, second] => match &op[..] {
            "+" | "-" | "*" | "/" => eval_binary_num_op(env, op, first.clone(), second.clone()),
            "let*" => eval_let_star(env, first.clone(), second.clone()),
            "def!" => eval_def_bang(env, first.clone(), second.clone()),
            _ => Err(LispError::NotImplemented(List(vec![
                LispVal::atom_from(op),
                first.clone(),
                second.clone(),
            ]))),
        },
        a => Err(LispError::BadSpecialForm(List(a.to_vec()))),
    }
}

fn eval_list(execy_boi: ExecyBoi) -> LispResult {
    let env = execy_boi.env;
    let list_contents = unpack_list(execy_boi.val)?;
    if list_contents.len() == 0 {
        Ok(ExecyBoi {
            env,
            val: List(list_contents),
        })
    } else {
        match &list_contents[0] {
            Atom(_) => eval_list_first_is_atom(list_contents, env),
            _ => Err(LispError::BadSpecialForm(List(list_contents))),
        }
    }
}

fn eval_vector(execy_boi: ExecyBoi) -> LispResult {
    let env = execy_boi.env;
    let vec_contents = unpack_vec(execy_boi.val)?;
    let vec_contents = vec_contents
        .into_iter()
        .map(|vc| eval(val_with_env(vc, Arc::clone(&env))))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .map(|eb| eb.val)
        .collect::<Vec<LispVal>>();
    Ok(val_with_env(Vector(vec_contents), Arc::clone(&env)))
}

fn eval_hashmap(execy_boi: ExecyBoi) -> LispResult {
    let env = execy_boi.env;
    let hash_contents = unpack_hash_map(execy_boi.val)?;
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

pub fn eval(execy_boi: ExecyBoi) -> LispResult {
    match execy_boi.val {
        // nil -> nil
        // "hi" -> "hi"
        // 3 -> 3
        // :hello -> :hello
        // true -> true
        // false -> false
        Nil | LString(_) | Number(_) | Keyword(_) | True | False => Ok(execy_boi),
        Atom(_) => eval_atom(execy_boi),
        // (+ 1 1) => 2
        // ((fn* [a] a) 3) => 3
        // () => ()
        List(_) => eval_list(execy_boi),
        // [1 2 (+ 1 2)] => [1 2 3]
        Vector(_) => eval_vector(execy_boi),
        // { (+ 1 2) (+ 2 2) } => { 3 4 }
        Map(_) => eval_hashmap(execy_boi),
        Closure(_) => Err(NotImplemented(execy_boi.val)),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use lisp_val::LispVal;
    use lisp_val::{Environment, ExecyBoi};
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
        ];
        for (input, expected) in test_data.into_iter() {
            let input = parse(input);
            let actual = eval(input).unwrap_or_else(|e| panic!("{}", e)).val;
            assert_eq!(actual, expected);
        }
    }

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
}
