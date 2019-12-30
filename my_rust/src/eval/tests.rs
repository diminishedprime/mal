use super::*;
use crate::parser::parse;
use crate::val;
use crate::val::EvalResult;
use crate::val::MalVal;

fn run_program(program: &str) -> EvalResult<MalVal> {
    let program = parse(program)?;
    eval(val::m_env(None), program)
}

#[test]
fn fn_star_nested_closures() {
    let actual = run_program("( ( (fn* (a) (fn* (b) (+ a b))) 5) 7)").unwrap();
    assert_eq!(actual, val::m_double(12.0));
}

#[test]
fn fn_star_with_and_more_empty_more() {
    let actual = run_program("((fn* (a & more) (count more)) 1)").unwrap();
    assert_eq!(actual, val::m_double(0.0));
}

#[test]
fn fn_star_with_and_more() {
    let actual = run_program("( (fn* [& more] more) 1 2 3)").unwrap();
    assert_eq!(
        actual,
        val::m_list(vec![
            val::m_double(1.0),
            val::m_double(2.0),
            val::m_double(3.0)
        ])
    );
}

#[test]
fn fn_star_thingy() {
    let actual = run_program("( (fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)").unwrap();
    assert_eq!(actual, val::m_double(8.0));
}

#[test]
fn fn_star_invoked_with_multiple_args() {
    let actual = run_program("((fn* [a b] (+ a b)) 3 5)").unwrap();
    assert_eq!(actual, val::m_double(8.0));
}

#[test]
fn fn_star_invoked_with_args() {
    let actual = run_program("((fn* [a] a) 3)").unwrap();
    assert_eq!(actual, val::m_double(3.0));
}

#[test]
fn fn_star_invoked_no_args() {
    let actual = run_program("((fn* [] 3))").unwrap();
    assert_eq!(actual, val::m_double(3.0));
}

// #[test]
// fn fn_star_not_invoked() {
//     let actual = run_program("(fn* [a] a)").unwrap();
//     assert_eq!(actual.is_lambda(), true);
// }

#[test]
fn if_true_only_eval_first() {
    let actual = run_program("(do (if true (def! a 1) (def! a 2)) a)").unwrap();
    assert_eq!(actual, val::m_double(1.0))
}

#[test]
fn if_false_only_eval_second() {
    let actual = run_program("(do (if false (def! a 1) (def! a 2)) a)").unwrap();
    assert_eq!(actual, val::m_double(2.0))
}

#[test]
fn if_false_no_third_return_nil() {
    let actual = run_program("(if false 1)").unwrap();
    assert_eq!(actual, val::m_nil())
}

#[test]
fn if_true_no_third() {
    let actual = run_program("(if true 1)").unwrap();
    assert_eq!(actual, val::m_double(1.0))
}

#[test]
fn def_bang() {
    let actual = run_program("(def! a 3)").unwrap();
    assert_eq!(actual, val::m_double(3.0));
}

#[test]
fn let_star() {
    let actual = run_program("(let* (a 3 b 4 a 6) a)").unwrap();
    assert_eq!(actual, val::m_double(6.0));
}

#[test]
fn list_builtin() {
    let actual = run_program("(list 1 2)").unwrap();
    assert_eq!(
        actual,
        val::m_list(vec![val::m_double(1.0), val::m_double(2.0)])
    );
}

#[test]
fn list_builtin_nested() {
    let actual = run_program("(list 1 2 (list 3))").unwrap();
    assert_eq!(
        actual,
        val::m_list(vec![
            val::m_double(1.0),
            val::m_double(2.0),
            val::m_list(vec![val::m_double(3.0)])
        ])
    );
}

#[test]
fn is_empty() {
    let actual = run_program("(empty? (list 1 2 3))").unwrap();
    assert_eq!(actual, val::m_bool(false));
}

#[test]
fn do_empty() {
    let actual = run_program("(do)").unwrap();
    assert_eq!(actual, val::m_nil());
}

#[test]
fn do_one_expr() {
    let actual = run_program("(do (+ 1 1))").unwrap();
    assert_eq!(actual, val::m_double(2.0));
}

#[test]
fn do_with_def_expr() {
    let actual = run_program("(do (def! a 1) a)").unwrap();
    assert_eq!(actual, val::m_double(1.0));
}

// #[test]
// fn create_atom() {
//     let actual = run_program("(atom 3)").unwrap();
//     assert_eq!(actual, val::m_atom(val::m_double(3.0)));
// }

#[test]
fn deref_atom() {
    let actual = run_program("(do (def! a (atom 3)) @a)").unwrap();
    assert_eq!(actual, val::m_double(3.0));
}

#[test]
fn reset_atom() {
    let actual = run_program("(do (def! a (atom 3)) (reset! a 4) @a)").unwrap();
    assert_eq!(actual, val::m_double(4.0));
}
