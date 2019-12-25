use super::*;
use crate::parser::parse;

fn m_false() -> AST {
    AST::Boolean(false)
}

fn m_double(d: f64) -> AST {
    AST::Double(d)
}

fn nil() -> AST {
    AST::Nil
}

fn run_program(program: &str) -> EvalResult<AST> {
    let program = parse(program)?;
    eval(Env::new(), program)
}

#[test]
fn fn_star_thingy() {
    let actual = run_program("( (fn* [f x] (f x)) (fn* [a] (+ 1 a)) 7)").unwrap();
    assert_eq!(actual, m_double(8.0));
}

#[test]
fn fn_star_invoked_with_multiple_args() {
    let actual = run_program("((fn* [a b] (+ a b)) 3 5)").unwrap();
    assert_eq!(actual, m_double(8.0));
}

#[test]
fn fn_star_invoked_with_args() {
    let actual = run_program("((fn* [a] a) 3)").unwrap();
    assert_eq!(actual, m_double(3.0));
}

#[test]
fn fn_star_invoked_no_args() {
    let actual = run_program("((fn* [] 3))").unwrap();
    assert_eq!(actual, m_double(3.0));
}

#[test]
fn fn_star_not_invoked() {
    let actual = run_program("(fn* [a] a)").unwrap();
    assert_eq!(actual.is_lambda(), true);
}

#[test]
fn if_true_only_eval_first() {
    let actual = run_program("(do (if true (def! a 1) (def! a 2)) a)").unwrap();
    assert_eq!(actual, m_double(1.0))
}

#[test]
fn if_false_only_eval_second() {
    let actual = run_program("(do (if false (def! a 1) (def! a 2)) a)").unwrap();
    assert_eq!(actual, m_double(2.0))
}

#[test]
fn if_false_no_third_return_nil() {
    let actual = run_program("(if false 1)").unwrap();
    assert_eq!(actual, nil())
}

#[test]
fn if_true_no_third() {
    let actual = run_program("(if true 1)").unwrap();
    assert_eq!(actual, m_double(1.0))
}

#[test]
fn def_bang() {
    let actual = run_program("(def! a 3)").unwrap();
    assert_eq!(actual, m_double(3.0));
}

#[test]
fn let_star() {
    let actual = run_program("(let* (a 3 b 4 a 6) a)").unwrap();
    assert_eq!(actual, m_double(6.0));
}

#[test]
fn list_builtin() {
    let actual = run_program("(list 1 2)").unwrap();
    assert_eq!(actual, list_of(vec![m_double(1.0), m_double(2.0)]));
}

#[test]
fn list_builtin_nested() {
    let actual = run_program("(list 1 2 (list 3))").unwrap();
    assert_eq!(
        actual,
        list_of(vec![
            m_double(1.0),
            m_double(2.0),
            list_of(vec![m_double(3.0)])
        ])
    );
}

#[test]
fn is_empty() {
    let actual = run_program("(empty? (list 1 2 3))").unwrap();
    assert_eq!(actual, m_false());
}

#[test]
fn do_empty() {
    let actual = run_program("(do)").unwrap();
    assert_eq!(actual, nil());
}

#[test]
fn do_one_expr() {
    let actual = run_program("(do (+ 1 1))").unwrap();
    assert_eq!(actual, m_double(2.0));
}

#[test]
fn do_with_def_expr() {
    let actual = run_program("(do (def! a 1) a)").unwrap();
    assert_eq!(actual, m_double(1.0));
}
