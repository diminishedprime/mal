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

// #[test]
// fn if_true() {
//     let actual = run_program("(if true (+ 1 1) (+ 2 2))").unwrap();
//     assert_eq!(actual, m_double(2.0))
// }

#[test]
fn def_bang() {
    let actual = run_program("(def! a 3)").unwrap();
    assert_eq!(actual, m_double(3.0));
}

#[test]
fn let_star() {
    let program = parse("(let* (a 3 b 4 a 6) a)").unwrap();
    let actual = eval(Env::new(), program).unwrap();
    assert_eq!(actual, m_double(6.0));
}

#[test]
fn list_builtin() {
    let program = parse("(list 1 2)").unwrap();
    let actual = eval(Env::new(), program).unwrap();
    assert_eq!(actual, list_of(vec![m_double(1.0), m_double(2.0)]));
}

#[test]
fn list_builtin_nested() {
    let program = parse("(list 1 2 (list 3))").unwrap();
    let actual = eval(Env::new(), program).unwrap();
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
    let program = parse("(empty? (list 1 2 3))").unwrap();
    let actual = eval(Env::new(), program).unwrap();
    assert_eq!(actual, m_false());
}

#[test]
fn do_empty() {
    let program = parse("(do)").unwrap();
    let actual = eval(Env::new(), program).unwrap();
    assert_eq!(actual, nil());
}

#[test]
fn do_one_expr() {
    let program = parse("(do (+ 1 1))").unwrap();
    let actual = eval(Env::new(), program).unwrap();
    assert_eq!(actual, m_double(2.0));
}

#[test]
fn do_with_def_expr() {
    let program = parse("(do (def! a 1) a)").unwrap();
    let actual = eval(Env::new(), program).unwrap();
    assert_eq!(actual, m_double(1.0));
}
