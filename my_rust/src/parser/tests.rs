use super::AST::Double;
use super::AST::LString;
use super::AST::Map;
use super::AST::Symbol;
use super::*;
use crate::env::Env;

#[test]
fn parse_env_symbol_names() {
    let env = Env::new().unwrap();
    let map = &env.borrow().env;
    map.keys().for_each(|key| {
        let actual = parse(key).unwrap();
        assert_eq!(actual, Symbol(key.to_string()))
    });
}

#[test]
fn program_with_inline_comments() {
    let actual = parse(
        r#";; this is a comment
(+ 1 1)
"#,
    )
    .unwrap();
    assert_eq!(
        actual,
        AST::m_list(vec![
            AST::m_symbol("+"),
            AST::m_double(1.0),
            AST::m_double(1.0)
        ])
    );
}

#[test]
fn parse_string() {
    let actual = parse(r#" ""  "#).unwrap();
    assert_eq!(actual, LString("".to_string()));
}

#[test]
fn parse_string_with_escaped_quote() {
    let actual = parse(r#" "abc\"def"  "#).unwrap();
    assert_eq!(actual, LString(r#"abc\"def"#.to_string()));
}

#[test]
fn parse_string_with_escaped_quotes() {
    let actual = parse(r#" "\"\""  "#).unwrap();
    assert_eq!(actual, LString(r#"\"\""#.to_string()));
}

#[test]
fn parse_string_with_newline() {
    let actual = parse(r#" "\n"  "#).unwrap();
    assert_eq!(actual, LString(r#"\n"#.to_string()));
}

#[test]
fn parse_symbol() {
    let actual = parse("+").unwrap();
    assert_eq!(actual, Symbol("+".to_string()));
}

#[test]
fn parse_def_bang() {
    let actual = parse("def!").unwrap();
    assert_eq!(actual, Symbol("def!".to_string()));
}

#[test]
fn parse_empty_question() {
    let actual = parse("empty?").unwrap();
    assert_eq!(actual, Symbol("empty?".to_string()));
}

#[test]
fn parse_exp() {
    let actual = parse("**").unwrap();
    assert_eq!(actual, Symbol("**".to_string()));
}

#[test]
fn parse_symbol_mid_special() {
    let actual = parse("abc-def").unwrap();
    assert_eq!(actual, Symbol("abc-def".to_string()));
}

#[test]
fn parse_double() {
    let actual = parse("1.23").unwrap();
    assert_eq!(actual, Double(1.23));
}

#[test]
fn parse_empty_list() {
    let actual = parse("( ) ").unwrap();
    assert_eq!(actual, (list_of(vec![])));
}

#[test]
fn parse_quote_1() {
    let actual = parse("'1").unwrap();
    assert_eq!(
        actual,
        (list_of(vec![Symbol("quote".to_string()), Double(1.0)]))
    );
}

#[test]
fn parse_one_item_list() {
    let actual = parse("(1.34)").unwrap();
    assert_eq!(actual, (list_of(vec![Double(1.34)])));
}

#[test]
fn parse_standard_list() {
    let actual = parse("(1.23 1.23)").unwrap();
    assert_eq!(actual, (list_of(vec![Double(1.23), Double(1.23)])));
}

#[test]
fn parse_list_weird_whitespace() {
    let actual = parse(" (  ,1.23 ,,, ,1.23,,, )   ").unwrap();
    assert_eq!(actual, (list_of(vec![Double(1.23), Double(1.23)])));
}

#[test]
fn parse_negative_number() {
    let actual = parse("-3").unwrap();
    assert_eq!(actual, Double(-3.0));
}

#[test]
fn parse_with_meta() {
    let actual = parse("^{1 2} [3 4]").unwrap();
    assert_eq!(
        actual,
        (list_of(vec![
            Symbol(String::from("with-meta")),
            (vec_of(vec![Double(3.0), Double(4.0)])),
            Map(vec![Double(1.0), Double(2.0)]),
        ]))
    );
}
