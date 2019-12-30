use super::*;
use crate::val;

#[test]
fn parse_env_symbol_names() {
    let env = val::m_env(None);
    env.borrow().keys().iter().for_each(|key| {
        let actual = parse(key).unwrap();
        assert_eq!(actual, val::m_symbol(key))
    });
}

#[test]
fn program_with_comments() {
    let input = std::fs::read_to_string("./src/parser/parse_input/inline-comments.mal").unwrap();
    let actual = parse(&input).unwrap();
    assert_eq!(actual, val::m_symbol("+"),)
}

#[test]
fn program_with_multi_line_comments() {
    let input =
        std::fs::read_to_string("./src/parser/parse_input/multi-line-comments.mal").unwrap();
    let actual = parse(&input).unwrap();
    assert_eq!(actual, val::m_symbol("+"),)
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
        val::m_list(vec![
            val::m_symbol("+"),
            val::m_double(1.0),
            val::m_double(1.0)
        ])
    );
}

#[test]
fn parse_string() {
    let actual = parse(r#" ""  "#).unwrap();
    assert_eq!(actual, val::m_string(""));
}

#[test]
fn parse_string_with_escaped_quote() {
    let actual = parse(r#" "abc\"def"  "#).unwrap();
    assert_eq!(actual, val::m_string(r#"abc\"def"#));
}

#[test]
fn parse_string_with_escaped_quotes() {
    let actual = parse(r#" "\"\""  "#).unwrap();
    assert_eq!(actual, val::m_string(r#"\"\""#));
}

#[test]
fn parse_string_with_newline() {
    let actual = parse(r#" "\n"  "#).unwrap();
    assert_eq!(actual, val::m_string(r#"\n"#));
}

#[test]
fn parse_symbol() {
    let actual = parse("+").unwrap();
    assert_eq!(actual, val::m_symbol("+"));
}

#[test]
fn parse_def_bang() {
    let actual = parse("def!").unwrap();
    assert_eq!(actual, val::m_symbol("def!"));
}

#[test]
fn parse_empty_question() {
    let actual = parse("empty?").unwrap();
    assert_eq!(actual, val::m_symbol("empty?"));
}

#[test]
fn parse_exp() {
    let actual = parse("**").unwrap();
    assert_eq!(actual, val::m_symbol("**"));
}

#[test]
fn parse_symbol_mid_special() {
    let actual = parse("abc-def").unwrap();
    assert_eq!(actual, val::m_symbol("abc-def"));
}

#[test]
fn parse_double() {
    let actual = parse("1.23").unwrap();
    assert_eq!(actual, val::m_double(1.23));
}

#[test]
fn parse_empty_list() {
    let actual = parse("( ) ").unwrap();
    assert_eq!(actual, (val::m_list(vec![])));
}

#[test]
fn parse_quote_1() {
    let actual = parse("'1").unwrap();
    assert_eq!(
        actual,
        (val::m_list(vec![val::m_symbol("quote"), val::m_double(1.0)]))
    );
}

#[test]
fn parse_one_item_list() {
    let actual = parse("(1.34)").unwrap();
    assert_eq!(actual, (val::m_list(vec![val::m_double(1.34)])));
}

#[test]
fn parse_standard_list() {
    let actual = parse("(1.23 1.23)").unwrap();
    assert_eq!(
        actual,
        (val::m_list(vec![val::m_double(1.23), val::m_double(1.23)]))
    );
}

#[test]
fn parse_list_weird_whitespace() {
    let actual = parse(" (  ,1.23 ,,, ,1.23,,, )   ").unwrap();
    assert_eq!(
        actual,
        (val::m_list(vec![val::m_double(1.23), val::m_double(1.23)]))
    );
}

#[test]
fn parse_negative_number() {
    let actual = parse("-3").unwrap();
    assert_eq!(actual, val::m_double(-3.0));
}

#[test]
fn parse_with_meta() {
    let actual = parse("^{1 2} [3 4]").unwrap();
    assert_eq!(
        actual,
        (val::m_list(vec![
            val::m_symbol("with-meta"),
            (val::m_vector(vec![val::m_double(3.0), val::m_double(4.0)])),
            val::m_map(vec![val::m_double(1.0), val::m_double(2.0)]),
        ]))
    );
}
