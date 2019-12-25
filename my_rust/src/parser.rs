use crate::ast::list_of;
use crate::ast::vec_of;
use crate::ast::AST;
use crate::eval::EvalResult;
use nom::branch::alt;
use nom::bytes::complete::escaped;
use nom::bytes::complete::is_a;
use nom::bytes::complete::is_not;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::character::complete::char;
use nom::character::complete::one_of;
use nom::combinator::map;
use nom::combinator::not;
use nom::combinator::opt;
use nom::multi::many0;
use nom::multi::separated_list;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::IResult;

fn vector(i: &str) -> IResult<&str, AST> {
    map(
        delimited(
            char('['),
            separated_list(whitespace, ast),
            preceded(optional_whitespace, char(']')),
        ),
        vec_of,
    )(i)
}

fn parse_map(i: &str) -> IResult<&str, AST> {
    map(
        delimited(
            char('{'),
            separated_list(whitespace, ast),
            preceded(optional_whitespace, char('}')),
        ),
        AST::Map,
    )(i)
}

fn list(i: &str) -> IResult<&str, AST> {
    map(
        delimited(
            char('('),
            separated_list(whitespace, ast),
            preceded(optional_whitespace, char(')')),
        ),
        list_of,
    )(i)
}

// TODO - I need this to check that its followed by whitespace, but I can't consume the
// whitespace...???
// TODO - I think I need to use peek here.
fn special_symbol(i: &str) -> IResult<&str, AST> {
    map(
        alt((
            terminated(is_a("&+*/-="), not(one_of("0123456789"))),
            tag("empty?"),
        )),
        |c: &str| {
            let mut s = String::new();
            s.push_str(c);
            AST::Symbol(s)
        },
    )(i)
}

fn symbol_char(i: &str) -> IResult<&str, &str> {
    alt((is_a("+*-?!"), alphanumeric1))(i)
}

fn symbol(i: &str) -> IResult<&str, AST> {
    let (remaining, first_bits) = alt((alpha1, is_a("+*-?/<>=")))(i)?;
    map(many0(symbol_char), move |rest: Vec<&str>| {
        let mut s = String::new();
        s.push_str(&first_bits);
        rest.iter().for_each(|r| s.push_str(&r));
        AST::Symbol(s)
    })(&remaining)
}

fn double(i: &str) -> IResult<&str, AST> {
    map(nom::number::complete::double, AST::Double)(i)
}

fn parse_str(i: &str) -> IResult<&str, AST> {
    map(
        escaped(is_not("\"\\"), '\\', one_of(r#"\"ntr"#)),
        |s: &str| AST::LString(s.to_string()),
    )(i)
}

fn string(i: &str) -> IResult<&str, AST> {
    preceded(char('\"'), terminated(parse_str, char('\"')))(i)
}

fn optional_whitespace(i: &str) -> IResult<&str, Option<&str>> {
    opt(whitespace)(i)
}

fn whitespace(i: &str) -> IResult<&str, &str> {
    is_a(" ,")(i)
}

fn quote(i: &str) -> IResult<&str, AST> {
    map(preceded(char('\''), ast), |ast| {
        list_of(vec![AST::Symbol(String::from("quote")), ast])
    })(i)
}

fn quasiquote(i: &str) -> IResult<&str, AST> {
    map(preceded(char('`'), ast), |ast| {
        list_of(vec![AST::Symbol(String::from("quasiquote")), ast])
    })(i)
}

fn splice_unquote(i: &str) -> IResult<&str, AST> {
    map(preceded(tag("~@"), ast), |ast| {
        list_of(vec![AST::Symbol(String::from("splice-unquote")), ast])
    })(i)
}

fn unquote(i: &str) -> IResult<&str, AST> {
    map(preceded(char('~'), ast), |ast| {
        list_of(vec![AST::Symbol(String::from("unquote")), ast])
    })(i)
}

fn deref(i: &str) -> IResult<&str, AST> {
    map(preceded(char('@'), ast), |ast| {
        list_of(vec![AST::Symbol(String::from("deref")), ast])
    })(i)
}

fn keyword(i: &str) -> IResult<&str, AST> {
    map(preceded(char(':'), alphanumeric1), |s: &str| {
        AST::Keyword(s.to_string())
    })(i)
}

fn with_meta(i: &str) -> IResult<&str, AST> {
    map(preceded(char('^'), pair(parse_map, ast)), |(m, a)| {
        list_of(vec![AST::Symbol(String::from("with-meta")), a, m])
    })(i)
}

fn nil(i: &str) -> IResult<&str, AST> {
    map(tag("nil"), |_| AST::Nil)(i)
}

fn truee(i: &str) -> IResult<&str, AST> {
    map(tag("true"), |_| AST::Boolean(true))(i)
}

fn falsee(i: &str) -> IResult<&str, AST> {
    map(tag("false"), |_| AST::Boolean(false))(i)
}

fn empty_string(i: &str) -> IResult<&str, AST> {
    map(tag(r#""""#), |_| AST::LString(String::new()))(i)
}

fn ast(i: &str) -> IResult<&str, AST> {
    let expressions = alt((
        list,
        vector,
        parse_map,
        nil,
        truee,
        falsee,
        with_meta,
        keyword,
        string,
        splice_unquote,
        unquote,
        quasiquote,
        quote,
        deref,
        special_symbol,
        double,
        symbol,
        empty_string,
    ));
    preceded(optional_whitespace, expressions)(i)
}

pub fn parse(input: &str) -> EvalResult<AST> {
    let (_remaining, parsed) = ast(input).map_err(|e| format!("{:?}", e))?;
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use super::AST::Double;
    use super::AST::LString;
    use super::AST::Map;
    use super::AST::Symbol;
    use super::*;
    use crate::env::Env;

    #[test]
    fn parse_env_symbol_names() {
        let env = Env::new();
        let map = &env.borrow().env;
        map.keys().for_each(|key| {
            let actual = parse(key).unwrap();
            assert_eq!(actual, Symbol(key.to_string()))
        });
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
}
