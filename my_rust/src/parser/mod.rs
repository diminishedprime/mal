#[cfg(test)]
mod tests;

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
            tag("eval"),
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
    is_a(" ,\n")(i)
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

fn comment(i: &str) -> IResult<&str, Option<&str>> {
    opt(preceded(tag(";;"), is_not("\n")))(i)
}

fn ast(i: &str) -> IResult<&str, AST> {
    let expression = alt((
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
    let parse_expr = preceded(optional_whitespace, expression);
    let ignore_comments = delimited(comment, parse_expr, comment);
    ignore_comments(i)
}

pub fn parse(input: &str) -> EvalResult<AST> {
    let (_remaining, parsed) = ast(input).map_err(|e| format!("{:?}", e))?;
    Ok(parsed)
}
