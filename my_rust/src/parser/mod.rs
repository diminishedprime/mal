#[cfg(test)]
mod tests;

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
use nom::error::context;
use nom::error::ParseError;
use nom::error::VerboseError;
use nom::multi::many0;
use nom::multi::separated_list;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::IResult;

fn vector<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    context(
        "vector",
        map(
            delimited(
                char('['),
                separated_list(whitespace, ast),
                preceded(optional_whitespace, char(']')),
            ),
            AST::m_vec,
        ),
    )(i)
}

fn parse_map<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    context(
        "map",
        map(
            delimited(
                char('{'),
                separated_list(whitespace, ast),
                preceded(optional_whitespace, char('}')),
            ),
            AST::Map,
        ),
    )(i)
}

fn list<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    context(
        "list",
        map(
            delimited(
                char('('),
                separated_list(whitespace, ast),
                preceded(optional_whitespace, char(')')),
            ),
            AST::m_list,
        ),
    )(i)
}

fn special_symbol<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    context(
        "special_symbol",
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
        ),
    )(i)
}

fn symbol_char<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    context("symbol_char", alt((is_a("+*-?!"), alphanumeric1)))(i)
}

fn symbol<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    let (remaining, first_bits) = alt((alpha1, is_a("+*-?/<>=")))(i)?;
    map(many0(symbol_char), move |rest: Vec<&str>| {
        let mut s = String::new();
        s.push_str(&first_bits);
        rest.iter().for_each(|r| s.push_str(&r));
        AST::Symbol(s)
    })(&remaining)
}

fn double<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    context("double", map(nom::number::complete::double, AST::Double))(i)
}

fn parse_str<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    context(
        "parse_str",
        map(
            escaped(is_not("\"\\"), '\\', one_of(r#"\"ntr"#)),
            |s: &str| AST::LString(s.to_string()),
        ),
    )(i)
}

fn string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    context(
        "string",
        preceded(char('\"'), terminated(parse_str, char('\"'))),
    )(i)
}

fn optional_whitespace<'a, E: ParseError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Option<&'a str>, E> {
    opt(whitespace)(i)
}

fn whitespace<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    is_a(" ,\n")(i)
}

fn quote<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(preceded(char('\''), ast), |ast| {
        AST::m_list(vec![AST::Symbol(String::from("quote")), ast])
    })(i)
}

fn quasiquote<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(preceded(char('`'), ast), |ast| {
        AST::m_list(vec![AST::Symbol(String::from("quasiquote")), ast])
    })(i)
}

fn splice_unquote<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(preceded(tag("~@"), ast), |ast| {
        AST::m_list(vec![AST::Symbol(String::from("splice-unquote")), ast])
    })(i)
}

fn unquote<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(preceded(char('~'), ast), |ast| {
        AST::m_list(vec![AST::Symbol(String::from("unquote")), ast])
    })(i)
}

fn deref<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(preceded(char('@'), ast), |ast| {
        AST::m_list(vec![AST::Symbol(String::from("deref")), ast])
    })(i)
}

fn keyword<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(preceded(char(':'), alphanumeric1), |s: &str| {
        AST::Keyword(s.to_string())
    })(i)
}

fn with_meta<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(preceded(char('^'), pair(parse_map, ast)), |(m, a)| {
        AST::m_list(vec![AST::Symbol(String::from("with-meta")), a, m])
    })(i)
}

fn nil<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(tag("nil"), |_| AST::Nil)(i)
}

fn truee<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(tag("true"), |_| AST::Boolean(true))(i)
}

fn falsee<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(tag("false"), |_| AST::Boolean(false))(i)
}

fn empty_string<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    map(tag(r#""""#), |_| AST::LString(String::new()))(i)
}

fn comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<&'a str>, E> {
    context(
        "comment",
        many0(delimited(one_of(";"), is_not("\n"), tag("\n"))),
    )(i)
}

fn ast<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, AST, E> {
    let expression = context(
        "ast",
        alt((
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
        )),
    );
    let parse_expr = preceded(optional_whitespace, expression);
    let ignore_comments = delimited(comment, parse_expr, comment);
    ignore_comments(i)
}

pub fn parse(input: &str) -> EvalResult<AST> {
    let (_remaining, parsed) = ast::<VerboseError<&str>>(input).map_err(|e| format!("{:?}", e))?;
    Ok(parsed)
}
