use crate::ast::list_of;
use crate::ast::Listy;
use crate::ast::AST;
use nom::branch::alt;
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
use nom::combinator::value;
use nom::combinator::verify;
use nom::multi::fold_many0;
use nom::multi::many0;
use nom::multi::separated_list;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::IResult;

fn vector(i: &str) -> IResult<&str, AST> {
    map(
        delimited(
            char('['),
            separated_list(whitespace, ast),
            preceded(optional_whitespace, char(']')),
        ),
        |s| AST::ListLike(Listy::Vector(s)),
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
        |s| AST::ListLike(Listy::List(s)),
    )(i)
}

// TODO - I need this to check that its followed by whitespace, but I can't consume the
// whitespace...???
// TODO - I think I need to use peek here.
fn special_symbol(i: &str) -> IResult<&str, AST> {
    map(
        pair(is_a("+*/-="), not(one_of("0123456789"))),
        |(c, _): (&str, _)| {
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
    let (remaining, first_bits) = alt((alpha1, is_a("+*-?/")))(i)?;
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str),
    Escaped(char),
}

fn string(i: &str) -> IResult<&str, AST> {
    map(
        delimited(
            char('"'),
            fold_many0(
                alt((
                    // Match either a bunch of non-escaped characters or one escaped character
                    map(
                        verify(is_not("\"\\"), |s: &str| !s.is_empty()),
                        StringFragment::Literal,
                    ),
                    map(
                        preceded(
                            char('\\'),
                            alt((
                                value('\\', char('\\')),
                                value('/', char('/')),
                                value('"', char('"')),
                                value('\u{08}', char('b')),
                                value('\u{0C}', char('f')),
                                value('\n', char('n')),
                                value('\r', char('r')),
                                value('\t', char('t')),
                            )),
                        ),
                        StringFragment::Escaped,
                    ),
                )),
                String::new(),
                |mut string, fragment| {
                    match fragment {
                        StringFragment::Literal(s) => string.push_str(s),
                        StringFragment::Escaped(c) => {
                            string.push('\\');
                            string.push(c)
                        }
                    };
                    string
                },
            ),
            char('"'),
        ),
        AST::LString,
    )(i)
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

fn ast(i: &str) -> IResult<&str, AST> {
    let expressions = alt((
        list,
        vector,
        parse_map,
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
    ));
    preceded(optional_whitespace, expressions)(i)
}

pub fn parse(input: &str) -> Result<AST, String> {
    let (_remaining, parsed) = ast(input).map_err(|e| format!("{:?}", e))?;
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use super::AST::Double;
    use super::AST::ListLike;
    use super::AST::Map;
    use super::AST::Symbol;
    use super::*;
    use crate::ast::Listy;

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
        assert_eq!(actual, ListLike(Listy::List(vec![])));
    }

    #[test]
    fn parse_quote_1() {
        let actual = parse("'1").unwrap();
        assert_eq!(
            actual,
            ListLike(Listy::List(vec![Symbol("quote".to_string()), Double(1.0)]))
        );
    }

    #[test]
    fn parse_one_item_list() {
        let actual = parse("(1.34)").unwrap();
        assert_eq!(actual, ListLike(Listy::List(vec![Double(1.34)])));
    }

    #[test]
    fn parse_standard_list() {
        let actual = parse("(1.23 1.23)").unwrap();
        assert_eq!(
            actual,
            ListLike(Listy::List(vec![Double(1.23), Double(1.23)]))
        );
    }

    #[test]
    fn parse_list_weird_whitespace() {
        let actual = parse(" (  ,1.23 ,,, ,1.23,,, )   ").unwrap();
        assert_eq!(
            actual,
            ListLike(Listy::List(vec![Double(1.23), Double(1.23)]))
        );
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
            ListLike(Listy::List(vec![
                Symbol(String::from("with-meta")),
                ListLike(Listy::Vector(vec![Double(3.0), Double(4.0)])),
                Map(vec![Double(1.0), Double(2.0)]),
            ]))
        );
    }
}
