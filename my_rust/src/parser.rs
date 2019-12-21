use nom::sequence::delimited;
use nom::combinator::map;
use nom::multi::separated_list;
use nom::branch::alt;
use nom::character::streaming::char;
use nom::IResult;

enum AST {
    List(Vec<AST>),
    Symbol(String),
    Double(f64),
    Int(i64)
}

fn list(i: &[u8]) -> IResult<&[u8], Vec<AST>> {
    delimited(
        char('['),
        separated_list(
            char(','),
            lisp_value
        ),
        char(']')
    )(i)
}

fn lisp_value(i: &[u8]) -> IResult<&[u8], AST> {
    alt(map(list, AST::List))(i)
}

pub fn parse(input: &str) -> Result<&str, String> {
    let (_remaining, parsed) = lisp_value(input.as_bytes()).unwrap();
    Ok(input)
}
