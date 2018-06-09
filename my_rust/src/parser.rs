use nom::types::CompleteStr;
use nom;
use lisp_val::LispVal;
use lisp_val::LispError;

fn is_double_quote(s: char) -> bool {
    s == '"'
}

named!(symbol<CompleteStr, char>, one_of!("!#$%&|*+-/:<=>?@^_~"));

named!(
    letter<CompleteStr, char>,
    one_of!("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
);

named!(digit<CompleteStr, char>, one_of!("0123456789"));


named!(
    parse_atom<CompleteStr, LispVal>,
    do_parse!(
        first: alt!(letter | symbol) >> rest: many0!(alt!(letter | symbol | digit)) >> ({
            let mut s = String::new();
            let rest = rest.into_iter().collect::<String>();
            s.push(first);
            s.push_str(&rest);
            // TODO(me) - this is really hacky. How can we be more nuanced about
            // negative numbers vs atoms.
            if first == '-'
                && rest.len() > 0
                && rest.chars().all(|c| c.is_digit(10)) {
                    // TODO(me) - Is this really the best way to return my own
                    // error code???
                    return Err(nom::Err::Error(nom::Context::Code(CompleteStr("temp"), nom::ErrorKind::Custom(3))));
                }
            match s.as_ref() {
                "#t" => LispVal::Bool(true),
                "#f" => LispVal::Bool(false),
                _ => LispVal::Atom(s)
            }
        })
    )
);

named!(
    parse_string<CompleteStr, LispVal>,
    do_parse!(
        char!('"') >> s: take_till!(is_double_quote) >> char!('"')
            >> (LispVal::String(s.to_string()))
    )
);

named!(
    parse_number<CompleteStr, LispVal>,
    do_parse!(
        tag: opt!(tag!("-"))
            >> digits: many1!(digit)
            >> ({
                let digits = digits.into_iter().collect::<String>();
                let num = if let Some(_) = tag {
                    -digits.parse::<i32>().unwrap()
                } else {
                    digits.parse::<i32>().unwrap()
                };
                LispVal::Number(num)
            })
    )
);

named!(
    spaces<CompleteStr, ()>,
    do_parse!(many0!(tag!(" ")) >> ())
);

named!(
    parse_list<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> v: map!(separated_list!(spaces, parse_expr), LispVal::List)
            >> spaces
            >> (v)
    )
);

named!(
    parse_dotted_list<CompleteStr, LispVal>,
    do_parse!(
        head: separated_list!(spaces, parse_expr)
            >> spaces
            >> tail: do_parse!(
                tag!(".")
                    >> spaces
                    >> val: parse_expr
                    >> spaces
                    >> (val)
            )
            >> (LispVal::DottedList(head, Box::new(tail)))
    )
);

named!(
    parse_quoted<CompleteStr, LispVal>,
    do_parse!(
        tag!("'")
            >> x: parse_expr
            >> (LispVal::List(vec!(LispVal::Atom(String::from("quote")), x)))
    )
);

named!(
    parse_lists<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> tag!("(")
            >> x: alt!(
                parse_dotted_list
                    | parse_list
            )
            >> spaces
            >> tag!(")")
            >> (x)
    )
);

named!(
    parse_expr<CompleteStr, LispVal>,
    alt!(
        parse_atom
            | parse_number
            | parse_string
            | parse_quoted
            | parse_lists
    )
);


pub fn parse(input: String) -> Result<LispVal, LispError> {
    let parsed = parse_expr(CompleteStr(&input));
    match parsed {
        Ok((_, parse_result)) => Ok(parse_result),
        Err(e) => Err(LispError::Parse(format!("{}", e))),
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol_test() {
        let input = CompleteStr("?");
        let (_, actual) = symbol(input).unwrap();
        assert_eq!(actual, '?')
    }

    #[test]
    fn letter_test() {
        let input = CompleteStr("O");
        let (_, actual) = letter(input).unwrap();
        assert_eq!(actual, 'O')
    }
    #[test]
    fn digit_test() {
        let input = CompleteStr("3");
        let (_, actual) = digit(input).unwrap();
        assert_eq!(actual, '3')
    }

    #[test]
    fn atom_parsing() {
        let input = CompleteStr("atom");
        let (_, actual) = parse_atom(input).unwrap();
        assert_eq!(actual, LispVal::Atom(String::from("atom")))
    }

    #[test]
    fn atom_parsing_true() {
        let input = CompleteStr("#t");
        let (_, actual) = parse_atom(input).unwrap();
        assert_eq!(actual, LispVal::Bool(true))
    }

    #[test]
    fn atom_parsing_false() {
        let input = CompleteStr("#f");
        let (_, actual) = parse_atom(input).unwrap();
        assert_eq!(actual, LispVal::Bool(false))
    }


    #[test]
    fn string_parsing() {
        let input = String::from("\"my string\"");
        let input = CompleteStr(&input);
        let (_, actual) = parse_string(input).unwrap();
        assert_eq!(actual, LispVal::String(String::from("my string")))
    }

    // #[test]
    // fn string_parsing_with_escaped_quotes() {
    //     let input = String::from("\"my \\\"string\"");
    //     let input = CompleteStr(&input);
    //     let (_, actual) = parse_string(input).unwrap();
    //     assert_eq!(actual, LispVal::String(String::from("my \"string")))
    // }

    // #[test]
    // fn string_parsing_with_all_escaped_chars() {
    //     let input = String::from("\" \" \n \t \r \\ \"");
    //     let input = CompleteStr(&input);
    //     let (_, actual) = parse_string(input).unwrap();
    //     assert_eq!(actual, LispVal::String(String::from("\" \n \t \n \\")))
    // }

    #[test]
    fn number_parsing() {
        let input = CompleteStr("123");
        let (_, actual) = parse_number(input).unwrap();
        assert_eq!(actual, LispVal::Number(123));
    }

    #[test]
    fn negative_number_parsing() {
        let input = CompleteStr("-123");
        let (_, actual) = parse_number(input).unwrap();
        assert_eq!(actual, LispVal::Number(-123));
    }

    #[test]
    fn list_parsing() {
        use self::LispVal::Number;
        let input = CompleteStr("3 4   5 ");
        let (_, actual) = parse_list(input).unwrap();
        assert_eq!(actual, LispVal::List(vec!(Number(3), Number(4), Number(5))))
    }

    #[test]
    fn dotted_list_parsing() {
        use self::LispVal::Number;
        let input = CompleteStr("3 4 . 5");
        let (_, actual) = parse_dotted_list(input).unwrap();
        assert_eq!(actual, LispVal::DottedList(vec!(Number(3), Number(4)),
                                               Box::new(Number(5))
        ))
    }

    #[test]
    fn quoted_parsing() {
        let input = CompleteStr("'atom");
        let (_, actual) = parse_quoted(input).unwrap();
        assert_eq!(actual, LispVal::List(vec!(
            LispVal::Atom(String::from("quote")),
            LispVal::Atom(String::from("atom"))
        )))
    }

    #[test]
    fn parse_lists_test_not_dotted() {
        use self::LispVal::Number;
        let input = CompleteStr("(1 2 3)");
        let (_, actual) = parse_lists(input).unwrap();
        assert_eq!(actual, LispVal::List(
            vec!(
                Number(1),
                Number(2),
                Number(3)
            )
        ))
    }
    #[test]
    fn parse_lists_test_dotted() {
        use self::LispVal::Number;
        let input = CompleteStr("(1 2 3 . 4)");
        let (_, actual) = parse_lists(input).unwrap();
        assert_eq!(actual, LispVal::DottedList(
            vec!(
                Number(1),
                Number(2),
                Number(3)
            ),
            Box::new(Number(4))
        ))
    }

    #[test]
    fn parse_list_test() {
        use self::LispVal::Number;
        let input = String::from("(1 2 3)");
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::List(
            vec!(
                Number(1),
                Number(2),
                Number(3)
            )))
    }

    #[test]
    fn parse_list_minus() {
        use self::LispVal::Number;
        let input = String::from("(- 1 -2 3)");
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::List(
            vec!(
                LispVal::Atom(String::from("-")),
                Number(1),
                Number(-2),
                Number(3)
            )))
    }

    #[test]
    fn parse_dotted_list_test() {
        use self::LispVal::Number;
        let input = String::from("(1 2 3 . 4)");
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::DottedList(
            vec!(
                Number(1),
                Number(2),
                Number(3)
            ),
            Box::new(Number(4))
        ))
    }

    #[test]
    fn parse_quoted_test() {
        let input = String::from("'hello");
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::List(
            vec!(LispVal::Atom(String::from("quote")),
                 LispVal::Atom(String::from("hello")))))
    }

    #[test]
    fn parse_atom_test() {
        let input = String::from("#t");
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::Bool(true))
    }

    #[test]
    fn parse_string_test() {
        let input = String::from("\"hello there\"");
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::String(String::from("hello there")))
    }

    #[test]
    fn parse_number_test() {
        let input = String::from("123");
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::Number(123))
    }
    #[test]
    fn parsing_weird_atom() {
        let input = CompleteStr("-123abc");
        let (_, actual) = parse_expr(input).unwrap();
        assert_eq!(actual, LispVal::Atom(String::from("-123abc")))
    }
}
