use im::HashMap;
use lisp_val::DottedListContents;
use lisp_val::LispError;
use lisp_val::LispVal;
use lisp_val::SpecialForm;
use nom;
use nom::types::CompleteStr;

named!(spaces<CompleteStr, ()>, do_parse!(
    many0!(one_of!(" ,")) >> ())
);

named!(symbol<CompleteStr, char>, one_of!("!#$%&|*+-/:<=>?@^_"));

named!(
    letter<CompleteStr, char>,
    one_of!("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
);

named!(digit<CompleteStr, char>, one_of!("0123456789"));

named!(
    parse_with_meta<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> one_of!("^")
            >> rest: many1!(parse_expr)
            >> ({
                let mut rest = rest;
                let first = rest.remove(0);
                rest.insert(0, LispVal::Atom(String::from("with-meta")));
                rest.push(first);
                LispVal::List(rest)
            })
    )
);

named!(
    parse_keyword<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> first: one_of!(":")
            >> rest: many1!(alt!(letter | symbol | digit))
            >> ({
                let mut s = String::new();
                let rest = rest.iter().collect::<String>();
                s.push(first);
                s.push_str(&rest);
                LispVal::Keyword(s)
            })
    )
);

named!(
    parse_deref<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> one_of!("@")
            >> rest: many1!(parse_expr)
            >> ({
                let mut rest = rest;
                rest.insert(0, LispVal::Atom(String::from("deref")));
                LispVal::List(rest)
            })
    )
);

named!(
    parse_atom<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> first: alt!(letter | symbol)
            >> rest: many0!(alt!(letter | symbol | digit))
            >> ({
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
                if let Some(s) = SpecialForm::from_str(&s) {
                    LispVal::SpecialForm(s)
                } else {
                    match s.as_ref() {
                        "nil" => LispVal::Nil,
                        "#t" => LispVal::True,
                        "#f" => LispVal::False,
                        _ => LispVal::Atom(s)
                    }
                }
            })
    )
);

named!(parse_string<CompleteStr, LispVal>,
       do_parse!(
           spaces
               >> parts: alt!(
                   tag!(r#""""#)
                       | delimited!(
                           tag!("\""),
                           escaped!(
                               many1!(
                                   alt!(
                                       letter | digit | one_of!("!#$%&|*+-/:<=>?@^_ ()")
                                   )
                               ),
                               '\\',
                               one_of!("\"\\ntr")
                           )
                               ,
                           tag!("\"")
                       )
               )
               >> ({
                   if parts.to_string() == r#""""# {
                       LispVal::String(String::from(""))
                   } else {
                       LispVal::String(parts.to_string())
                   }
               })
       )
);

named!(
    parse_number<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> tag: opt!(tag!("-"))
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

named!(parse_hash_map<CompleteStr, LispVal>,
       do_parse!(
           spaces
               >> tag!("{")
               >> x: do_parse!(
                   spaces
                       >> v: map!(
                           separated_list!(spaces, parse_expr),
                           |things| {
                               // TODO(me) This can fail, figure out how to make
                               // this return Result<LispVal, ParseError>
                               let mut h = HashMap::new();
                               things
                                   .chunks(2)
                                   .for_each(|chunk| {
                                       h.insert_mut(chunk[0].clone(), chunk[1].clone());
                                   });
                               LispVal::Map(h)
                           }
                       )
                       >> spaces
                       >> (v)
               )
               >> spaces
               >> tag!("}")
               >> (x)
       )
);
named!(
    parse_vector<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> tag!("[")
            >> x: do_parse!(
                spaces
                    >> v: map!(separated_list!(spaces, parse_expr), LispVal::Vector)
                    >> spaces
                    >> (v)
            )
            >> spaces
            >> tag!("]")
            >> (x)
    )
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
            >> (LispVal::DottedList(
                DottedListContents {
                    head,
                    tail: Box::new(tail),
                }))
    )
);

named!(
    parse_quasiquoted<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> tag!("`")
            >> x: parse_expr
            >> (LispVal::List(vec!(LispVal::Atom(String::from("quasiquote")), x)))
    )
);

named!(
    parse_splicing_unquote<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> tag!("~@")
            >> x: parse_expr
            >> (LispVal::List(vec!(LispVal::Atom(String::from("splice-unquote")), x)))
    )
);

named!(
    parse_unquote<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> tag!("~")
            >> x: parse_expr
            >> (LispVal::List(vec!(LispVal::Atom(String::from("unquote")), x)))
    )
);

named!(
    parse_quoted<CompleteStr, LispVal>,
    do_parse!(
        spaces
            >> tag!("'")
            >> x: parse_expr
            >> (LispVal::List(vec!(
                LispVal::SpecialForm(SpecialForm::Quote), x)))
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
        parse_with_meta
            | parse_deref
            | parse_keyword
            | parse_splicing_unquote
            | parse_atom
            | parse_number
            | parse_string
            | parse_quoted
            | parse_unquote
            | parse_quasiquoted
            | parse_lists
            | parse_vector
            | parse_hash_map
    )
);

pub fn parse(input: &str) -> Result<LispVal, LispError> {
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
        assert_eq!(actual, LispVal::True)
    }

    #[test]
    fn atom_parsing_false() {
        let input = CompleteStr("#f");
        let (_, actual) = parse_atom(input).unwrap();
        assert_eq!(actual, LispVal::False)
    }

    #[test]
    fn string_parsing() {
        let input = "\"my string\"";
        let input = CompleteStr(&input);
        let (_, actual) = parse_string(input).unwrap();
        assert_eq!(actual, LispVal::String(String::from("my string")))
    }

    #[test]
    fn string_parsing_with_escaped_quotes() {
        let input = String::from(r#""my \"string\" is great""#);
        let input = CompleteStr(&input);
        let (_, actual) = parse_string(input).unwrap();
        assert_eq!(
            actual,
            LispVal::String(String::from(r#"my \"string\" is great"#))
        )
    }

    #[test]
    fn string_parsing_with_all_escaped_chars() {
        let input = String::from(r#"  " \n \t \r "     "#);
        let input = CompleteStr(&input);
        let (_, actual) = parse_string(input).unwrap();
        assert_eq!(actual, LispVal::String(String::from(r#" \n \t \r "#)))
    }

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
        assert_eq!(actual, LispVal::List(vec![Number(3), Number(4), Number(5)]))
    }

    #[test]
    fn dotted_list_parsing() {
        use self::LispVal::Number;
        use lisp_val::DottedListContents;

        let input = CompleteStr("3 4 . 5");
        let (_, actual) = parse_dotted_list(input).unwrap();
        assert_eq!(
            actual,
            LispVal::DottedList(DottedListContents {
                head: vec![Number(3), Number(4)],
                tail: Box::new(Number(5)),
            })
        )
    }

    #[test]
    fn quoted_parsing() {
        let input = CompleteStr("'atom");
        let (_, actual) = parse_quoted(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::SpecialForm(SpecialForm::Quote),
                LispVal::Atom(String::from("atom")),
            ])
        )
    }

    #[test]
    fn parse_lists_test_not_dotted() {
        use self::LispVal::Number;
        let input = CompleteStr("(1 2 3)");
        let (_, actual) = parse_lists(input).unwrap();
        assert_eq!(actual, LispVal::List(vec![Number(1), Number(2), Number(3)]))
    }
    #[test]
    fn parse_lists_test_dotted() {
        use self::LispVal::Number;
        use lisp_val::DottedListContents;
        let input = CompleteStr("(1 2 3 . 4)");
        let (_, actual) = parse_lists(input).unwrap();
        assert_eq!(
            actual,
            LispVal::DottedList(DottedListContents {
                head: vec![Number(1), Number(2), Number(3)],
                tail: Box::new(Number(4)),
            })
        )
    }

    #[test]
    fn parsing_weird_atom() {
        let input = CompleteStr("-123abc");
        let (_, actual) = parse_expr(input).unwrap();
        assert_eq!(actual, LispVal::Atom(String::from("-123abc")))
    }

    #[test]
    fn parse_list_test() {
        use self::LispVal::Number;
        let actual = parse("(1 2 3)").unwrap();
        assert_eq!(actual, LispVal::List(vec![Number(1), Number(2), Number(3)]))
    }

    #[test]
    fn parse_list_strings_simplier() {
        let input = r#"("hi there")"#;
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![LispVal::String(String::from("hi there"))])
        )
    }

    #[test]
    fn parse_list_strings_less_simplier() {
        let input = r#"("hi there" "johnny")"#;
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::String(String::from("hi there")),
                LispVal::String(String::from("johnny")),
            ])
        )
    }

    #[test]
    fn parse_list_strings() {
        let input = r#"("hi there" "I am strings" "hooray")"#;
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::String(String::from("hi there")),
                LispVal::String(String::from("I am strings")),
                LispVal::String(String::from("hooray")),
            ])
        )
    }

    #[test]
    fn parse_list_minus() {
        use self::LispVal::Number;
        let input = "(- 1 -2 3)";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::Atom(String::from("-")),
                Number(1),
                Number(-2),
                Number(3),
            ])
        )
    }

    #[test]
    fn parse_dotted_list_test() {
        use self::LispVal::Number;
        use lisp_val::DottedListContents;

        let input = "(1 2 3 . 4)";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::DottedList(DottedListContents {
                head: vec![Number(1), Number(2), Number(3)],
                tail: Box::new(Number(4)),
            })
        )
    }

    #[test]
    fn parse_quoted_test() {
        let input = "'hello";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::SpecialForm(SpecialForm::Quote),
                LispVal::Atom(String::from("hello")),
            ])
        )
    }

    #[test]
    fn parse_quasiquoted_test() {
        let input = "`hello";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::Atom(String::from("quasiquote")),
                LispVal::Atom(String::from("hello")),
            ])
        )
    }

    #[test]
    fn parse_quasiquoted_unquote_test() {
        let input = "`~1";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::Atom(String::from("quasiquote")),
                LispVal::List(vec![
                    LispVal::Atom(String::from("unquote")),
                    LispVal::Number(1),
                ]),
            ])
        );
    }

    #[test]
    fn parse_atom_test() {
        let input = "#t";
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::True)
    }

    #[test]
    fn parse_string_test() {
        let input = "\"hello there\"";
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::String(String::from("hello there")))
    }

    #[test]
    fn parse_empty_string_test() {
        let input = r#""""#;
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::String(String::from(r#""#)))
    }

    #[test]
    fn parse_parens_string_test() {
        let input = r#""abc (with parens)""#;
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::String(String::from(r#"abc (with parens)"#))
        )
    }

    #[test]
    fn parse_number_test() {
        let input = "123";
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::Number(123))
    }

    #[test]
    fn parse_with_weird_spaces() {
        let input = "       7         ";
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::Number(7))
    }

    #[test]
    fn parse_keyword_test() {
        let input = " :123";
        let actual = parse(input).unwrap();
        assert_eq!(actual, LispVal::Keyword(String::from(":123")))
    }

    #[test]
    fn parse_vector_test() {
        let input = " [ 1 2 3, 4] ";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::Vector(vec![
                LispVal::Number(1),
                LispVal::Number(2),
                LispVal::Number(3),
                LispVal::Number(4),
            ])
        )
    }

    #[test]
    fn parse_hash_map_test() {
        let input = " { :key \"value\"} ";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::Map(hashmap!{
                LispVal::Keyword(String::from(":key")) => LispVal::String(String::from("value"))
            })
        )
    }

    #[test]
    fn parse_hash_map_test_2() {
        let input = r#"{1 "abc" }"#;
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::Map(hashmap!{
                LispVal::Number(1) => LispVal::String(String::from("abc"))
            })
        )
    }

    #[test]
    fn parse_hash_map_test_3() {
        let input = r#"{"abc" 1}"#;
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::Map(hashmap!{
                LispVal::String(String::from("abc")) => LispVal::Number(1)
            })
        )
    }

    // fn parse_whole_line_comment() {
    //     let input = "      ;; this is a comment '1";
    //     let actual = parse(input).unwrap();
    //     assert_eq!(actual, )
    // }

    #[test]
    fn deref() {
        let input = r#"@a"#;
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::Atom(String::from("deref")),
                LispVal::Atom(String::from("a")),
            ])
        )
    }

    #[test]
    fn def_bang_parsing() {
        let input = "(def! a 3)";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::SpecialForm(SpecialForm::DefBang),
                LispVal::atom_from("a"),
                LispVal::from(3),
            ])
        )
    }

    #[test]
    fn let_star_parsing() {
        let input = "(let* (q (+ 1 2)) q)";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::SpecialForm(SpecialForm::LetStar),
                LispVal::from(vec![
                    LispVal::atom_from("q"),
                    LispVal::from(vec![
                        LispVal::atom_from("+"),
                        LispVal::from(1),
                        LispVal::from(2),
                    ]),
                ]),
                LispVal::atom_from("q"),
            ])
        )
    }

    #[test]
    fn do_test() {
        let input = "(do (def! a 5) a)";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::from(vec![
                LispVal::SpecialForm(SpecialForm::Do),
                LispVal::from(vec![
                    LispVal::SpecialForm(SpecialForm::DefBang),
                    LispVal::atom_from("a"),
                    LispVal::from(5),
                ]),
                LispVal::atom_from("a"),
            ])
        )
    }

    #[test]
    fn splicing_unqoute() {
        let input = "~@(1 2 3)";
        let actual = parse(input).unwrap();
        assert_eq!(
            actual,
            LispVal::List(vec![
                LispVal::Atom(String::from("splice-unquote")),
                LispVal::List(vec![
                    LispVal::Number(1),
                    LispVal::Number(2),
                    LispVal::Number(3),
                ]),
            ])
        )
    }
}
