use std::fmt;
use std::fmt::Display;

#[derive(Clone, PartialEq, Debug)]
pub enum AST {
    List(Vec<AST>),
    Symbol(String),
    Double(f64),
    LString(String),
    // Int(i64),
}

impl Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AST::Double;
        use AST::LString;
        use AST::List;
        use AST::Symbol;
        match self {
            Double(a) => write!(f, "{}", a),
            Symbol(a) => write!(f, "{}", a),
            LString(a) => write!(f, r#""{}""#, a),
            List(contents) => {
                write!(f, "(")?;
                let mut contents = contents.iter().peekable();
                while let Some(val) = contents.next() {
                    if contents.peek().is_some() {
                        write!(f, "{} ", val)?;
                    } else {
                        write!(f, "{}", val)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::AST::Double;
    use super::AST::List;
    use super::AST::Symbol;

    #[test]
    fn display_double() {
        let actual = Double(1.23);
        assert_eq!(format!("{}", actual), String::from("1.23"));
    }

    #[test]
    fn display_symbol() {
        let actual = Symbol(String::from("abc"));
        assert_eq!(format!("{}", actual), String::from("abc"));
    }

    #[test]
    fn display_list() {
        let actual = List(vec![Symbol(String::from("abc")), Double(1.23)]);
        assert_eq!(format!("{}", actual), String::from("(abc 1.23)"));
    }

}
