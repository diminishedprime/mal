use std::fmt::Display;
use std::fmt;
use std::slice::SliceConcatExt;

#[derive(PartialEq, Debug, Clone)]
pub enum LispError {
    NumArgs(i32, LispVal),
    TypeMismatch(String, LispVal),
    BadSpecialForm(String, LispVal),
    NotFunction(String, String),
    // UnboundVar(String, String),
    // Default(String),
    Parse(String),
}

impl Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LispError::TypeMismatch(ref expected, ref found) =>
                write!(f, "Invalid type: expected {}, found {}",
                       expected, found),
            &LispError::BadSpecialForm(ref message, ref form) =>
                write!(f, "{}: {}", message, form),
            &LispError::NotFunction(ref message, ref func) =>
                write!(f, "{}: {}", message, func),
            &LispError::NumArgs(ref expected, ref found) =>
                write!(f, "Expected: {} args; found values {}",
                       expected, found),
            // &LispError::UnboundVar(ref a, ref b) =>
            //     write!(f, "a: {} b: {}", a, b),
            // &LispError::Default(ref a) =>
            //     write!(f, "a: {}", a),
            &LispError::Parse(ref a) =>
                write!(f, "a: {}", a),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LispVal {
    Atom(String),
    List(Vec<LispVal>),
    DottedList(Vec<LispVal>, Box<LispVal>),
    Number(i32),
    String(String),
    Bool(bool),
}

impl Display for LispVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LispVal::Atom(ref a) => write!(f, "'{}", a),
            &LispVal::List(ref l) => {
                write!(f, "(")?;
                // TODO(me) - This seems overly complicated. Is there a way I
                // can just call .join() on its own?
                let parts = l
                    .iter()
                    .map(|part| format!("{}", part))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "{}", parts)?;
                write!(f, ")")
            }
            &LispVal::DottedList(ref l, ref val) => {
                write!(f, "(")?;
                let parts = l
                    .iter()
                    .map(|part| format!("{}", part))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "{}", parts)?;
                write!(f, " . {}", val)?;
                write!(f, ")")
            }
            &LispVal::Number(n) => write!(f, "{}", n),
            &LispVal::Bool(b) => match b {
                true => write!(f, "#t"),
                false => write!(f, "#f"),
            },
            &LispVal::String(ref s) => write!(f, "\"{}\"", s),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lisp_val::LispVal::Atom;
    use lisp_val::LispVal::List;
    use lisp_val::LispVal::DottedList;
    use lisp_val::LispVal::Bool;
    use lisp_val::LispVal::Number;
    // use lisp_val::LispError::TypeMismatch;

    #[test]
    fn display_atom() {
        let atom = Atom(String::from("my_atom"));
        assert_eq!(String::from("'my_atom"), format!("{}", atom));
    }

    #[test]
    fn display_list() {
        let list = List(vec!(Bool(true), Bool(false)));
        assert_eq!(String::from("(#t #f)"), format!("{}", list));
    }

    #[test]
    fn display_dottedlist() {
        let dottedlist = DottedList(vec!(Bool(true), Bool(false)), Box::new(Bool(true)));
        assert_eq!(String::from("(#t #f . #t)"), format!("{}", dottedlist));
    }

    #[test]
    fn display_number() {
        let number = Number(3);
        assert_eq!(String::from("3"), format!("{}", number));
    }

    #[test]
    fn display_bool() {
        let bool = Bool(true);
        assert_eq!(String::from("#t"), format!("{}", bool));
    }

    #[test]
    fn display_bool_false() {
        let bool = Bool(false);
        assert_eq!(String::from("#f"), format!("{}", bool));
    }

    #[test]
    fn display_string() {
        let string = LispVal::String(String::from("hello"));
        assert_eq!(String::from("\"hello\""), format!("{}", string));
    }

    #[test]
    fn display_list_nested() {
        let one = Number(1);
        let two = LispVal::String(String::from("two"));
        let three_and_four = DottedList(vec!(Number(3)), Box::new(Number(4)));

        let list = List(vec!(one, two, three_and_four));
        assert_eq!(String::from("(1 \"two\" (3 . 4))"), format!("{}", list));
    }

    // #[test]
    // fn display_type_mismatch() {
    //     let err = TypeMismatch(String::from("thing"), Number(3));
    //     let actual = format!("{}", err);
    //     assert_eq!(actual, String::from(""));
    // }
}
