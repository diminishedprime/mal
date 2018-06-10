use std::fmt::Display;
use std::fmt;
use std::slice::SliceConcatExt;
use im::HashMap;

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

pub type ListContents = Vec<LispVal>;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct DottedListContents {
    pub head: ListContents,
    pub tail: Box<LispVal>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum LispVal {
    Atom(String),
    List(ListContents),
    DottedList(DottedListContents),
    Number(i32),
    String(String),
    True,
    False,
    Keyword(String),
    Vector(Vec<LispVal>),
    Map(HashMap<LispVal, LispVal>),
}

impl LispVal {
    // TODO(me) - is there some sort of "from" trait?
    pub fn bool_for(b: bool) -> LispVal {
        if b {
            LispVal::True
        } else {
            LispVal::False
        }
    }
}

impl Display for LispVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LispVal::Map(ref m) => {
                write!(f, "{{")?;
                // TODO(me) - This seems overly complicated. Is there a way I
                // can just call .join() on its own?
                let parts = m
                    .iter()
                    .map(|(k, v)| format!("{} {}", k, v))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "{}", parts)?;
                write!(f, "}}")
            },
            &LispVal::Vector(ref v) => {
                write!(f, "[")?;
                // TODO(me) - This seems overly complicated. Is there a way I
                // can just call .join() on its own?
                let parts = v
                    .iter()
                    .map(|part| format!("{}", part))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "{}", parts)?;
                write!(f, "]")
            }
            &LispVal::Keyword(ref s) => write!(f, "{}", s),
            &LispVal::Atom(ref a) => write!(f, "{}", a),
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
            &LispVal::DottedList(DottedListContents{ref head, ref tail}) => {
                write!(f, "(")?;
                let parts = head
                    .iter()
                    .map(|part| format!("{}", part))
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "{}", parts)?;
                write!(f, " . {}", tail)?;
                write!(f, ")")
            }
            &LispVal::Number(n) => write!(f, "{}", n),
            &LispVal::True => write!(f, "#t"),
            &LispVal::False => write!(f, "#f"),
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
    use lisp_val::LispVal::True;
    use lisp_val::LispVal::False;
    use lisp_val::LispVal::Number;
    // use lisp_val::LispError::TypeMismatch;

    #[test]
    fn display_atom() {
        let atom = Atom(String::from("my_atom"));
        assert_eq!(String::from("my_atom"), format!("{}", atom));
    }

    #[test]
    fn display_list() {
        let list = List(vec!(True, False));
        assert_eq!(String::from("(#t #f)"), format!("{}", list));
    }

    #[test]
    fn display_dottedlist() {
        let dottedlist = DottedList(
            DottedListContents {
                head: vec!(True, False),
                tail: Box::new(True)
            });
        assert_eq!(String::from("(#t #f . #t)"), format!("{}", dottedlist));
    }

    #[test]
    fn display_number() {
        let number = Number(3);
        assert_eq!(String::from("3"), format!("{}", number));
    }

    #[test]
    fn display_bool() {
        let bool = True;
        assert_eq!(String::from("#t"), format!("{}", bool));
    }

    #[test]
    fn display_bool_false() {
        let bool = False;
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
        let three_and_four = DottedList(
            DottedListContents {
                head: vec!(Number(3)),
                tail: Box::new(Number(4))});

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
