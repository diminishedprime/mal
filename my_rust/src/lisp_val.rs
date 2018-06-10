use im::HashMap;
use std::fmt;
use std::fmt::Display;
use std::slice::SliceConcatExt;
use std::sync::Arc;

#[derive(PartialEq, Debug, Clone)]
pub struct ExecyBoi {
    pub val: LispVal,
    pub env: Environment,
}

impl Display for ExecyBoi {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.val)
    }
}

pub type LispResult = Result<ExecyBoi, LispError>;

#[derive(PartialEq, Debug, Clone)]
pub enum LispError {
    NumArgs(i32, LispVal),
    TypeMismatch(String, LispVal),
    BadSpecialForm(String, LispVal),
    NotFunction(String),
    UnboundVar(String),
    Default(String),
    Parse(String),
}

impl Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LispError::TypeMismatch(ref expected, ref found) => {
                write!(f, "Invalid type: expected {}, found {}", expected, found)
            }
            &LispError::BadSpecialForm(ref message, ref form) => write!(f, "{}: {}", message, form),
            &LispError::NotFunction(ref func) => write!(f, "{} is not a bound function.", func),
            &LispError::NumArgs(ref expected, ref found) => {
                write!(f, "Expected: {} args; found values {}", expected, found)
            }
            &LispError::UnboundVar(ref name) => {
                write!(f, "{} was not bound in the current environment.", name)
            }
            &LispError::Default(ref a) => write!(f, "Default Error:\n{}", a),
            &LispError::Parse(ref a) => write!(f, "Parse Error:\n{}", a),
        }
    }
}

pub type AtomContents = String;

pub type ListContents = Vec<LispVal>;

pub type VecContents = Vec<LispVal>;

pub type MapContents = HashMap<LispVal, LispVal>;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct DottedListContents {
    pub head: ListContents,
    pub tail: Box<LispVal>,
}

impl Clone for Environment {
    fn clone(&self) -> Environment {
        Environment {
            previous: match *self.previous {
                Some(ref p) => Box::new(Some(p.clone())),
                None => Box::new(None),
            },
            contents: self.contents.clone(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Environment {
    previous: Box<Option<Environment>>,
    contents: HashMap<AtomContents, LispVal>,
}

impl Environment {
    pub fn shadowing(env: Environment) -> Environment {
        Environment {
            previous: Box::new(Some(env)),
            contents: hashmap!(),
        }
    }

    pub fn new() -> Self {
        Environment {
            // I think this needs to be a Rc?
            previous: Box::new(None),
            contents: hashmap!(),
        }
    }

    pub fn set_mut(&mut self, key: AtomContents, val: LispVal) {
        self.contents.insert_mut(key, val)
    }

    pub fn set(&self, key: AtomContents, val: LispVal) -> Self {
        let contents = self.contents.insert(key, val);
        Environment {
            contents,
            previous: self.previous.clone(),
        }
    }

    // TODO(me) - I need to figure out how Arcs work better. I'm not really sure
    // why I have to do this as an arc (why can't I dereference the value?)
    pub fn get(&self, key: &AtomContents) -> Option<Arc<LispVal>> {
        if let Some(val) = self.contents.get(key) {
            Some(val)
        } else if let Some(ref previous) = *self.previous {
            previous.get(key)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum SpecialForm {
    If,
    Quote,
    DefBang,
    LetStar,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum LispVal {
    True,
    False,

    Number(i32),

    String(String),
    SpecialForm(SpecialForm),
    Atom(AtomContents),
    Keyword(String),

    Map(MapContents),
    List(ListContents),
    DottedList(DottedListContents),
    Vector(VecContents),
}

impl LispVal {
    pub fn atom_from(s: &str) -> LispVal {
        LispVal::Atom(s.to_owned())
    }
    pub fn string_from(s: &str) -> LispVal {
        LispVal::String(s.to_owned())
    }
    pub fn keyword_from(s: &str) -> LispVal {
        LispVal::Keyword(s.to_owned())
    }
}

impl From<MapContents> for LispVal {
    fn from(mc: MapContents) -> Self {
        LispVal::Map(mc)
    }
}

impl From<String> for LispVal {
    fn from(s: String) -> Self {
        LispVal::String(s)
    }
}

impl From<i32> for LispVal {
    fn from(i: i32) -> Self {
        LispVal::Number(i)
    }
}

impl From<bool> for LispVal {
    fn from(b: bool) -> Self {
        if b {
            LispVal::True
        } else {
            LispVal::False
        }
    }
}

impl From<ListContents> for LispVal {
    fn from(lc: ListContents) -> Self {
        LispVal::List(lc)
    }
}

impl From<DottedListContents> for LispVal {
    fn from(lc: DottedListContents) -> Self {
        LispVal::DottedList(lc)
    }
}

impl Display for SpecialForm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &SpecialForm::If => write!(f, "if"),
            &SpecialForm::Quote => write!(f, "quote"),
            &SpecialForm::DefBang => write!(f, "def!"),
            &SpecialForm::LetStar => write!(f, "let*"),
        }
    }
}

impl Display for LispVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LispVal::SpecialForm(ref s) => write!(f, "{}", s),
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
            }
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
            &LispVal::DottedList(DottedListContents { ref head, ref tail }) => {
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
    use lisp_val::LispVal;
    use lisp_val::LispVal::False;
    use lisp_val::LispVal::True;

    #[test]
    fn display_atom() {
        let atom = LispVal::atom_from("my_atom");
        assert_eq!(String::from("my_atom"), format!("{}", atom));
    }

    #[test]
    fn display_list() {
        let list = LispVal::from(vec![True, False]);
        assert_eq!(String::from("(#t #f)"), format!("{}", list));
    }

    #[test]
    fn display_dottedlist() {
        let dottedlist = LispVal::from(DottedListContents {
            head: vec![True, False],
            tail: Box::new(True),
        });
        assert_eq!(String::from("(#t #f . #t)"), format!("{}", dottedlist));
    }

    #[test]
    fn display_number() {
        let number = LispVal::from(3);
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
        let string = LispVal::string_from("hello");
        assert_eq!(String::from("\"hello\""), format!("{}", string));
    }

    #[test]
    fn display_list_nested() {
        let one = LispVal::from(1);
        let two = LispVal::string_from("two");
        let three_and_four = LispVal::from(DottedListContents {
            head: vec![LispVal::from(3)],
            tail: Box::new(LispVal::from(4)),
        });

        let list = LispVal::from(vec![one, two, three_and_four]);
        assert_eq!(String::from("(1 \"two\" (3 . 4))"), format!("{}", list));
    }

    // #[test]
    // fn display_type_mismatch() {
    //     let err = TypeMismatch(String::from("thing"), LispVal::from(3));
    //     let actual = format!("{}", err);
    //     assert_eq!(actual, String::from(""));
    // }
}
