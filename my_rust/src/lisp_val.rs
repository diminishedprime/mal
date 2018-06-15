use im::HashMap;
use std::fmt;
use std::fmt::Display;
use std::slice::SliceConcatExt;
use std::sync::Arc;

#[derive(PartialEq, Debug, Clone)]
pub struct ExecyBoi {
    pub val: LispVal,
    pub env: Arc<Environment>,
}

impl ExecyBoi {
    pub fn with_value(self, val: LispVal) -> Self {
        ExecyBoi { val, ..self }
    }
}

impl From<LispVal> for ExecyBoi {
    fn from(lv: LispVal) -> Self {
        ExecyBoi {
            val: lv,
            env: Arc::new(Environment::new()),
        }
    }
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
    BadSpecialForm(LispVal),
    NotFunction(String),
    UnboundVar(String),
    Default(String),
    Parse(String),
    NotImplemented(LispVal),
}

impl Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LispError::TypeMismatch(ref expected, ref found) => {
                write!(f, "Invalid type: expected {}, found {}", expected, found)
            }
            &LispError::BadSpecialForm(ref form) => write!(f, "Bad special form:\n{}", form),
            &LispError::NotFunction(ref func) => write!(f, "{} is not a bound function.", func),
            &LispError::NumArgs(ref expected, ref found) => {
                write!(f, "Expected: {} args; found values {}", expected, found)
            }
            &LispError::UnboundVar(ref name) => {
                write!(f, "{} was not bound in the current environment.", name)
            }
            &LispError::Default(ref a) => write!(f, "Default Error:\n{}", a),
            &LispError::Parse(ref a) => write!(f, "Parse Error:\n{}", a),
            &LispError::NotImplemented(ref a) => {
                write!(f, "Not implemented, expression was:\n{}\n", a)
            }
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

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Environment {
    contents: HashMap<AtomContents, LispVal>,
}

pub type Binding = (AtomContents, LispVal);

impl Environment {
    pub fn shadowing(env: Environment) -> Environment {
        Environment {
            contents: env.contents,
        }
    }

    pub fn new() -> Self {
        Environment {
            contents: hashmap!(),
        }
    }

    pub fn with_binding(&self, binding: Binding) -> Self {
        Environment {
            contents: self.contents.insert(binding.0, binding.1),
        }
    }

    pub fn with_bindings(&self, bindings: Vec<Binding>) -> Self {
        Environment {
            contents: bindings
                .iter()
                .fold(self.contents.clone(), |acc, (name, val)| {
                    acc.insert(name, val)
                }),
        }
    }

    pub fn set(&self, key: AtomContents, val: LispVal) -> Self {
        let contents = self.contents.insert(key, val);
        Environment { contents }
    }

    // TODO(me) - I need to figure out how Arcs work better. I'm not really sure
    // why I have to do this as an arc (why can't I dereference the value?)
    pub fn get(&self, key: &AtomContents) -> Option<Arc<LispVal>> {
        self.contents.get(key)
    }
}

#[derive(Debug, Clone, Hash, Eq)]
pub enum LispVal {
    Nil,
    True,
    False,

    Number(i32),

    LString(String),
    Atom(AtomContents),
    Keyword(String),

    Map(MapContents),
    List(ListContents),
    Vector(VecContents),

    Closure(ClosureData),
}

use self::LispVal::{Atom, Closure, False, Keyword, LString, List, Map, Nil, Number, True, Vector};

impl PartialEq for LispVal {
    fn eq(&self, other: &LispVal) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,

            (Number(n), Number(n2)) => n == n2,

            (LString(s), LString(s2)) => s == s2,
            (Atom(s), Atom(s2)) => s == s2,
            (Keyword(s), Keyword(s2)) => s == s2,

            (Map(m), Map(m2)) => m == m2,

            (List(c), List(c2)) => c == c2,
            (List(c), Vector(c2)) => c == c2,
            (Vector(c), Vector(c2)) => c == c2,
            (Vector(c), List(c2)) => c == c2,

            (Closure(c), Closure(c2)) => c == c2,

            _ => false,
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ClosureData {
    pub name_bindings: Vec<AtomContents>,
    pub body: Arc<LispVal>,
    pub env: Arc<Environment>,
}

impl LispVal {
    pub fn atom_from(s: &str) -> LispVal {
        LispVal::Atom(s.to_owned())
    }
    pub fn string_from(s: &str) -> LispVal {
        LispVal::LString(s.to_owned())
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
        LispVal::LString(s)
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

impl Display for LispVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LispVal::Closure(_) => write!(f, "#<function>"),
            &LispVal::Nil => write!(f, "nil"),
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
            &LispVal::Keyword(ref s) => write!(f, ":{}", s),
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
            &LispVal::Number(n) => write!(f, "{}", n),
            &LispVal::True => write!(f, "true"),
            &LispVal::False => write!(f, "false"),
            &LispVal::LString(ref s) => write!(f, "\"{}\"", s),
        }
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(String::from("(true false)"), format!("{}", list));
    }

    #[test]
    fn display_number() {
        let number = LispVal::from(3);
        assert_eq!(String::from("3"), format!("{}", number));
    }

    #[test]
    fn display_bool() {
        let bool = True;
        assert_eq!(String::from("true"), format!("{}", bool));
    }

    #[test]
    fn display_bool_false() {
        let bool = False;
        assert_eq!(String::from("false"), format!("{}", bool));
    }

    #[test]
    fn display_string() {
        let string = LispVal::string_from("hello");
        assert_eq!(String::from("\"hello\""), format!("{}", string));
    }
}
