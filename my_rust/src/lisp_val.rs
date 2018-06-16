use eval;
use im::HashMap;
use parser;
use std::fmt;
use std::fmt::Display;
use std::slice::SliceConcatExt;
use std::sync::Arc;

pub fn unpack_num(l: &LispVal) -> Result<i32, LispError> {
    match l {
        Number(ac) => Ok(*ac),
        _ => Err(LispError::TypeMismatch(String::from("number"), l.clone())),
    }
}

pub fn unpack_atom(l: &LispVal) -> Result<AtomContents, LispError> {
    match l {
        Atom(ac) => Ok(ac.clone()),
        _ => Err(LispError::TypeMismatch(String::from("Atom"), l.clone())),
    }
}

pub fn unpack_list(l: LispVal) -> Result<ListContents, LispError> {
    match l {
        List(lc) => Ok(lc),
        _ => Err(LispError::TypeMismatch(String::from("List"), l.clone())),
    }
}

pub fn unpack_vec(l: LispVal) -> Result<ListContents, LispError> {
    match l {
        Vector(vc) => Ok(vc),
        _ => Err(LispError::TypeMismatch(String::from("Vector"), l.clone())),
    }
}

pub fn unpack_list_or_vec(l: LispVal) -> Result<Vec<LispVal>, LispError> {
    match l {
        Vector(c) | List(c) => Ok(c),
        _ => Err(LispError::TypeMismatch(
            String::from("Vector or List"),
            l.clone(),
        )),
    }
}

pub fn unpack_hash_map(l: LispVal) -> Result<MapContents, LispError> {
    match l {
        Map(mc) => Ok(mc),
        _ => Err(LispError::TypeMismatch(String::from("Map"), l.clone())),
    }
}

pub fn unpack_closure(l: LispVal) -> Result<ClosureData, LispError> {
    match l {
        Closure(cd) => Ok(cd),
        _ => Err(LispError::TypeMismatch(String::from("Closure"), l.clone())),
    }
}

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
    pub fn prelude() -> Result<Self, LispError> {
        let parsed_forms = eval::prelude::prelude_forms()
            .iter()
            .map(|s| parser::parse(&s))
            .collect::<Result<Vec<_>, _>>()?;
        let mut env = Arc::new(Environment::new());
        for expr in parsed_forms {
            let eb = ExecyBoi { val: expr, env };
            let evaled = eval::eval(eb)?;
            env = evaled.env;
        }
        Ok((*env).clone())
    }

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

    pub fn over_env(self, env: &Environment) -> Self {
        Environment {
            contents: self.contents.union(env.contents.clone()),
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

fn escape_str(s: &str) -> String {
    let mut escaped = String::new();
    escaped.push_str("\"");
    for c in s.chars() {
        match c {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\x08' => escaped.push_str("\\b"),
            '\x0c' => escaped.push_str("\\f"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(c),
        }
    }
    escaped.push_str("\"");
    escaped
}

pub fn print_list(
    args: &Vec<LispVal>,
    print_readably: bool,
    left_str: &str,
    right_str: &str,
    join_str: &str,
) -> String {
    format!(
        "{}{}{}",
        left_str,
        args.into_iter()
            .map(|a| a.pr_str(print_readably))
            .collect::<Vec<_>>()
            .join(join_str),
        right_str,
    )
}

impl LispVal {
    pub fn pr_str(&self, print_readably: bool) -> String {
        match self {
            Nil => format!("nil"),
            True => format!("true"),
            False => format!("false"),
            Number(n) => format!("{}", n),
            Atom(n) => format!("{}", n),
            LString(s) => format!("{}", {
                if print_readably {
                    escape_str(&s)
                } else {
                    s.clone()
                }
            }),
            Keyword(k) => format!(":{}", k),
            Map(m) => format!(
                "{{{}}}",
                m.into_iter()
                    .map(|(k, v)| format!(
                        "{} {}",
                        k.pr_str(print_readably),
                        v.pr_str(print_readably),
                    ))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            List(l) => print_list(l, print_readably, "(", ")", " "),
            Vector(v) => print_list(v, print_readably, "[", "]", " "),
            Closure(_) => format!("fn"),
        }
    }
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
    pub params: Vec<AtomContents>,
    pub vararg: Option<AtomContents>,
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

impl<'a> From<&'a str> for LispVal {
    fn from(s: &str) -> Self {
        LispVal::LString(s.to_string())
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

impl From<usize> for LispVal {
    fn from(i: usize) -> Self {
        LispVal::Number(i as i32)
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
