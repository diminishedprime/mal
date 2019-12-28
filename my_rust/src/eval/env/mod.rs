mod standard_library;
#[cfg(test)]
mod tests;
pub mod util;

use crate::ast::SymbolVal;
use crate::ast::AST;
use crate::eval::EvalResult;
use im::hashmap;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Env {
    env: HashMap<SymbolVal, AST>,
    parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> EvalResult<Rc<RefCell<Self>>> {
        let env = Rc::new(RefCell::new(Env {
            env: standard_library::with_standard_library(),
            parent: None,
        }));
        let add_to_ns = crate::parser::parse(
            r#"
(eval
  (read-string
    (slurp "./stdlib/env.mal")))
"#,
        )?;
        println!("{}", add_to_ns);
        let result = crate::eval::eval(env.clone(), add_to_ns)?;
        println!("{}", result);
        Ok(env)
    }

    pub fn keys(&self) -> Vec<String> {
        let parent_keys: Vec<String> = match &self.parent {
            Some(env) => env.clone().borrow().keys(),
            None => vec![],
        };
        self.env
            .keys()
            .map(String::from)
            .chain(parent_keys.into_iter())
            .collect::<Vec<String>>()
    }

    pub fn get(&self, key: &SymbolVal) -> EvalResult<AST> {
        match self.env.get(key) {
            Some(val) => return Ok(val.clone()),
            None => match &self.parent {
                Some(env) => env.borrow().get(&key),
                None => Err(format!("Key: {} is not in the enviroment.", key)),
            },
        }
    }

    pub fn set(&mut self, key: SymbolVal, value: AST) -> EvalResult<AST> {
        // TODO - there should be a special set for def that goes up to the topmost level.
        self.env.insert(key.clone(), value.clone());
        Ok(value)
    }

    pub fn with_scope(current_scope: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        let new_scope = Rc::new(RefCell::new(Env {
            env: hashmap![],
            parent: Some(current_scope.clone()),
        }));
        new_scope
    }
}
