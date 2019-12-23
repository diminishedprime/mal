use crate::ast::ClosureVal;
use crate::ast::SymbolVal;
use crate::ast::AST;
use crate::ast::AST::Closure;
use im::hashmap;
use im::HashMap;
use std::rc::Rc;

mod standard_library;
use standard_library::{define, divide, eq, let_star, multiply, plus, subtract};

pub struct Env {
    pub envs: Vec<HashMap<SymbolVal, AST>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            envs: vec![hashmap! {
                String::from("+") => Closure(ClosureVal(Rc::new(plus))),
                String::from("*") => Closure(ClosureVal(Rc::new(multiply))),
                String::from("/") => Closure(ClosureVal(Rc::new(divide))),
                String::from("-") => Closure(ClosureVal(Rc::new(subtract))),
                String::from("def!") => Closure(ClosureVal(Rc::new(define))),
                String::from("let*") => Closure(ClosureVal(Rc::new(let_star))),
                String::from("=") => Closure(ClosureVal(Rc::new(eq))),
            }],
        }
    }

    pub fn get(&self, key: &SymbolVal) -> Result<AST, String> {
        let mut envs = self.envs.iter().rev();
        while let Some(env) = envs.next() {
            match env.get(key) {
                Some(val) => return Ok(val.clone()),
                None => continue,
            }
        }
        return Err(format!("Key: {} is not in the enviroment.", key));
    }

    // TODO - the enviroment management could just be a macro.
    pub fn new_local(&mut self) {
        self.envs.push(hashmap![])
    }

    pub fn clear_local(&mut self) {
        if self.envs.len() == 1 {
            panic!("cannot clear the last environment. You probably forgot to call new_local before calling this method.")
        }
        self.envs.remove(self.envs.len() - 1);
    }

    pub fn set(&mut self, key: SymbolVal, value: AST) -> Result<AST, String> {
        let len = self.envs.len();
        // TODO - is there a way to avoid this unsafe?
        unsafe {
            let env = self.envs.get_unchecked_mut(len - 1);
            env.insert(key.clone(), value.clone());
        }
        Ok(value)
    }
}
