pub mod standard_library;
#[cfg(test)]
mod tests;
pub mod util;

use crate::val;
use crate::val::Env;
use crate::val::EnvType;
use crate::val::EvalResult;
use crate::val::MalVal;

impl EnvType {
    pub fn keys(&self) -> Vec<String> {
        let parent_keys: Vec<String> = match &self.parent {
            Some(env) => env.keys(),
            None => vec![],
        };
        self.env
            .keys()
            .map(String::from)
            .chain(parent_keys.into_iter())
            .collect::<Vec<String>>()
    }

    pub fn get(&self, key: &str) -> Option<MalVal> {
        match self.env.get(key) {
            Some(val) => return Some(val.clone()),
            None => match &self.parent {
                Some(env) => env.get(&key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: String, value: MalVal) -> EvalResult<MalVal> {
        // TODO - there should be a special set for def that goes up to the topmost level.
        self.env.insert(key.clone(), value.clone());
        Ok(value)
    }

    pub fn with_scope(current_scope: Env) -> Env {
        let new_scope = val::m_env(Some(current_scope.clone()));
        new_scope
    }
}
