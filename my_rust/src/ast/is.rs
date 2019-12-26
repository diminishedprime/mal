use super::Listy;
use super::AST;

impl AST {
    pub fn is_nil(&self) -> bool {
        match self {
            AST::Nil => true,
            _ => false,
        }
    }
    pub fn is_list(&self) -> bool {
        match self {
            AST::ListLike(Listy::List(_)) => true,
            _ => false,
        }
    }
    pub fn is_lambda(&self) -> bool {
        match self {
            AST::Lambda(_) => true,
            _ => false,
        }
    }
    pub fn is_falsy(&self) -> bool {
        match self {
            AST::Nil | AST::Boolean(false) => true,
            _ => false,
        }
    }
    pub fn is_atom(&self) -> bool {
        match self {
            AST::Atom(_) => true,
            _ => false,
        }
    }
}
