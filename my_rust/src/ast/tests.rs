#[cfg(test)]
mod tests {
    use crate::ast::AST;

    #[test]
    fn display_double() {
        let actual = AST::m_double(1.23);
        assert_eq!(format!("{}", actual), String::from("1.23"));
    }

    #[test]
    fn display_symbol() {
        let actual = AST::m_symbol("abc");
        assert_eq!(format!("{}", actual), String::from("abc"));
    }

    #[test]
    fn display_list() {
        let actual = AST::m_list(vec![AST::m_symbol("abc"), AST::m_double(1.23)]);
        assert_eq!(format!("{}", actual), String::from("(abc 1.23)"));
    }

    #[test]
    fn display_keyword() {
        let actual = AST::m_keyword("abc");
        assert_eq!(format!("{}", actual), String::from(":abc"));
    }
}
