pub fn prelude_forms() -> Vec<String> {
    vec!["(def! not (fn* (a) (if a false true)))"]
        .into_iter()
        .map(String::from)
        .collect::<Vec<_>>()
}
