use super::*;

#[test]
fn namespace_includes_fns_defined_in_mal() {
    let env = Env::new().unwrap();
    println!("{:?}", env.borrow().keys());
    let read_file = env.borrow().keys().into_iter().find(|k| k == "load-file");
    assert_eq!(read_file, Some(String::from("load-file")))
}
