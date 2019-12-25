use crate::ast::list_of;
use crate::ast::ClosureVal;
use crate::ast::SymbolVal;
use crate::ast::AST;
use crate::ast::AST::Boolean;
use crate::ast::AST::Closure;
use crate::ast::AST::Double;
use crate::ast::AST::LString;
use crate::eval::env::util;
use crate::eval::env::Env;
use crate::eval::eval;
use crate::eval::EvalResult;
use crate::print::pr_seq;
use core::cell::RefCell;
use im::hashmap;
use im::HashMap;
use std::rc::Rc;

pub fn with_standard_library() -> HashMap<SymbolVal, AST> {
    hashmap! {
        String::from("+") => Closure(ClosureVal(Rc::new(plus))),
        String::from("*") => Closure(ClosureVal(Rc::new(multiply))),
        String::from("/") => Closure(ClosureVal(Rc::new(divide))),
        String::from("-") => Closure(ClosureVal(Rc::new(subtract))),
        String::from("def!") => Closure(ClosureVal(Rc::new(define))),
        String::from("=") => Closure(ClosureVal(Rc::new(eq))),
        String::from("list") => Closure(ClosureVal(Rc::new(list))),
        String::from("list?") => Closure(ClosureVal(Rc::new(is_list))),
        String::from("empty?") => Closure(ClosureVal(Rc::new(is_empty))),
        String::from("count") => Closure(ClosureVal(Rc::new(count))),
        String::from("str") => Closure(ClosureVal(Rc::new(strr))),
        String::from("pr-str") => Closure(ClosureVal(Rc::new(pr_strr))),
        String::from("println") => Closure(ClosureVal(Rc::new(print_ln))),
        String::from("prn") => Closure(ClosureVal(Rc::new(prn))),
        String::from("not") => Closure(ClosureVal(Rc::new(not))),
        String::from("<") => Closure(ClosureVal(Rc::new(lt))),
        String::from("<=") => Closure(ClosureVal(Rc::new(lte))),
        String::from(">") => Closure(ClosureVal(Rc::new(gt))),
        String::from(">=") => Closure(ClosureVal(Rc::new(gte))),
        String::from("read-string") => Closure(ClosureVal(Rc::new(read_string))),
        String::from("eval") => Closure(ClosureVal(Rc::new(lib_eval))),
        String::from("slurp") => Closure(ClosureVal(Rc::new(slurp))),
    }
}

pub fn plus(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    Ok(Double(
        args.map(AST::unwrap_double)
            .collect::<EvalResult<Vec<_>>>()?
            .into_iter()
            .fold(0.0, |acc, arg| acc + arg),
    ))
}

pub fn multiply(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    Ok(Double(
        args.map(AST::unwrap_double)
            .collect::<EvalResult<Vec<_>>>()?
            .into_iter()
            .fold(1.0, |acc, arg| acc * arg),
    ))
}

pub fn divide(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args("/", args)?;
    let first = first.unwrap_double()?;
    let mut rest = rest
        .map(AST::unwrap_double)
        .collect::<EvalResult<Vec<_>>>()?
        .into_iter()
        .peekable();
    if rest.peek().is_none() {
        Ok(Double(1.0 / first))
    } else {
        Ok(Double(rest.fold(first, |acc, arg| acc / arg)))
    }
}

pub fn subtract(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args("-", args)?;
    let first = first.unwrap_double()?;
    let mut rest = rest
        .map(AST::unwrap_double)
        .collect::<EvalResult<Vec<_>>>()?
        .into_iter()
        .peekable();
    if rest.peek().is_none() {
        Ok(Double(-first))
    } else {
        Ok(Double(rest.fold(first, |acc, arg| acc - arg)))
    }
}

pub fn define(env: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, second) = util::two_args("def!", args)?;
    let first = first.unwrap_symbol()?;
    let second = eval(env.clone(), second)?;
    env.borrow_mut().set(first, second)
}

pub fn eq(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args("=", args)?;
    Ok(Boolean(rest.fold(true, |acc, next: AST| {
        if acc == false {
            false
        } else {
            next == first
        }
    })))
}

pub fn list(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    Ok(list_of(args.collect::<Vec<_>>()))
}

pub fn is_list(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("list?", args)?;
    Ok(Boolean(arg.is_list()))
}

pub fn is_empty(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("empty?", args)?;
    let contents = arg.unwrap_list_like()?;
    Ok(Boolean(contents.is_empty()))
}

pub fn count(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("empty?", args)?;
    Ok(Double(if arg.is_nil() {
        0.0
    } else {
        let arg = arg.unwrap_list_like()?;
        (arg.len() as f64)
    }))
}

pub fn strr(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    Ok(AST::LString(pr_seq(
        args.collect::<Vec<AST>>(),
        false,
        "",
        "",
        "",
    )))
}

pub fn pr_strr(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    Ok(AST::LString(pr_seq(
        args.collect::<Vec<AST>>(),
        true,
        "",
        "",
        " ",
    )))
}

pub fn print_ln(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    println!("{}", pr_seq(args.collect::<Vec<AST>>(), false, "", "", " "));
    Ok(AST::Nil)
}

pub fn prn(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    println!("{}", pr_seq(args.collect::<Vec<AST>>(), true, "", "", " "));
    Ok(AST::Nil)
}

pub fn not(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("not", args)?;
    Ok(AST::Boolean(arg.is_falsy()))
}

pub fn lt(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args("<", args)?;
    let mut last: f64 = first.unwrap_double()?;
    Ok(AST::Boolean(rest.fold(
        Ok(true),
        |acc: EvalResult<bool>, next| {
            let acc = acc?;
            let next = next.unwrap_double()?;
            let next_acc = acc && last < next;
            last = next;
            Ok(next_acc)
        },
    )?))
}

pub fn lte(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args("<=", args)?;
    let mut last: f64 = first.unwrap_double()?;
    Ok(AST::Boolean(rest.fold(
        Ok(true),
        |acc: EvalResult<bool>, next| {
            let acc = acc?;
            let next = next.unwrap_double()?;
            let next_acc = acc && last <= next;
            last = next;
            Ok(next_acc)
        },
    )?))
}

pub fn gt(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args(">", args)?;
    let mut last: f64 = first.unwrap_double()?;
    Ok(AST::Boolean(rest.fold(
        Ok(true),
        |acc: EvalResult<bool>, next| {
            let acc = acc?;
            let next = next.unwrap_double()?;
            let next_acc = acc && last > next;
            last = next;
            Ok(next_acc)
        },
    )?))
}

pub fn gte(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let (first, rest) = util::one_or_more_args(">=", args)?;
    let mut last: f64 = first.unwrap_double()?;
    Ok(AST::Boolean(rest.fold(
        Ok(true),
        |acc: EvalResult<bool>, next| {
            let acc = acc?;
            let next = next.unwrap_double()?;
            let next_acc = acc && last >= next;
            last = next;
            Ok(next_acc)
        },
    )?))
}

pub fn read_string(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("read-string", args)?;
    let arg = arg.unwrap_string()?;
    crate::parser::parse(&arg)
}

pub fn lib_eval(env: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("eval", args)?;
    arg.assert_list()?;
    crate::eval::eval(env.clone(), arg)
}

pub fn slurp(_: Rc<RefCell<Env>>, args: impl Iterator<Item = AST>) -> EvalResult<AST> {
    let arg = util::one_arg("eval", args)?;
    let file_name = arg.unwrap_string()?;
    let s = std::fs::read_to_string(file_name).map_err(|e| format!("{:?}", e))?;
    Ok(LString(s))
}
