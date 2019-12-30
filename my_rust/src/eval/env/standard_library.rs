use crate::eval::env::util;
use crate::eval::eval;
use crate::print::pr_seq;
use crate::val;
use crate::val::malval::{assert_list, unwrap_atom, unwrap_double, unwrap_list, unwrap_string};
use crate::val::ClosureVal;
use crate::val::Env;
use crate::val::EvalError;
use crate::val::EvalResult;
use crate::val::MalType;
use crate::val::MalVal;
use im::hashmap;
use im::HashMap;
use std::rc::Rc;

pub fn with_standard_library() -> HashMap<String, MalVal> {
    hashmap! {
        String::from("+") => Rc::new(MalType::Closure(ClosureVal(Rc::new(plus)))),
        String::from("*") => Rc::new(MalType::Closure(ClosureVal(Rc::new(multiply)))),
        String::from("/") => Rc::new(MalType::Closure(ClosureVal(Rc::new(divide)))),
        String::from("-") => Rc::new(MalType::Closure(ClosureVal(Rc::new(subtract)))),
        String::from("=") => Rc::new(MalType::Closure(ClosureVal(Rc::new(eq)))),
        String::from("list") => Rc::new(MalType::Closure(ClosureVal(Rc::new(list)))),
        String::from("list?") => Rc::new(MalType::Closure(ClosureVal(Rc::new(is_list)))),
        String::from("empty?") => Rc::new(MalType::Closure(ClosureVal(Rc::new(is_empty)))),
        String::from("count") => Rc::new(MalType::Closure(ClosureVal(Rc::new(count)))),
        String::from("str") => Rc::new(MalType::Closure(ClosureVal(Rc::new(strr)))),
        String::from("pr-str") => Rc::new(MalType::Closure(ClosureVal(Rc::new(pr_strr)))),
        String::from("println") => Rc::new(MalType::Closure(ClosureVal(Rc::new(print_ln)))),
        String::from("prn") => Rc::new(MalType::Closure(ClosureVal(Rc::new(prn)))),
        String::from("not") => Rc::new(MalType::Closure(ClosureVal(Rc::new(not)))),
        String::from("<") => Rc::new(MalType::Closure(ClosureVal(Rc::new(lt)))),
        String::from("<=") => Rc::new(MalType::Closure(ClosureVal(Rc::new(lte)))),
        String::from(">") => Rc::new(MalType::Closure(ClosureVal(Rc::new(gt)))),
        String::from(">=") => Rc::new(MalType::Closure(ClosureVal(Rc::new(gte)))),
        String::from("read-string") => Rc::new(MalType::Closure(ClosureVal(Rc::new(read_string)))),
        String::from("eval") => Rc::new(MalType::Closure(ClosureVal(Rc::new(lib_eval)))),
        String::from("slurp") => Rc::new(MalType::Closure(ClosureVal(Rc::new(slurp)))),
        String::from("atom") => Rc::new(MalType::Closure(ClosureVal(Rc::new(atom)))),
        String::from("atom?") => Rc::new(MalType::Closure(ClosureVal(Rc::new(is_atom)))),
        String::from("deref") => Rc::new(MalType::Closure(ClosureVal(Rc::new(deref)))),
        String::from("reset!") => Rc::new(MalType::Closure(ClosureVal(Rc::new(reset)))),
        String::from("swap!") => Rc::new(MalType::Closure(ClosureVal(Rc::new(swap)))),
    }
}

pub fn plus(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    Ok(val::m_double(
        args.map(unwrap_double)
            .collect::<EvalResult<Vec<_>>>()?
            .into_iter()
            .fold(0.0, |acc, arg| acc + arg),
    ))
}

pub fn multiply(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    Ok(val::m_double(
        args.map(unwrap_double)
            .collect::<EvalResult<Vec<_>>>()?
            .into_iter()
            .fold(1.0, |acc, arg| acc * arg),
    ))
}

pub fn divide(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (first, rest) = util::one_or_more_args("/", args)?;
    let first = unwrap_double(first)?;
    let mut rest = rest
        .map(unwrap_double)
        .collect::<EvalResult<Vec<_>>>()?
        .into_iter()
        .peekable();
    if rest.peek().is_none() {
        Ok(val::m_double(1.0 / first))
    } else {
        Ok(val::m_double(rest.fold(first, |acc, arg| acc / arg)))
    }
}

pub fn subtract(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (first, rest) = util::one_or_more_args("-", args)?;
    let first = unwrap_double(first)?;
    let mut rest = rest
        .map(unwrap_double)
        .collect::<EvalResult<Vec<_>>>()?
        .into_iter()
        .peekable();
    if rest.peek().is_none() {
        Ok(val::m_double(-first))
    } else {
        Ok(val::m_double(rest.fold(first, |acc, arg| acc - arg)))
    }
}

pub fn eq(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (first, rest) = util::one_or_more_args("=", args)?;
    Ok(val::m_bool(rest.fold(true, |acc, next: MalVal| {
        if acc == false {
            false
        } else {
            next == first
        }
    })))
}

pub fn list(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    Ok(val::m_list(args.collect::<Vec<_>>()))
}

pub fn is_list(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("list?", args)?;
    Ok(val::m_bool(arg.is_list()))
}

pub fn is_empty(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("empty?", args)?;
    let contents = unwrap_list(arg)?;
    Ok(val::m_bool(contents.is_empty()))
}

pub fn count(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("empty?", args)?;
    Ok(val::m_double(if arg.is_nil() {
        0.0
    } else {
        let arg = unwrap_list(arg)?;
        (arg.len() as f64)
    }))
}

pub fn strr(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    Ok(val::m_string(pr_seq(
        &args.collect::<Vec<MalVal>>(),
        false,
        "",
        "",
        "",
    )))
}

pub fn pr_strr(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    Ok(val::m_string(pr_seq(
        &args.collect::<Vec<MalVal>>(),
        true,
        "",
        "",
        " ",
    )))
}

pub fn print_ln(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    println!(
        "{}",
        pr_seq(&args.collect::<Vec<MalVal>>(), false, "", "", " ")
    );
    Ok(val::m_nil())
}

pub fn prn(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    println!(
        "{}",
        pr_seq(&args.collect::<Vec<MalVal>>(), true, "", "", " ")
    );
    Ok(val::m_nil())
}

pub fn not(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("not", args)?;
    Ok(val::m_bool(arg.is_falsy()))
}

pub fn lt(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (first, rest) = util::one_or_more_args("<", args)?;
    let mut last: f64 = unwrap_double(first)?;
    Ok(val::m_bool(rest.fold(
        Ok(true),
        |acc: EvalResult<bool>, next| {
            let acc = acc?;
            let next = unwrap_double(next)?;
            let next_acc = acc && last < next;
            last = next;
            Ok(next_acc)
        },
    )?))
}

pub fn lte(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (first, rest) = util::one_or_more_args("<=", args)?;
    let mut last: f64 = unwrap_double(first)?;
    Ok(val::m_bool(rest.fold(
        Ok(true),
        |acc: EvalResult<bool>, next| {
            let acc = acc?;
            let next = unwrap_double(next)?;
            let next_acc = acc && last <= next;
            last = next;
            Ok(next_acc)
        },
    )?))
}

pub fn gt(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (first, rest) = util::one_or_more_args(">", args)?;
    let mut last: f64 = unwrap_double(first)?;
    Ok(val::m_bool(rest.fold(
        Ok(true),
        |acc: EvalResult<bool>, next| {
            let acc = acc?;
            let next = unwrap_double(next)?;
            let next_acc = acc && last > next;
            last = next;
            Ok(next_acc)
        },
    )?))
}

pub fn gte(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (first, rest) = util::one_or_more_args(">=", args)?;
    let mut last: f64 = unwrap_double(first)?;
    Ok(val::m_bool(rest.fold(
        Ok(true),
        |acc: EvalResult<bool>, next| {
            let acc = acc?;
            let next = unwrap_double(next)?;
            let next_acc = acc && last >= next;
            last = next;
            Ok(next_acc)
        },
    )?))
}

pub fn read_string(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("read-string", args)?;
    let arg = unwrap_string(arg)?;
    crate::parser::parse(&arg)
}

pub fn lib_eval(env: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("eval", args)?;
    assert_list(&arg)?;
    crate::eval::eval(env.clone(), arg)
}

pub fn slurp(_: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("eval", args)?;
    let file_name = unwrap_string(arg)?;
    let s =
        std::fs::read_to_string(file_name).map_err(|e| EvalError::IOError(format!("{:?}", e)))?;
    Ok(val::m_string(&s))
}

pub fn atom(env: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("atom", args)?;
    let arg = eval(env.clone(), arg)?;
    Ok(val::m_atom(arg))
}

pub fn is_atom(env: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("atom", args)?;
    let arg = eval(env.clone(), arg)?;
    Ok(val::m_bool(arg.is_atom()))
}

pub fn deref(env: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let arg = util::one_arg("atom", args)?;
    let arg = eval(env.clone(), arg)?;
    Ok(unwrap_atom(&arg)?)
}

pub fn reset(env: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (atom, new_value) = util::two_args("reset!", args)?;
    let arg = eval(env.clone(), new_value)?;
    atom.set_atom(arg)?;
    Ok(atom)
}

pub fn swap(env: Env, args: impl Iterator<Item = MalVal>) -> EvalResult<MalVal> {
    let (atom, update_fn, rest) = util::two_or_more_args("swap!", args)?;
    let update_fn = eval(env.clone(), update_fn)?;
    let new_val = eval(
        env.clone(),
        val::m_list(
            std::iter::once(update_fn)
                .chain(std::iter::once(unwrap_atom(&atom)?))
                .chain(rest)
                .collect::<Vec<_>>(),
        ),
    )?;
    atom.set_atom(new_val)?;
    Ok(atom)
}
