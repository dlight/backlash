#![feature(impl_trait_in_bindings)]
#![allow(unused)]
#![allow(incomplete_features)]

use std::collections::HashMap;

use fehler::throw;

use color_eyre::{
    eyre::{bail, eyre},
    Report, Result,
};

use dyn_clone::DynClone;

use lasso::{Rodeo, Spur};

use pom::parser::*;
use pom::Parser;

use maplit::hashmap;

use Expr::*;

type Error = Report;

type Symbol = Spur;

type Map<T> = HashMap<Symbol, T>;

type FnArgs = dyn Iterator<Item = Expr>;

trait FnClone: DynClone + for<'a> Fn(&'a mut FnArgs) -> Result<Expr> {}

impl<T> FnClone for T where T: DynClone + for<'a> Fn(&'a mut FnArgs) -> Result<Expr> {}

dyn_clone::clone_trait_object!(FnClone);

struct Env {
    vars: Map<Expr>,
    interner: Rodeo,
}

#[derive(Clone)]
enum Expr {
    Num(f64),
    Var(Symbol),
    App(Box<Expr>, Vec<Expr>),
    PrimOp(Box<dyn FnClone>),
}

#[fehler::throws]
fn apply(env: &mut Env, f: Expr, args: Vec<Expr>) -> Expr {
    if let PrimOp(op) = eval(env, f)? {
        let args = args.into_iter().map(|x| eval(env, x));

        let a = args.map(|x| x.unwrap());

        op(&mut a)?
    } else {
        bail!("not a primop");
    }
}

#[fehler::throws]
fn eval(env: &mut Env, expr: Expr) -> Expr {
    match expr {
        Var(x) => env.vars.get(&x).map(|x| (*x).clone()).unwrap_or(Var(x)),
        App(f, args) => apply(env, *f, args)?,
        x => x,
    }
}

#[fehler::throws]
fn bin_op(f: impl Fn(f64, f64) -> f64, init: f64, args: &mut FnArgs) -> Expr {
    let mut acc = init;

    for el in args {
        match el {
            Num(num) => acc = f(acc, num),
            _ => bail!("operating on a non-number"),
        }
    }

    Num(acc)
}

fn env() -> Env {
    let mut rodeo = Rodeo::new();

    let mut key = |s: &'static str| rodeo.get_or_intern_static(s);

    let map = hashmap![
        key("+") => PrimOp(Box::new(|args| bin_op(|a, b| a + b, 0.0, args))),
        key("-") => PrimOp(Box::new(|args| {
        let init = args
            .next()
            .ok_or_else(|| eyre!("can't subtract from nothing"))?;

        let init = match init {
            Num(x) => x,
            _ => bail!("operating on a non-number"),
        };

        bin_op(|a, b| a - b, init, args)
    })),
        key("*") => PrimOp(Box::new(|args| bin_op(|a, b| a * b, 1.0, args))),

        key("e") => Num(std::f64::consts::E),
        key("pi") => Num(std::f64::consts::PI),
    ];

    Env {
        vars: map,
        interner: rodeo,
    }
}

#[test]
fn test_add() {
    let env = env();

    let add = env.vars[&env.interner.get("+").unwrap()].clone();

    let expr = App(
        Box::new(add),
        vec![Num(1.0), Var(env.interner.get("e").unwrap()), Num(0.5)],
    );

    matches!(eval(&mut env, expr).unwrap(), Num(val) if val == 1.0 + std::f64::consts::E + 0.5);
}

fn main() {
    println!("Hello, world!");
}
