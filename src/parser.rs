use combine::stream::position;
use combine::*;
use combine::{parser::char::*, *};

use crate::Env;
use crate::Expr;
use crate::Symbol;

use color_eyre::Result;

parser! {
pub fn parse_expr[Input]()(Input) -> Vec<String>
where [Input: Stream<Token = char>] {
    let symbol = many1(letter());

    let words = spaces().silent().with(sep_end_by(symbol, spaces()));

    let mut expr = words.skip(eof());

    expr
}
}
