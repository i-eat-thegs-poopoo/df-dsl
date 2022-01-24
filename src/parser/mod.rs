
pub mod expr;
pub mod stmt;
pub mod decl;

use chumsky::prelude::*;
use decl::Decl;
use std::ops::Range;

pub fn gen() -> impl Parser<char, Vec<(Decl, Range<usize>)>, Error = Simple<char>> {
    decl::gen()
}
