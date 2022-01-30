
mod parser;
mod emitter;

use chumsky::prelude::*;
use emitter::Program;

fn main() {

    // get the input file data
    let src = std::env::args().nth(1)
        .and_then(|path| std::fs::read_to_string(path).ok())
        .expect("error: could not find file");

    let (out, mut errs) = parser::gen().parse_recovery(src);
    if let Some(decls) = out {
        let (json, e) = Program::gen(decls, errs, 24);
        println!("{}", json.join("\n\n"));
        errs = e;
    }
    errs.into_iter().for_each(|x| println!("error: {x:?}"))
}
