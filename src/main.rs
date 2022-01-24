
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
    if let Some(v) = out {
        let mut program = Program::new(errs);
        program.lines(v);
        program.emit();
        println!("{:#?}", &mut program);
        errs = program.errs;
    }
    errs.into_iter().for_each(|x| println!("error: {x:?}"))
}
