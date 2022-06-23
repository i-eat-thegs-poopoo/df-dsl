
mod parser;
mod emitter;
mod nbt_edit;

use chumsky::{ prelude::*, Stream };
use emitter::Program;
use std::{ iter::Peekable, ops::Range };
use itertools::Itertools;

struct Comments<I> where 
    I: Iterator<Item = (char, Range<usize>)> 
{
    iter: Peekable<I>
}

impl <I> Iterator for Comments<I> where 
    I: Iterator<Item = (char, Range<usize>)>
{
    type Item = (char, Range<usize>);
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.iter.next()?;
        if let ('/', _) = next {
            return match self.iter.peek() {
                Some(('/', _)) => {
                    self.iter
                        .peeking_take_while(|(x, _)| *x != '\n')
                        .for_each(drop);
                    self.iter.next()
                }
                Some(('*', _)) => {
                    self.iter.next();
                    while self.iter.peek().is_some() {
                        self.iter.by_ref()
                            .take_while(|(x, _)| *x != '*')
                            .for_each(drop);
                        if let Some(('/', _)) = self.iter.peek() {
                            let (_, r) = self.iter.next().unwrap();
                            return Some((' ', r));
                        }
                    }
                    None
                }
                _ => Some(next),
            };
        }
        Some(next)
    }
}

fn main() {

    let mut args = std::env::args();
    let size = match args.nth(1)
        .expect("error: no arguments supplied")
        .as_str()
    {
        "basic" => 24,
        "large" => 49,
        "massive" => 149,
        got => panic!("error: expected `basic`, `large`, or `massive` as first argument, got `{got}`")
    };
    let path = args.next()
        .expect("error: no src file supplied");
    let src = std::fs::read_to_string(path.as_str()).ok()
        .expect("error: could not find src file");

    let into = args.next()
        .expect("error: no output file supplied");

    if !std::path::Path::new(into.as_str()).exists() {
        panic!("error: could not find output file");
    }

    println!("compiling...");

    let parser = parser::gen();
    let (out, mut errs) = parser.parse_recovery({
        let len = src.len();
        Stream::from_iter(
            len..len + 1, 
            Comments { 
                iter: src.chars()
                    .enumerate()
                    .map(|(i, c)| (c, i..i + 1))
                    .peekable() 
            }
        )
    });

    println!("successfully parsed src");

    if let Some(decls) = out {
        let out = Program::gen(path, decls, &mut errs, size);
        if errs.is_empty() {
            nbt_edit::edit(into, out);
            return println!("successfully compiled and updated nbt");
        }
    }

    errs.into_iter().for_each(|x| println!("error: {x:?}"));
}
