
use chumsky::prelude::*;
use super::{ stmt, stmt::Stmt };
use std::ops::Range;

#[derive(Debug)]
pub enum Decl {
    Func(String, Vec<String>, Stmt),
    Use(String)
}

pub fn gen() -> impl Parser<char, Vec<(Decl, Range<usize>)>, Error = Simple<char>> {

    let func = just("fn")
        .padded()
        .ignore_then(text::ident()
            .map_with_span(|name, span| (name, span))
        ).then(text::ident()
            .padded()
            .separated_by(just(','))
            .allow_trailing() 
            .delimited_by('(', ')')
        ).then(stmt::gen())
        .map(|(((name, span), args), body)| (Decl::Func(name, args, body), span))
        .labelled("function declaration");

    let path = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .padded()
        .collect::<String>()
        .labelled("file path");

    let use_extern = just("use")
        .ignore_then(path
            .map_with_span(|path, span| (Decl::Use(path), span))
        ).then_ignore(just(';').padded())
        .labelled("use declaration");

    let decl = func
        .or(use_extern)
        .padded()
        .labelled("declaration");

    decl.repeated().then_ignore(end())

}
