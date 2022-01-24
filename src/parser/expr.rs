
#![allow(dead_code)]

use chumsky::prelude::*;
use std::ops::Range;

#[derive(Debug)]
pub enum Expr {

    // Literals
    Num(String),
    Str(String),
    True,
    False,

    // Identifiers
    Ident(String),
    Func(Range<usize>, Box<Expr>, Vec<Expr>),

    // Arithmetic
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),

    // Comparison
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),

    // Boolean
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>)
}

// parser
pub fn gen() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {

        // numbers
        let number = text::int(10)
            .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
            .collect::<String>()
            .map(Expr::Num)
            .labelled("number");

        // strings
        let escape = just::<_, _, Simple<char>>('\\')
            .ignore_then(just('\\')
                .or(just('"'))
                .or(just('n').to('\n'))
            );

        let string = just('"')
            .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
            .then_ignore(just('"'))
            .collect::<String>()
            .map(Expr::Str)
            .labelled("string");

        // idents
        let ident = text::ident()
            .validate(|id: String, span, emit| {
                if let "if" 
                | "else" 
                | "return" 
                | "continue" 
                | "break" 
                | "end" 
                | "loop" 
                | "while" 
                | "for"
                | "in"
                | "wait"
                | "fn"
                | "use"
                = id.as_str() { emit(Simple::custom(
                    span, 
                    format!("keyword {} cannot be used as an identifier", id))
                ) }
                id
            }).map(Expr::Ident)
            .labelled("uppercase identifier");

        // function applications
        let call = ident.clone()
            .or(expr.clone()
                .delimited_by('(', ')')
            ).map_with_span(|x, s| (x, s))
            .then(expr.clone()
                .separated_by(just(','))
                .allow_trailing() 
                .delimited_by('(', ')')
            ).map(|((func, s), args)| Expr::Func(s, Box::new(func), args))
            .labelled("function call");

        // atoms
        let atom = number
            .or(string)
            .or(just("true") .map(|_| Expr::True))
            .or(just("false").map(|_| Expr::False))
            .or(call)
            .or(ident)
            .or(expr.delimited_by('(', ')'))
            .padded();

        // operators
        let op = |c| just(c).padded().labelled("operator");
        let ops = |c| just(c).padded().labelled("operator");
        
        let unary = op('-').to(Expr::Neg as fn(_) -> _)
            .or(op('!').to(Expr::Not as fn(_) -> _))
            .repeated()
            .then(atom)
            .foldr(|op, rhs| op(Box::new(rhs)))
            .labelled("unary operation");

        let product = unary.clone()
            .then(op('*').to(Expr::Mul as fn(_, _) -> _)
                .or(op('/').to(Expr::Div as fn(_, _) -> _))
                .or(op('%').to(Expr::Mod as fn(_, _) -> _))
                .then(unary)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
        
        let sum = product.clone()
            .then(op('+').to(Expr::Add as fn(_, _) -> _)
                .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                .then(product)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .labelled("arithmetic operation");

        let compare = sum.clone()
            .then(ops("==").to(Expr::Eq as fn(_, _) -> _)
                .or(ops("!=").to(Expr::Ne as fn(_, _) -> _))
                .or(op('>').to(Expr::Gt as fn(_, _) -> _))
                .or(op('<').to(Expr::Lt as fn(_, _) -> _))
                .or(ops(">=").to(Expr::Ge as fn(_, _) -> _))
                .or(ops("<=").to(Expr::Le as fn(_, _) -> _))
                .then(sum)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .labelled("arithmetic operation");

        let and = compare.clone()
            .then(ops("&&").to(Expr::And as fn(_, _) -> _)
                .then(compare)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .labelled("arithmetic operation");

        let or = and.clone()
            .then(ops("||").to(Expr::Or as fn(_, _) -> _)
                .then(and)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .labelled("arithmetic operation");

        or.padded()
    })
}
