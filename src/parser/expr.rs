
#![allow(dead_code)]

use chumsky::prelude::*;
use std::ops::Range;

#[derive(Debug, Clone)]
pub enum Expr {

    // Literals
    Num(String),
    Str(String),
    True,
    False,
    List(Vec<Expr>),
    Value(String, String),
    Item,

    // Identifiers
    Ident(String),
    Func(Range<usize>, Box<Expr>, Vec<Expr>),
    Game(String),
    Save(String),

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
        let number = filter(|c: &char| c.is_ascii_digit())
            .repeated()
            .at_least(1)
            .map(|vec| vec.into_iter().collect::<String>())
            .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
            .collect::<String>()
            .map(Expr::Num)
            .labelled("number")
            .boxed();

        // strings
        let escape = just::<_, _, Simple<char>>('\\')
            .ignore_then(just('\\')
                .or(just('"'))
                .or(just('n').to('\n')))
            .boxed();

        let raw_string = |quote| just(quote)
            .ignore_then(filter(move |c| *c != '\\' && *c != quote)
                .or(escape.clone())
                .repeated())
            .then_ignore(just(quote))
            .collect::<String>()
            .boxed();

        let string = raw_string('"').clone()
            .map(Expr::Str)
            .labelled("string")
            .boxed();

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
                | "of"
                | "wait"
                | "process"
                | "bind"
                | "fn"
                | "use"
                | "ITEM"
                | "game"
                | "save"
                = id.as_str() { 
                    emit(Simple::custom(span, format!("keyword {} cannot be used as an identifier", id)));
                }

                id})
            .labelled("identifier")
            .boxed();

        // function applications
        let call = ident.clone()
            .map(Expr::Ident)
            .or(expr.clone().delimited_by('(', ')'))
            .map_with_span(|x, s| (x, s))
            .then(expr.clone()
                .separated_by(just(','))
                .then_ignore(just(',').padded().or_not())
                .delimited_by('(', ')'))
            .map(|((func, s), args)| Expr::Func(s, Box::new(func), args))
            .labelled("function call")
            .boxed();
        
        let value = raw_string('`')
            .then(just('@')
                .padded()
                .ignore_then(choice((
                    text::keyword("default").to("Default"),
                    text::keyword("selection").to("Selection"),
                    text::keyword("killer").to("Killer"),
                    text::keyword("damager").to("Damager"),
                    text::keyword("victim").to("Victim"),
                    text::keyword("shooter").to("Shooter"),
                    text::keyword("projectile").to("Projectile"),
                    text::keyword("lastentity").to("Last Entity")))
                .map(|s| s.to_string()))
                .or_not())
            .padded()
            .map(|(val, sel)| Expr::Value(val, sel.map_or_else(
                || "default".to_string(), 
                |s| s.to_string())))
            .labelled("game value")
            .boxed();

        fn scope(keyword: &'static str) -> impl Parser<char, String, Error = Simple<char>> {
            text::keyword(keyword).padded()
                .ignore_then(just('.'))
                .ignore_then(ident.clone().padded())
        }

        // atoms
        let atom = choice((
            number,
            string,
            value,
            text::keyword("true") .map(|_| Expr::True),
            text::keyword("false").map(|_| Expr::False),
            text::keyword("ITEM").map(|_| Expr::Item),
            scope("game").map(Expr::Game),
            scope("save").map(Expr::Save),
            call,
            ident.clone().map(Expr::Ident),
            expr.clone().delimited_by('(', ')'),
            expr.clone()
                .separated_by(just(','))
                .then_ignore(just(',').padded().or_not())
                .delimited_by('[', ']')
                .map(Expr::List)
                .labelled("list")))
        .padded()
        .boxed();
        
        let method_tail = just('.').padded()
            .ignore_then(ident.map(Expr::Ident).map_with_span(|x, s| (x, s)))
            .then(expr
                .separated_by(just(','))
                .then_ignore(just(',').padded().or_not())
                .delimited_by('(', ')'))
                .boxed();
        let method = atom
            .then(method_tail.repeated())
            .foldl(|head, ((func, s), mut rest)| {
                rest.insert(0, head);
                Expr::Func(s, Box::new(func), rest)})
            .labelled("method call")
            .boxed();

        // operators
        let op = |c| just(c).padded().labelled("operator");
        let ops = |c| just(c).padded().labelled("operator");
        
        let unary = op('-').to(Expr::Neg as fn(_) -> _)
            .or(op('!').to(Expr::Not as fn(_) -> _))
            .repeated()
            .then(method)
            .foldr(|op, rhs| op(Box::new(rhs)))
            .labelled("unary operation")
            .boxed();

        let product = unary.clone()
            .then(choice((
                    op('*').to(Expr::Mul as fn(_, _) -> _),
                    op('/').to(Expr::Div as fn(_, _) -> _),
                    op('%').to(Expr::Mod as fn(_, _) -> _)))
                .then(unary)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .boxed();
        
        let sum = product.clone()
            .then(op('+').to(Expr::Add as fn(_, _) -> _)
                .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                .then(product)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .labelled("arithmetic operation")
            .boxed();

        let compare = sum.clone()
            .then(choice((
                    ops("==").to(Expr::Eq as fn(_, _) -> _),
                    ops("!=").to(Expr::Ne as fn(_, _) -> _),
                    op('>').to(Expr::Gt as fn(_, _) -> _),
                    op('<').to(Expr::Lt as fn(_, _) -> _),
                    ops(">=").to(Expr::Ge as fn(_, _) -> _),
                    ops("<=").to(Expr::Le as fn(_, _) -> _)))
                .then(sum)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .labelled("arithmetic operation")
            .boxed();

        let and = compare.clone()
            .then(ops("&&").to(Expr::And as fn(_, _) -> _)
                .then(compare)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .labelled("arithmetic operation")
            .boxed();

        let or = and.clone()
            .then(ops("||").to(Expr::Or as fn(_, _) -> _)
                .then(and)
                .repeated())
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
            .labelled("arithmetic operation")
            .boxed();

        or.padded().boxed()
    })
}
