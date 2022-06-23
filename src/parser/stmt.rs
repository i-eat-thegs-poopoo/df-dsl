
use chumsky::prelude::*;
use super::{ expr, expr::Expr };

#[derive(Debug, Clone)]
pub enum Stmt {

    // normal stuff
    Expr(Expr),
    Assign(Expr, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Loop(Loop, Box<Stmt>),
    Return(Option<Expr>),
    Continue,
    Break,
    End,
    Wait(Expr),
    Process(bool, Box<Stmt>),

    None
}

impl Default for Stmt {
    fn default() -> Self { Self::None }
}

#[derive(Debug, Clone)]
pub enum Loop {
    Forever,
    While(Expr),
    Range(String, Expr, Expr, Option<Expr>),
    List(String, Expr),
    Entry(String, Expr)
}

pub fn gen() -> impl Parser<char, Stmt, Error = Simple<char>> {

    recursive(|stmt| {

        // other stuff
        let block = stmt.clone()
            .repeated()
            .delimited_by('{', '}')
            .padded()
            .map(Stmt::Block)
            .labelled("block statement")
            .boxed();
        
        let if_stmt = text::keyword("if")
            .padded()
            .ignore_then(expr::gen()
                .delimited_by('(', ')'))
            .then(stmt.clone())
            .then(text::keyword("else")
                .padded()
                .ignore_then(stmt.clone())
                .or_not())
            .map(|((cond, if_clause), else_clause)| Stmt::If(
                cond, 
                Box::new(if_clause),
                else_clause.map(Box::new)))
            .labelled("if statement")
            .boxed();

        let forever = text::keyword("loop")
            .ignore_then(stmt.clone())
            .map(|body| Stmt::Loop(Loop::Forever, Box::new(body)))
            .labelled("forever loop")
            .boxed();

        let while_loop = text::keyword("while")
            .padded()
            .ignore_then(expr::gen()
                .delimited_by('(', ')')
                .map(Loop::While))
            .then(stmt.clone())
            .map(|(clause, body)| Stmt::Loop(clause, Box::new(body)))
            .labelled("while loop")
            .boxed();

        let for_range = text::keyword("for")
            .padded()
            .ignore_then(text::ident()
                .padded()
                .then_ignore(just("in"))
                .then(expr::gen())
                .then_ignore(just(".."))
                .then(expr::gen())
                .then(just("by")
                    .padded()
                    .ignore_then(expr::gen())
                    .or_not())
                .delimited_by('(', ')')
                .padded()
                .map(|(((x, min), max), by)| Loop::Range(x, min, max, by)))
            .then(stmt.clone())
            .map(|(clause, body)| Stmt::Loop(clause, Box::new(body)))
            .labelled("for-in-range loop")
            .boxed();

        let for_in = text::keyword("for")
            .padded()
            .ignore_then(text::ident()
                .padded()
                .then_ignore(just("in"))
                .then(expr::gen())
                .delimited_by('(', ')')
                .padded()
                .map(|(x, list)| Loop::List(x, list)))
            .then(stmt.clone())
            .map(|(clause, body)| Stmt::Loop(clause, Box::new(body)))
            .labelled("for-in-list loop")
            .boxed();

        let for_entry = text::keyword("for")
            .padded()
            .ignore_then(text::ident()
                .padded()
                .then_ignore(just("of"))
                .then(expr::gen())
                .delimited_by('(', ')')
                .padded()
                .map(|(x, list)| Loop::Entry(x, list)))
            .then(stmt.clone())
            .map(|(clause, body)| Stmt::Loop(clause, Box::new(body)))
            .labelled("for-in-dict loop")
            .boxed();

        let loop_stmt = choice((
            forever,
            while_loop,
            for_range,
            for_in,
            for_entry));

        let ret_stmt = text::keyword("return")
            .padded()
            .ignore_then(expr::gen().or_not())
            .then_ignore(just(";").padded())
            .map(Stmt::Return)
            .labelled("return statement")
            .boxed();

        let wait = text::keyword("wait")
            .padded()
            .ignore_then(expr::gen())
            .then_ignore(just(";").padded())
            .map(Stmt::Wait)
            .labelled("wait statement")
            .boxed();
        
        let expr_stmt = expr::gen()
            .then_ignore(just(';'))
            .map(Stmt::Expr)
            .labelled("expression statement")
            .boxed();
        
        let assign = expr::gen()
            .padded()
            .then_ignore(just('='))
            .then(expr::gen())
            .then_ignore(just(';'))
            .map(|(var, val)| Stmt::Assign(var, val))
            .labelled("assignment")
            .boxed();
        
        let flow = choice((
                text::keyword("continue").to(Stmt::Continue),
                text::keyword("break").to(Stmt::Break),
                text::keyword("end").to(Stmt::End)))
            .then_ignore(just(";").padded())
            .labelled("control flow")
            .boxed();

        let process = text::keyword("bind")
            .or_not()
            .map(|x| x.is_none())
            .then_ignore(text::keyword("process").padded())
            .then(stmt)
            .map(|(gl, s)| Stmt::Process(gl, Box::new(s)))
            .boxed();

        choice((
            just(';').map(|_| Stmt::None),
            process,
            block,
            if_stmt,
            flow,
            ret_stmt,
            wait,
            loop_stmt,
            assign,
            expr_stmt))
        .padded()
        .labelled("statement")
        .boxed()
    })
}
