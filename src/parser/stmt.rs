
use chumsky::prelude::*;
use super::expr::Expr;
use super::expr;

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Assign(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Loop(Loop, Box<Stmt>),
    Return(Option<Expr>),
    Continue,
    Break,
    End,
    Wait(Expr),
    None
}

impl Default for Stmt {
    fn default() -> Self { Self::None }
}

#[derive(Debug)]
pub enum Loop {
    Forever,
    While(Expr),
    Range(String, Expr, Expr),
    List(String, Expr)
}

pub fn gen() -> impl Parser<char, Stmt, Error = Simple<char>> {
    recursive(|stmt| {

        let block = stmt.clone()
            .repeated()
            .delimited_by('{', '}')
            .padded()
            .map(Stmt::Block)
            .labelled("block statement");
        
        let if_stmt = just("if")
            .padded()
            .ignore_then(expr::gen()
                .delimited_by('(', ')')
            ).then(stmt.clone())
            .then(just("else")
                .padded()
                .ignore_then(stmt.clone())
                .or_not()
            ).map(|((cond, if_clause), else_clause)| Stmt::If(
                cond, 
                Box::new(if_clause),
                else_clause.map(Box::new)
            )).labelled("if statement");

        let forever = just("loop")
            .ignore_then(stmt.clone())
            .map(|body| Stmt::Loop(Loop::Forever, Box::new(body)))
            .labelled("forever loop");

        let while_loop = just("while")
            .padded()
            .ignore_then(expr::gen()
                .delimited_by('(', ')')
                .map(Loop::While)
            ).then(stmt.clone())
            .map(|(clause, body)| Stmt::Loop(clause, Box::new(body)))
            .labelled("while loop");

        let for_range = just("for")
            .padded()
            .ignore_then(text::ident()
                .padded()
                .then_ignore(just("in"))
                .then(expr::gen())
                .then_ignore(just(".."))
                .then(expr::gen())
                .delimited_by('(', ')')
                .padded()
                .map(|((x, min), max)| Loop::Range(x, min, max))
            ).then(stmt.clone())
            .map(|(clause, body)| Stmt::Loop(clause, Box::new(body)))
            .labelled("for-in-range loop");

        let for_in = just("for")
            .padded()
            .ignore_then(text::ident()
                .padded()
                .then_ignore(just("in"))
                .then(expr::gen())
                .delimited_by('(', ')')
                .padded()
                .map(|(x, list)| Loop::List(x, list))
            ).then(stmt)
            .map(|(clause, body)| Stmt::Loop(clause, Box::new(body)))
            .labelled("for-in-list loop");

        let loop_stmt = forever
            .or(while_loop)
            .or(for_range)
            .or(for_in);

        let ret_stmt = just("return")
            .padded()
            .ignore_then(expr::gen().or_not())
            .map(Stmt::Return)
            .labelled("return statement");

        let wait = just("wait")
            .padded()
            .ignore_then(expr::gen())
            .map(Stmt::Wait)
            .labelled("wait statement");
        
        let expr_stmt = expr::gen()
            .then_ignore(just(';'))
            .map(Stmt::Expr)
            .labelled("expression statement");
        
        let assign = text::ident()
            .padded()
            .then_ignore(just('='))
            .then(expr::gen())
            .then_ignore(just(';'))
            .map(|(var, val)| Stmt::Assign(var, val))
            .labelled("assignment");

        just(';')
            .padded()
            .map(|_| Stmt::None)
            .or(block)
            .or(if_stmt)
            .or(just("continue").map(|_| Stmt::Continue))
            .or(just("break").map(|_| Stmt::Break))
            .or(just("end").map(|_| Stmt::End))
            .or(ret_stmt)
            .or(wait)
            .or(loop_stmt)
            .or(assign)
            .or(expr_stmt)
            .padded()
            .labelled("statement")
    })
}
