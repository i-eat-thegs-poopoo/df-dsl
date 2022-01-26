
use chumsky::prelude::*;
use crate::parser::{ expr::Expr, stmt::{ Stmt, /*Loop*/ } };
use super::{ Program, Action, codeblock::{ Item, Block }, if_blocks };

macro_rules! code {
    ($var: ident, $name: expr $( , $extra: expr )*; $( $args: expr ),*) => {
        Block::$var(
            $name.to_string(),
            $( $extra, )*
            vec![$( $args ),*]
        )
    };
}

#[derive(Debug)]
pub struct EmitStmt<'a> {
    parent: &'a mut Program,
    pub out: Vec<Block>,
    temp: u32
}

impl <'a> EmitStmt<'a> {

    pub fn run(parent: &'a mut Program, stmt: Stmt) -> Self {
        let emit = Self {
            parent,
            out: Vec::new(),
            temp: 0
        }.emit(stmt);
        emit.out.push(code!(BlockAction, "-="; Item::Var("?d".to_string())));
        emit
    }

    fn temp(&mut self) -> Item {
        let out = format!("%var(?d)?{}", self.temp);
        self.temp += 1;
        Item::Var(out)
    }
    
    fn emit(mut self, stmt: Stmt) -> Self {
        self.temp = 0;

        use Stmt as S;
        match stmt {
            S::Expr(e) => { self.emit_expr(e, None); },
            S::Assign(var, e) => {
                let (operand, assign) = self.emit_expr(e, Some(Item::Var(var.clone())));
                if assign {
                    self.out.push(code!(BlockAction, "="; Item::Var(var), operand));
                }
            }
            S::Block(vec) => for stmt in vec.into_iter() {
                self = self.emit(stmt);
            }
            S::If(e, if_clause, else_clause) => self = self.emit_if(
                false,
                e, 
                *if_clause, 
                else_clause.map(|x| *x)
            ),

            S::Return(e) => {
                if let Some(v) = e {
                    let value = self.emit_expr(v, None).0;
                    self.out.push(code!(BlockAction, "="; Item::Var("%var(%var(?d)?return)".to_string()), value));
                }
            }

            S::Wait(e) => { 
                let wait = self.emit_expr(e, None).0;
                self.out.push(Block::Wait(wait));
            }


            

            // !
            // ! TODO
            // !

            S::Loop(_kind, stmt) => {
                // use Loop as L;
                // match kind {
                //     // L::Forever => self.out.push(Block::Repeat("Forever".to_string(), Vec::new())),
                //     // L::While(e) => todo!(),
                //     _ => todo!()
                // }
                self = self.emit(*stmt);
                self.out.push(Block::ReClose);
            }





            S::Continue => self.out.push(Block::Ctrl("StopRepeat".to_string())),
            S::Break => self.out.push(Block::Ctrl("Skip".to_string())),
            S::End => self.out.push(Block::Ctrl("End".to_string())),

            S::None => (),
        }
        
        self
    }

    fn emit_if(
        mut self,
        not: bool,
        e: Expr,
        if_clause: Stmt,
        else_clause: Option<Stmt>
    ) -> Self {

        use Expr as E;

        macro_rules! if_body {
            (@if) => { if_body!(@in if_clause) };
            (@else $body: expr) => { if_body!(@in $body) };
            (@in $body: expr) => {
                self = self.emit($body);
                self.out.push(Block::IfClose);
            };
        }

        macro_rules! emit_if {
            ($( $stmts: stmt; )*) => {{
                $( $stmts )*
                if_body!(@if);
                if let Some(v) = else_clause {
                    self.out.push(Block::Else);
                    if_body!(@else v);
                }
            }};
        }

        macro_rules! comp {
            ($name: expr, $lhs: expr, $rhs: expr) => { emit_if! {
                let lhs = self.emit_expr(*$lhs, None).0;
                let rhs = self.emit_expr(*$rhs, None).0;
                self.out.push(code!(IfAction, $name, not; lhs, rhs));
            } };
        }

        macro_rules! expr { 
            ($e: expr) => { emit_if! {
                let operand = self.emit_expr($e, None).0;
                self.out.push(code!(IfAction, "!=", not; operand, Item::Num("0".to_string())));
            } };
        }

        match e {
            E::Not(e) => self = self.emit_if(!not, *e, if_clause, else_clause),

            E::Eq(lhs, rhs) => comp!("=", lhs, rhs),
            E::Ne(lhs, rhs) => comp!("!=", lhs, rhs),
            E::Gt(lhs, rhs) => comp!(">", lhs, rhs),
            E::Lt(lhs, rhs) => comp!("<", lhs, rhs),
            E::Ge(lhs, rhs) => comp!(">=", lhs, rhs),
            E::Le(lhs, rhs) => comp!("<=", lhs, rhs),

            E::Func(span, func, args) => if let E::Ident(func) = *func {
                match self.parent.actions.get(&func) {

                    Some(Action { block, .. }) if matches!(block.as_str(), if_blocks!()) => emit_if! {
                        let args = args
                            .into_iter()
                            .map(|arg| self.emit_expr(arg, None).0)
                            .collect::<Vec<_>>();
                        self.out.push(Block::IfAction(func, not, args));
                    },

                    _ => expr!(E::Func(span, Box::new(E::Ident(func)), args))
                }
            } else { todo!() }

            e => expr!(e)
        }

        self
    }

    fn emit_expr(&mut self, expr: Expr, default: Option<Item>) -> (Item, bool) {
        use Expr as E;

        macro_rules! comp {
            ($name: literal, $lhs: ident, $rhs: ident) => {{
                let var = default.unwrap_or_else(|| self.temp());
                let lhs = self.emit_expr(*$lhs, None).0;
                let rhs = self.emit_expr(*$rhs, None).0;
                self.out.append(&mut vec![
                    code!(BlockAction, "="; var.clone(), Item::Num("0".to_string())),
                    code!(IfAction, $name, false; lhs, rhs),
                    code!(BlockAction, "="; var.clone(), Item::Num("1".to_string())),
                    Block::IfClose
                ]);
                (var, false)
            }}
        }

        macro_rules! expr {
            ($name: expr, $( $op: ident ),*; $( $args: expr ),*) => {{
                let var = default.unwrap_or_else(|| self.temp());
                $( let $op = self.emit_expr(*$op, None).0; )*
                self.out.push(code!(BlockAction, $name; var.clone(), $( $args ),*));
                (var, false)
            }};
        }

        match expr {
            E::Num(e) => (Item::Num(e), true),
            E::Str(e) => (Item::Text(e), true),
            E::True => (Item::Num("1".to_string()), true),
            E::False => (Item::Num("0".to_string()), true),
            E::Ident(e) => (Item::Var(format!("%var(?d)!{}", e)), true),

            E::Neg(e) => expr!("-", e; Item::Num("0".to_string()), e),            
            E::Add(lhs, rhs) => expr!("+", lhs, rhs; lhs, rhs),
            E::Sub(lhs, rhs) => expr!("-", lhs, rhs; lhs, rhs),
            E::Mul(lhs, rhs) => expr!("*", lhs, rhs; lhs, rhs),
            E::Div(lhs, rhs) => expr!("/", lhs, rhs; lhs, rhs),
            E::Mod(lhs, rhs) => expr!("%", lhs, rhs; lhs, rhs),

            E::Not(e) => expr!("-", e; Item::Num("1".to_string()), e),
            E::And(lhs, rhs) => expr!("*", lhs, rhs; lhs, rhs),
            E::Or(lhs, rhs) => expr!("=", lhs, rhs; Item::Num(format!(
                "%math({}+{}-%math({}*{}))", 
                &lhs, &rhs, &lhs, &rhs
            ))),

            E::Eq(lhs, rhs) => comp!("=", lhs, rhs),
            E::Ne(lhs, rhs) => comp!("!=", lhs, rhs),
            E::Gt(lhs, rhs) => comp!(">", lhs, rhs),
            E::Lt(lhs, rhs) => comp!("<", lhs, rhs),
            E::Ge(lhs, rhs) => comp!(">=", lhs, rhs),
            E::Le(lhs, rhs) => comp!("<=", lhs, rhs),

            E::Func(span, e, args) => if let E::Ident(func) = *e {
                let var = default.unwrap_or_else(|| self.temp());
                if let Some(v) = self.parent.args.get(&func) {
                    let mut params = v.clone().into_iter();
                    self.out.append(&mut vec![
                        code!(BlockAction, "="; Item::Var("?return".to_string()), Item::Text(var.clone().get())),
                        code!(BlockAction, "+="; Item::Var("?d".to_string())),
                        code!(BlockAction, "="; Item::Var("%var(?d)?return".to_string()), Item::Var("?return".to_string())),
                    ]);
                    for (param, arg) in params.by_ref().zip(args.into_iter()) {
                        let arg = self.emit_expr(arg, None).0;
                        self.out.push(code!(BlockAction, "="; Item::Var(format!("%var(?d)!{}", param)), arg));
                    }
                    if params.next().is_some() {
                        self.parent.errs.push(Simple::custom(span, 
                            format!("not all parameters have been supplied in function call")
                        ));
                    }
                    self.out.push(Block::Call(func));
                } else if let Some(Action { block, void, .. }) = self.parent.actions.get(&func) {
                    if let if_blocks!() = block.as_str() {

                        let args = args
                            .into_iter()
                            .map(|arg| self.emit_expr(arg, None).0)
                            .collect::<Vec<_>>();
                        self.out.append(&mut vec![
                            code!(BlockAction, "="; var.clone(), Item::Num("0".to_string())),
                            Block::BlockAction(func, args),
                            code!(BlockAction, "="; var.clone(), Item::Num("1".to_string())),
                            Block::IfClose
                        ]);

                    } else {

                        let void = !void;
                        let mut args = args
                            .into_iter()
                            .map(|arg| self.emit_expr(arg, None).0)
                            .collect::<Vec<_>>();
                        if void { args.insert(0, var.clone()) }
                        self.out.push(Block::BlockAction(func, args));

                    }
                } else {
                    self.parent.errs.push(Simple::custom(span, 
                        format!("function `{func}` is not defined")
                    ));
                }
                (var, false)
            } else { todo!() }
        }
    }

}
