
use chumsky::prelude::*;
use crate::parser::{ expr::Expr, stmt::{ Stmt, /*Loop*/ } };
use super::{ Program, Action, codeblock::{ Item, Block }, if_blocks };

macro_rules! code {
    ($var: ident, $name: literal $( , $extra: expr )*; $( $args: expr ),*) => {
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

    pub fn run(parent: &'a mut Program, stmt: Stmt) -> (Self, bool) {
        Self {
            parent,
            out: Vec::new(),
            temp: 0
        }.emit(stmt)
    }

    fn temp(&mut self) -> Item {
        let out = format!("%var(?d)?{}", self.temp);
        self.temp += 1;
        Item::Var(out)
    }
    
    pub fn emit(mut self, stmt: Stmt) -> (Self, bool) {
        self.temp = 0;

        use Stmt as S;
        let void = match stmt {
            S::Expr(e) => { self.emit_expr(e, None); true }
            S::Assign(var, e) => {
                let (operand, assign) = self.emit_expr(e, Some(Item::Var(var.clone())));
                if assign {
                    self.out.push(code!(BlockAction, "="; Item::Var(var), operand));
                }
                true
            }
            S::Block(vec) => {
                let mut void = true;
                for stmt in vec.into_iter() {
                    let (new, v) = self.emit(stmt);
                    self = new;
                    void = void && v;
                }
                void
            }
            S::If(e, if_clause, else_clause) => {
                let mut if_void = true;
                let mut else_void = true;

                self = self.emit_if(
                    false,
                    e, 
                    *if_clause, 
                    else_clause.map(|x| *x), 
                    &mut if_void, 
                    &mut else_void
                );

                if_void || else_void
            }

            S::Return(e) => {
                if let Some(v) = e {
                    let value = self.emit_expr(v, None).0;
                    self.out.push(code!(BlockAction, "="; Item::Var("%var(%var(?d)?return)".to_string()), value));
                }
                self.out.push(Block::Call("%var(%var(?d)?jump)".to_string()));
                false
            }

            S::Wait(e) => { 
                let wait = self.emit_expr(e, None).0;
                self.out.push(Block::Wait(wait));
                true 
            }

            // !
            // ! TODO
            // ! Change closing bracket insertion to handle function jumps
            // !
            S::Loop(_kind, stmt) => {
                // use Loop as L;
                // match kind {
                //     // L::Forever => self.out.push(Block::Repeat("Forever".to_string(), Vec::new())),
                //     // L::While(e) => todo!(),
                //     _ => todo!()
                // }
                let (new, _) = self.emit(*stmt);
                self = new;
                self.out.push(Block::ReClose);
                true
            }

            S::Continue => { self.out.push(Block::Ctrl("StopRepeat".to_string())); true }
            S::Break => { self.out.push(Block::Ctrl("Skip".to_string())); true }
            S::End => { self.out.push(Block::Ctrl("End".to_string())); true }

            S::None => true,
        };
        (self, void)
    }

    fn emit_if(
        mut self,
        not: bool,
        e: Expr,
        if_clause: Stmt,
        else_clause: Option<Stmt>,
        if_void: &mut bool,
        else_void: &mut bool
    ) -> Self {

        use Expr as E;

        macro_rules! binary {
            ($name: literal, $lhs: ident, $rhs: ident) => {
                let lhs = self.emit_expr(*$lhs, None).0;
                let rhs = self.emit_expr(*$rhs, None).0;
                self.out.push(code!(IfAction, $name, not; lhs, rhs))
            };
        }

        macro_rules! if_body {
            (@if $jump: expr) => {
                let (new, v) = self.emit(if_clause);
                self = new;
                *if_void = *if_void && v;
                self.out.push(Block::IfClose);
            };
            (@else $jump: expr, $body: ident) => {
                let (new, v) = self.emit($body);
                self = new;
                *else_void = *else_void && v;
                self.out.push(Block::IfClose);
            };
        }

        macro_rules! if_stmt {
            ($name: literal, $lhs: ident, $rhs: ident) => {{
                binary!($name, $lhs, $rhs);
                if_body!(@if jump);
                if let Some(v) = else_clause {
                    self.out.push(Block::Else);
                    if_body!(@else jump, v);
                }
            }}
        }

        macro_rules! expr {
            ($e: expr) => {{
                let operand = self.emit_expr($e, None).0;
                self.out.push(code!(IfAction, "!=", not; operand, Item::Num("0".to_string())));
                if_body!(@if jump);
                if let Some(v) = else_clause {
                    self.out.push(Block::Else);
                    if_body!(@else jump, v);
                }
            }};
        }

        match e {
            E::Not(e) => self = self.emit_if(!not, *e, if_clause, else_clause, if_void, else_void),

            E::Eq(lhs, rhs) => if_stmt!("=", lhs, rhs),
            E::Ne(lhs, rhs) => if_stmt!("!=", lhs, rhs),
            E::Gt(lhs, rhs) => if_stmt!(">", lhs, rhs),
            E::Lt(lhs, rhs) => if_stmt!("<", lhs, rhs),
            E::Ge(lhs, rhs) => if_stmt!(">=", lhs, rhs),
            E::Le(lhs, rhs) => if_stmt!("<=", lhs, rhs),

            E::Func(span, func, args) => if let E::Ident(func) = *func {
                macro_rules! else_expr {
                    () => { expr!(E::Func(span, Box::new(E::Ident(func)), args)) };
                }

                if let Some(Action { block, .. }) = self.parent.actions.get(&func) {
                    if let if_blocks!() = block.as_str() {

                        let args = args
                            .into_iter()
                            .map(|arg| self.emit_expr(arg, None).0)
                            .collect::<Vec<_>>();
                        self.out.push(Block::IfAction(func, not, args));
                        if_body!(@if jump);
                        if let Some(v) = else_clause {
                            self.out.push(Block::Else);
                            if_body!(@else jump, v);
                        }

                    } else { else_expr!() }
                } else { else_expr!() }
            } else { todo!() }
            e => expr!(e)
        }

        self

    }

    fn emit_expr(&mut self, expr: Expr, default: Option<Item>) -> (Item, bool) {
        use Expr as E;

        macro_rules! binary {
            ($name: literal, $lhs: ident, $rhs: ident) => {{
                let var = default.unwrap_or_else(|| self.temp());
                let lhs = self.emit_expr(*$lhs, None).0;
                let rhs = self.emit_expr(*$rhs, None).0;
                self.out.push(code!(BlockAction, $name; var.clone(), lhs, rhs));
                (var, false)
            }};
        }

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

        match expr {
            E::Num(e) => (Item::Num(e), true),
            E::Str(e) => (Item::Text(e), true),
            E::True => (Item::Num("1".to_string()), true),
            E::False => (Item::Num("0".to_string()), true),
            E::Ident(e) => (Item::Var(format!("%var(?d)!{}", e)), true),

            E::Neg(e) => {
                let var = default.unwrap_or_else(|| self.temp());
                let operand = self.emit_expr(*e, None).0;
                self.out.push(code!(BlockAction, "-"; var.clone(), Item::Num("0".to_string()), operand));
                (var, false)
            }
            E::Add(lhs, rhs) => binary!("+", lhs, rhs),
            E::Sub(lhs, rhs) => binary!("-", lhs, rhs),
            E::Mul(lhs, rhs) => binary!("*", lhs, rhs),
            E::Div(lhs, rhs) => binary!("/", lhs, rhs),
            E::Mod(lhs, rhs) => binary!("%", lhs, rhs),

            E::Not(e) => {
                let var = default.unwrap_or_else(|| self.temp());
                let operand = self.emit_expr(*e, None).0;
                self.out.push(code!(BlockAction, "-"; var.clone(), Item::Num("1".to_string()), operand));
                (var, false)
            }
            E::And(lhs, rhs) => binary!("*", lhs, rhs),
            E::Or(lhs, rhs) => {
                let var = default.unwrap_or_else(|| self.temp());
                let lhs = self.emit_expr(*lhs, None).0;
                let rhs = self.emit_expr(*rhs, None).0;
                self.out.push(code!(BlockAction, "="; var.clone(), Item::Num(format!(
                    "%math({}+{}-%math({}*{}))", 
                    &lhs, &rhs, &lhs, &rhs
                ))));
                (var, false)
            }

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
                            format!("not all parameters have been supplied function call")
                        ));
                    }
                    self.out.push(Block::Call(func));
                    self.out.push(code!(BlockAction, "-="; Item::Var("?d".to_string())));
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
