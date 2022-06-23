
use chumsky::prelude::*;
use crate::parser::{ expr::Expr, stmt::{ Stmt, Loop } };
use super::{ Program, Action, codeblock::{ Item, Block }, if_blocks };
use std::{ collections::HashMap, mem::take as mtake };

macro_rules! code {
    ($var: ident, $name: expr $( , $extra: expr )*; $( $args: expr ),*) => {
        Block::$var(
            $name.to_string(),
            $( $extra, )*
            vec![$( $args ),*]
        )
    };
}

#[derive(Debug, Clone, Copy)]
pub enum Line { Func, Proc }

impl Default for Line {
    fn default() -> Self { Line::Func }
}

#[derive(Debug)]
pub struct EmitStmt<'a, 'b> {
    parent: &'a mut Program<'b>,
    out: EmitLine,
    temp: u32
}

impl <'a, 'b> EmitStmt<'a, 'b> {

    pub fn run(parent: &'a mut Program<'b>, stmt: Stmt, name: String, line: Line) {
        let emit = Self {
            parent,
            out: EmitLine::new(name, line),
            temp: 0
        }.emit(stmt);
        let templates = &mut emit.parent.templates;
        let mut exprs = HashMap::new();
        emit.out.insert(templates);
        templates.retain(|name, (line, vec)| if vec.len() == 1 {
            if let Line::Proc = line { return true }
            if let Some('?') = name.chars().next() {
                let func = vec.get_mut(0).unwrap();
                exprs.insert(name.clone(), mtake(func));
                return false;
            }
            true
        } else { true });
        for (line, vec) in templates.values_mut() {
            if let Line::Proc = line { continue }
            vec.iter_mut().for_each(|item| if let Block::Call(s) = item {
                if let Some(replace) = exprs.get(s) { *item = replace.clone() }
            })
        }
    }

    fn temp(&mut self) -> Item {
        let out = format!("%var(?d)?{}", self.temp);
        self.temp += 1;
        Item::Var(out)
    }

    fn push(&mut self, item: Block) {
        self.out.push(item, self.parent);
    }

    fn append(&mut self, items: Vec<Block>) {
        for item in items.into_iter() { self.push(item) }
    }
    
    fn emit(mut self, stmt: Stmt) -> Self {
        self.temp = 0;

        use Stmt as S;
        match stmt {
            S::Expr(e) => { self.emit_expr(e, None); },
            S::Assign(var, e) => {
                let var = format!("%var(?d)!{}", var);
                let (operand, assign) = self.emit_expr(e, Some(Item::Var(var.clone())));
                if assign {
                    self.push(code!(BlockAction, "="; Item::Var(var), operand));
                }
            }

            S::Block(vec) => for stmt in vec.into_iter() { self = self.emit(stmt) }
            S::If(e, if_clause, else_clause) => self = self.emit_if(
                false,
                e, 
                *if_clause, 
                else_clause.map(|x| *x)
            ),

            S::Return(e) => if let Some(v) = e {
                let value = self.emit_expr(v, None).0;
                self.push(code!(BlockAction, "="; Item::Var("%var(%var(?d)?return)".to_string()), value));
                self.push(Block::RetPlaceholder);
            }

            S::Wait(e) => { 
                let wait = self.emit_expr(e, None).0;
                self.push(code!(BlockAction, "?Wait"; wait));
            }

            S::Process(unbound, stmt) => {
                let name = self.parent.temp();
                self.push(Block::Process(unbound, name.clone()));
                EmitStmt::run(self.parent, *stmt, name, Line::Proc);
            }

            S::Loop(Loop::Forever, stmt) => {
                self.push(Block::Repeat("?Forever".to_string(), Vec::new()));
                self = self.emit(*stmt);
                self.push(Block::ReClose);
            }

            S::Loop(Loop::While(e), stmt) => {
                self.push(Block::Repeat("?Forever".to_string(), Vec::new()));
                self = self.emit(Stmt::If(e, stmt, Some(Box::new(Stmt::Break))));
                self.push(Block::ReClose);
            }

            S::Loop(Loop::Range(var, from, to, by), stmt) => {
                let from = self.emit_expr(from, None).0;
                let to = self.emit_expr(to, None).0;
                let mut args = vec![Item::Var(format!("%var(?d)!{}", var)), from, to];
                if let Some(v) = by {
                    let by = self.emit_expr(v, None).0;
                    args.push(by);
                }
                self.push(Block::Repeat("?Range".to_string(), args));
                self = self.emit(*stmt);
                self.push(Block::ReClose);
            }

            S::Loop(Loop::Entry(var, dict), stmt) => {
                let dict = self.emit_expr(dict, None).0;
                self.push(Block::Repeat(
                    "?ForEachEntry".to_string(), 
                    vec![Item::Var(format!("%var(?d)!{}", var)), dict]
                ));
                self = self.emit(*stmt);
                self.push(Block::ReClose);
            }

            S::Loop(Loop::List(var, list), stmt) => {
                macro_rules! body { ($e: expr) => {{
                    let list = self.emit_expr($e, None).0;
                    self.push(Block::Repeat(
                        "?ForEach".to_string(), 
                        vec![Item::Var(format!("%var(?d)!{}", var)), list]
                    ));
                }}; }
                if let Expr::Func(x, func, args) = list {
                    macro_rules! special { ($x: expr) => {{
                        let mut args = args
                            .into_iter()
                            .map(|arg| self.emit_expr(arg, None).0)
                            .collect::<Vec<_>>();
                        args.insert(0, Item::Var(format!("%var(?d)!{}", var)));
                        self.push(Block::Repeat($x.to_string(), args));
                    }}; }
                    if let Expr::Ident(f) = *func {
                        match f.as_str() {
                            "adjacent" => special!("?Adjacent"),
                            "grid" => special!("?Grid"),
                            "path" => special!("?Path"),
                            "sphere" => special!("?Sphere"),
                            _ => { body!(Expr::Func(x, Box::new(Expr::Ident(f)), args)) }
                        }
                    } else { body!(Expr::Func(x, func, args)) } 
                } else { body!(list) }
                self = self.emit(*stmt);
                self.push(Block::ReClose);
            }

            S::Continue => self.push(code!(BlockAction, "?SkipIteration";)),
            S::Break => self.push(code!(BlockAction, "?StopRepeat";)),
            S::End => self.push(code!(BlockAction, "?End";)),

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
        let has_else = else_clause.is_some();

        macro_rules! if_body {
            (@if) => { if_body!(@in if_clause) };
            (@else $body: expr) => { if_body!(@in $body) };
            (@in $body: expr) => {
                self = self.emit($body);
                self.push(Block::IfClose);
            };
        }

        macro_rules! emit_if {
            ($( $stmts: stmt; )*) => {{
                $( $stmts )*
                if_body!(@if);
                if let Some(v) = else_clause {
                    self.push(Block::Else);
                    if_body!(@else v);
                }
            }};
        }

        macro_rules! comp {
            ($name: expr, $lhs: expr, $rhs: expr) => { emit_if! {
                let lhs = self.emit_expr(*$lhs, None).0;
                let rhs = self.emit_expr(*$rhs, None).0;
                self.push(code!(IfAction, $name, not, has_else; lhs, rhs));
            } };
        }

        macro_rules! expr { 
            ($e: expr) => { emit_if! {
                let operand = self.emit_expr($e, None).0;
                self.push(code!(IfAction, "!=", not, has_else; operand, Item::Num("0".to_string())));
            } };
        }

        match e {
            E::Not(e) => self = self.emit_if(!not, *e, if_clause, else_clause),

            E::Eq(lhs, rhs) => comp!("==", lhs, rhs),
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
                        self.push(Block::IfAction(func, not, has_else, args));
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
                self.append(vec![
                    code!(BlockAction, "="; var.clone(), Item::Num("0".to_string())),
                    code!(IfAction, $name, false, false; lhs, rhs),
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
                self.push(code!(BlockAction, $name; var.clone(), $( $args ),*));
                (var, false)
            }};
        }

        match expr {
            E::Num(e) => (Item::Num(e), true),
            E::Str(e) => (Item::Text(e), true),
            E::True => (Item::Num("1".to_string()), true),
            E::False => (Item::Num("0".to_string()), true),
            E::Value(e, sel) => (Item::Value(e, sel), true),
            E::Item => (Item::Item, true),
            E::Ident(e) => (Item::Var(format!("%var(?d)!{}", e)), true),
            E::Game(e) => (Item::Game(e), true),
            E::Save(e) => (Item::Save(e), true),

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

            E::Eq(lhs, rhs) => comp!("==", lhs, rhs),
            E::Ne(lhs, rhs) => comp!("!=", lhs, rhs),
            E::Gt(lhs, rhs) => comp!(">", lhs, rhs),
            E::Lt(lhs, rhs) => comp!("<", lhs, rhs),
            E::Ge(lhs, rhs) => comp!(">=", lhs, rhs),
            E::Le(lhs, rhs) => comp!("<=", lhs, rhs),

            E::List(list) => {
                let var = default.unwrap_or_else(|| self.temp());
                let mut iter = list.into_iter().peekable();

                macro_rules! push {
                    ($name: expr) => {
                        let vec = std::iter::once(var.clone()).chain(iter
                            .by_ref()
                            .take(26)
                            .map(|arg| self.emit_expr(arg, None).0)
                        ).collect();
                        self.push(Block::BlockAction($name.to_string(), vec));
                    };
                }

                push!("create_list");
                while iter.peek().is_some() {
                    push!("push");
                }
                (var, false)
            }

            E::Func(span, e, args) => if let E::Ident(func) = *e {
                let var = default.unwrap_or_else(|| self.temp());
                if let Some(v) = self.parent.args.get(&func) {
                    let mut params = v.clone().into_iter();
                    self.push(code!(BlockAction, "+"; 
                        Item::Var("?d_plus".to_string()),
                        Item::Var("?d".to_string()),
                        Item::Num("1".to_string())
                    ));
                    for (param, arg) in params.by_ref().zip(args.into_iter()) {
                        let arg = self.emit_expr(arg, None).0;
                        self.push(code!(BlockAction, "="; Item::Var(format!("%var(?d_plus)!{}", param)), arg));
                    }
                    if params.next().is_some() {
                        self.parent.errs.push(Simple::custom(span, 
                            format!("not all parameters have been supplied in function call")
                        ));
                    }
                    self.append(vec![
                        code!(BlockAction, "="; Item::Var("?return".to_string()), Item::Text(var.clone().get())),
                        code!(BlockAction, "+="; Item::Var("?d".to_string())),
                        code!(BlockAction, "="; Item::Var("%var(?d)?return".to_string()), Item::Var("?return".to_string())),
                    ]);
                    self.push(Block::Call(func));
                    self.push(code!(BlockAction, "-="; Item::Var("?d".to_string())));
                } else if let Some(Action { block, void, .. }) = self.parent.actions.get(&func) {
                    if let if_blocks!() = block.as_str() {

                        let args = args
                            .into_iter()
                            .map(|arg| self.emit_expr(arg, None).0)
                            .collect::<Vec<_>>();
                        self.append(vec![
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
                        self.push(Block::BlockAction(func, args));

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

#[derive(Debug)]
struct EmitLine {
    map: HashMap<String, (Line, Vec<Block>)>,
    stack: Vec<(usize, String, Vec<Block>)>,
    line: Line
}

impl EmitLine {

    fn new(name: String, line: Line) -> Self {
        Self {
            map: HashMap::new(),
            stack: vec![(0, name, Vec::new())],
            line
        }
    }

    fn push(&mut self, item: Block, program: &mut Program) {
        // max: 5
        // - - - - -
        // - { # } #
        // (depth * 2 + 1) from the end
        let depth = self.stack.len();
        let (if_stmts, _, vec) = self.stack.last_mut().unwrap();

        macro_rules! new { ($item: expr) => {
            let temp = program.temp();
            vec.push(Block::Call(temp.clone()));
            self.stack.push((0, temp, Vec::new()));
            self.push($item, program);
        }; }

        let wrap = vec.len() + *if_stmts;

        use Block as B;
        match item {

            // if + else
            B::IfAction(.., true, _) => if wrap + 6 >= program.len {
                new!(item);
            } else {
                *if_stmts += 5;
                vec.push(item);
            }

            // expect to be >= 3 width
            B::IfAction(..) | B::Repeat(..) => if wrap + 3 >= program.len {
                new!(item);
            } else {
                *if_stmts += 2;
                vec.push(item);
            }

            B::Else => {
                *if_stmts -= 1;
                vec.push(item);
            },

            B::IfClose | B::ReClose => if *if_stmts == 0 {
                let (_, name, vec) = self.stack.pop().unwrap();
                self.map.insert(name, (mtake(&mut self.line), vec));
                self.stack
                    .last_mut()
                    .unwrap().2
                    .push(item);
            } else {
                *if_stmts -= 2;
                vec.push(item);
            }

            B::RetPlaceholder => {
                let item = Block::BlockAction("?ReturnNTimes".to_string(), vec![Item::Num(depth.to_string())]);
                if wrap + 1 >= program.len {
                    let temp = program.temp();
                    vec.push(Block::Call(temp.clone()));
                    self.stack.push((0, temp, Vec::new()));
                    self.push(item, program);
                } else {
                    vec.push(item);
                }
            }

            _ => if wrap + 1 >= program.len { 
                new!(item);
            } else {
                vec.push(item);
            }
        }
    }

    fn insert(mut self, into: &mut HashMap<String, (Line, Vec<Block>)>) {
        for (_, name, vec) in self.stack {
            self.map.insert(name, (mtake(&mut self.line), vec));
        }
        into.extend(self.map);
    }

}
