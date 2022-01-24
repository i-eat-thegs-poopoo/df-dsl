
mod codeblock;
mod emit;

use chumsky::prelude::*;
use serde::{ Serialize, Deserialize };
use serde_json::{ Value, error::Category, json };
use crate::parser::{stmt::{ Stmt, /*Loop*/ }, decl::Decl };
use std::{ ops::Range, collections::{ HashMap, hash_map::Entry }, mem::take as mtake };
use codeblock::Block;
use emit::EmitStmt;

#[derive(Debug)]
pub struct Program {
    pub errs: Vec<Simple<char>>,
    funcs: Option<HashMap<String, Stmt>>,
    args: HashMap<String, Vec<String>>,
    actions: HashMap<String, Action>,
    templates: HashMap<String, Vec<Block>>,
    temp: usize
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Action {
    id: String,
    block: String,
    action: String,
    #[serde(default, skip_serializing)]
    void: bool,
    args: Items
}

#[derive(Debug, Serialize, Deserialize)]
struct Items {
    items: Vec<Value>
}

impl Program {

    pub fn new(errs: Vec<Simple<char>>) -> Self {
        let actions = include!("base.in");
        Self {
            errs,
            funcs: Some(HashMap::new()),
            args: HashMap::new(),
            actions,
            templates: HashMap::new(),
            temp: 0
        }
    }

    fn temp(&mut self) -> String {
        let out = format!("?{}", self.temp);
        self.temp += 1;
        out
    }

    pub fn lines(&mut self, decls: Vec<(Decl, Range<usize>)>) {
        for (decl, span) in decls.into_iter() {
            match decl {
                Decl::Func(name, args, body) => match self.funcs
                    .as_mut()
                    .expect("something something no template list something something quirky joke")
                    .entry(name.clone()) 
                {
                    Entry::Occupied(_) => self.errs.push(Simple::custom(span, 
                        format!("function `{name}` is already defined")
                    )),
                    entry => if self.actions.get(&name).is_none() {
                        entry.or_insert(body);
                        self.args.insert(name, args);
                    } else {
                        self.errs.push(Simple::custom(span, 
                            format!("`{name}` is already defined as an action")
                        ));
                    }
                }

                Decl::Use(path) => std::fs::read_to_string(path.clone())
                    .map_err(|_| self.errs.push(Simple::custom(span.clone(), 
                        format!("could not find file {path} in directory")
                    )))
                    .and_then(|v| serde_json::from_str::<HashMap<String, Action>>(v.as_str())
                        .map_err(|e| {
                            use Category as C;
                            let pos = format!("line {}, column {}", e.line(), e.column());
                            self.errs.push(Simple::custom(span.clone(), 
                            match e.classify() {
                                C::Io => format!("read failure in {path} at {pos}"),
                                C::Syntax => format!("malformed json in {path} at {pos}"),
                                C::Data => format!("type error in {path} at {pos}"),
                                C::Eof => format!("unexpected end of input in {path} at {pos}")
                            }))
                        })
                    ).map(|x| x.into_iter())
                    .map(|iter| iter.for_each(|(name, body)| match self.actions.entry(name.clone())  {
                        Entry::Occupied(_) => self.errs.push(Simple::custom(span.clone(),
                            format!("action `{name}` is already defined")
                        )),
                        entry => if self.funcs
                            .as_mut()
                            .expect("something abcdefg")
                            .get(&name)
                            .is_none() 
                        { entry.or_insert(body); } else {
                            self.errs.push(Simple::custom(span.clone(), 
                                format!("`{name}` is already defined as an function")
                            ));
                        }
                    })).unwrap_or_default()
            }
        }
    }

    pub fn emit(&mut self) {
        let mut map = HashMap::new();
        let funcs = self.funcs
            .take()
            .expect("why is template list empty mmm,")
            .into_iter();
        for (key, mut body) in funcs {
            let body = mtake(&mut body);
            let (mut stmt, void) = EmitStmt::new(self, key).emit(body);
            stmt.finalize(None, void, false);
            map.extend(stmt.map);
        }
        self.templates = map;
    }

}
