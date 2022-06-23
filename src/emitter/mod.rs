
mod codeblock;
mod emit;
mod translate;

use chumsky::prelude::*;
use serde::{ Serialize, Deserialize };
use serde_json::{ Value, error::Category };
use crate::{ parser::{ stmt::Stmt, Decl }, nbt_edit::Template };
use std::{ ops::Range, collections::{ HashMap, hash_map::Entry }, mem::take as mtake, path::{ Path, PathBuf } };
use codeblock::Block;
use emit::{ EmitStmt, Line };

#[derive(Debug)]
pub struct Program<'a> {
    errs: &'a mut Vec<Simple<char>>,
    path: PathBuf,
    funcs: Option<HashMap<String, Stmt>>,
    args: HashMap<String, Vec<String>>,
    actions: HashMap<String, Action>,
    templates: HashMap<String, (Line, Vec<Block>)>,
    len: usize,
    temp: u32
}

macro_rules! if_blocks {
    () => { "if_var" | "if_player" | "if_entity" | "if_game" };
}
pub(crate) use if_blocks;


#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Action {
    id: String,
    block: String,
    action: String,
    #[serde(default, skip_serializing)]
    void: bool,
    #[serde(flatten)]
    extra: HashMap<String, Value>,
    args: Items
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Items {
    items: Vec<DfItem>,
    #[serde(flatten)]
    extra: HashMap<String, Value>,
    #[serde(default, skip_serializing)]
    slots: Option<Vec<u32>>,
    #[serde(default, skip_serializing)]
    curr: u32
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DfItem {
    item: Value,
    slot: u32
}

impl Action {

    pub fn push(&mut self, item: Value) {
        let slots = self.args.slots.as_mut().unwrap();
        if slots.contains(&self.args.curr) {
            self.args.curr += 1;
            self.push(item);
        } else {
            slots.push(self.args.curr);
            self.args.items.push(DfItem { item, slot: self.args.curr });
        }
    }

    pub fn append(&mut self, items: Vec<Value>) {
        for i in items { self.push(i) }
    }

}

impl <'a> Program<'a> {

    pub fn gen(
        path: String, 
        decls: Vec<(Decl, Range<usize>)>, 
        errs: &'a mut Vec<Simple<char>>, 
        len: usize
    ) -> Vec<Template> {

        let actions = include!("base.rs");
        let mut out = Self {
            errs,
            path: Path::new(path.as_str())
                .parent()
                .expect(format!("error: file {path} has no parent directory").as_str())
                .to_path_buf(),
            funcs: Some(HashMap::new()),
            args: HashMap::new(),
            actions,
            templates: HashMap::new(),
            len,
            temp: 0
        };

        out.lines(decls);
        out.emit();
        translate::translate(out)
    }

    fn temp(&mut self) -> String {
        let out = format!("?{}", self.temp);
        self.temp += 1;
        out
    }

    fn lines(&mut self, decls: Vec<(Decl, Range<usize>)>) {
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

                Decl::Use(path) => std::fs::read_to_string(self.path.join(path.as_str()))
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
                    .map(|iter| iter.for_each(|(name, mut body)| match self.actions.entry(name.clone())  {
                        Entry::Occupied(_) => self.errs.push(Simple::custom(span.clone(),
                            format!("action `{name}` is already defined")
                        )),
                        entry => if self.funcs
                            .as_mut()
                            .expect("something abcdefg")
                            .get(&name)
                            .is_none() 
                        {
                            if body.args.slots.is_none() {
                                let _ = body.args.slots.insert(body.args.items
                                    .iter()
                                    .map(|x| x.slot)
                                    .collect()
                                );
                            }
                            body.args.curr = 0;
                            entry.or_insert(body); 
                        } else {
                            self.errs.push(Simple::custom(span.clone(), 
                                format!("`{name}` is already defined as an function")
                            ));
                        }
                    })).unwrap_or_default()
            }
        }
    }

    fn emit(&mut self) {
        let funcs = self.funcs
            .take()
            .expect("why is template list empty mmm,")
            .into_iter();
        for (key, mut body) in funcs {
            let body = mtake(&mut body);
            EmitStmt::run(self, body, key, Line::Func);
        }
    }

}
