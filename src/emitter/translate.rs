
use std::collections::HashMap;
use chumsky::prelude::Simple;
use serde_json::Value as Val;
use std::io::prelude::*;
use flate2::{ Compression, read::{ GzEncoder, GzDecoder} };
use crate::emitter::{ Action, Block, codeblock::Item };
use super::Program;

fn append(mut action: Action, vec: Vec<Item>) -> String {
    action.append(vec
        .into_iter()
        .map(|x| x.json())
        .collect()
    );
    let mut out = serde_json::to_string(&action).expect("failed to serialize codeblock");
    out.push(',');
    out
}

pub fn translate(program: Program) -> (Vec<String>, Vec<Simple<char>>) {
    let map = &program.actions;
    let errs = program.errs;
    let vec = program.templates
        .into_iter()
        .map(|(name, vec)| translate_line(map, name, vec))
        .collect();
    (vec, errs)
}

fn translate_line(map: &HashMap<String, Action>, name: String, vec: Vec<Block>) -> String {
    
    use Block as B;

    let len = vec.len();
    let mut data = format!(r#"{{"blocks":[{{"id":"block","block":"func","args":{{"items":[{{"item":{{"id":"bl_tag","data":{{"option":"False","tag":"Is Hidden","action":"dynamic","block":"func"}}}},"slot":26}}]}},"data":"{}"}},"#, &name);
    vec.into_iter().map(|item| match item {
        B::BlockAction(n, vec) => {
            let action = map.get(&n).expect("expected a valid codeblock").clone();
            append(action, vec)
        },
        B::Call(n) => format!(r#"{{"id":"block","block":"call_func","args":{{"items":[]}},"data":"{}"}},"#, n),
        B::IfAction(n, not, _, vec) => {
            let mut action = map.get(&n).expect("expected a valid codeblock").clone();
            if action.extra.get("inverted") == Some(&Val::String("NOT".to_string())) {
                if not { action.extra.remove("inverted"); }
            } else if not {
                action.extra.insert("inverted".to_string(), Val::String("NOT".to_string()));
            }
            format!(r#"{}{{"id":"bracket","direct":"open","type":"norm"}},"#, append(action, vec))
        },
        B::Else => r#"{"id":"block","block":"else"},{"id":"bracket","direct":"open","type":"norm"},"#.to_string(),
        B::IfClose => r#"{"id":"bracket","direct":"close","type":"norm"},"#.to_string(),
        _ => todo!()
    }).for_each(|x| data.push_str(x.as_str()));
    data.pop();
    data.push_str("]}");

    let mut vec = Vec::new();
    let mut gz = GzEncoder::new(data.as_bytes(), Compression::best());
    gz.read_to_end(&mut vec).unwrap();

    format!(
        r#"{{"author":"none","name":"§b§lFunction §3» §b{} §8({} bl.)","version":1,"code":"{}"}}"#, 
        name, len, base64::encode(&vec[..])
    )
    
}