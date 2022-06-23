
use std::collections::HashMap;
use serde_json::Value as Val;
use std::io::prelude::*;
use flate2::{ Compression, read::GzEncoder };
use crate::nbt_edit::Template;
use super::{ Program, Action, Block, codeblock::Item, emit::Line };

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

pub fn translate(program: Program) -> Vec<Template> {
    let map = &program.actions;
    program.templates
        .into_iter()
        .map(|(name, (line, vec))| translate_line(map, name, vec, line))
        .collect()
}

fn translate_line(map: &HashMap<String, Action>, name: String, vec: Vec<Block>, line: Line) -> Template {
    
    use Block as B;

    let len = vec.len();
    let mut data = match line {

        Line::Func => {
            let q_mark = if name.chars().next().unwrap() == '?' { "True".to_string() } else { "False".to_string() };
            format!(concat!(
                r#"{{"blocks":[{{"id":"block","block":"func","args":{{"items":["#,
                r#"{{"item":{{"id":"bl_tag","data":{{"option":"{}","tag":"Is Hidden","#,
                r#""action":"dynamic","block":"func"}}}},"slot":26}}]}},"data":"{}"}},"#
            ), q_mark, &name)
        }

        Line::Proc => format!(concat!(
            r#"{{"blocks":[{{"id":"block","block":"process","args":{{"items":[{{"item":{{"id":"bl_tag","data":"#,
            r#"{{"option":"True","tag":"Is Hidden","action":"dynamic","block":"process"}}}},"slot":26}}]}},"data":"{}"}},"#
        ), &name),

    };
    vec.into_iter().map(|item| match item {
        B::BlockAction(n, vec) => {
            let action = map.get(&n).expect("expected a valid codeblock").clone();
            append(action, vec)
        },
        B::Call(n) => format!(r#"{{"id":"block","block":"call_func","args":{{"items":[]}},"data":"{}"}},"#, n),
        B::Process(unbound, n) => {
            let (locals, targets) = if unbound { ("Don't copy", "With no targets") } else { ("Share", "With current targets") };  
            format!(concat!(
                r#"{{"id":"block","block":"start_process","args":{{"items":[{{"item":{{"id":"bl_tag","data":"#,
                r#"{{"option":"{}","tag":"Local Variables","action":"dynamic","block":"start_process"}}}},"#,
                r#""slot":25}},{{"item":{{"id":"bl_tag","data":{{"option":"{}","tag":"Target Mode","#,
                r#""action":"dynamic","block":"start_process"}}}},"slot":26}}]}},"data":"{}"}},"#
            ), locals, targets, n)
        }
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
        B::Repeat(n, vec) => {
            let action = map.get(&n).expect("expected a valid codeblock").clone();
            format!(r#"{}{{"id":"bracket","direct":"open","type":"repeat"}},"#, append(action, vec))
        },
        B::ReClose => r#"{"id":"bracket","direct":"close","type":"repeat"},"#.to_string(),
        _ => unreachable!()
    }).for_each(|x| data.push_str(x.as_str()));
    data.pop();
    data.push_str("]}");

    let mut vec = Vec::new();
    let mut gz = GzEncoder::new(data.as_bytes(), Compression::best());
    gz.read_to_end(&mut vec).unwrap();

    Template::new(match line {
        Line::Func => format!("§b§lFunction §3» §b{} §8({} bl.)", name, len),
        Line::Proc => format!("§a§lProcess §2» §a{} §8({} bl.)", name, len)
    }, base64::encode(&vec[..]))    
}