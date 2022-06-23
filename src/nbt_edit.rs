
use serde::{ Serialize, Deserialize };
use nbt::Value as Val;
type Map = nbt::Map<String, Val>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Template {
    author: String,
    name: String,
    version: usize,
    code: String
}

impl Template {
    pub fn new(name: String, code: String) -> Self {
        Self {
            author: "none".to_string(),
            name,
            version: 1,
            code
        }
    }
}

macro_rules! map {
    ($( $key: expr => $val: expr ),*) => {
        {
            let mut map = Map::new();
            $( map.insert($key.to_string(), $val); )*
            map
        }
    };
}

pub fn edit(path: String, templates: Vec<Template>) {
    
    let src = std::fs::read(path.clone()).ok()
        .expect("error: could not find output file");
    let mut toolbars: Map = nbt::from_reader(&src[..])
        .expect("error: failed to parse nbt");

    let templates: Vec<Val> = templates.as_slice()
        .chunks(27)
        .map(|s| Val::List(s.iter()
            .enumerate()
            .map(|(i, t)| Val::Compound(map![
                "id" => Val::String("minecraft:ender_chest".to_string()),
                "Slot" => Val::Byte(i as i8),
                "Count" => Val::Byte(1),
                "tag" => Val::Compound(map![
                    "display" => Val::Compound(map![
                        "Name" => Val::String(format!(r#"{{"text":"{}"}}"#, &t.name))
                    ]),
                    "PublicBukkitValues" => Val::Compound(map![
                        "hypercube:codetemplatedata" => Val::String(
                            serde_json::to_string(t).expect("failed to serialize template")
                        )
                    ])
                ])
            ]))
            .collect()
        ))
        .map(|v| Val::Compound(map![
            "id" => Val::String("minecraft:white_shulker_box".to_string()),
            "Count" => Val::Byte(1),
            "tag" => Val::Compound(map![
                "BlockEntityTag" => Val::Compound(map![
                    "id" => Val::String("minecraft:shulker_box".to_string()),
                    "Items" => v
                ])
            ])
        ]))
        .chain(std::iter::repeat(Val::Compound(map![
            "id" => Val::String("minecraft:air".to_string()),
            "Count" => Val::Byte(1)
        ])))
        .take(9)
        .collect();

    toolbars.insert("8".to_string(), Val::List(templates));

    let mut vec = Vec::new();
    nbt::to_writer(&mut vec, &toolbars, None).unwrap();

    std::fs::write(path, &vec[..]).unwrap();
}
