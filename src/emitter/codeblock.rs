
use serde_json::{ Value, json };

#[derive(Debug, Clone)]
pub enum Item {
    Num(String),
    Text(String),
    Var(String),
    Game(String),
    Save(String),
    Value(String, String),
    Item,
}

impl Item {

    pub fn get(self) -> String {
        match self {
            Self::Num(e) => e,
            Self::Text(e) => e,
            Self::Var(e) => e,
            Self::Game(e) => e,
            Self::Save(e) => e,
            Self::Value(e, _) => e,
            Self::Item => panic!("tried to get text of invalid item")
        }
    }

    pub fn json(self) -> Value {
        match self {
            Self::Num(e) => json!({
                "id": "num",
                "data": {
                    "name": e
                }
            }),
            Self::Text(e) => json!({
                "id": "txt",
                "data": {
                    "name": e
                }
            }),
            Self::Var(e) => json!({
                "id": "var",
                "data": {
                    "name": e,
                    "scope": "local"
                }
            }),
            Self::Game(e) => json!({
                "id": "var",
                "data": {
                    "name": e,
                    "scope": "unsaved"
                }
            }),
            Self::Save(e) => json!({
                "id": "var",
                "data": {
                    "name": e,
                    "scope": "saved"
                }
            }),
            Self::Value(e, sel) => json!({
                "id": "g_val",
                "data": {
                    "type": e,
                    "target": sel
                }
            }),
            Self::Item => json!({
                "id": "item",
                "data": {
                    "item": "{DF_NBT:2586,id:\"minecraft:stone\",Count:1b}"
                }
            }),
        }
    }

}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(e) => f.write_str(e.as_str()),
            Self::Text(e) => f.write_str(e.as_str()),
            Self::Var(e) => write!(f, "%var({})", e),
            Self::Game(e) => write!(f, "%var({})", e),
            Self::Save(e) => write!(f, "%var({})", e),
            Self::Value(..) => write!(f, "GAME_VALUE"),
            Self::Item => write!(f, "ITEM"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Block {
    BlockAction(String, Vec<Item>),
    Call(String),
    Process(bool, String),

    IfAction(String, bool, bool, Vec<Item>), // second bool is if it has else stmt
    Else,
    IfClose,

    Repeat(String, Vec<Item>),
    ReClose,
    
    RetPlaceholder,
    None
}

impl Default for Block {
    fn default() -> Self { Self::None }
}
