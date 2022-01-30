
use serde_json::{ Value, json };

#[derive(Debug, Clone)]
pub enum Item {
    Num(String),
    Text(String),
    Var(String)
}

impl Item {

    pub fn get(self) -> String {
        match self {
            Self::Num(e) => e,
            Self::Text(e) => e,
            Self::Var(e) => e
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
            })
        }
    }

}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(e) => f.write_str(e.as_str()),
            Self::Text(e) => f.write_str(e.as_str()),
            Self::Var(e) => write!(f, "%var({})", e)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Block {
    BlockAction(String, Vec<Item>),
    Call(String),

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
