
use super::Action;

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

#[derive(Debug)]
pub enum Block {
    BlockAction(String, Vec<Item>),
    Call(String),

    IfAction(String, bool, Vec<Item>),
    Else,
    IfClose,

    Repeat(String, Vec<Item>),
    ReClose,
    
    Ctrl(String),
    Wait(Item)
}
