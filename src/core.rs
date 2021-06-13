use std::fmt::Display;
use serde::{Serialize, Deserialize, Serializer};

#[derive(Debug, Clone, Copy, PartialOrd, PartialEq, Deserialize)]
pub struct Location(pub usize, pub usize);

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl Serialize for Location {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> where
        S: Serializer {
        serializer.serialize_str(&format!("{}", self))
    }
}