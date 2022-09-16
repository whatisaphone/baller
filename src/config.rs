use serde::Deserialize;
use std::{collections::HashMap, error::Error};

#[derive(Default, Deserialize)]
pub struct Config {
    pub globals: HashMap<u16, String>,
}

impl Config {
    pub fn from_ini(ini: &str) -> Result<Self, Box<dyn Error>> {
        let data: HashMap<String, HashMap<String, String>> = serde_ini::from_str(ini)?;
        let mut result = Self {
            globals: HashMap::new(),
        };
        for (section, pairs) in data {
            match section.as_str() {
                "globals" => {
                    for (key, value) in pairs {
                        let key: u16 = key.parse()?;
                        result.globals.insert(key, value);
                    }
                }
                _ => {
                    return Err("unexpected section in ini".into());
                }
            }
        }
        Ok(result)
    }
}
