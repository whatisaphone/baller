use std::error::Error;

#[derive(Default)]
pub struct Config {
    pub global_names: Vec<Option<String>>,
    pub script_names: Vec<Option<String>>,
    pub rooms: Vec<Room>,
}

#[derive(Default)]
pub struct Room {
    pub scripts: Vec<LocalScript>,
}

#[derive(Default)]
pub struct LocalScript {
    pub name: Option<String>,
}

impl Config {
    pub fn from_ini(ini: &str) -> Result<Self, Box<dyn Error>> {
        let mut result = Self {
            global_names: Vec::with_capacity(1024),
            script_names: Vec::with_capacity(512),
            rooms: Vec::with_capacity(64),
        };
        for (ln, line) in ini.lines().enumerate() {
            let line = line.split_once(';').map_or(line, |(a, _)| a); // Trim comments
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            let (lhs, rhs) = line.split_once('=').ok_or_else(|| parse_err(ln))?;
            let key = lhs.trim();
            let value = rhs.trim();
            let mut dots = key.split('.');
            match dots.next() {
                Some("global") => {
                    let id = it_final(&mut dots, ln)?;
                    let id: usize = id.parse().map_err(|_| parse_err(ln))?;
                    extend(&mut result.global_names, id);
                    result.global_names[id] = Some(value.to_string());
                }
                Some("script") => {
                    let id = it_next(&mut dots, ln)?;
                    let id: usize = id.parse().map_err(|_| parse_err(ln))?;
                    extend(&mut result.script_names, id);
                    match dots.next() {
                        None => {
                            result.script_names[id] = Some(value.to_string());
                        }
                        Some(_) => {
                            return Err(parse_err(ln));
                        }
                    }
                }
                Some("room") => {
                    let room = it_next(&mut dots, ln)?;
                    let room: usize = room.parse().map_err(|_| parse_err(ln))?;
                    extend(&mut result.rooms, room);
                    if it_next(&mut dots, ln)? != "script" {
                        return Err(parse_err(ln));
                    }
                    let script = it_next(&mut dots, ln)?;
                    let script: usize = script.parse().map_err(|_| parse_err(ln))?;
                    // XXX: this wastes a bunch of memory since local scripts start at 2048
                    extend(&mut result.rooms[room].scripts, script);
                    match dots.next() {
                        None => {
                            result.rooms[room].scripts[script].name = Some(value.to_string());
                        }
                        Some(_) => {
                            return Err(parse_err(ln));
                        }
                    }
                }
                _ => {
                    return Err(parse_err(ln));
                }
            }
        }
        Ok(result)
    }
}

fn it_next<T>(it: &mut impl Iterator<Item = T>, ln: usize) -> Result<T, Box<dyn Error>> {
    it.next().ok_or_else(|| parse_err(ln))
}

fn it_end<T>(it: &mut impl Iterator<Item = T>, ln: usize) -> Result<(), Box<dyn Error>> {
    match it.next() {
        Some(_) => return Err(parse_err(ln)),
        None => Ok(()),
    }
}

fn it_final<T>(it: &mut impl Iterator<Item = T>, ln: usize) -> Result<T, Box<dyn Error>> {
    let result = it_next(it, ln);
    it_end(it, ln)?;
    result
}

fn extend<T: Default>(xs: &mut Vec<T>, upto: usize) {
    if xs.len() < upto + 1 {
        xs.resize_with(upto + 1, T::default);
    }
}

fn parse_err(line_index: usize) -> Box<dyn Error> {
    let line_number = line_index + 1;
    format!("bad config on line {line_number}").into()
}
