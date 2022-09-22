use std::error::Error;

#[derive(Default)]
pub struct Config {
    pub global_names: Vec<Option<String>>,
    pub scripts: Vec<Script>,
    pub rooms: Vec<Room>,
    pub suppress_preamble: bool,
}

#[derive(Default)]
pub struct Room {
    pub scripts: Vec<Script>,
}

#[derive(Default)]
pub struct Script {
    pub name: Option<String>,
    pub params: Option<u16>,
    pub locals: Vec<Var>,
}

#[derive(Default)]
pub struct Var {
    pub name: Option<String>,
}

impl Config {
    pub fn from_ini(ini: &str) -> Result<Self, Box<dyn Error>> {
        let mut result = Self {
            global_names: Vec::with_capacity(1024),
            scripts: Vec::with_capacity(512),
            rooms: Vec::with_capacity(64),
            suppress_preamble: false,
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
                    handle_script_key(ln, &mut dots, value, &mut result.scripts)?;
                }
                Some("room") => {
                    let room = it_next(&mut dots, ln)?;
                    let room: usize = room.parse().map_err(|_| parse_err(ln))?;
                    extend(&mut result.rooms, room);
                    if it_next(&mut dots, ln)? != "script" {
                        return Err(parse_err(ln));
                    }
                    handle_script_key(ln, &mut dots, value, &mut result.rooms[room].scripts)?;
                }
                _ => {
                    return Err(parse_err(ln));
                }
            }
        }
        Ok(result)
    }
}

fn handle_script_key<'a>(
    ln: usize,
    dots: &mut impl Iterator<Item = &'a str>,
    mut value: &str,
    scripts: &mut Vec<Script>,
) -> Result<(), Box<dyn Error>> {
    let script = it_next(dots, ln)?;
    let script: usize = script.parse().map_err(|_| parse_err(ln))?;
    // XXX: this wastes a bunch of memory since local scripts start at 2048
    extend(scripts, script);
    match dots.next() {
        None => {
            // parse param count as in `func(2)`
            if let Some(paren) = value.find('(') {
                if *value.as_bytes().last().unwrap() != b')' {
                    return Err(parse_err(ln));
                }
                let params = &value[paren + 1..value.len() - 1];
                let params: u16 = params.parse().map_err(|_| parse_err(ln))?;
                scripts[script].params = Some(params);
                value = &value[..paren];
            }
            scripts[script].name = Some(value.to_string());
        }
        Some("local") => {
            let local = it_final(dots, ln)?;
            let local: usize = local.parse().map_err(|_| parse_err(ln))?;
            extend(&mut scripts[script].locals, local);
            scripts[script].locals[local].name = Some(value.to_string());
        }
        Some(_) => {
            return Err(parse_err(ln));
        }
    }
    Ok(())
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
