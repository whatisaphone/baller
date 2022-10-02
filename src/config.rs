use std::{collections::HashMap, error::Error};

#[derive(Default)]
pub struct Config {
    pub global_names: Vec<Option<String>>,
    pub global_types: Vec<Option<EnumId>>,
    pub scripts: Vec<Script>,
    pub rooms: Vec<Room>,
    pub enums: Vec<Enum>,
    pub enum_names: HashMap<String, EnumId>,
    pub suppress_preamble: bool,
}

#[derive(Default)]
pub struct Room {
    pub vars: Vec<Var>,
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
    pub ty: Option<EnumId>,
}

pub struct Enum {
    pub name: String,
    pub values: HashMap<i32, String>,
}

pub type EnumId = usize;

impl Config {
    pub fn from_ini(ini: &str) -> Result<Self, Box<dyn Error>> {
        let mut result = Self {
            global_names: Vec::with_capacity(1024),
            global_types: Vec::with_capacity(1024),
            scripts: Vec::with_capacity(512),
            rooms: Vec::with_capacity(64),
            enums: Vec::with_capacity(64),
            enum_names: HashMap::with_capacity(64),
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
                Some("enum") => {
                    handle_enum_key(ln, &mut dots, value, &mut result)?;
                }
                Some("global") => {
                    let id = it_final(&mut dots, ln)?;
                    let id: usize = id.parse().map_err(|_| parse_err(ln))?;
                    let (name, type_) = parse_var_name_type(value, &result, ln)?;
                    extend(&mut result.global_names, id);
                    result.global_names[id] = Some(name.to_string());
                    extend(&mut result.global_types, id);
                    result.global_types[id] = type_;
                }
                Some("script") => {
                    handle_script_key(ln, &mut dots, value, &mut result, |c| &mut c.scripts)?;
                }
                Some("room") => {
                    let room = it_next(&mut dots, ln)?;
                    let room: usize = room.parse().map_err(|_| parse_err(ln))?;
                    extend(&mut result.rooms, room);
                    match it_next(&mut dots, ln)? {
                        "var" => {
                            let var = it_final(&mut dots, ln)?;
                            let var: usize = var.parse().map_err(|_| parse_err(ln))?;
                            extend(&mut result.rooms[room].vars, var);
                            result.rooms[room].vars[var].name = Some(value.to_string());
                        }
                        "script" => {
                            handle_script_key(ln, &mut dots, value, &mut result, |c| {
                                &mut c.rooms[room].scripts
                            })?;
                        }
                        _ => {
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

fn handle_script_key<'a>(
    ln: usize,
    dots: &mut impl Iterator<Item = &'a str>,
    mut value: &str,
    config: &mut Config,
    scripts_vec: impl Fn(&mut Config) -> &mut Vec<Script>,
) -> Result<(), Box<dyn Error>> {
    let script = it_next(dots, ln)?;
    let script: usize = script.parse().map_err(|_| parse_err(ln))?;
    // XXX: this wastes a bunch of memory since local scripts start at 2048
    let scripts = scripts_vec(config);
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
            let (name, ty) = parse_var_name_type(value, config, ln)?;
            let scripts = scripts_vec(config);
            extend(&mut scripts[script].locals, local);
            scripts[script].locals[local].name = Some(name.to_string());
            scripts[script].locals[local].ty = ty;
        }
        Some(_) => {
            return Err(parse_err(ln));
        }
    }
    Ok(())
}

fn handle_enum_key<'a>(
    ln: usize,
    dots: &mut impl Iterator<Item = &'a str>,
    value: &str,
    config: &mut Config,
) -> Result<(), Box<dyn Error>> {
    let enum_name = it_next(dots, ln)?;
    let const_value: i32 = it_final(dots, ln)?.parse().map_err(|_| parse_err(ln))?;
    let const_name = value.to_string();

    let enum_id = config
        .enum_names
        .get(enum_name)
        .copied()
        .unwrap_or_else(|| {
            let id = config.enums.len();
            config.enums.push(Enum {
                name: enum_name.to_string(),
                values: HashMap::new(),
            });
            config.enum_names.insert(enum_name.to_string(), id);
            id
        });
    config.enums[enum_id].values.insert(const_value, const_name);
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

fn parse_var_name_type<'a>(
    s: &'a str,
    config: &Config,
    ln: usize,
) -> Result<(&'a str, Option<EnumId>), Box<dyn Error>> {
    match s.split_once(':') {
        None => Ok((s, None)),
        Some((name, ty)) => {
            let enum_id = *config
                .enum_names
                .get(ty.trim_start())
                .ok_or_else(|| parse_err(ln))?;
            Ok((name.trim_end(), Some(enum_id)))
        }
    }
}
