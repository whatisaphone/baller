use crate::{
    script::{decompile, disasm_to_string},
    xor::XorStream,
};
use byteordered::byteorder::{ReadBytesExt, BE};
use std::{
    collections::HashMap,
    error::Error,
    fmt::Write,
    io,
    io::{BufReader, Read, Seek, SeekFrom},
    str,
};

pub const NICE: u8 = 0x69;

pub fn extract(
    s: &mut (impl Read + Seek),
    write: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
    let s = XorStream::new(s, NICE);
    let mut s = BufReader::new(s);

    let len = s.seek(SeekFrom::End(0))?;
    s.rewind()?;

    let mut path = String::with_capacity(64);
    path.push('.');
    let mut state = State {
        path,
        blocks: HashMap::new(),
        tmp_buf: Vec::new(),
    };

    extract_blocks(&mut s, &mut state, write, len)?;
    Ok(())
}

struct State {
    path: String,
    blocks: HashMap<[u8; 4], i32>,
    tmp_buf: Vec<u8>,
}

fn extract_blocks<S: Read + Seek>(
    s: &mut S,
    state: &mut State,
    write: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    parent_len: u64,
) -> Result<(), Box<dyn Error>> {
    let mut map = String::with_capacity(1 << 10);
    let start = s.stream_position()?;
    while s.stream_position()? < start + parent_len {
        read_block(s, |s, id, len| {
            let next_index = state.blocks.entry(id).or_insert(1);
            let index = *next_index;
            *next_index += 1;

            let id = str::from_utf8(&id)?;

            writeln!(map, "{id}_{index:02}").unwrap();

            if is_block_recursive(s, len)? {
                write!(state.path, "/{id}_{index:02}").unwrap();

                extract_blocks(s, state, write, len)?;

                state.path.truncate(state.path.rfind('/').unwrap_or(0));
            } else {
                state.tmp_buf.clear();
                state.tmp_buf.reserve(len.try_into()?);
                io::copy(&mut s.take(len), &mut state.tmp_buf)?;
                let blob = &state.tmp_buf[..len.try_into()?];

                if id == "SCRP" {
                    if let Some(decomp) = decompile(blob) {
                        let filename = format!("{path}/{id}_{index:02}.scu", path = state.path);
                        write(&filename, decomp.as_bytes())?;
                    }

                    let disasm = disasm_to_string(blob);
                    let filename = format!("{path}/{id}_{index:02}.s", path = state.path);
                    write(&filename, disasm.as_bytes())?;
                }

                let filename = format!("{path}/{id}_{index:02}.bin", path = state.path);
                write(&filename, &state.tmp_buf)?;
            }
            Ok(())
        })?;
    }
    write(&format!("{path}/.map", path = state.path), map.as_bytes())?;
    Ok(())
}

fn read_block<S: Read + Seek, R>(
    s: &mut S,
    f: impl FnOnce(&mut S, [u8; 4], u64) -> Result<R, Box<dyn Error>>,
) -> Result<R, Box<dyn Error>> {
    let start = s.stream_position()?;
    let mut id = [0; 4];
    s.read_exact(&mut id)?;
    let len = s.read_i32::<BE>()?;
    let len: u64 = len.try_into()?;
    let result = f(s, id, len - 8)?;
    let end = s.stream_position()?;
    if end - start != len {
        return Err("bug: block reader read wrong length".into());
    }
    Ok(result)
}

// heuristic
fn is_block_recursive<S: Read + Seek>(s: &mut S, len: u64) -> Result<bool, Box<dyn Error>> {
    if len < 8 {
        return Ok(false);
    }
    let start = s.stream_position()?;
    let mut id = [0; 4];
    s.read_exact(&mut id)?;
    let len = s.read_i32::<BE>()?;
    s.seek(SeekFrom::Start(start))?; // only peek, don't consume

    Ok(id.iter().all(|&ch| (32..=126).contains(&ch)) && (0..0x100_0000).contains(&len))
}
