use crate::{
    config::Config,
    script::{decompile, disasm_to_string, Scope},
    xor::XorStream,
};
use byteordered::byteorder::{ReadBytesExt, BE, LE};
use std::{
    collections::HashMap,
    error::Error,
    fmt,
    fmt::Write,
    io,
    io::{BufReader, Read, Seek, SeekFrom},
    mem,
    str,
};
use tracing::info_span;

pub const NICE: u8 = 0x69;

pub struct Index {
    pub lfl_disks: Vec<u8>,
    pub lfl_offsets: Vec<i32>,
    pub scripts: Directory,
    pub sounds: Directory,
}

pub struct Directory {
    /// The game internally calls this "disk number"
    pub room_numbers: Vec<u8>,
    /// The game internally calls this "disk offset"
    pub offsets: Vec<i32>,
    pub glob_sizes: Vec<i32>,
}

pub fn read_index(s: &mut (impl Read + Seek)) -> Result<Index, Box<dyn Error>> {
    let s = XorStream::new(s, NICE);
    let mut s = BufReader::new(s);

    let len = s.seek(SeekFrom::End(0))?;
    s.rewind()?;

    let mut state = IndexState {
        buf: Vec::with_capacity(64 << 10),
        lfl_disks: None,
        lfl_offsets: None,
        scripts: None,
        sounds: None,
    };

    scan_blocks(&mut s, &mut state, handle_index_block, len)?;

    Ok(Index {
        lfl_disks: state.lfl_disks.ok_or("index incomplete")?,
        lfl_offsets: state.lfl_offsets.ok_or("index incomplete")?,
        scripts: state.scripts.ok_or("index incomplete")?,
        sounds: state.sounds.ok_or("index incomplete")?,
    })
}

struct IndexState {
    buf: Vec<u8>,
    lfl_disks: Option<Vec<u8>>,
    lfl_offsets: Option<Vec<i32>>,
    scripts: Option<Directory>,
    sounds: Option<Directory>,
}

fn handle_index_block<R: Read>(
    stream: &mut R,
    state: &mut IndexState,
    id: [u8; 4],
    len: u64,
) -> Result<(), Box<dyn Error>> {
    state.buf.resize(len.try_into().unwrap(), 0);
    stream.read_exact(&mut state.buf)?;
    let mut r = io::Cursor::new(&state.buf);

    match &id {
        b"DISK" => {
            let count = r.read_i16::<LE>()?;
            let mut list = vec![0; count.try_into()?];
            r.read_exact(&mut list)?;
            state.lfl_disks = Some(list);
        }
        b"DLFL" => {
            let count = r.read_i16::<LE>()?;
            let mut list = vec![0; count.try_into()?];
            r.read_i32_into::<LE>(&mut list)?;
            state.lfl_offsets = Some(list);
        }
        b"DIRS" => {
            state.scripts = Some(read_directory(&mut r)?);
        }
        b"DIRN" => {
            state.sounds = Some(read_directory(&mut r)?);
        }
        _ => {
            r.seek(SeekFrom::End(0))?;
        }
    }
    Ok(())
}

fn read_directory(r: &mut impl Read) -> Result<Directory, Box<dyn Error>> {
    let count = r.read_i16::<LE>()?;
    let mut dir = Directory {
        room_numbers: vec![0; count.try_into()?],
        offsets: vec![0; count.try_into()?],
        glob_sizes: vec![0; count.try_into()?],
    };
    r.read_exact(&mut dir.room_numbers)?;
    r.read_i32_into::<LE>(&mut dir.offsets)?;
    r.read_i32_into::<LE>(&mut dir.glob_sizes)?;
    Ok(dir)
}

pub fn extract(
    index: &Index,
    disk_number: u8,
    config: &Config,
    s: &mut (impl Read + Seek),
    write: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
    let s = XorStream::new(s, NICE);
    let mut s = BufReader::new(s);

    let len = s.seek(SeekFrom::End(0))?;
    s.rewind()?;

    let mut state = ExtractState {
        disk_number,
        index,
        config,
        write,
        path: {
            let mut path = String::with_capacity(64);
            path.push('.');
            path
        },
        current_room: 0,
        block_numbers: HashMap::new(),
        map: String::with_capacity(1 << 10),
        buf: Vec::with_capacity(64 << 10),
    };

    scan_blocks(&mut s, &mut state, handle_extract_block, len)?;

    (state.write)(&format!("{}/.map", state.path), state.map.as_bytes())?;
    Ok(())
}

struct ExtractState<'a> {
    disk_number: u8,
    index: &'a Index,
    config: &'a Config,
    write: &'a mut dyn FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    path: String,
    current_room: i32,
    block_numbers: HashMap<[u8; 4], i32>,
    map: String,
    buf: Vec<u8>,
}

fn handle_extract_block<R: Read + Seek>(
    r: &mut R,
    state: &mut ExtractState,
    id: [u8; 4],
    len: u64,
) -> Result<(), Box<dyn Error>> {
    let offset = r.stream_position()?;

    if guess_is_block_recursive(r, len)? {
        extract_recursive(r, state, id, offset, len)?;
    } else {
        extract_flat(r, state, id, len, offset)?;
    }
    Ok(())
}

fn extract_recursive<R: Read + Seek>(
    r: &mut R,
    state: &mut ExtractState,
    id: [u8; 4],
    offset: u64,
    len: u64,
) -> Result<(), Box<dyn Error>> {
    let number = match &id {
        b"LFLF" => {
            state.current_room = find_lfl_number(state.disk_number, offset, state.index)
                .ok_or("LFL not in index")?;
            state.current_room
        }
        b"TALK" => {
            find_object_number(
                state.index,
                &state.index.sounds,
                state.disk_number,
                offset - 8,
            )
            .ok_or("TALK not in index")?
        }
        _ => {
            *state
                .block_numbers
                .entry(id)
                .and_modify(|n| *n += 1)
                .or_insert(1)
        }
    };

    writeln!(state.map, "{}", IdAndNum(id, number))?;

    write!(state.path, "/{}", IdAndNum(id, number))?;

    // copy most fields, temporarily move some
    let mut inner = ExtractState {
        disk_number: state.disk_number,
        index: state.index,
        config: state.config,
        write: state.write,
        path: mem::take(&mut state.path),
        current_room: state.current_room,
        block_numbers: HashMap::new(),
        map: String::with_capacity(1 << 10),
        buf: mem::take(&mut state.buf),
    };

    scan_blocks(r, &mut inner, handle_extract_block, len)?;

    // return temporarily moved fields
    state.buf = mem::take(&mut inner.buf);
    state.path = mem::take(&mut inner.path);

    let map = inner.map;
    (state.write)(&format!("{}/.map", state.path), map.as_bytes())?;

    state.path.truncate(state.path.rfind('/').unwrap());
    Ok(())
}

fn extract_flat<R: Read + Seek>(
    r: &mut R,
    state: &mut ExtractState,
    id: [u8; 4],
    len: u64,
    offset: u64,
) -> Result<(), Box<dyn Error>> {
    state.buf.clear();
    state.buf.reserve(len.try_into()?);
    io::copy(&mut r.take(len), &mut state.buf)?;

    let number = match &id {
        // SCRP number comes from index
        b"SCRP" => {
            find_object_number(
                state.index,
                &state.index.scripts,
                state.disk_number,
                offset - 8,
            )
            .ok_or("script missing from index")?
        }
        // LSC2 number comes from block header
        b"LSC2" => {
            let number_bytes = state.buf.get(..4).ok_or("local script missing header")?;
            i32::from_le_bytes(number_bytes.try_into().unwrap())
        }
        // Otherwise use a counter per block type
        _ => {
            *state
                .block_numbers
                .entry(id)
                .and_modify(|n| *n += 1)
                .or_insert(1)
        }
    };

    writeln!(state.map, "{}", IdAndNum(id, number))?;

    let filename = format!("{}/{}.bin", state.path, IdAndNum(id, number));
    (state.write)(&filename, &state.buf)?;

    match &id {
        b"SCRP" | b"LSC2" | b"ENCD" | b"EXCD" => extract_script(state, id, number)?,
        _ => {}
    }
    Ok(())
}

fn extract_script(
    state: &mut ExtractState,
    id: [u8; 4],
    number: i32,
) -> Result<(), Box<dyn Error>> {
    let mut blob = state.buf.as_slice();

    if id == *b"LSC2" {
        // Skip header. Code starts at offset 4.
        blob = &blob[4..];
    }

    let disasm = disasm_to_string(blob);
    let filename = format!("{}/{}.s", state.path, IdAndNum(id, number));
    (state.write)(&filename, disasm.as_bytes())?;

    let decomp = {
        let _span = info_span!("decompile", scrp = number).entered();
        let scope = match &id {
            b"SCRP" => Scope::Global(number),
            b"LSC2" => Scope::RoomLocal(state.current_room, number),
            b"ENCD" => Scope::RoomEnter(state.current_room),
            b"EXCD" => Scope::RoomExit(state.current_room),
            _ => unreachable!(),
        };
        decompile(blob, scope, state.config)
    };
    let filename = format!("{}/{}.scu", state.path, IdAndNum(id, number),);
    (state.write)(&filename, decomp.as_bytes())?;
    Ok(())
}

fn find_lfl_number(disk_number: u8, offset: u64, index: &Index) -> Option<i32> {
    for i in 0..index.lfl_disks.len() {
        if index.lfl_disks[i] == disk_number && Ok(index.lfl_offsets[i]) == offset.try_into() {
            return Some(i.try_into().unwrap());
        }
    }
    None
}

fn find_object_number(index: &Index, dir: &Directory, disk_number: u8, offset: u64) -> Option<i32> {
    let offset: i32 = offset.try_into().ok()?;
    for i in 0..dir.room_numbers.len() {
        let room_number: usize = dir.room_numbers[i].into();
        let dnum = index.lfl_disks[room_number];
        let doff = index.lfl_offsets[room_number] + dir.offsets[i];
        if dnum == disk_number && doff == offset {
            return Some(i.try_into().unwrap());
        }
    }
    None
}

struct IdAndNum([u8; 4], i32);

impl fmt::Display for IdAndNum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(str::from_utf8(&self.0).unwrap())?;
        f.write_char('_')?;
        write!(f, "{:02}", self.1)?;
        Ok(())
    }
}

type BlockHandler<Stream, State> =
    fn(&mut Stream, &mut State, [u8; 4], u64) -> Result<(), Box<dyn Error>>;

fn scan_blocks<Stream: Read + Seek, State>(
    s: &mut Stream,
    state: &mut State,
    handle_block: BlockHandler<Stream, State>,
    parent_len: u64,
) -> Result<(), Box<dyn Error>> {
    let start = s.stream_position()?;
    loop {
        let pos = s.stream_position()?;
        if pos == start + parent_len {
            break;
        }
        if pos > start + parent_len {
            return Err("misaligned block end".into());
        }

        read_block(s, |s, id, len| handle_block(s, state, id, len))?;
    }
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
fn guess_is_block_recursive<S: Read + Seek>(s: &mut S, len: u64) -> Result<bool, Box<dyn Error>> {
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
