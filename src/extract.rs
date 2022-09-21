use crate::{
    config::Config,
    script::{decompile, disasm_to_string, Scope},
    xor::XorStream,
};
use byteordered::byteorder::{ReadBytesExt, BE, LE};
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    fmt::Write,
    io,
    io::{BufReader, Read, Seek, SeekFrom},
    str,
};
use tracing::info_span;

pub const NICE: u8 = 0x69;

pub struct Index {
    pub lfl_disks: Vec<u8>,
    pub lfl_offsets: Vec<i32>,
    pub scripts: Directory,
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

    let mut path = String::with_capacity(64);
    path.push('.');
    let mut state = State {
        path,
        blocks: HashMap::new(),
        tmp_buf: Vec::new(),
    };

    let mut lfl_disks = None;
    let mut lfl_offsets = None;
    let mut scripts = None;

    let handle_block: &mut BlockHandler = &mut |_path, id, _index, _pos, blob| {
        let mut r = io::Cursor::new(blob);
        if id == "DISK" {
            let count = r.read_i16::<LE>()?;
            let mut list = vec![0; count.try_into()?];
            r.read_exact(&mut list)?;
            lfl_disks = Some(list);
        } else if id == "DLFL" {
            let count = r.read_i16::<LE>()?;
            let mut list = vec![0; count.try_into()?];
            r.read_i32_into::<LE>(&mut list)?;
            lfl_offsets = Some(list);
        } else if id == "DIRS" {
            let count = r.read_i16::<LE>()?;
            let mut dir = Directory {
                room_numbers: vec![0; count.try_into()?],
                offsets: vec![0; count.try_into()?],
                glob_sizes: vec![0; count.try_into()?],
            };
            r.read_exact(&mut dir.room_numbers)?;
            r.read_i32_into::<LE>(&mut dir.offsets)?;
            r.read_i32_into::<LE>(&mut dir.glob_sizes)?;
            scripts = Some(dir);
        } else {
            r.seek(SeekFrom::End(0))?;
        }
        if r.stream_position()? != blob.len().try_into()? {
            return Err("wrong block size".into());
        }
        Ok(())
    };

    let handle_map: &mut MapHandler = &mut |_, _| Ok(());

    scan_blocks(&mut s, &mut state, handle_block, handle_map, len)?;
    Ok(Index {
        lfl_disks: lfl_disks.ok_or("index incomplete")?,
        lfl_offsets: lfl_offsets.ok_or("index incomplete")?,
        scripts: scripts.ok_or("index incomplete")?,
    })
}

pub fn extract(
    dirs: &Index,
    disk_number: u8,
    config: &Config,
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

    let write = RefCell::new(write);

    let handle_block: &mut BlockHandler = &mut |path, id, index, pos, mut blob| {
        match id {
            // SCRP number comes from index
            "SCRP" => {
                *index = find_index(dirs, &dirs.scripts, disk_number, pos)
                    .ok_or("script missing from index")?;
            }
            // LSC2 number comes from block header
            "LSC2" => {
                let index_bytes = blob.get(..4).ok_or("local script missing header")?;
                *index = i32::from_le_bytes(index_bytes.try_into().unwrap());
            }
            // Otherwise the number is a counter per block type (passed by caller)
            _ => {}
        }

        let filename = format!("{path}/{id}_{index:02}.bin");
        write.borrow_mut()(&filename, blob)?;

        if id == "SCRP" || id == "ENCD" || id == "EXCD" || id == "LSC2" {
            if id == "LSC2" {
                // Skip header. Code starts at offset 4.
                blob = &blob[4..];
            }

            let disasm = disasm_to_string(blob);
            let filename = format!("{path}/{id}_{index:02}.s");
            write.borrow_mut()(&filename, disasm.as_bytes())?;

            let scope = match id {
                "SCRP" => Scope::Global(*index),
                "LSC2" | "ENCD" | "EXCD" => {
                    // HACK: get room from path
                    // TODO: this is not accurate. get real room number from directory
                    debug_assert!(&path[0..15] == "./LECF_01/LFLF_");
                    let room: i32 = path[15..17].parse().unwrap();
                    debug_assert!(&path[17..18] == "/");
                    match id {
                        "LSC2" => Scope::RoomLocal(room, *index),
                        "ENCD" => Scope::RoomEnter(room),
                        "EXCD" => Scope::RoomExit(room),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };

            let decomp = {
                let _span = info_span!("decompile", scrp = *index).entered();
                decompile(blob, scope, config)
            };
            let filename = format!("{path}/{id}_{index:02}.scu");
            write.borrow_mut()(&filename, decomp.as_bytes())?;
        }
        Ok(())
    };

    let handle_map: &mut MapHandler = &mut |path, blob| {
        write.borrow_mut()(path, blob)?;
        Ok(())
    };

    scan_blocks(&mut s, &mut state, handle_block, handle_map, len)?;
    Ok(())
}

struct State {
    path: String,
    blocks: HashMap<[u8; 4], i32>,
    tmp_buf: Vec<u8>,
}

fn find_index(index: &Index, dir: &Directory, disk_number: u8, offset: u64) -> Option<i32> {
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

type BlockHandler<'a> =
    dyn FnMut(&str, &str, &mut i32, u64, &[u8]) -> Result<(), Box<dyn Error>> + 'a;
type MapHandler<'a> = dyn FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>> + 'a;

fn scan_blocks<S: Read + Seek>(
    s: &mut S,
    state: &mut State,
    handle_block: &mut BlockHandler,
    handle_map: &mut MapHandler,
    parent_len: u64,
) -> Result<(), Box<dyn Error>> {
    let mut map = String::with_capacity(1 << 10);
    let start = s.stream_position()?;
    loop {
        let pos = s.stream_position()?;
        if pos >= start + parent_len {
            break;
        }

        read_block(s, |s, id, len| {
            let next_index = state.blocks.entry(id).or_insert(1);
            let mut index = *next_index;
            *next_index += 1;

            let id = str::from_utf8(&id)?;

            if is_block_recursive(s, len)? {
                write!(state.path, "/{id}_{index:02}").unwrap();

                scan_blocks(s, state, handle_block, handle_map, len)?;

                state.path.truncate(state.path.rfind('/').unwrap_or(0));
            } else {
                state.tmp_buf.clear();
                state.tmp_buf.reserve(len.try_into()?);
                io::copy(&mut s.take(len), &mut state.tmp_buf)?;
                let blob = &state.tmp_buf[..len.try_into()?];

                handle_block(&state.path, id, &mut index, pos, blob)?;
            }

            // The block handler might have modified the index.
            writeln!(map, "{id}_{index:02}").unwrap();
            Ok(())
        })?;
    }
    handle_map(&format!("{path}/.map", path = state.path), map.as_bytes())?;
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
