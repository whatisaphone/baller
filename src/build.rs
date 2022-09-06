use crate::{extract::NICE, xor::XorWriteStream};
use byteordered::byteorder::{WriteBytesExt, BE};
use std::{
    error::Error,
    io::{BufWriter, Seek, SeekFrom, Write},
    str,
};

pub enum FsEntry {
    Dir(Vec<String>),
    File(Vec<u8>),
}

struct State {
    src: String,
    pending_lengths: Vec<(u64, i32)>,
}

pub fn build<S: Write + Seek>(
    out: &mut S,
    read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), Box<dyn Error>> {
    let out = BufWriter::new(out);
    let mut out = XorWriteStream::new(out, NICE);

    let mut src = String::with_capacity(64);
    src.push('.');
    let mut state = State {
        src,
        pending_lengths: Vec::with_capacity(1 << 10),
    };
    let dir = match read(&state.src)? {
        FsEntry::Dir(names) => names,
        FsEntry::File(_) => panic!(),
    };
    slurp_dir(&mut out, read, &mut state, &dir)?;

    for &(offset, value) in &state.pending_lengths {
        out.seek(SeekFrom::Start(offset))?;
        out.write_i32::<BE>(value)?;
    }
    out.flush()?;
    Ok(())
}

fn slurp_dir<S: Write + Seek>(
    out: &mut S,
    read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
    state: &mut State,
    names: &[String],
) -> Result<(), Box<dyn Error>> {
    let map = match read(&format!("{}/.map", state.src)) {
        Ok(FsEntry::File(data)) => data,
        _ => return Err("missing map".into()),
    };
    for file in String::from_utf8(map)?.lines() {
        let file = names
            .iter()
            .find(|n| n.starts_with(file) && matches!(&n.as_bytes()[file.len()..], b"" | b".bin"))
            .ok_or("missing block")?;

        let id = parse_filename(file.as_bytes()).ok_or("bad block name")?;

        let entry = read(&format!("{}/{}", state.src, file))?;
        match entry {
            FsEntry::Dir(names) => {
                state.src.push('/');
                state.src.push_str(file);

                write_block(out, state, id, |out, state| {
                    slurp_dir(out, read, state, &names)?;
                    Ok(())
                })?;

                state.src.truncate(state.src.rfind('/').unwrap());
            }
            FsEntry::File(data) => {
                write_block(out, state, id, |out, _state| {
                    out.write_all(&data)?;
                    Ok(())
                })?;
            }
        }
    }
    Ok(())
}

// Parse filenames of the form "BLOK_123"
fn parse_filename(name: &[u8]) -> Option<[u8; 4]> {
    if name.len() < 5 || name[4] != b'_' {
        return None;
    }
    Some(name[..4].try_into().unwrap())
}

fn write_block<S: Write + Seek>(
    out: &mut S,
    state: &mut State,
    id: [u8; 4],
    f: impl FnOnce(&mut S, &mut State) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
    let start = out.stream_position()?;
    out.write_all(&id)?;
    out.write_i32::<BE>(0)?; // placeholder
    f(out, state)?;
    let end = out.stream_position()?;
    let length: i32 = (end - start).try_into().unwrap();
    state.pending_lengths.push((start + 4, length));
    Ok(())
}
