use crate::{script::disasm_to_string, xor::XorStream};
use byteordered::byteorder::{ReadBytesExt, BE};
use clap::Parser;
use std::{
    collections::HashMap,
    error::Error,
    fs,
    fs::{create_dir, File},
    io,
    io::{BufReader, Read, Seek, SeekFrom},
    path::PathBuf,
    str,
};

const NICE: u8 = 0x69;

#[derive(Parser)]
pub struct Extract {
    input: PathBuf,
    #[clap(short)]
    output: PathBuf,
}

impl Extract {
    pub fn run(self) -> Result<(), Box<dyn Error>> {
        if !self.output.exists() {
            create_dir(&self.output)?;
        }

        let s = File::open(&self.input)?;
        let s = XorStream::new(s, NICE);
        let mut s = BufReader::new(s);

        let len = s.seek(SeekFrom::End(0))?;
        s.rewind()?;

        let mut state = State {
            dest: self.output,
            blocks: HashMap::new(),
        };

        while s.stream_position()? < len {
            extract_block(&mut s, &mut state)?;
        }
        Ok(())
    }
}

struct State {
    dest: PathBuf,
    blocks: HashMap<[u8; 4], i32>,
}

fn extract_block<S: Read + Seek>(s: &mut S, state: &mut State) -> Result<(), Box<dyn Error>> {
    read_block(s, |s, id, len| {
        let next_index = state.blocks.entry(id).or_insert(0);
        let index = *next_index;
        *next_index += 1;

        let id = str::from_utf8(&id)?;

        if id == "SCRP" {
            let mut blob = vec![0; len.try_into()?];
            s.read_exact(&mut blob)?;
            let script = disasm_to_string(&blob);

            let filename = format!("{id}_{index:02}.s");
            fs::write(state.dest.join(filename), &script)?;
        } else if is_block_recursive(s, len)? {
            let dirname = format!("{id}_{index:02}");
            state.dest.push(dirname);
            if !state.dest.exists() {
                create_dir(&state.dest)?;
            }

            let start = s.stream_position()?;

            while s.stream_position()? < start + len {
                extract_block(s, state)?;
            }

            state.dest.pop();
        } else {
            let filename = format!("{id}_{index:02}.bin");
            let mut f = File::create(state.dest.join(filename))?;
            io::copy(&mut s.take(len), &mut f)?;
        }
        Ok(())
    })
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
