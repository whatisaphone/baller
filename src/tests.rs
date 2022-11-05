use crate::{blocks::BLOCK_HEADER_SIZE, extract_old::NICE, read_index};
use std::{
    error::Error,
    fs::File,
    io::{Read, Seek, SeekFrom},
    path::PathBuf,
};

pub fn read_scrp(number: usize) -> Result<Vec<u8>, Box<dyn Error>> {
    let index = read_index(&mut File::open(&fixture_path("baseball 2001.he0"))?)?;

    let room: usize = index.scripts.room_numbers[number].into();
    let mut offset: u64 = (index.lfl_offsets[room] + index.scripts.offsets[number]).try_into()?;
    let mut len: usize = index.scripts.sizes[number].try_into()?;

    // skip block header
    offset += BLOCK_HEADER_SIZE;
    len -= usize::try_from(BLOCK_HEADER_SIZE).unwrap();

    assert!(index.lfl_disks[room] == 2);
    let mut disk = File::open(&fixture_path("baseball 2001.(b)"))?;
    disk.seek(SeekFrom::Start(offset))?;
    let mut result = vec![0; len];
    disk.read_exact(&mut result)?;
    for b in &mut result {
        *b ^= NICE;
    }
    Ok(result)
}

fn fixture_path(name: &str) -> PathBuf {
    let mut result = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    result.push("tests");
    result.push("fixtures");
    result.push("baseball2001");
    result.push(name);
    result
}
