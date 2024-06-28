use crate::blocks::BlockScanner;
use std::{
    error::Error,
    io::{Cursor, Seek, SeekFrom},
};

pub fn decode(pals_raw: &[u8]) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut r = Cursor::new(pals_raw);
    let mut pals_scanner = BlockScanner::new(pals_raw.len().try_into().unwrap());
    let wrap_len = pals_scanner
        .next_block_must_be(&mut r, *b"WRAP")?
        .ok_or("expected WRAP")?;

    let mut wrap_scanner = BlockScanner::new(r.position() + wrap_len);
    let offs_len = wrap_scanner
        .next_block_must_be(&mut r, *b"OFFS")?
        .ok_or("expected OFFS")?;
    r.seek(SeekFrom::Current(offs_len.try_into().unwrap()))?;

    let apal_len = wrap_scanner
        .next_block_must_be(&mut r, *b"APAL")?
        .ok_or("expected APAL")?;

    if apal_len != 0x300 {
        return Err("bad APAL length".into());
    }
    let offset: usize = r.position().try_into().unwrap();
    Ok(pals_raw[offset..offset + 0x300].to_vec())
}
