use byteordered::byteorder::{ReadBytesExt, WriteBytesExt, BE};
use std::{
    error::Error,
    io,
    io::{Read, Seek, SeekFrom, Write},
};

pub type BlockId = [u8; 4];

pub struct BlockScanner {
    parent_end: u64,
    cur_block_end: Option<u64>,
}

impl BlockScanner {
    pub fn new(parent_end: u64) -> Self {
        Self {
            parent_end,
            cur_block_end: None,
        }
    }

    pub fn next_block(
        &mut self,
        s: &mut (impl Read + Seek),
    ) -> Result<Option<(BlockId, u64)>, Box<dyn Error>> {
        let pos = s.stream_position()?;
        // For all blocks but the first, ensure the correct number of bytes is consumed.
        if let Some(cur_block_end) = self.cur_block_end {
            if pos != cur_block_end {
                return Err("read wrong length".into());
            }
        }
        // Check for end of parent block
        if pos == self.parent_end {
            return Ok(None);
        }
        if pos > self.parent_end {
            return Err("read wrong length".into());
        }

        // Read block header and return
        let mut id = [0; 4];
        s.read_exact(&mut id)?;
        let len = s.read_i32::<BE>()?;
        let len: u64 = len.try_into()?;
        if !(len >= 8 && pos + len <= self.parent_end) {
            return Err("bad block header".into());
        }
        self.cur_block_end = Some(pos + len);
        // The length includes the header, but the caller doesn't care about that.
        let len = len - 8;
        Ok(Some((id, len)))
    }

    pub fn next_block_must_be(
        &mut self,
        s: &mut (impl Read + Seek),
        expected_id: BlockId,
    ) -> Result<Option<u64>, Box<dyn Error>> {
        let (id, len) = match self.next_block(s)? {
            Some((id, len)) => (id, len),
            None => return Ok(None),
        };
        if id != expected_id {
            return Err("unexpected block".into());
        }
        Ok(Some(len))
    }

    pub fn exhausted(&self, s: &mut impl Seek) -> Result<bool, Box<dyn Error>> {
        Ok(s.stream_position()? == self.parent_end)
    }

    pub fn finish(&mut self, s: &mut impl Seek) -> Result<(), Box<dyn Error>> {
        if !self.exhausted(s)? {
            return Err("read wrong length".into());
        }
        Ok(())
    }
}

pub struct WritingBlock(u32);

pub fn start_block(out: &mut (impl Write + Seek), id: BlockId) -> io::Result<WritingBlock> {
    let start = out.stream_position()?;
    out.write_all(&id)?;
    out.write_i32::<BE>(0)?; // placeholder
    Ok(WritingBlock(start.try_into().unwrap()))
}

pub fn finish_block(
    out: &mut impl Seek,
    WritingBlock(start): WritingBlock,
    fixups: &mut Vec<(u32, i32)>,
) -> io::Result<i32> {
    let end = out.stream_position()?;
    let end: u32 = end.try_into().unwrap();
    let length: i32 = (end - start).try_into().unwrap();
    fixups.push((start + 4, length));
    Ok(length)
}

pub fn write_block<S: Write + Seek>(
    out: &mut S,
    id: BlockId,
    fixups: &mut Vec<(u32, i32)>,
    f: impl FnOnce(&mut S, &mut Vec<(u32, i32)>) -> io::Result<()>,
) -> io::Result<()> {
    let block = start_block(out, id)?;
    f(out, fixups)?;
    finish_block(out, block, fixups)?;
    Ok(())
}

pub fn apply_fixups(out: &mut (impl Write + Seek), fixups: &[(u32, i32)]) -> io::Result<()> {
    for &(offset, value) in fixups {
        out.seek(SeekFrom::Start(offset.try_into().unwrap()))?;
        out.write_i32::<BE>(value)?;
    }
    Ok(())
}

pub fn push_disk_number(path: &mut String, number: u8) {
    debug_assert!((1..=26).contains(&number));
    path.push('(');
    path.push((number + b'a' - 1).into());
    path.push(')');
}

pub fn strip_disk_number(path: &mut String) {
    debug_assert!(path.ends_with("he0") || path.ends_with(')'));
    path.truncate(path.len() - 3);
}

pub fn push_index_ext(path: &mut String) {
    path.push_str("he0");
}
