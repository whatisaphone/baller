use byteordered::byteorder::ReadBytesExt;
use std::io::{self, Read};

pub struct BitStream {
    buf: u16,
    len: u8,
}

impl BitStream {
    pub fn new() -> Self {
        Self { buf: 0, len: 0 }
    }

    pub fn buf_remaining(&self) -> u8 {
        self.len
    }

    fn fill(&mut self, mut r: impl Read) -> Result<(), io::Error> {
        if self.len >= 8 {
            return Ok(());
        }
        let byte = r.read_u8()?;
        self.buf |= u16::from(byte) << self.len;
        self.len += 8;
        Ok(())
    }

    pub fn read_bit(&mut self, r: impl Read) -> Result<bool, io::Error> {
        let bit = self.read_bits(r, 1)?;
        Ok(bit != 0)
    }

    pub fn read_bits(&mut self, r: impl Read, bits: u8) -> Result<u8, io::Error> {
        debug_assert!((1..=8).contains(&bits));
        if self.len < bits {
            self.fill(r)?;
        }
        let mask = (1 << bits) - 1;
        let result: u8 = (self.buf & mask).try_into().unwrap();
        self.buf >>= bits;
        self.len -= bits;
        Ok(result)
    }
}
