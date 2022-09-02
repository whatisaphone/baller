use std::io::{Read, Seek, SeekFrom};

pub struct XorStream<S> {
    inner: S,
    key: u8,
}

impl<S> XorStream<S> {
    pub fn new(inner: S, key: u8) -> Self {
        Self { inner, key }
    }
}

impl<S: Read> Read for XorStream<S> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let bytes = self.inner.read(buf)?;
        for b in &mut buf[..bytes] {
            *b ^= self.key;
        }
        Ok(bytes)
    }
}

impl<S: Seek> Seek for XorStream<S> {
    fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        self.inner.seek(pos)
    }
}
