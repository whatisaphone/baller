use std::{
    cmp::min,
    io::{Read, Seek, SeekFrom, Write},
};

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

const BUF_SIZE: usize = 8 << 10;

pub struct XorWriteStream<S> {
    inner: S,
    key: u8,
    buffer: Box<[u8; BUF_SIZE]>,
}

impl<S> XorWriteStream<S> {
    pub fn new(inner: S, key: u8) -> Self {
        Self {
            inner,
            key,
            buffer: Box::new([0; BUF_SIZE]),
        }
    }
}

impl<S: Write> Write for XorWriteStream<S> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let len = min(buf.len(), self.buffer.len());
        #[allow(clippy::needless_range_loop)]
        for i in 0..len {
            self.buffer[i] = buf[i] ^ self.key;
        }
        self.inner.write(&self.buffer[..len])
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

impl<S: Seek> Seek for XorWriteStream<S> {
    fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        self.inner.seek(pos)
    }
}
