use std::{fmt, iter::Copied, ops::Deref, slice};

#[derive(Clone)]
pub struct ByteArray<const CAPACITY: usize> {
    len: u8,
    data: [u8; CAPACITY],
}

impl<const CAPACITY: usize> Default for ByteArray<CAPACITY> {
    fn default() -> Self {
        Self {
            len: 0,
            data: [0; CAPACITY],
        }
    }
}

impl<const CAPACITY: usize> ByteArray<CAPACITY> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn as_slice(&self) -> &[u8] {
        self
    }

    pub fn push(&mut self, item: u8) {
        self.data[usize::from(self.len)] = item;
        self.len += 1;
    }
}

impl<const CAPACITY: usize> PartialEq for ByteArray<CAPACITY> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<const CAPACITY: usize> Deref for ByteArray<CAPACITY> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.data[..usize::from(self.len)]
    }
}

impl<const CAPACITY: usize> fmt::Debug for ByteArray<CAPACITY> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_slice().fmt(f)
    }
}

impl<'a, const CAPACITY: usize> IntoIterator for &'a ByteArray<CAPACITY> {
    type Item = u8;
    type IntoIter = Copied<slice::Iter<'a, u8>>;

    fn into_iter(self) -> Self::IntoIter {
        self.data[..self.len.into()].iter().copied()
    }
}
