use std::ops::Range;

pub mod byte_array;
pub mod vec;

pub fn subslice_offset(this: &[u8], other: &[u8]) -> usize {
    let Range { start, end } = this.as_ptr_range();
    assert!(start <= other.as_ptr() && other.as_ptr() <= end);
    let result = unsafe { other.as_ptr().offset_from(start) };
    result.try_into().unwrap()
}
