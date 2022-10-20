use std::{error::Error, fmt};

pub fn grow_with_default<T: Default>(xs: &mut Vec<T>, new_len: usize) {
    if xs.len() < new_len {
        xs.resize_with(new_len, <_>::default);
    }
}

pub fn extend_insert_some<T>(
    xs: &mut Vec<Option<T>>,
    index: usize,
    element: T,
) -> Result<(), DuplicateError> {
    grow_with_default(xs, index + 1);

    if xs[index].is_some() {
        return Err(DuplicateError);
    }
    xs[index] = Some(element);
    Ok(())
}

#[derive(Debug)]
pub struct DuplicateError;

impl fmt::Display for DuplicateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Error for DuplicateError {}
