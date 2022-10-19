use std::error::Error;

pub fn grow_with_default<T: Default>(xs: &mut Vec<T>, new_len: usize) {
    if xs.len() < new_len {
        xs.resize_with(new_len, <_>::default);
    }
}

pub fn extend_insert_some<T>(
    xs: &mut Vec<Option<T>>,
    index: usize,
    element: T,
) -> Result<(), Box<dyn Error>> {
    grow_with_default(xs, index + 1);

    if xs[index].is_some() {
        return Err("duplicate index".into());
    }
    xs[index] = Some(element);
    Ok(())
}
