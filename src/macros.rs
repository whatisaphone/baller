macro_rules! bytearray {
    ($($e:expr),* $(,)?) => {{
        use crate::utils::byte_array::ByteArray;
        let mut a = ByteArray::new();
        $(a.push($e);)*
        a
    }};
}
