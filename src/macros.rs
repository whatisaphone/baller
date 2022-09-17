macro_rules! arrayvec {
    ($($x:expr),* $(,)?) => {{
        #[allow(unused_mut)]
        let mut result = ::arrayvec::ArrayVec::new();
        $(result.push($x);)*
        result
    }};
}

macro_rules! bytearray {
    ($($e:expr),* $(,)?) => {{
        use crate::utils::byte_array::ByteArray;
        let mut a = ByteArray::new();
        $(a.push($e);)*
        a
    }};
}
