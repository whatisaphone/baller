use crate::script::ins::Variable;

pub fn read_u8(code: &mut &[u8]) -> Option<u8> {
    Some(read_bytes(code, 1)?[0])
}

pub fn read_i16(code: &mut &[u8]) -> Option<i16> {
    let bytes = read_bytes(code, 2)?;
    Some(i16::from_le_bytes(bytes.try_into().unwrap()))
}

pub fn read_u16(code: &mut &[u8]) -> Option<u16> {
    let bytes = read_bytes(code, 2)?;
    Some(u16::from_le_bytes(bytes.try_into().unwrap()))
}

pub fn read_i32(code: &mut &[u8]) -> Option<i32> {
    let bytes = read_bytes(code, 4)?;
    Some(i32::from_le_bytes(bytes.try_into().unwrap()))
}

pub fn read_var(code: &mut &[u8]) -> Option<Variable> {
    Some(Variable(read_u16(code)?))
}

fn read_bytes<'a>(code: &mut &'a [u8], len: usize) -> Option<&'a [u8]> {
    if code.len() < len {
        return None;
    }
    let result = &code[..len];
    *code = &code[len..];
    Some(result)
}

pub fn read_string<'a>(code: &mut &'a [u8]) -> Option<&'a [u8]> {
    let len = code.iter().position(|&b| b == b'\0')?;
    let result = &code[..len];
    *code = &code[len + 1..];
    Some(result)
}
