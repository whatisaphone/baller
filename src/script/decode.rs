use crate::{
    script::{
        cursor::{read_i16, read_i32, read_string, read_u8, read_var},
        ins::{Ins, ItemSize, Operand},
    },
    utils::subslice_offset,
};
use std::fmt::Write;

pub fn disasm_to_string(code: &[u8]) -> String {
    let mut output = String::new();
    let mut decoder = Decoder::new(code);
    while let Some((pos, ins)) = decoder.next() {
        writeln!(output, "0x{:04x}  {}", pos, ins).unwrap();
    }
    // Anything left was not decoded; dump the raw bytes
    for pos in decoder.pos..decoder.code.len() {
        writeln!(output, "{:04x}  .db 0x{:02x}", pos, decoder.code[pos]).unwrap();
    }
    output
}

struct Decoder<'a> {
    code: &'a [u8],
    pos: usize,
}

impl<'a> Decoder<'a> {
    fn new(code: &'a [u8]) -> Self {
        Self { code, pos: 0 }
    }

    fn next(&mut self) -> Option<(usize, Ins)> {
        let pos = self.pos;
        let code = &self.code[pos..];
        let mut cur = code;
        let ins = decode_ins(&mut cur)?;
        self.pos += subslice_offset(code, cur);
        Some((pos, ins))
    }
}

fn decode_ins<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let opcode = read_u8(code)?;
    match opcode {
        0x00 => op_00_push_byte(code),
        0x01 => op_01_push_i16(code),
        0x02 => op_02_push_i32(code),
        0x03 => op_03_push_var(code),
        0x04 => op_04_push_str(code),
        0x07 => op_07_get_array_item(code),
        0x0c => Some(Ins::StackDup),
        0x0d => Some(Ins::Not),
        0x0e => Some(Ins::Equal),
        0x0f => Some(Ins::NotEqual),
        0x10 => Some(Ins::Greater),
        0x11 => Some(Ins::Less),
        0x12 => Some(Ins::LessOrEqual),
        0x14 => Some(Ins::Add),
        0x15 => Some(Ins::Sub),
        0x19 => Some(Ins::LogicalOr),
        0x1a => Some(Ins::PopDiscard),
        0x1b => Some(Ins::Undecoded1([0x1b])),
        0x26 => op_26_sprite(code),
        0x37 => op_37_dim_array(code),
        0x43 => op_43_set(code),
        0x47 => op_47_set_array_item(code),
        0x4f => op_4f_inc(code),
        0x5d => op_5d_jump_unless(code),
        0x6b => op_6b_cursor(code),
        0x73 => op_73_jump(code),
        0x7b => Some(Ins::Undecoded1([0x7b])),
        0x87 => Some(Ins::Random),
        0x9b => op_9b(code),
        0x9c => op_9c(code),
        0xa4 => op_a4_array(code),
        0xbc => op_bc_array(code),
        0xd0 => Some(Ins::Now),
        0xf3 => op_f3(code),
        0xfa => op_fa_window_title(code),
        _ => None,
    }
}

fn op_00_push_byte<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Push(Operand::Byte(read_u8(code)?)))
}

fn op_01_push_i16<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Push(Operand::I16(read_i16(code)?)))
}

fn op_02_push_i32<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Push(Operand::I32(read_i32(code)?)))
}

fn op_03_push_var<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Push(Operand::Var(read_var(code)?)))
}

fn op_04_push_str<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Push(Operand::String(read_string(code)?)))
}

fn op_07_get_array_item<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::GetArrayItem(read_var(code)?))
}

fn op_26_sprite<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Undecoded2([0x26, read_u8(code)?]))
}

fn op_37_dim_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::DimArray(read_item_size(code)?, read_var(code)?))
}

fn read_item_size(code: &mut &[u8]) -> Option<ItemSize> {
    match read_u8(code)? {
        4 => Some(ItemSize::Byte),
        5 => Some(ItemSize::I16),
        _ => None,
    }
}

fn op_43_set<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Set(read_var(code)?))
}

fn op_47_set_array_item<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::SetArrayItem(read_var(code)?))
}

fn op_4f_inc<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Inc(read_var(code)?))
}

fn op_5d_jump_unless<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::JumpUnless(read_i16(code)?))
}

fn op_6b_cursor<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Undecoded2([0x6b, read_u8(code)?]))
}

fn op_73_jump<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Jump(read_i16(code)?))
}

fn op_9b<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Undecoded2([0x9b, read_u8(code)?]))
}

fn op_9c<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Undecoded2([0x9c, read_u8(code)?]))
}

fn op_a4_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x07 => Some(Ins::AssignString(read_var(code)?)),
        _ => None,
    }
}

fn op_bc_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xcc => Some(Ins::FreeArray(read_var(code)?)),
        _ => None,
    }
}

fn op_f3<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Undecoded2([0xf3, read_u8(code)?]))
}

fn op_fa_window_title<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xf3 => Some(Ins::SetWindowTitle),
        _ => None,
    }
}
