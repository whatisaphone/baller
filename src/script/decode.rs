use crate::{
    script::{
        cursor::{read_i16, read_i32, read_string, read_u8, read_var},
        ins::{GenericArg, GenericIns, Ins, ItemSize, Operand},
    },
    utils::subslice_offset,
};
use std::{cell::Cell, fmt::Write};

pub fn disasm_to_string(code: &[u8]) -> String {
    let mut output = String::new();
    let decoder = Decoder::new(code);
    while let Some((pos, ins)) = decoder.next() {
        writeln!(output, "0x{:04x}  {}", pos, ins).unwrap();
    }
    // Anything left was not decoded; dump the raw bytes
    for pos in decoder.pos()..decoder.code.len() {
        writeln!(output, "0x{:04x}  .db 0x{:02x}", pos, decoder.code[pos]).unwrap();
    }
    output
}

pub struct Decoder<'a> {
    code: &'a [u8],
    pub pos: Cell<usize>,
}

impl<'a> Decoder<'a> {
    pub fn new(code: &'a [u8]) -> Self {
        Self {
            code,
            pos: Cell::new(0),
        }
    }

    pub fn next(&self) -> Option<(usize, Ins<'a>)> {
        let pos = self.pos.get();
        let code = &self.code[pos..];
        let mut cur = code;
        let ins = decode_ins(&mut cur)?;
        self.pos.set(pos + subslice_offset(code, cur));
        Some((pos, ins))
    }

    pub fn pos(&self) -> usize {
        self.pos.get()
    }

    pub fn set_pos(&self, value: usize) {
        self.pos.set(value);
    }

    pub fn exhausted(&self) -> bool {
        self.pos.get() == self.code.len()
    }
}

#[allow(clippy::too_many_lines)]
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
        0x13 => Some(Ins::GreaterOrEqual),
        0x14 => Some(Ins::Add),
        0x15 => Some(Ins::Sub),
        0x18 => Some(Ins::LogicalAnd),
        0x19 => Some(Ins::LogicalOr),
        0x1a => Some(Ins::PopDiscard),
        0x1b => {
            Some(Ins::Generic(bytearray![0x1b], &GenericIns {
                name: "x1b",
                args: &[GenericArg::Int, GenericArg::List],
                returns_value: true,
            }))
        }
        0x26 => op_26_sprite(code),
        0x37 => op_37_dim_array(code),
        0x43 => op_43_set(code),
        0x47 => op_47_set_array_item(code),
        0x4f => op_4f_inc(code),
        0x5c => op_5c_jump_if(code),
        0x5d => op_5d_jump_unless(code),
        0x5e => op_5e_start_script(code),
        0x66 => {
            Some(Ins::Generic(bytearray![0x66], &GenericIns {
                name: "free-script",
                args: &[],
                returns_value: false,
            }))
        }
        0x6b => op_6b_cursor(code),
        0x6c => {
            Some(Ins::Generic(bytearray![0x6c], &GenericIns {
                name: "stop-script",
                args: &[],
                returns_value: false,
            }))
        }
        0x73 => op_73_jump(code),
        0x74 => op_74(code),
        0x75 => {
            Some(Ins::Generic(bytearray![0x75], &GenericIns {
                name: "x75",
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0x7b => {
            Some(Ins::Generic(bytearray![0x7b], &GenericIns {
                name: "x7b",
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0x7c => {
            Some(Ins::Generic(bytearray![0x7c], &GenericIns {
                name: "x7c",
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0x87 => {
            Some(Ins::Generic(bytearray![0x87], &GenericIns {
                name: "random",
                args: &[GenericArg::Int],
                returns_value: true,
            }))
        }
        0x88 => {
            Some(Ins::Generic(bytearray![0x88], &GenericIns {
                name: "random2",
                args: &[GenericArg::Int, GenericArg::Int],
                returns_value: true,
            }))
        }
        0x98 => {
            Some(Ins::Generic(bytearray![0x98], &GenericIns {
                name: "x98",
                args: &[GenericArg::Int],
                returns_value: true,
            }))
        }
        0x9b => op_9b(code),
        0x9c => op_9c(code),
        0xa4 => op_a4_array(code),
        0xb6 => op_b6(code),
        0xa9 => op_a9(code),
        0xb7 => op_b7(code),
        0xbc => op_bc_array(code),
        0xca => {
            Some(Ins::Generic(bytearray![0xca], &GenericIns {
                name: "xca",
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0xd0 => {
            Some(Ins::Generic(bytearray![0xd0], &GenericIns {
                name: "now",
                args: &[],
                returns_value: false,
            }))
        }
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
    Some(Ins::PushString(read_string(code)?))
}

fn op_07_get_array_item<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::GetArrayItem(read_var(code)?))
}

fn op_26_sprite<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x39 => {
            Some(Ins::Generic(bytearray![0x26, 0x39], &GenericIns {
                name: "x26-x39",
                args: &[GenericArg::Int, GenericArg::Int],
                returns_value: false,
            }))
        }
        0x7d => {
            Some(Ins::Generic(bytearray![0x26, 0x7d], &GenericIns {
                name: "x26-x7d",
                args: &[GenericArg::List],
                returns_value: false,
            }))
        }
        0x9e => {
            Some(Ins::Generic(bytearray![0x26, 0x9e], &GenericIns {
                name: "x26-x9e",
                args: &[],
                returns_value: false,
            }))
        }
        op => Some(Ins::Undecoded2([0x26, op])),
    }
}

fn op_37_dim_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let item_size = to_item_size(read_u8(code)?)?;
    let var = read_var(code)?;
    Some(Ins::DimArray(item_size, var))
}

fn to_item_size(n: u8) -> Option<ItemSize> {
    match n {
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

fn op_5c_jump_if<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::JumpIf(read_i16(code)?))
}

fn op_5d_jump_unless<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::JumpUnless(read_i16(code)?))
}

fn op_5e_start_script<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let sub = read_u8(code)?;
    Some(Ins::Generic(bytearray![0x5e, sub], &GenericIns {
        name: "start-script",
        args: &[GenericArg::Int, GenericArg::List],
        returns_value: false,
    }))
}

fn op_6b_cursor<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        b @ (0x91 | 0x93) => Some(Ins::Generic2Simple([0x6b, b])),
        0x9c => Some(Ins::CursorCharset),
        b => Some(Ins::Undecoded2([0x6b, b])),
    }
}

fn op_73_jump<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Jump(read_i16(code)?))
}

fn op_74<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xe6 => {
            Some(Ins::Generic(bytearray![0x74, 0xe6], &GenericIns {
                name: "x74-xe6",
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0xe8 => {
            Some(Ins::Generic(bytearray![0x74, 0xe8], &GenericIns {
                name: "x74-xe8",
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0xff => {
            Some(Ins::Generic(bytearray![0x74, 0xff], &GenericIns {
                name: "x74-xff",
                args: &[],
                returns_value: false,
            }))
        }
        _ => None,
    }
}

fn op_9b<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x64 => Some(Ins::LoadScript),
        0x6c => Some(Ins::LockScript),
        0x75 => Some(Ins::LoadCharset),
        _ => None,
    }
}

fn op_9c<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xb5 => {
            Some(Ins::Generic(bytearray![0x9c, 0xb5], &GenericIns {
                name: "x9c-xb5",
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        _ => None,
    }
}

fn op_a4_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x07 => Some(Ins::AssignString(read_var(code)?)),
        0xc2 => Some(Ins::Sprintf(read_var(code)?)),
        _ => None,
    }
}

fn op_a9<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xa9 => {
            Some(Ins::Generic(bytearray![0xa9, 0xa9], &GenericIns {
                name: "xa9-xa9",
                args: &[],
                returns_value: false,
            }))
        }
        _ => None,
    }
}

fn op_b6<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    op_b4_thru_b9(0xb6, code)
}

fn op_b7<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    op_b4_thru_b9(0xb7, code)
}

fn op_b4_thru_b9<'a>(opcode: u8, code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x4b => Some(Ins::SomethingWithString([opcode, 0x4b], read_string(code)?)),
        0xfe => {
            Some(Ins::Generic(bytearray![opcode, 0xfe], &GenericIns {
                name: "xb?-xfe",
                args: &[], // NOTE: this pops an int for opcode b8 and b9
                returns_value: false,
            }))
        }
        _ => None,
    }
}

fn op_bc_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let n = read_u8(code)?;
    if let Some(item_size) = to_item_size(n) {
        return Some(Ins::DimArray1D(item_size, read_var(code)?));
    }
    match n {
        0xcc => Some(Ins::FreeArray(read_var(code)?)),
        _ => None,
    }
}

fn op_f3<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x06 => {
            Some(Ins::Generic(bytearray![0xf3, 0x06], &GenericIns {
                name: "read-ini-int",
                args: &[GenericArg::String],
                returns_value: true,
            }))
        }
        0x07 => {
            Some(Ins::Generic(bytearray![0xf3, 0x07], &GenericIns {
                name: "read-ini-string",
                args: &[GenericArg::String],
                returns_value: true,
            }))
        }
        _ => None,
    }
}

fn op_fa_window_title<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xf3 => Some(Ins::SetWindowTitle),
        _ => None,
    }
}
