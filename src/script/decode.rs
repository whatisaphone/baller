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
        0x0b => op_0b_get_array_item_2d(code),
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
        0x16 => Some(Ins::Mul),
        0x17 => Some(Ins::Div),
        0x18 => Some(Ins::LogicalAnd),
        0x19 => Some(Ins::LogicalOr),
        0x1a => Some(Ins::PopDiscard),
        0x1b => {
            Some(Ins::Generic(bytearray![0x1b], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int, GenericArg::List],
                returns_value: true,
            }))
        }
        0x25 => op_25_sprite_retval(code),
        0x26 => op_26_sprite(code),
        0x37 => op_37_dim_array(code),
        0x43 => op_43_set(code),
        0x47 => op_47_set_array_item(code),
        0x4f => op_4f_inc(code),
        0x5a => {
            Some(Ins::Generic(bytearray![0x5a], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int],
                returns_value: true,
            }))
        }
        0x5c => op_5c_jump_if(code),
        0x5d => op_5d_jump_unless(code),
        0x5e => op_5e_start_script(code),
        0x66 => {
            Some(Ins::Generic(bytearray![0x66], arrayvec![], &GenericIns {
                name: Some("free-script"),
                args: &[],
                returns_value: false,
            }))
        }
        0x6b => op_6b_cursor(code),
        0x6c => {
            Some(Ins::Generic(bytearray![0x6c], arrayvec![], &GenericIns {
                name: Some("stop-script"),
                args: &[],
                returns_value: false,
            }))
        }
        0x73 => op_73_jump(code),
        0x74 => op_74(code),
        0x75 => {
            Some(Ins::Generic(bytearray![0x75], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0x7b => {
            Some(Ins::Generic(bytearray![0x7b], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0x7c => {
            Some(Ins::Generic(bytearray![0x7c], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0x87 => {
            Some(Ins::Generic(bytearray![0x87], arrayvec![], &GenericIns {
                name: Some("random"),
                args: &[GenericArg::Int],
                returns_value: true,
            }))
        }
        0x88 => {
            Some(Ins::Generic(bytearray![0x88], arrayvec![], &GenericIns {
                name: Some("random2"),
                args: &[GenericArg::Int, GenericArg::Int],
                returns_value: true,
            }))
        }
        0x98 => {
            Some(Ins::Generic(bytearray![0x98], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int],
                returns_value: true,
            }))
        }
        0x9b => op_9b(code),
        0x9c => op_9c(code),
        0x9f => {
            Some(Ins::Generic(bytearray![0x9f], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int, GenericArg::Int],
                returns_value: true,
            }))
        }
        0xa4 => op_a4_array(code),
        0xb6 => op_b6(code),
        0xa9 => op_a9(code),
        0xb7 => op_b7(code),
        0xbc => op_bc_array(code),
        0xc1 => {
            Some(Ins::Generic(bytearray![0xc1], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int, GenericArg::String],
                returns_value: false,
            }))
        }
        0xc4 => {
            Some(Ins::Generic(bytearray![0xc4], arrayvec![], &GenericIns {
                name: Some("abs"),
                args: &[GenericArg::Int],
                returns_value: true,
            }))
        }
        0xc8 => {
            Some(Ins::Generic(bytearray![0xc8], arrayvec![], &GenericIns {
                name: Some("kludge-retval"),
                args: &[GenericArg::List],
                returns_value: true,
            }))
        }
        0xc9 => {
            Some(Ins::Generic(bytearray![0xc8], arrayvec![], &GenericIns {
                name: Some("kludge"),
                args: &[GenericArg::List],
                returns_value: false,
            }))
        }
        0xca => {
            Some(Ins::Generic(bytearray![0xca], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0xd0 => {
            Some(Ins::Generic(bytearray![0xd0], arrayvec![], &GenericIns {
                name: Some("now"),
                args: &[],
                returns_value: false,
            }))
        }
        0xd4 => {
            let var = read_var(code)?;
            Some(Ins::Generic(
                bytearray![0xd4],
                arrayvec![Operand::Var(var)],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::Int, GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        0xd7 => Some(Ins::BitwiseOr),
        0xd9 => {
            Some(Ins::Generic(bytearray![0xd9], arrayvec![], &GenericIns {
                name: Some("close-file"),
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0xda => {
            Some(Ins::Generic(bytearray![0xda], arrayvec![], &GenericIns {
                name: Some("open-file"),
                args: &[GenericArg::String, GenericArg::Int],
                returns_value: true,
            }))
        }
        0xde => {
            Some(Ins::Generic(bytearray![0xde], arrayvec![], &GenericIns {
                name: Some("delete-file"),
                args: &[GenericArg::String],
                returns_value: true,
            }))
        }
        0xe2 => {
            Some(Ins::Generic(bytearray![0xe2], arrayvec![], &GenericIns {
                name: None,
                args: &[GenericArg::Int],
                returns_value: false,
            }))
        }
        0xee => {
            Some(Ins::Generic(bytearray![0xee], arrayvec![], &GenericIns {
                name: Some("strlen"),
                args: &[GenericArg::Int],
                returns_value: true,
            }))
        }
        0xf3 => op_f3(code),
        0xf4 => op_f4(code),
        0xf8 => op_f8(code),
        0xf9 => {
            Some(Ins::Generic(bytearray![0xf9], arrayvec![], &GenericIns {
                name: Some("create-directory"),
                args: &[GenericArg::String],
                returns_value: false,
            }))
        }
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

fn op_0b_get_array_item_2d<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::GetArrayItem2D(read_var(code)?))
}

fn op_25_sprite_retval<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x2d => {
            Some(Ins::Generic(
                bytearray![0x25, 0x2d],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::List,
                    ],
                    returns_value: true,
                },
            ))
        }
        0x7d => {
            Some(Ins::Generic(
                bytearray![0x25, 0x7d],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::Int, GenericArg::List],
                    returns_value: true,
                },
            ))
        }
        _ => None,
    }
}

fn op_26_sprite<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x39 => {
            Some(Ins::Generic(
                bytearray![0x26, 0x39],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::Int, GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        0x7d => {
            Some(Ins::Generic(
                bytearray![0x26, 0x7d],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::List],
                    returns_value: false,
                },
            ))
        }
        0x9e => {
            Some(Ins::Generic(
                bytearray![0x26, 0x9e],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[],
                    returns_value: false,
                },
            ))
        }
        _ => None,
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
        6 => Some(ItemSize::I32),
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
    Some(Ins::Generic(
        bytearray![0x5e, sub],
        arrayvec![],
        &GenericIns {
            name: Some("start-script"),
            args: &[GenericArg::Int, GenericArg::List],
            returns_value: false,
        },
    ))
}

fn op_6b_cursor<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        b @ (0x91 | 0x93) => {
            Some(Ins::Generic(
                bytearray![0x6b, b],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[],
                    returns_value: false,
                },
            ))
        }
        0x9c => {
            Some(Ins::Generic(
                bytearray![0x6b, 0x9c],
                arrayvec![],
                &GenericIns {
                    name: Some("cursor-charset"),
                    args: &[GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        _ => None,
    }
}

fn op_73_jump<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Jump(read_i16(code)?))
}

fn op_74<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x09 => {
            Some(Ins::Generic(
                bytearray![0x74, 0x09],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[],
                    returns_value: false,
                },
            ))
        }
        0xe6 => {
            Some(Ins::Generic(
                bytearray![0x74, 0xe6],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        0xe7 => {
            Some(Ins::Generic(
                bytearray![0x74, 0xe7],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        0xe8 => {
            Some(Ins::Generic(
                bytearray![0x74, 0xe8],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        0xff => {
            Some(Ins::Generic(
                bytearray![0x74, 0xff],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[],
                    returns_value: false,
                },
            ))
        }
        _ => None,
    }
}

fn op_9b<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x64 => {
            Some(Ins::Generic(
                bytearray![0x9b, 0x64],
                arrayvec![],
                &GenericIns {
                    name: Some("load-script"),
                    args: &[GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        0x6c => {
            Some(Ins::Generic(
                bytearray![0x9b, 0x6c],
                arrayvec![],
                &GenericIns {
                    name: Some("lock-script"),
                    args: &[GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        0x75 => {
            Some(Ins::Generic(
                bytearray![0x9b, 0x75],
                arrayvec![],
                &GenericIns {
                    name: Some("load-charset"),
                    args: &[GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        _ => None,
    }
}

fn op_9c<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xb5 => {
            Some(Ins::Generic(
                bytearray![0x9c, 0xb5],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::Int],
                    returns_value: false,
                },
            ))
        }
        _ => None,
    }
}

fn op_a4_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x07 => Some(Ins::AssignString(read_var(code)?)),
        0x7f => {
            let var = read_var(code)?;
            let var2 = read_var(code)?;
            Some(Ins::Generic(
                bytearray![0xa4, 0x7f],
                arrayvec![Operand::Var(var), Operand::Var(var2)],
                &GenericIns {
                    name: None,
                    args: &[
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                    ],
                    returns_value: false,
                },
            ))
        }
        0x80 => {
            let var = read_var(code)?;
            Some(Ins::Generic(
                bytearray![0xa4, 0x80],
                arrayvec![Operand::Var(var)],
                &GenericIns {
                    name: None,
                    args: &[
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                        GenericArg::Int,
                    ],
                    returns_value: false,
                },
            ))
        }
        0xc2 => Some(Ins::Sprintf(read_var(code)?)),
        _ => None,
    }
}

fn op_a9<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xa9 => {
            Some(Ins::Generic(
                bytearray![0xa9, 0xa9],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[],
                    returns_value: false,
                },
            ))
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
        0x4b => {
            let s = read_string(code)?;
            Some(Ins::Generic(
                bytearray![opcode, 0x4b],
                arrayvec![Operand::String(s)],
                &GenericIns {
                    name: None,
                    args: &[],
                    returns_value: false,
                },
            ))
        }
        0xc2 => {
            let s = read_string(code)?;
            Some(Ins::Generic(
                bytearray![opcode, 0xc2],
                arrayvec![Operand::String(s)],
                &GenericIns {
                    name: None,
                    args: &[],
                    returns_value: false,
                },
            ))
        }
        0xfe => {
            Some(Ins::Generic(
                bytearray![opcode, 0xfe],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[], // NOTE: this pops an int for opcode b8 and b9
                    returns_value: false,
                },
            ))
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
        0xcc => {
            let var = read_var(code)?;
            Some(Ins::Generic(
                bytearray![0xbc, 0xcc],
                arrayvec![Operand::Var(var)],
                &GenericIns {
                    name: Some("free-array"),
                    args: &[],
                    returns_value: false,
                },
            ))
        }
        _ => None,
    }
}

fn op_f3<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x06 => {
            Some(Ins::Generic(
                bytearray![0xf3, 0x06],
                arrayvec![],
                &GenericIns {
                    name: Some("read-ini-int"),
                    args: &[GenericArg::String],
                    returns_value: true,
                },
            ))
        }
        0x07 => {
            Some(Ins::Generic(
                bytearray![0xf3, 0x07],
                arrayvec![],
                &GenericIns {
                    name: Some("read-ini-string"),
                    args: &[GenericArg::String],
                    returns_value: true,
                },
            ))
        }
        _ => None,
    }
}

fn op_f4<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x07 => {
            Some(Ins::Generic(
                bytearray![0xf4, 0x07],
                arrayvec![],
                &GenericIns {
                    name: Some("write-ini-string"),
                    args: &[GenericArg::String, GenericArg::String],
                    returns_value: false,
                },
            ))
        }
        _ => None,
    }
}

fn op_f8<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x0d => {
            Some(Ins::Generic(
                bytearray![0xf8, 0x0d],
                arrayvec![],
                &GenericIns {
                    name: None,
                    args: &[GenericArg::Int],
                    returns_value: true,
                },
            ))
        }
        _ => None,
    }
}

fn op_fa_window_title<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xf3 => {
            Some(Ins::Generic(
                bytearray![0xfa, 0xf3],
                arrayvec![],
                &GenericIns {
                    name: Some("set-window-title"),
                    args: &[GenericArg::String],
                    returns_value: false,
                },
            ))
        }
        _ => None,
    }
}
