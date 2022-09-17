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

/// I regret everything.
macro_rules! ins {
    ////////////////////////////////////////////////////////////////////////////
    // Parse
    ////////////////////////////////////////////////////////////////////////////

    // Parse bytecode
    (
        @parse,
        bytecode = {},
        name = $name:tt,
        ops = $ops:tt,
        args = $args:tt,
        retval = $retval:tt,
        rest = {[$($byte:expr),+] $(, $($rest:tt)*)?},
    ) => {
        ins!(
            @parse,
            bytecode = {bytearray![$($byte),+]},
            name = $name,
            ops = $ops,
            args = $args,
            retval = $retval,
            rest = {$($($rest)*)?},
        )
    };
    // Parse name
    (
        @parse,
        bytecode = $bytecode:tt,
        name = {},
        ops = $ops:tt,
        args = $args:tt,
        retval = $retval:tt,
        rest = {name = $name:expr $(, $($rest:tt)*)?},
    ) => {
        ins!(
            @parse,
            bytecode = $bytecode,
            name = {Some($name)},
            ops = $ops,
            args = $args,
            retval = $retval,
            rest = {$($($rest)*)?},
        )
    };
    // Parse ops
    (
        @parse,
        bytecode = $bytecode:tt,
        name = $name:tt,
        ops = {},
        args = $args:tt,
        retval = $retval:tt,
        rest = {ops = [$($ops:tt)*] $(, $($rest:tt)*)?},
    ) => {
        ins!(
            @parse,
            bytecode = $bytecode,
            name = $name,
            ops = {ins!(@ops, begin = {$($ops)*})},
            args = $args,
            retval = $retval,
            rest = {$($($rest)*)?},
        )
    };
    // Parse args
    (
        @parse,
        bytecode = $bytecode:tt,
        name = $name:tt,
        ops = $ops:tt,
        args = {},
        retval = $retval:tt,
        rest = {args = [$($args:tt)*] $(, $($rest:tt)*)?},
    ) => {
        ins!(
            @parse,
            bytecode = $bytecode,
            name = $name,
            ops = $ops,
            args = {ins!(@args, begin = {$($args)*})},
            retval = $retval,
            rest = {$($($rest)*)?},
        )
    };
    // Parse retval
    (
        @parse,
        bytecode = $bytecode:tt,
        name = $name:tt,
        ops = $ops:tt,
        args = $args:tt,
        retval = {},
        rest = {retval $(, $($rest:tt)*)?},
    ) => {
        ins!(
            @parse,
            bytecode = $bytecode,
            name = $name,
            ops = $ops,
            args = $args,
            retval = {true},
            rest = {$($($rest)*)?},
        )
    };
    // Done parsing. Transfer control to @set_defaults
    (
        @parse,
        bytecode = $bytecode:tt,
        name = $name:tt,
        ops = $ops:tt,
        args = $args:tt,
        retval = $retval:tt,
        rest = {},
    ) => {
        ins!(
            @set_defaults,
            bytecode = $bytecode,
            name = $name,
            ops = $ops,
            args = $args,
            retval = $retval,
        )
    };

    ////////////////////////////////////////////////////////////////////////////
    // Defaults
    ////////////////////////////////////////////////////////////////////////////

    // Set default name
    (
        @set_defaults,
        bytecode = $bytecode:tt,
        name = {},
        ops = $ops:tt,
        args = $args:tt,
        retval = $retval:tt,
    ) => {
        ins!(
            @set_defaults,
            bytecode = $bytecode,
            name = {None},
            ops = $ops,
            args = $args,
            retval = $retval,
        )
    };
    // Set default ops
    (
        @set_defaults,
        bytecode = $bytecode:tt,
        name = $name:tt,
        ops = {},
        args = $args:tt,
        retval = $retval:tt,
    ) => {
        ins!(
            @set_defaults,
            bytecode = $bytecode,
            name = $name,
            ops = {arrayvec![]},
            args = $args,
            retval = $retval,
        )
    };
    // Set default args
    (
        @set_defaults,
        bytecode = $bytecode:tt,
        name = $name:tt,
        ops = $ops:tt,
        args = {},
        retval = $retval:tt,
    ) => {
        ins!(
            @set_defaults,
            bytecode = $bytecode,
            name = $name,
            ops = $ops,
            args = {&[]},
            retval = $retval,
        )
    };
    // Set default retval
    (
        @set_defaults,
        bytecode = $bytecode:tt,
        name = $name:tt,
        ops = $ops:tt,
        args = $args:tt,
        retval = {},
    ) => {
        ins!(
            @set_defaults,
            bytecode = $bytecode,
            name = $name,
            ops = $ops,
            args = $args,
            retval = {false},
        )
    };
    // All defaults set. Emit.
    (
        @set_defaults,
        bytecode = {$($bytecode:tt)*},
        name = {$($name:tt)*},
        ops = {$($ops:tt)*},
        args = {$($args:tt)*},
        retval = {$retval:tt},
    ) => {
        Some(Ins::Generic({$($bytecode)*}, {$($ops)*}, &GenericIns {
            name: {$($name)*},
            args: {$($args)*},
            returns_value: $retval,
        }))
    };

    ////////////////////////////////////////////////////////////////////////////
    // Ops
    ////////////////////////////////////////////////////////////////////////////

    // Entry point
    (
        @ops,
        begin = {$($in:tt)*}
    ) => {
        ins!(
            @ops,
            in = {$($in)*},
            out = {,},
        )
    };
    // Parse one item
    (
        @ops,
        in = {$type:ident: $value:expr $(, $($tail:tt)*)?},
        out = {$($out:tt)*},
    ) => {
        ins!(
            @ops,
            in = {$($($tail)*)?},
            out = {$($out)* ins!(@op_kind $type)($value),},
        )
    };
    (@op_kind var) => {
        Operand::Var
    };
    (@op_kind string) => {
        Operand::String
    };
    // Done. Return them.
    (
        @ops,
        in = {},
        out = {, $($out:tt)*},
    ) => {
        arrayvec![$($out)*]
    };

    ////////////////////////////////////////////////////////////////////////////
    // Args
    ////////////////////////////////////////////////////////////////////////////

    // Entry point
    (
        @args,
        begin = {$($in:tt)*}
    ) => {
        ins!(
            @args,
            in = {$($in)*},
            out = {,},
        )
    };
    // Parse one item
    (
        @args,
        in = {$arg:ident $(, $($rest:tt)*)?},
        out = {$($out:tt)*},
    ) => {
        ins!(
            @args,
            in = {$($($rest)*)?},
            out = {$($out)* ins!(@one_arg $arg),},
        )
    };
    (@one_arg int) => {
        GenericArg::Int
    };
    (@one_arg string) => {
        GenericArg::String
    };
    (@one_arg list) => {
        GenericArg::List
    };
    // Done. Return a slice.
    (
        @args,
        in = {},
        out = {, $($out:tt)*},
    ) => {
        &[$($out)*]
    };

    ////////////////////////////////////////////////////////////////////////////
    // Public matchers
    ////////////////////////////////////////////////////////////////////////////

    // Fallback error
    (@ $($rest:tt)*) => {
        compile_error!("macro has failed")
    };

    // Main entrypoint. Transfer control to @parse
    ($($rest:tt)*) => {
        ins!(
            @parse,
            bytecode = {},
            name = {},
            ops = {},
            args = {},
            retval = {},
            rest = {$($rest)*},
        )
    };
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
        0x1b => ins!([0x1b], args = [int, list], retval),
        0x25 => op_25_sprite_retval(code),
        0x26 => op_26_sprite(code),
        0x37 => op_37_dim_array(code),
        0x43 => op_43_set(code),
        0x47 => op_47_set_array_item(code),
        0x4f => op_4f_inc(code),
        0x57 => op_57_dec(code),
        0x5a => ins!([0x5a], args = [int], retval),
        0x5c => op_5c_jump_if(code),
        0x5d => op_5d_jump_unless(code),
        0x5e => op_5e_start_script(code),
        0x66 => ins!([0x66], name = "free-script"),
        0x6b => op_6b_cursor(code),
        0x6c => ins!([0x6c], name = "stop-script"),
        0x6d => ins!([0x6d], args = [int, list], retval),
        0x73 => op_73_jump(code),
        0x74 => op_74(code),
        0x75 => ins!([0x75], args = [int]),
        0x7b => ins!([0x7b], args = [int]),
        0x7c => ins!([0x7c], args = [int]),
        0x87 => ins!([0x87], name = "random", args = [int], retval),
        0x88 => ins!([0x88], name = "random2", args = [int, int], retval),
        0x98 => ins!([0x98], args = [int], retval),
        0x9b => op_9b(code),
        0x9c => op_9c(code),
        0x9f => ins!([0x9f], args = [int, int], retval),
        0xa0 => ins!([0xa0], args = [int, int], retval),
        0xa3 => ins!([0xa0], args = [int, int], retval),
        0xa4 => op_a4_array(code),
        0xa7 => ins!([0xa7], name = "pop-discard", args = [int]),
        0xad => ins!([0xad], args = [int, list], retval),
        0xb6 => op_b6(code),
        0xa9 => op_a9(code),
        0xb7 => op_b7(code),
        0xbd => ins!([0xbd], name = "return", args = [int]),
        0xbf => ins!([0xbf], name = "call-script", args = [int, list], retval),
        0xbc => op_bc_array(code),
        0xc1 => ins!([0xc1], args = [int, string]),
        0xc4 => ins!([0xc4], name = "abs", args = [int], retval),
        0xc8 => ins!([0xc8], name = "kludge-retval", args = [list], retval),
        0xc9 => ins!([0xc9], name = "kludge", args = [list]),
        0xca => ins!([0xca], args = [int]),
        0xd0 => ins!([0xd0], name = "now"),
        0xd4 => ins!([0xd4], ops = [var: read_var(code)?], args = [int, int]),
        0xd6 => Some(Ins::BitwiseAnd),
        0xd7 => Some(Ins::BitwiseOr),
        0xd9 => ins!([0xd9], name = "close-file", args = [int]),
        0xda => ins!([0xda], name = "open-file", args = [string, int], retval),
        0xde => ins!([0xde], name = "delete-file", args = [string]),
        0xe2 => ins!([0xe2], args = [int]),
        0xee => ins!([0xee], name = "strlen", args = [int], retval),
        0xf3 => op_f3(code),
        0xf4 => op_f4(code),
        0xf8 => op_f8(code),
        0xf9 => ins!([0xf9], name = "create-directory", args = [string]),
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
        0x2d => ins!([0x25, 0x2d], args = [int, int, int, int, list], retval),
        0x7d => ins!([0x25, 0x7d], args = [int, list], retval),
        _ => None,
    }
}

fn op_26_sprite<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x39 => ins!([0x26, 0x39], args = [int, int]),
        0x7d => ins!([0x26, 0x7d], args = [list]),
        0x9e => ins!([0x26, 0x9e]),
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

fn op_57_dec<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Dec(read_var(code)?))
}

fn op_5c_jump_if<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::JumpIf(read_i16(code)?))
}

fn op_5d_jump_unless<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::JumpUnless(read_i16(code)?))
}

fn op_5e_start_script<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let sub = read_u8(code)?;
    ins!([0x5e, sub], name = "run-script", args = [int, list])
}

fn op_6b_cursor<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        sub @ (0x13 | 0x14) => ins!([0x6b, sub], args = [int]),
        sub @ (0x91 | 0x93) => ins!([0x6b, sub]),
        0x9c => ins!([0x6b, 0x9c], name = "cursor-charset", args = [int]),
        _ => None,
    }
}

fn op_73_jump<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Jump(read_i16(code)?))
}

fn op_74<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x09 => ins!([0x74, 0x09]),
        0xe6 => ins!([0x74, 0xe6], args = [int]),
        0xe7 => ins!([0x74, 0xe7], args = [int]),
        0xe8 => ins!([0x74, 0xe8], args = [int]),
        0xff => ins!([0x74, 0xff]),
        _ => None,
    }
}

fn op_9b<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x64 => ins!([0x9b, 0x64], name = "load-script", args = [int]),
        0x6c => ins!([0x9b, 0x6c], name = "lock-script", args = [int]),
        0x75 => ins!([0x9b, 0x75], name = "load-charset", args = [int]),
        _ => None,
    }
}

fn op_9c<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xb5 => ins!([0x9c, 0xb5], args = [int]),
        _ => None,
    }
}

fn op_a4_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x07 => Some(Ins::AssignString(read_var(code)?)),
        0x7f => {
            ins!(
                [0xa4, 0x7f],
                ops = [var: read_var(code)?, var: read_var(code)?],
                args = [int, int, int, int, int, int, int, int],
            )
        }
        0x80 => {
            ins!(
                [0xa4, 0x80],
                ops = [var: read_var(code)?],
                args = [int, int, int, int, int, int],
            )
        }
        0xc2 => Some(Ins::Sprintf(read_var(code)?)),
        _ => None,
    }
}

fn op_a9<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xa9 => ins!([0xa9, 0xa9]),
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
        0x4b => ins!([opcode, 0x4b], ops = [string: read_string(code)?]),
        0xc2 => ins!([opcode, 0xc2], ops = [string: read_string(code)?]),
        0xfe => {
            // NOTE: this changes for different opcodes
            if !(opcode == 0xb6 || opcode == 0xb7) {
                return None;
            }
            ins!([opcode, 0xfe])
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
        0xcc => ins!([0xbc, 0xcc], name = "free-array", ops = [var: read_var(code)?]),
        _ => None,
    }
}

fn op_f3<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x06 => ins!([0xf3, 0x06], name = "read-ini-int", args = [string], retval),
        0x07 => {
            ins!(
                [0xf3, 0x07],
                name = "read-ini-string",
                args = [string],
                retval,
            )
        }
        _ => None,
    }
}

fn op_f4<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x07 => {
            ins!(
                [0xf4, 0x07],
                name = "write-ini-string",
                args = [string, string],
            )
        }
        _ => None,
    }
}

fn op_f8<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x0d => ins!([0xf8, 0x0d], args = [int], retval),
        _ => None,
    }
}

fn op_fa_window_title<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xf3 => ins!([0xfa, 0xf3], name = "set-window-title", args = [string]),
        _ => None,
    }
}
