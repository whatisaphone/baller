use crate::{
    script::{
        cursor::{read_i16, read_i32, read_string, read_u16, read_u8, read_var},
        ins::{GenericArg, GenericIns, Ins, ItemSize, Operand},
    },
    utils::subslice_offset,
};
use arrayvec::ArrayVec;
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
    (@op_kind u8) => {
        Operand::Byte
    };
    (@op_kind i16) => {
        Operand::I16
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
    (@one_arg script) => {
        GenericArg::IntScript
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
    #[allow(clippy::match_same_arms)]
    match opcode {
        0x00 => op_00_push_byte(code),
        0x01 => op_01_push_i16(code),
        0x02 => op_02_push_i32(code),
        0x03 => op_03_push_var(code),
        0x04 => op_04_push_str(code),
        0x07 => op_07_get_array_item(code),
        0x0a => Some(Ins::StackDupN(read_u16(code)?)),
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
        0x1b => Some(Ins::In),
        0x1c => op_1c_image(code),
        0x1d => ins!([0x1d], name = "min", args = [int, int], retval),
        0x1e => ins!([0x1e], name = "max", args = [int, int], retval),
        0x1f => ins!([0x1f], name = "sin", args = [int], retval),
        0x20 => ins!([0x20], name = "cos", args = [int], retval),
        0x21 => ins!([0x21], args = [int], retval),
        0x22 => ins!([0x22], name = "atan2", args = [int, int], retval),
        0x23 => ins!([0x22], name = "atan4", args = [int, int, int, int], retval),
        0x24 => op_24(code),
        0x25 => op_25_sprite_retval(code),
        0x26 => op_26_sprite(code),
        0x27 => op_27_sprite_group_retval(code),
        0x28 => op_28_sprite_group(code),
        0x29 => op_29_image_retval(code),
        0x2a => {
            ins!(
                [0x2a],
                name = "actor-get-property",
                args = [int, int, int],
                retval,
            )
        }
        0x2b => op_2b_begin_script(code),
        0x2c => op_2c_become_script(code),
        0x30 => ins!([0x30], name = "mod", args = [int, int], retval),
        0x31 => ins!([0x31], name = "shl", args = [int, int], retval),
        0x32 => ins!([0x32], name = "shr", args = [int, int], retval),
        0x34 => {
            ins!(
                [0x34],
                name = "find-all-objects-of-class",
                args = [int, list],
                retval,
            )
        }
        0x36 => ins!([0x36], name = "iif", args = [int, int, int], retval),
        0x37 => op_37_dim_array(code),
        0x38 => op_38_redim_array(code),
        0x3a => op_3a_array_sort(code),
        0x43 => op_43_set(code),
        0x46 => ins!([0x46], name = "file-get-size", args = [string], retval),
        0x47 => op_47_set_array_item(code),
        0x48 => ins!([0x48], name = "atoi", args = [int], retval),
        0x4b => op_4b_set_array_item_2d(code),
        0x4d => op_4d_read_ini(code),
        0x4e => op_4e_write_ini(code),
        0x4f => op_4f_inc(code),
        0x50 => ins!([0x50]),
        0x53 => ins!([0x53], name = "inc-array-item", ops = [var: read_var(code)?], args = [int]),
        0x57 => op_57_dec(code),
        0x58 => op_58(code),
        0x5a => ins!([0x5a], args = [int], retval),
        0x5b => ins!([0x5b], name = "dec-array-item", ops = [var: read_var(code)?], args = [int]),
        0x5c => op_5c_jump_if(code),
        0x5d => op_5d_jump_unless(code),
        0x5e => op_5e_run_script(code),
        0x60 => op_60_start_script(code),
        0x61 => op_61_draw_object(code),
        0x62 => ins!([0x62], name = "draw-x62", args = [int]),
        0x63 => op_63_array_sizes(code),
        0x64 => ins!([0x64], name = "get-free-arrays", retval),
        0x65 => ins!([0x65], name = "finish-script"),
        0x66 => ins!([0x66], name = "free-script"),
        0x69 => op_69_window(code),
        0x6a => ins!([0x6a], args = [int]),
        0x6b => op_6b_cursor(code),
        0x6c => ins!([0x6c], name = "stop-script"),
        0x6d => ins!([0x6d], args = [int, list], retval),
        0x6e => ins!([0x6e], args = [int, list]),
        0x6f => ins!([0x6f], name = "object-get-state", args = [int], retval),
        0x70 => ins!([0x70], name = "object-put-state", args = [int, int]),
        0x73 => op_73_jump(code),
        0x74 => op_74_sound(code),
        0x75 => ins!([0x75], name = "stop-sound", args = [int]),
        0x7b => ins!([0x7b], name = "go-to-room", args = [int]),
        0x7c => ins!([0x7c], name = "free-running-script", args = [script]),
        0x7f => ins!([0x7f], name = "put-actor", args = [int, int, int, int]),
        0x82 => ins!([0x82], name = "actor-do-anim", args = [int, int]),
        0x87 => ins!([0x87], name = "random", args = [int], retval),
        0x88 => ins!([0x88], name = "random2", args = [int, int], retval),
        0x8b => ins!([0x8b], name = "is-script-running", args = [script], retval),
        0x8c => ins!([0x8c], name = "get-room", args = [int], retval),
        0x8d => ins!([0x8d], args = [int], retval),
        0x8e => ins!([0x8e], args = [int], retval),
        0x91 => ins!([0x91], name = "actor-get-costume", args = [int], retval),
        0x94 => op_94(code),
        0x95 => op_95_cutscene_start(code),
        0x96 => ins!([0x96], name = "cutscene-end"),
        0x98 => ins!([0x98], name = "is-sound-playing", args = [int], retval),
        0x9b => op_9b(code),
        0x9c => op_9c(code),
        0x9d => op_9d_actor(code),
        0x9e => op_9e_palette(code),
        0x9f => ins!([0x9f], args = [int, int], retval),
        0xa0 => ins!([0xa0], args = [int, int], retval),
        0xa2 => ins!([0xa2], args = [int], retval),
        0xa3 => ins!([0xa3], args = [int, int], retval),
        0xa4 => op_a4_array(code),
        0xa6 => ins!([0xa6], args = [int, int, int, int, int]),
        0xa7 => ins!([0xa7], name = "pop-discard", args = [int]),
        0xa9 => op_a9(code),
        0xaa => ins!([0xaa], name = "actor-get-xaa", args = [int], retval),
        0xad => Some(Ins::In), // same as 0x1b except iterates in reverse order?
        0xae => op_ae(code),
        0xb0 => ins!([0xb0], name = "sleep-frames", args = [int]),
        0xb1 => ins!([0xb1], name = "sleep-seconds", args = [int]),
        0xb3 => ins!([0xb3], name = "stop-script-34"),
        0xb5 => op_b5(code),
        0xb6 => op_b6(code),
        0xb7 => op_b7(code),
        0xb8 => op_b8(code),
        0xb9 => op_b9(code),
        0xba => ins!([0xba], ops = [string: read_string(code)?], args = [int]),
        0xbb => ins!([0xbb], ops = [string: read_string(code)?]),
        0xbc => op_bc_array(code),
        0xbd => ins!([0xbd], name = "return", args = [int]),
        0xbf => ins!([0xbf], name = "call-script", args = [script, list], retval),
        0xc0 => op_c0_dim_array(code),
        0xc1 => ins!([0xc1], name = "pop-discard-2", args = [int, string]),
        0xc4 => ins!([0xc4], name = "abs", args = [int], retval),
        0xc8 => ins!([0xc8], name = "kludge-retval", args = [list], retval),
        0xc9 => ins!([0xc9], name = "kludge", args = [list]),
        0xca => ins!([0xca], name = "sleep", args = [int]),
        0xcb => ins!([0xcb], name = "pick", args = [int, list], retval),
        0xcf => ins!([0xcf], name = "input-dialog", args = [string], retval),
        0xd0 => ins!([0xd0], name = "now"),
        0xd1 => ins!([0xd1]),
        0xd2 => ins!([0xd2], name = "actor-get-var", args = [int, int], retval),
        0xd4 => {
            ins!([0xd4], name = "shuffle-array", ops = [var: read_var(code)?], args = [int, int])
        }
        0xd5 => op_d5_exec_script(code),
        0xd6 => Some(Ins::BitwiseAnd),
        0xd7 => Some(Ins::BitwiseOr),
        0xd9 => ins!([0xd9], name = "file-close", args = [int]),
        0xda => ins!([0xda], name = "file-open", args = [string, int], retval),
        0xdb => op_db_file_read(code),
        0xdc => op_dc_file_write(code),
        0xde => ins!([0xde], name = "file-delete", args = [string]),
        0xe0 => op_e0(code),
        0xe2 => ins!([0xe2], args = [int]),
        0xe3 => ins!([0xe3], ops = [var: read_var(code)?], args = [list], retval),
        0xe9 => ins!([0xe9], name = "file-seek", args = [int, int, int]),
        0xea => {
            ins!(
                [0xea],
                name = "redim",
                ops = [u8: read_u8(code)?, var: read_var(code)?],
                args = [int, int],
            )
        }
        0xeb => ins!([0xeb], name = "file-get-pos", args = [int], retval),
        0xec => ins!([0xec], name = "strcpy", args = [int], retval),
        0xed => ins!([0xed], args = [int, int, int], retval),
        0xee => ins!([0xee], name = "strlen", args = [int], retval),
        0xef => ins!([0xef], name = "substr", args = [int, int, int], retval),
        0xf1 => ins!([0xf1], name = "strcmp", args = [int, int], retval),
        0xf2 => op_f2_percent_loaded(code),
        0xf3 => op_f3_read_ini(code),
        0xf4 => op_f4_write_ini(code),
        0xf5 => ins!([0xf5], args = [int, int, int], retval),
        0xf6 => {
            ins!(
                [0xf6],
                name = "array-find",
                args = [int, int, int, int],
                retval,
            )
        }
        0xf8 => op_f8_get_size(code),
        0xf9 => ins!([0xf9], name = "create-directory", args = [string]),
        0xfa => op_fa_window_title(code),
        0xfb => op_fb(code),
        0xfc => ins!([0xfc], args = [int, int], retval),
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

fn op_1c_image<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x20 => ins!([0x1c, 0x20], name = "image-x20", args = [int]),
        0x21 => ins!([0x1c, 0x21], name = "image-x21", args = [int]),
        0x30 => ins!([0x1c, 0x30], name = "image-x30"),
        0x31 => ins!([0x1c, 0x31], name = "image-x31", args = [string]),
        0x33 => {
            ins!(
                [0x1c, 0x33],
                name = "image-x33",
                args = [int, int, int, int, int],
            )
        }
        0x34 => ins!([0x1c, 0x34], name = "image-x34", args = [int]),
        0x36 => ins!([0x1c, 0x36], name = "image-x36", args = [int]),
        0x38 => {
            ins!(
                [0x1c, 0x38],
                name = "image-x38",
                args = [int, int, int, int, int],
            )
        }
        0x39 => ins!([0x1c, 0x39], name = "image-x39", args = [int]),
        0x41 => ins!([0x1c, 0x41], name = "image-x41", args = [int, int]),
        0x56 => ins!([0x1c, 0x56], name = "image-x56", args = [int]),
        0x62 => ins!([0x1c, 0x62], name = "image-x62", args = [int]),
        0x85 => {
            ins!(
                [0x1c, 0x85],
                name = "image-x85",
                args = [int, int, int, int, int],
            )
        }
        0x89 => ins!([0x1c, 0x89], name = "image-x89", args = [int]),
        0x9a => ins!([0x1c, 0x9a], name = "image-x9a", args = [int, int]),
        0xd9 => ins!([0x1c, 0xd9], name = "image-xd9"),
        0xf6 => ins!([0x1c, 0xf6], name = "image-xf6", args = [int]),
        0xff => ins!([0x1c, 0xff], name = "image-xff"),
        _ => None,
    }
}

fn op_24<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x1c => ins!([0x24, 0x1c], args = [int, int, int, int], retval),
        0x1d => ins!([0x24, 0x1d], args = [int, int, int, int, int, int], retval),
        _ => None,
    }
}

fn op_25_sprite_retval<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x1e => {
            ins!(
                [0x25, 0x1e],
                name = "sprite-get-hotspot-x",
                args = [int],
                retval,
            )
        }
        0x1f => {
            ins!(
                [0x25, 0x1f],
                name = "sprite-get-hotspot-y",
                args = [int],
                retval,
            )
        }
        0x24 => ins!([0x25, 0x24], name = "sprite-get-x24", args = [int], retval),
        0x25 => {
            ins!(
                [0x25, 0x25],
                name = "sprite-get-group",
                args = [int],
                retval,
            )
        }
        0x26 => {
            ins!(
                [0x25, 0x26],
                name = "sprite-get-translated-hotspot-x",
                args = [int],
                retval,
            )
        }
        0x27 => {
            ins!(
                [0x25, 0x27],
                name = "sprite-get-translated-hotspot-y",
                args = [int],
                retval,
            )
        }
        0x2b => ins!([0x25, 0x2b], name = "sprite-get-x2b", args = [int], retval),
        0x2d => {
            ins!(
                [0x25, 0x2d],
                name = "sprite-get-x2d",
                args = [int, int, int, int, list],
                retval,
            )
        }
        0x34 => ins!([0x25, 0x34], name = "sprite-get-wiz", args = [int], retval),
        0x3f => {
            ins!(
                [0x25, 0x3f],
                name = "sprite-get-image",
                args = [int],
                retval,
            )
        }
        0x56 => ins!([0x25, 0x56], name = "sprite-get-x56", args = [int], retval),
        0x7c => ins!([0x25, 0x7c], name = "sprite-get-x7c", args = [int], retval),
        0x7d => {
            ins!(
                [0x25, 0x7d],
                name = "sprite-has-class",
                args = [int, list],
                retval,
            )
        }
        0xc6 => {
            ins!(
                [0x25, 0xc6],
                name = "sprite-get-xc6",
                args = [int, int],
                retval,
            )
        }
        _ => None,
    }
}

fn op_26_sprite<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x25 => ins!([0x26, 0x25], name = "sprite-x25", args = [int]),
        0x2a => ins!([0x26, 0x2a], name = "sprite-x2a", args = [int, int]),
        0x2b => ins!([0x26, 0x2b], name = "sprite-x2b", args = [int]),
        0x2c => {
            ins!(
                [0x26, 0x2c],
                name = "sprite-move-hotspot",
                args = [int, int],
            )
        }
        0x34 => ins!([0x26, 0x34], name = "sprite-x34", args = [int]),
        0x39 => ins!([0x26, 0x39], name = "sprite-set-range", args = [int, int]),
        0x3f => ins!([0x26, 0x3f], name = "sprite-x3f", args = [int]),
        0x41 => ins!([0x26, 0x41], name = "sprite-set-hotspot", args = [int, int]),
        0x4d => ins!([0x26, 0x4d], name = "sprite-x4d", args = [int, int]),
        0x52 => ins!([0x26, 0x52], name = "sprite-x52", args = [int]),
        0x56 => ins!([0x26, 0x56], name = "sprite-x56", args = [int]),
        0x61 => ins!([0x26, 0x61], name = "sprite-x61", args = [int]),
        0x62 => ins!([0x26, 0x62], name = "sprite-x62", args = [int]),
        0x7c => ins!([0x26, 0x7c], name = "sprite-x7c", args = [int]),
        0x7d => ins!([0x26, 0x7d], name = "sprite-x7d", args = [list]),
        0x8c => ins!([0x26, 0x8c], name = "sprite-x8c", args = [int]),
        0x9e => ins!([0x26, 0x9e], name = "sprite-x9e"),
        0xc6 => ins!([0x26, 0xc6], name = "sprite-xc6", args = [int, int]),
        0xd9 => ins!([0x26, 0xd9], name = "sprite-clear"),
        _ => None,
    }
}

fn op_27_sprite_group_retval<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x08 => {
            ins!(
                [0x27, 0x08],
                name = "sprite-group-get-x08",
                args = [int],
                retval,
            )
        }
        0x1e => {
            ins!(
                [0x27, 0x1e],
                name = "sprite-group-get-x",
                args = [int],
                retval,
            )
        }
        0x1f => {
            ins!(
                [0x27, 0x1f],
                name = "sprite-group-get-y",
                args = [int],
                retval,
            )
        }
        _ => None,
    }
}

fn op_28_sprite_group<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x2c => ins!([0x28, 0x2c], name = "sprite-group-x2c", args = [int, int]),
        0x39 => ins!([0x28, 0x39], name = "sprite-group-select", args = [int]),
        0x41 => ins!([0x28, 0x41], name = "sprite-group-x41", args = [int, int]),
        0x43 => {
            ins!(
                [0x28, 0x43],
                name = "sprite-group-x43",
                args = [int, int, int, int],
            )
        }
        0xd9 => ins!([0x28, 0xd9], name = "sprite-group-clear"),
        _ => None,
    }
}

fn op_29_image_retval<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x1e => {
            ins!(
                [0x29, 0x1e],
                name = "image-get-hotspot-x",
                args = [int, int],
                retval,
            )
        }
        0x1f => {
            ins!(
                [0x29, 0x1f],
                name = "image-get-hotspot-y",
                args = [int, int],
                retval,
            )
        }
        0x20 => {
            ins!(
                [0x29, 0x20],
                name = "image-get-width",
                args = [int, int],
                retval,
            )
        }
        0x21 => {
            ins!(
                [0x29, 0x21],
                name = "image-get-height",
                args = [int, int],
                retval,
            )
        }
        0x24 => ins!([0x29, 0x24], name = "image-get-x24", args = [int], retval),
        0x42 => {
            ins!(
                [0x29, 0x42],
                name = "image-get-x42",
                args = [int, int, int, int],
                retval,
            )
        }
        _ => None,
    }
}

fn op_2b_begin_script<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x01 => {
            ins!(
                [0x2b, 0x01],
                name = "begin-script",
                args = [script, int, list],
            )
        }
        _ => None,
    }
}

fn op_2c_become_script<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x01 => {
            ins!(
                [0x2c, 0x01],
                name = "become-script",
                args = [script, int, list],
            )
        }
        _ => None,
    }
}

fn op_37_dim_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let item_size = to_item_size(read_u8(code)?)?;
    let var = read_var(code)?;
    Some(Ins::DimArray2D(item_size, var))
}

fn op_38_redim_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let item_size = to_item_size(read_u8(code)?)?;
    let var = read_var(code)?;
    Some(Ins::RedimArray2D(item_size, var))
}

fn to_item_size(n: u8) -> Option<ItemSize> {
    match n {
        2 => Some(ItemSize::Bit),
        4 => Some(ItemSize::Byte),
        5 => Some(ItemSize::I16),
        6 => Some(ItemSize::I32),
        7 => Some(ItemSize::Char),
        _ => None,
    }
}

fn op_3a_array_sort<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x81 => {
            ins!(
                [0x3a, 0x81],
                name = "array-sort",
                ops = [var: read_var(code)?],
                args = [int, int, int, int, int],
            )
        }
        _ => None,
    }
}

fn op_43_set<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Set(read_var(code)?))
}

fn op_47_set_array_item<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::SetArrayItem(read_var(code)?))
}

fn op_4b_set_array_item_2d<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::SetArrayItem2D(read_var(code)?))
}

fn op_4d_read_ini<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x06 => {
            ins!(
                [0x4d, 0x06],
                name = "read-ini-int",
                args = [string, string, string],
                retval,
            )
        }
        0x07 => {
            ins!(
                [0x4d, 0x07],
                name = "read-ini-string",
                args = [string, string, string],
                retval,
            )
        }
        _ => None,
    }
}

fn op_4e_write_ini<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x06 => {
            ins!(
                [0x4e, 0x06],
                name = "write-ini-int",
                args = [string, string, string, int],
            )
        }
        0x07 => {
            ins!(
                [0x4e, 0x07],
                name = "write-ini-string",
                args = [string, string, string, string],
            )
        }
        _ => None,
    }
}

fn op_4f_inc<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Inc(read_var(code)?))
}

fn op_57_dec<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Dec(read_var(code)?))
}

fn op_58<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x0a => ins!([0x58, 0x0a], name = "timer-x0a", args = [int], retval),
        _ => None,
    }
}

fn op_5c_jump_if<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::JumpIf(read_i16(code)?))
}

fn op_5d_jump_unless<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::JumpUnless(read_i16(code)?))
}

fn op_5e_run_script<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x01 => ins!([0x5e, 0x01], name = "run-script", args = [script, list]),
        0xc3 => ins!([0x5e, 0xc3], name = "run-script-xc3", args = [script, list]),
        _ => None,
    }
}

fn op_60_start_script<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x01 => {
            ins!(
                [0x60, 0x01],
                name = "start-script",
                args = [script, int, list],
            )
        }
        0xc3 => {
            ins!(
                [0x60, 0xc3],
                name = "start-script-xc3",
                args = [script, int, list],
            )
        }
        _ => None,
    }
}

fn op_61_draw_object<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x3f => ins!([0x61, 0x3f], name = "draw-object-x3f", args = [int, int]),
        _ => None,
    }
}

fn op_63_array_sizes<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x01 => ins!([0x63, 0x01], name = "array-width", ops = [var: read_var(code)?], retval),
        0x03 => ins!([0x63, 0x03], name = "array-width", ops = [var: read_var(code)?], retval),
        0x02 => ins!([0x63, 0x02], name = "array-height", ops = [var: read_var(code)?], retval),
        _ => None,
    }
}

fn op_69_window<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x39 => ins!([0x69, 0x39], name = "window-select", args = [int]),
        0x3a => ins!([0x69, 0x3a], name = "window-x3a", args = [int]),
        0x3f => ins!([0x69, 0x3f], name = "window-set-image", args = [int]),
        0xd9 => ins!([0x69, 0xd9], name = "window-destroy"),
        0xf3 => ins!([0x69, 0xf3], name = "window-set-text", args = [string]),
        0xff => ins!([0x69, 0xff], name = "window-xff"),
        _ => None,
    }
}

fn op_6b_cursor<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x13 => ins!([0x6b, 0x13], name = "cursor-set-image-bw", args = [int]),
        0x14 => ins!([0x6b, 0x14], name = "cursor-set-image-color", args = [int]),
        0x90 => ins!([0x6b, 0x90], name = "cursor-x90"),
        0x91 => ins!([0x6b, 0x91], name = "cursor-x91"),
        0x92 => ins!([0x6b, 0x92], name = "cursor-x92"),
        0x93 => ins!([0x6b, 0x93], name = "cursor-x93"),
        0x94 => ins!([0x6b, 0x94], name = "cursor-x94"),
        0x95 => ins!([0x6b, 0x95], name = "cursor-x95"),
        0x9c => ins!([0x6b, 0x9c], name = "cursor-charset", args = [int]),
        _ => None,
    }
}

fn op_73_jump<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    Some(Ins::Jump(read_i16(code)?))
}

fn op_74_sound<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x09 => ins!([0x74, 0x09], name = "sound-x09"),
        0xe6 => ins!([0x74, 0xe6], name = "sound-xe6", args = [int]),
        0xe7 => ins!([0x74, 0xe7], name = "sound-xe7", args = [int]),
        0xe8 => ins!([0x74, 0xe8], name = "sound-select", args = [int]),
        0xf5 => ins!([0x74, 0xf5], name = "sound-xf5"),
        0xff => ins!([0x74, 0xff], name = "sound-play"),
        _ => None,
    }
}

fn op_94<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x42 => {
            ins!(
                [0x94, 0x42],
                name = "palette-x42",
                args = [int, int],
                retval,
            )
        }
        0xd9 => {
            ins!(
                [0x94, 0xd9],
                name = "palette-xd9",
                args = [int, int, int],
                retval,
            )
        }
        _ => None,
    }
}

fn op_95_cutscene_start<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    ins!([0x95], name = "cutscene-start", ops = [u8: read_u8(code)?, i16: read_i16(code)?])
}

fn op_9b<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x64 => ins!([0x9b, 0x64], name = "load-script", args = [script]),
        0x65 => ins!([0x9b, 0x65], name = "load-sound", args = [int]),
        0x66 => ins!([0x9b, 0x66], name = "load-costume", args = [int]),
        0x69 => ins!([0x9b, 0x69], name = "free-sound", args = [int]),
        0x6a => ins!([0x9b, 0x6a], name = "free-costume", args = [int]),
        0x6c => ins!([0x9b, 0x6c], name = "lock-script", args = [script]),
        0x6e => ins!([0x9b, 0x6e], name = "lock-costume", args = [int]),
        0x72 => ins!([0x9b, 0x72], name = "unlock-costume", args = [int]),
        0x75 => ins!([0x9b, 0x75], name = "load-charset", args = [int]),
        0x79 => ins!([0x9b, 0x79], name = "queue-sound", args = [int]),
        0x7a => ins!([0x9b, 0x7a], name = "queue-costume", args = [int]),
        0x7b => ins!([0x9b, 0x7b], name = "queue-room", args = [int]),
        0x9f => ins!([0x9b, 0x9f], name = "unlock-image", args = [int]),
        0xc0 => ins!([0x9b, 0xc0], name = "free-image", args = [int]),
        0xc9 => ins!([0x9b, 0xc9], name = "load-image", args = [int]),
        0xca => ins!([0x9b, 0xca], name = "lock-image", args = [int]),
        0xcb => ins!([0x9b, 0xcb], name = "queue-image", args = [int]),
        _ => None,
    }
}

fn op_9c<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xb5 => ins!([0x9c, 0xb5], args = [int]),
        0xd5 => ins!([0x9c, 0xd5], args = [int]),
        0xdd => ins!([0x9c, 0xdd], name = "save-game", args = [int, string]),
        _ => None,
    }
}

fn op_9d_actor<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x15 => ins!([0x9d, 0x15], name = "actor-set-conditons", args = [list]),
        0x2b => ins!([0x9d, 0x2b], name = "actor-x2b", args = [int]),
        0x40 => {
            ins!(
                [0x9d, 0x40],
                name = "actor-default-clip-rect",
                args = [int, int, int, int],
            )
        }
        0x41 => ins!([0x9d, 0x41], name = "actor-move", args = [int, int]),
        0x43 => {
            ins!(
                [0x9d, 0x43],
                name = "actor-clip-rect",
                args = [int, int, int, int],
            )
        }
        0x4c => ins!([0x9d, 0x4c], name = "actor-x4c", args = [int]),
        0x4e => ins!([0x9d, 0x4e], name = "actor-set-sounds", args = [list]),
        0x50 => ins!([0x9d, 0x50], name = "actor-x50", args = [int, int]),
        0x54 => ins!([0x9d, 0x54], name = "actor-x54", args = [int]),
        0x56 => ins!([0x9d, 0x56], name = "actor-x56", args = [int, int]),
        0x57 => ins!([0x9d, 0x57], name = "actor-x57", args = [int]),
        0x5c => ins!([0x9d, 0x5c], name = "actor-x5c", args = [int]),
        0x5d => ins!([0x9d, 0x5d], name = "actor-x5d"),
        0x5e => ins!([0x9d, 0x5e], name = "actor-x5e", args = [int]),
        0x5f => ins!([0x9d, 0x5f], name = "actor-x5f"),
        0x61 => ins!([0x9d, 0x61], name = "actor-x61", args = [int]),
        0x62 => ins!([0x9d, 0x62], name = "actor-x62", args = [int]),
        0x63 => ins!([0x9d, 0x63], name = "actor-x63", args = [int, int]),
        0xc5 => ins!([0x9d, 0xc5], name = "actor-select", args = [int]),
        0xc6 => ins!([0x9d, 0xc6], name = "actor-set-var", args = [int, int]),
        0xd9 => ins!([0x9d, 0xd9], name = "actor-xd9"),
        0xda => ins!([0x9d, 0xda], name = "actor-xda"),
        _ => None,
    }
}

fn op_9e_palette<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x39 => ins!([0x9e, 0x39], name = "palette-select", args = [int]),
        0x3f => ins!([0x9e, 0x3f], name = "palette-load", args = [int, int]),
        0x42 => {
            ins!(
                [0x9e, 0x42],
                name = "palette-set-rgb",
                args = [int, int, int, int, int],
            )
        }
        0x46 => {
            ins!(
                [0x9e, 0x46],
                name = "palette-set-16bit",
                args = [int, int, int],
            )
        }
        0x56 => ins!([0x9e, 0x56], name = "palette-copy", args = [int]),
        0xd9 => ins!([0x9e, 0xd9], name = "palette-reset"),
        0xff => ins!([0x9e, 0xff], name = "palette-unselect"),
        _ => None,
    }
}

fn op_a4_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x07 => Some(Ins::AssignString(read_var(code)?)),
        0x7e => {
            ins!(
                [0xa4, 0x7e],
                name = "array-fill-list",
                ops = [var: read_var(code)?],
                args = [int, int, int, int, list],
            )
        }
        0x7f => {
            ins!(
                [0xa4, 0x7f],
                name = "array-copy-range",
                ops = [var: read_var(code)?, var: read_var(code)?],
                args = [int, int, int, int, int, int, int, int],
            )
        }
        0x80 => {
            ins!(
                [0xa4, 0x80],
                name = "array-fill-values",
                ops = [var: read_var(code)?],
                args = [int, int, int, int, int, int],
            )
        }
        0xc2 => Some(Ins::Sprintf(read_var(code)?)),
        0xd0 => {
            ins!(
                [0xa4, 0xd0],
                name = "array-set",
                ops = [var: read_var(code)?],
                args = [list, int],
            )
        }
        0xd4 => {
            ins!(
                [0xa4, 0xd4],
                name = "array-set-row",
                ops = [var: read_var(code)?],
                args = [int, list],
            )
        }
        _ => None,
    }
}

fn op_a9<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xa9 => ins!([0xa9, 0xa9]),
        _ => None,
    }
}

fn op_ae<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x1a => ins!([0xae, 0x1a], name = "paint"),
        0xa0 => ins!([0xae, 0xa0], name = "quit-without-prompt"),
        0xf4 => ins!([0xae, 0xf4], name = "prompt-quit"),
        _ => None,
    }
}

fn op_b5<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    op_b4_thru_b9(0xb5, false, code)
}

fn op_b6<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    op_b4_thru_b9(0xb6, false, code)
}

fn op_b7<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    op_b4_thru_b9(0xb7, false, code)
}

fn op_b8<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    op_b4_thru_b9(0xb8, true, code)
}

fn op_b9<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    op_b4_thru_b9(0xb9, true, code)
}

fn op_b4_thru_b9<'a>(opcode: u8, flag: bool, code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x41 => ins!([opcode, 0x41], args = [int, int]),
        0x45 => ins!([opcode, 0x45]),
        0x4b => ins!([opcode, 0x4b], ops = [string: read_string(code)?]),
        0xc2 => ins!([opcode, 0xc2], ops = [string: read_string(code)?], args = [int, list]),
        0xe1 => ins!([opcode, 0xe1], args = [int]),
        0xf9 => ins!([opcode, 0xf9], args = [list]),
        0xfe => {
            Some(Ins::Generic(
                bytearray![opcode, 0xfe],
                ArrayVec::new(),
                if flag {
                    &GenericIns {
                        name: None,
                        args: &[GenericArg::Int],
                        returns_value: false,
                    }
                } else {
                    &GenericIns {
                        name: None,
                        args: &[],
                        returns_value: false,
                    }
                },
            ))
        }
        0xff => ins!([opcode, 0xff]),
        _ => None,
    }
}

fn op_bc_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let n = read_u8(code)?;
    if let Some(item_size) = to_item_size(n) {
        return Some(Ins::DimArray1DSimple(item_size, read_var(code)?));
    }
    match n {
        0xcc => ins!([0xbc, 0xcc], name = "free-array", ops = [var: read_var(code)?]),
        _ => None,
    }
}

fn op_c0_dim_array<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    let item_size = to_item_size(read_u8(code)?)?;
    let var = read_var(code)?;
    Some(Ins::DimArray2DSimple(item_size, var))
}

fn op_d5_exec_script<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x01 => {
            ins!([0xd5, 0x01], name = "exec-script", args = [script, list])
        }
        0xc3 => {
            ins!(
                [0xd5, 0xc3],
                name = "exec-script-xc3",
                args = [script, list],
            )
        }
        _ => None,
    }
}

fn op_db_file_read<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x05 => ins!([0xdb, 0x05], name = "file-read-i16", args = [int], retval),
        0x08 => {
            ins!(
                [0xdb, 0x08],
                name = "file-read-array",
                ops = [u8: read_u8(code)?],
                args = [int, int],
                retval,
            )
        }
        _ => None,
    }
}

fn op_dc_file_write<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x05 => ins!([0xdc, 0x05], name = "file-write-i16", args = [int, int]),
        0x08 => {
            ins!(
                [0xdc, 0x08],
                name = "file-write-array",
                ops = [u8: read_u8(code)?],
                args = [int, int],
            )
        }
        _ => None,
    }
}

fn op_e0<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x42 => ins!([0xe0, 0x42], args = [int, int, int, int, int, int]),
        _ => None,
    }
}

fn op_f2_percent_loaded<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xe3 => {
            ins!(
                [0xf2, 0xe3],
                name = "percent-loaded-costume",
                args = [int],
                retval,
            )
        }
        _ => None,
    }
}

fn op_f3_read_ini<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
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

fn op_f4_write_ini<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x06 => ins!([0xf4, 0x06], name = "write-ini-int", args = [string, int]),
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

fn op_f8_get_size<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0x0d => ins!([0xf8, 0x0d], name = "get-sound-size", args = [int], retval),
        _ => None,
    }
}

fn op_fa_window_title<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xf3 => ins!([0xfa, 0xf3], name = "set-window-title", args = [string]),
        _ => None,
    }
}

fn op_fb<'a>(code: &mut &'a [u8]) -> Option<Ins<'a>> {
    match read_u8(code)? {
        0xf7 => ins!([0xfb, 0xf7], name = "polygon-xf7", args = [int, int]),
        0xf8 => {
            ins!(
                [0xfb, 0xf8],
                name = "polygon-quadrilateral",
                args = [int, int, int, int, int, int, int, int, int],
            )
        }
        _ => None,
    }
}
