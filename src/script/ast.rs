use crate::{
    config::{Config, Script},
    script::{
        ins::{GenericArg, GenericIns, ItemSize, Variable},
        misc::{write_indent, AnsiStr},
    },
    utils::byte_array::ByteArray,
};
use std::{fmt, fmt::Write};

pub enum Stmt<'a> {
    DimArray {
        var: Variable,
        item_size: ItemSize,
        min_y: Expr<'a>,
        max_y: Expr<'a>,
        min_x: Expr<'a>,
        max_x: Expr<'a>,
        swap: Expr<'a>,
    },
    RedimArray {
        var: Variable,
        item_size: ItemSize,
        min_y: Expr<'a>,
        max_y: Expr<'a>,
        min_x: Expr<'a>,
        max_x: Expr<'a>,
    },
    Assign(Variable, Expr<'a>),
    SetArrayItem(Variable, Expr<'a>, Expr<'a>),
    SetArrayItem2D(Variable, Expr<'a>, Expr<'a>, Expr<'a>),
    Inc(Variable),
    Dec(Variable),
    #[allow(dead_code)]
    Goto(usize),
    If {
        condition: Expr<'a>,
        true_: Vec<Stmt<'a>>,
        false_: Vec<Stmt<'a>>,
    },
    While {
        condition: Expr<'a>,
        body: Vec<Stmt<'a>>,
    },
    Do {
        body: Vec<Stmt<'a>>,
        condition: Option<Expr<'a>>,
    },
    Case {
        value: Expr<'a>,
        cases: Vec<Case<'a>>,
    },
    Generic {
        bytecode: ByteArray<2>,
        ins: &'a GenericIns,
        args: Vec<Expr<'a>>,
    },
    DecompileError(usize, DecompileErrorKind<'a>),
}

#[derive(Clone)]
pub enum DecompileErrorKind<'a> {
    StackUnderflow,
    StackOrphan(Box<Expr<'a>>),
    WrongBlockExit,
    Other(&'static str),
}

pub struct Case<'a> {
    pub cond: CaseCond<'a>,
    pub body: Vec<Stmt<'a>>,
}

pub enum CaseCond<'a> {
    Eq(Expr<'a>),
    In(Expr<'a>),
    Else,
}

#[derive(Clone)]
pub enum Expr<'a> {
    Number(i32),
    String(&'a [u8]),
    Variable(Variable),
    StackDup(Box<Expr<'a>>),
    StackUnderflow,
    List(Vec<Expr<'a>>),
    ArrayIndex(Variable, Box<Expr<'a>>),
    ArrayIndex2D(Variable, Box<(Expr<'a>, Expr<'a>)>),
    Not(Box<Expr<'a>>),
    Equal(Box<(Expr<'a>, Expr<'a>)>),
    NotEqual(Box<(Expr<'a>, Expr<'a>)>),
    Greater(Box<(Expr<'a>, Expr<'a>)>),
    Less(Box<(Expr<'a>, Expr<'a>)>),
    LessOrEqual(Box<(Expr<'a>, Expr<'a>)>),
    GreaterOrEqual(Box<(Expr<'a>, Expr<'a>)>),
    Add(Box<(Expr<'a>, Expr<'a>)>),
    Sub(Box<(Expr<'a>, Expr<'a>)>),
    Mul(Box<(Expr<'a>, Expr<'a>)>),
    Div(Box<(Expr<'a>, Expr<'a>)>),
    LogicalAnd(Box<(Expr<'a>, Expr<'a>)>),
    LogicalOr(Box<(Expr<'a>, Expr<'a>)>),
    BitwiseAnd(Box<(Expr<'a>, Expr<'a>)>),
    BitwiseOr(Box<(Expr<'a>, Expr<'a>)>),
    In(Box<(Expr<'a>, Expr<'a>)>),
    Call(ByteArray<2>, &'a GenericIns, Vec<Expr<'a>>),
    DecompileError(usize, DecompileErrorKind<'a>),
}

pub struct WriteCx<'a> {
    pub scope: Scope,
    pub config: &'a Config,
}

pub enum Scope {
    Global(i32),
    RoomLocal(i32, i32),
    RoomEnter(i32),
    RoomExit(i32),
    Verb(i32, u16),
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum EmitAs {
    Script,
}

const LOCAL_SCRIPT_CUTOFF: i32 = 2048;

pub fn write_preamble(w: &mut impl Write, vars: &[Variable], cx: &WriteCx) -> fmt::Result {
    match cx.scope {
        Scope::Global(num) => write!(w, "; SCRP {num}")?,
        Scope::RoomLocal(room, num) => write!(w, "; LSC2 {room} {num}")?,
        Scope::RoomEnter(room) => write!(w, "; ENCD {room}")?,
        Scope::RoomExit(room) => write!(w, "; EXCD {room}")?,
        Scope::Verb(room, object) => write!(w, "; VERB {room} {object}")?,
    }
    let script = get_script_config(cx);
    if let Some(name) = script.and_then(|s| s.name.as_ref()) {
        write!(w, " {name}")?;
    }
    writeln!(w)?;
    writeln!(w)?;

    let num_params: u16 = script.and_then(|c| c.params).unwrap_or(0);
    let params_end = vars.partition_point(|v| v.0 & 0x0fff < num_params);

    if !vars[..params_end].is_empty() {
        for &var in &vars[..params_end] {
            w.write_str("parameter ")?;
            write_var(w, var, cx)?;
            writeln!(w)?;
        }
        writeln!(w)?;
    }

    if !vars[params_end..].is_empty() {
        for &var in &vars[params_end..] {
            w.write_str("local variable ")?;
            write_var(w, var, cx)?;
            writeln!(w)?;
        }
        writeln!(w)?;
    }

    Ok(())
}

pub fn write_stmts(w: &mut impl Write, stmts: &[Stmt], indent: usize, cx: &WriteCx) -> fmt::Result {
    for stmt in stmts {
        write_indent(w, indent)?;
        write_stmt(w, stmt, indent, cx)?;
        writeln!(w)?;
    }
    Ok(())
}

#[allow(clippy::too_many_lines)]
fn write_stmt(w: &mut impl Write, stmt: &Stmt, indent: usize, cx: &WriteCx) -> fmt::Result {
    match *stmt {
        Stmt::DimArray {
            var,
            item_size,
            ref min_y,
            ref max_y,
            ref min_x,
            ref max_x,
            ref swap,
        } => {
            w.write_str("dim array ")?;
            write_var(w, var, cx)?;
            w.write_str(" ")?;
            w.write_str(format_item_size(item_size))?;
            w.write_char('[')?;
            write_expr(w, min_y, cx)?;
            w.write_str("...")?;
            write_expr(w, max_y, cx)?;
            w.write_str("][")?;
            write_expr(w, min_x, cx)?;
            w.write_str("...")?;
            write_expr(w, max_x, cx)?;
            w.write_str("] swap=")?;
            write_expr(w, swap, cx)?;
        }
        Stmt::RedimArray {
            var,
            item_size,
            ref min_y,
            ref max_y,
            ref min_x,
            ref max_x,
        } => {
            w.write_str("redim array ")?;
            write_var(w, var, cx)?;
            w.write_str(" ")?;
            w.write_str(format_item_size(item_size))?;
            w.write_char('[')?;
            write_expr(w, min_y, cx)?;
            w.write_str("...")?;
            write_expr(w, max_y, cx)?;
            w.write_str("][")?;
            write_expr(w, min_x, cx)?;
            w.write_str("...")?;
            write_expr(w, max_x, cx)?;
            w.write_char(']')?;
        }
        Stmt::Assign(var, ref expr) => {
            write_var(w, var, cx)?;
            w.write_str(" = ")?;
            write_expr(w, expr, cx)?;
        }
        Stmt::SetArrayItem(var, ref index, ref value) => {
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, index, cx)?;
            w.write_str("] = ")?;
            write_expr(w, value, cx)?;
        }
        Stmt::SetArrayItem2D(var, ref index_y, ref index_x, ref value) => {
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, index_y, cx)?;
            w.write_str("][")?;
            write_expr(w, index_x, cx)?;
            w.write_str("] = ")?;
            write_expr(w, value, cx)?;
        }
        Stmt::Inc(var) => {
            write_var(w, var, cx)?;
            w.write_str("++")?;
        }
        Stmt::Dec(var) => {
            write_var(w, var, cx)?;
            w.write_str("--")?;
        }
        Stmt::Goto(target) => {
            write!(w, "goto 0x{target:x}")?;
        }
        Stmt::If {
            ref condition,
            ref true_,
            ref false_,
        } => {
            w.write_str("if (")?;
            write_expr(w, condition, cx)?;
            writeln!(w, ") {{")?;
            write_stmts(w, true_, indent + 1, cx)?;
            if !false_.is_empty() {
                write_indent(w, indent)?;
                writeln!(w, "}} else {{")?;
                write_stmts(w, false_, indent + 1, cx)?;
            }
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::Case {
            ref value,
            ref cases,
        } => {
            w.write_str("case ")?;
            write_expr(w, value, cx)?;
            writeln!(w, " {{")?;
            for case in cases {
                write_indent(w, indent + 1)?;
                match &case.cond {
                    CaseCond::Eq(value) => {
                        w.write_str("of ")?;
                        write_expr(w, value, cx)?;
                    }
                    CaseCond::In(list) => {
                        w.write_str("in ")?;
                        write_expr(w, list, cx)?;
                    }
                    CaseCond::Else => {
                        w.write_str("else")?;
                    }
                }
                writeln!(w, " {{")?;
                write_stmts(w, &case.body, indent + 2, cx)?;
                write_indent(w, indent + 1)?;
                writeln!(w, "}}")?;
            }
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::While {
            ref condition,
            ref body,
        } => {
            w.write_str("while (")?;
            write_expr(w, condition, cx)?;
            writeln!(w, ") {{")?;
            write_stmts(w, body, indent + 1, cx)?;
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::Do {
            ref body,
            ref condition,
        } => {
            writeln!(w, "do {{")?;
            write_stmts(w, body, indent + 1, cx)?;
            write_indent(w, indent)?;
            w.write_char('}')?;
            if let Some(condition) = condition {
                w.write_str(" until (")?;
                write_expr(w, condition, cx)?;
                w.write_char(')')?;
            }
        }
        Stmt::Generic {
            ref bytecode,
            ins,
            ref args,
        } => {
            write_generic(w, bytecode, ins, args, cx)?;
        }
        Stmt::DecompileError(offset, ref kind) => {
            write_decomile_error(w, offset, kind, cx)?;
        }
    }
    Ok(())
}

fn write_expr(w: &mut impl Write, expr: &Expr, cx: &WriteCx) -> fmt::Result {
    write_expr_as(w, expr, None, cx)
}

#[allow(clippy::too_many_lines)]
fn write_expr_as(
    w: &mut impl Write,
    expr: &Expr,
    emit_as: Option<EmitAs>,
    cx: &WriteCx,
) -> fmt::Result {
    match expr {
        &Expr::Number(n) => {
            'done: loop {
                if emit_as == Some(EmitAs::Script) {
                    if let Some(name) = get_script_name(n, cx) {
                        w.write_str(name)?;
                        w.write_str("/*")?;
                        write!(w, "{n}")?;
                        w.write_str("*/")?;
                        break 'done;
                    }
                }
                write!(w, "{n}")?;
                break 'done;
            }
        }
        Expr::String(s) => {
            write!(w, "{:?}", AnsiStr(s))?;
        }
        &Expr::Variable(var) => {
            write_var(w, var, cx)?;
        }
        Expr::StackDup(expr) => {
            write_expr_as(w, expr, emit_as, cx)?;
        }
        Expr::StackUnderflow => {
            w.write_str("@DECOMPILE ERROR stack underflow")?;
        }
        Expr::List(exprs) => {
            w.write_char('[')?;
            for (i, expr) in exprs.iter().enumerate() {
                if i != 0 {
                    w.write_str(", ")?;
                }
                write_expr(w, expr, cx)?;
            }
            w.write_char(']')?;
        }
        &Expr::ArrayIndex(var, ref index) => {
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, index, cx)?;
            w.write_char(']')?;
        }
        &Expr::ArrayIndex2D(var, ref indices) => {
            let (index1, index2) = &**indices;
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, index1, cx)?;
            w.write_str("][")?;
            write_expr(w, index2, cx)?;
            w.write_char(']')?;
        }
        Expr::Not(expr) => {
            w.write_char('!')?;
            write_expr(w, expr, cx)?;
        }
        Expr::Equal(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" == ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::NotEqual(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" != ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::Greater(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" > ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::Less(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" < ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::LessOrEqual(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" <= ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::GreaterOrEqual(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" >= ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::Add(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" + ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::Sub(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" - ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::Mul(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" * ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::Div(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" / ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::LogicalAnd(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" && ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::LogicalOr(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" || ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::BitwiseAnd(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" & ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::BitwiseOr(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" | ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::In(xs) => {
            write_expr(w, &xs.0, cx)?;
            w.write_str(" in ")?;
            write_expr(w, &xs.1, cx)?;
        }
        Expr::Call(bytecode, ins, args) => {
            write_generic(w, bytecode, ins, args, cx)?;
        }
        &Expr::DecompileError(offset, ref kind) => {
            write_decomile_error(w, offset, kind, cx)?;
        }
    }
    Ok(())
}

fn write_var(w: &mut impl Write, var: Variable, cx: &WriteCx) -> fmt::Result {
    let (scope, number) = (var.0 & 0xf000, var.0 & 0x0fff);
    match scope {
        0x0000 => {
            // global
            if let Some(name) = cx
                .config
                .global_names
                .get(usize::from(number))
                .and_then(Option::as_deref)
            {
                return w.write_str(name);
            }
            return write!(w, "global{number}");
        }
        0x4000 => {
            // local
            write_local_var_name(w, number, cx)
        }
        0x8000 => {
            // room
            return write!(w, "room{number}");
        }
        _ => panic!("bad variable scope bits"),
    }
}

fn write_generic(
    w: &mut impl Write,
    bytecode: &[u8],
    ins: &GenericIns,
    args: &[Expr],
    cx: &WriteCx,
) -> fmt::Result {
    GenericIns::write_name(w, ins, bytecode)?;
    for (i, expr) in args.iter().enumerate() {
        // XXX: assume ins args are at the end of expr args
        let arg = i
            .checked_sub(args.len() - ins.args.len())
            .map(|i| &ins.args[i]);
        let emit_as = match arg {
            Some(GenericArg::IntScript) => Some(EmitAs::Script),
            _ => None,
        };
        w.write_char(' ')?;
        write_expr_as(w, expr, emit_as, cx)?;
    }
    Ok(())
}

fn format_item_size(item_size: ItemSize) -> &'static str {
    match item_size {
        ItemSize::Bit => "bit",
        ItemSize::Byte => "byte",
        ItemSize::I16 => "i16",
        ItemSize::I32 => "i32",
        ItemSize::Char => "char",
    }
}

fn get_script_name<'a>(number: i32, cx: &WriteCx<'a>) -> Option<&'a str> {
    if number < LOCAL_SCRIPT_CUTOFF {
        // Global script
        return cx
            .config
            .scripts
            .get(usize::try_from(number).ok()?)?
            .name
            .as_deref();
    }
    // Local script
    cx.config
        .rooms
        .get(usize::try_from(cx.scope.room()?).ok()?)?
        .scripts
        .get(usize::try_from(number).ok()?)?
        .name
        .as_deref()
}

fn write_local_var_name<'a>(w: &mut impl Write, number: u16, cx: &WriteCx<'a>) -> fmt::Result {
    'have_script: loop {
        let script = match get_script_config(cx) {
            Some(script) => script,
            None => break 'have_script,
        };
        if let Some(name) = script
            .locals
            .get(usize::try_from(number).unwrap())
            .and_then(|v| v.name.as_ref())
        {
            w.write_str(name)?;
            return Ok(());
        }
        if number < script.params.unwrap_or(0) {
            write!(w, "arg{number}")?;
            return Ok(());
        }
        break 'have_script;
    }
    write!(w, "local{number}")?;
    Ok(())
}

fn get_script_config<'a>(cx: &WriteCx<'a>) -> Option<&'a Script> {
    match cx.scope {
        Scope::Global(script) => cx.config.scripts.get(usize::try_from(script).unwrap()),
        Scope::RoomLocal(room, script) => {
            cx.config
                .rooms
                .get(usize::try_from(room).unwrap())
                .map(|r| &r.scripts)
                .and_then(|s| s.get(usize::try_from(script).unwrap()))
        }
        Scope::RoomEnter(_) | Scope::RoomExit(_) | Scope::Verb(_, _) => {
            None // TODO
        }
    }
}

fn write_decomile_error(
    w: &mut impl Write,
    offset: usize,
    kind: &DecompileErrorKind,
    cx: &WriteCx,
) -> fmt::Result {
    write!(w, "@DECOMPILE ERROR near 0x{offset:x} ")?;
    match kind {
        DecompileErrorKind::StackUnderflow => {
            w.write_str("stack underflow")?;
        }
        DecompileErrorKind::StackOrphan(expr) => {
            w.write_str("stack orphan ")?;
            write_expr(w, expr, cx)?;
        }
        DecompileErrorKind::WrongBlockExit => {
            w.write_str("wrong block exit")?;
        }
        DecompileErrorKind::Other(msg) => {
            w.write_str(msg)?;
        }
    }
    Ok(())
}

impl Scope {
    pub fn room(&self) -> Option<i32> {
        match *self {
            Scope::Global(_) => None,
            Scope::RoomLocal(room, _)
            | Scope::RoomEnter(room)
            | Scope::RoomExit(room)
            | Scope::Verb(room, _) => Some(room),
        }
    }
}
