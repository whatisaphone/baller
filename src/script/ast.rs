use crate::{
    config::{Config, EnumId, Script},
    script::{
        ins::{GenericArg, GenericIns, ItemSize, Variable},
        misc::{write_indent, AnsiStr},
    },
    utils::byte_array::ByteArray,
};
use std::{fmt, fmt::Write};

#[derive(Default)]
pub struct Scripto<'a> {
    pub exprs: Vec<Expr<'a>>,
}

#[derive(Default)]
pub struct StmtBlock<'a> {
    pub addrs: Vec<usize>,
    pub stmts: Vec<Stmt<'a>>,
}

pub enum Stmt<'a> {
    DimArray {
        var: Variable,
        item_size: ItemSize,
        min_y: ExprId,
        max_y: ExprId,
        min_x: ExprId,
        max_x: ExprId,
        swap: ExprId,
    },
    RedimArray {
        var: Variable,
        item_size: ItemSize,
        min_y: ExprId,
        max_y: ExprId,
        min_x: ExprId,
        max_x: ExprId,
    },
    Assign(Variable, ExprId),
    SetArrayItem(Variable, ExprId, ExprId),
    SetArrayItem2D(Variable, ExprId, ExprId, ExprId),
    Inc(Variable),
    Dec(Variable),
    Goto(usize),
    Label,
    If {
        condition: ExprId,
        true_: StmtBlock<'a>,
        false_: StmtBlock<'a>,
    },
    While {
        condition: ExprId,
        body: StmtBlock<'a>,
    },
    Do {
        body: StmtBlock<'a>,
        condition: Option<ExprId>,
    },
    Case {
        value: ExprId,
        cases: Vec<Case<'a>>,
    },
    Generic {
        bytecode: ByteArray<2>,
        ins: &'a GenericIns,
        args: Vec<ExprId>,
    },
    DecompileError(usize, DecompileErrorKind),
}

pub enum DecompileErrorKind {
    StackUnderflow,
    StackOrphan(ExprId),
    WrongBlockExit,
    Other(&'static str),
}

pub struct Case<'a> {
    pub cond: CaseCond,
    pub body: StmtBlock<'a>,
}

pub enum CaseCond {
    Eq(ExprId),
    In(ExprId),
    Else,
}

pub type ExprId = usize;

pub enum Expr<'a> {
    Number(i32),
    String(&'a [u8]),
    Variable(Variable),
    StackDup(ExprId),
    StackUnderflow,
    List(Vec<ExprId>),
    ArrayIndex(Variable, ExprId),
    ArrayIndex2D(Variable, ExprId, ExprId),
    Not(ExprId),
    Equal(ExprId, ExprId),
    NotEqual(ExprId, ExprId),
    Greater(ExprId, ExprId),
    Less(ExprId, ExprId),
    LessOrEqual(ExprId, ExprId),
    GreaterOrEqual(ExprId, ExprId),
    Add(ExprId, ExprId),
    Sub(ExprId, ExprId),
    Mul(ExprId, ExprId),
    Div(ExprId, ExprId),
    LogicalAnd(ExprId, ExprId),
    LogicalOr(ExprId, ExprId),
    BitwiseAnd(ExprId, ExprId),
    BitwiseOr(ExprId, ExprId),
    In(ExprId, ExprId),
    Call(ByteArray<2>, &'a GenericIns, Vec<ExprId>),
    EnumConst(EnumId, i32),
    DecompileError(usize, DecompileErrorKind),
}

pub struct WriteCx<'a> {
    pub scope: Scope,
    pub config: &'a Config,
}

#[derive(Copy, Clone)]
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

impl<'a> StmtBlock<'a> {
    pub fn push(&mut self, addr: usize, stmt: Stmt<'a>) {
        self.addrs.push(addr);
        self.stmts.push(stmt);
    }

    pub fn insert(&mut self, index: usize, addr: usize, stmt: Stmt<'a>) {
        self.addrs.insert(index, addr);
        self.stmts.insert(index, stmt);
    }

    pub fn remove(&mut self, index: usize) {
        self.addrs.remove(index);
        self.stmts.remove(index);
    }
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

pub fn write_block(
    w: &mut impl Write,
    script: &Scripto,
    block: &StmtBlock,
    indent: usize,
    cx: &WriteCx,
) -> fmt::Result {
    debug_assert!(block.stmts.len() == block.addrs.len());
    for i in 0..block.stmts.len() {
        write_stmt(w, script, block.addrs[i], &block.stmts[i], indent, cx)?;
        writeln!(w)?;
    }
    Ok(())
}

#[allow(clippy::too_many_lines)]
fn write_stmt(
    w: &mut impl Write,
    script: &Scripto,
    addr: usize,
    stmt: &Stmt,
    indent: usize,
    cx: &WriteCx,
) -> fmt::Result {
    match *stmt {
        Stmt::DimArray {
            var,
            item_size,
            min_y,
            max_y,
            min_x,
            max_x,
            swap,
        } => {
            write_indent(w, indent)?;
            w.write_str("dim array ")?;
            write_var(w, var, cx)?;
            w.write_str(" ")?;
            w.write_str(format_item_size(item_size))?;
            w.write_char('[')?;
            write_expr(w, script, min_y, cx)?;
            w.write_str("...")?;
            write_expr(w, script, max_y, cx)?;
            w.write_str("][")?;
            write_expr(w, script, min_x, cx)?;
            w.write_str("...")?;
            write_expr(w, script, max_x, cx)?;
            w.write_str("] swap=")?;
            write_expr(w, script, swap, cx)?;
        }
        Stmt::RedimArray {
            var,
            item_size,
            min_y,
            max_y,
            min_x,
            max_x,
        } => {
            write_indent(w, indent)?;
            w.write_str("redim array ")?;
            write_var(w, var, cx)?;
            w.write_str(" ")?;
            w.write_str(format_item_size(item_size))?;
            w.write_char('[')?;
            write_expr(w, script, min_y, cx)?;
            w.write_str("...")?;
            write_expr(w, script, max_y, cx)?;
            w.write_str("][")?;
            write_expr(w, script, min_x, cx)?;
            w.write_str("...")?;
            write_expr(w, script, max_x, cx)?;
            w.write_char(']')?;
        }
        Stmt::Assign(var, expr) => {
            write_indent(w, indent)?;
            write_var(w, var, cx)?;
            w.write_str(" = ")?;
            write_expr(w, script, expr, cx)?;
        }
        Stmt::SetArrayItem(var, index, value) => {
            write_indent(w, indent)?;
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, script, index, cx)?;
            w.write_str("] = ")?;
            write_expr(w, script, value, cx)?;
        }
        Stmt::SetArrayItem2D(var, index_y, index_x, value) => {
            write_indent(w, indent)?;
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, script, index_y, cx)?;
            w.write_str("][")?;
            write_expr(w, script, index_x, cx)?;
            w.write_str("] = ")?;
            write_expr(w, script, value, cx)?;
        }
        Stmt::Inc(var) => {
            write_indent(w, indent)?;
            write_var(w, var, cx)?;
            w.write_str("++")?;
        }
        Stmt::Dec(var) => {
            write_indent(w, indent)?;
            write_var(w, var, cx)?;
            w.write_str("--")?;
        }
        Stmt::Goto(target) => {
            write_indent(w, indent)?;
            w.write_str("goto ")?;
            write_label(w, target)?;
        }
        Stmt::Label => {
            write_label(w, addr)?;
            w.write_char(':')?;
        }
        Stmt::If {
            condition,
            ref true_,
            ref false_,
        } => {
            write_indent(w, indent)?;
            w.write_str("if (")?;
            write_expr(w, script, condition, cx)?;
            writeln!(w, ") {{")?;
            write_block(w, script, true_, indent + 1, cx)?;
            write_if_else(w, script, false_, indent, cx)?;
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::Case { value, ref cases } => {
            write_indent(w, indent)?;
            w.write_str("case ")?;
            write_expr(w, script, value, cx)?;
            writeln!(w, " {{")?;
            for case in cases {
                write_indent(w, indent + 1)?;
                match case.cond {
                    CaseCond::Eq(value) => {
                        w.write_str("of ")?;
                        write_expr(w, script, value, cx)?;
                    }
                    CaseCond::In(list) => {
                        w.write_str("in ")?;
                        write_expr(w, script, list, cx)?;
                    }
                    CaseCond::Else => {
                        w.write_str("else")?;
                    }
                }
                writeln!(w, " {{")?;
                write_block(w, script, &case.body, indent + 2, cx)?;
                write_indent(w, indent + 1)?;
                writeln!(w, "}}")?;
            }
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::While {
            condition,
            ref body,
        } => {
            write_indent(w, indent)?;
            w.write_str("while (")?;
            write_expr(w, script, condition, cx)?;
            writeln!(w, ") {{")?;
            write_block(w, script, body, indent + 1, cx)?;
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::Do {
            ref body,
            condition,
        } => {
            write_indent(w, indent)?;
            writeln!(w, "do {{")?;
            write_block(w, script, body, indent + 1, cx)?;
            write_indent(w, indent)?;
            w.write_char('}')?;
            if let Some(condition) = condition {
                w.write_str(" until (")?;
                write_expr(w, script, condition, cx)?;
                w.write_char(')')?;
            }
        }
        Stmt::Generic {
            ref bytecode,
            ins,
            ref args,
        } => {
            write_indent(w, indent)?;
            write_generic(w, script, bytecode, ins, args, cx)?;
        }
        Stmt::DecompileError(offset, ref kind) => {
            write_indent(w, indent)?;
            write_decomile_error(w, script, offset, kind, cx)?;
        }
    }
    Ok(())
}

fn write_if_else(
    w: &mut impl Write,
    script: &Scripto,
    block: &StmtBlock,
    indent: usize,
    cx: &WriteCx,
) -> fmt::Result {
    if block.stmts.is_empty() {
        return Ok(());
    }

    if block.stmts.len() == 1 {
        if let Stmt::If {
            condition,
            ref true_,
            ref false_,
        } = block.stmts[0]
        {
            write_indent(w, indent)?;
            w.write_str("} else if (")?;
            write_expr(w, script, condition, cx)?;
            writeln!(w, ") {{")?;
            write_block(w, script, true_, indent + 1, cx)?;
            write_if_else(w, script, false_, indent, cx)?;
            return Ok(());
        }
    }

    write_indent(w, indent)?;
    writeln!(w, "}} else {{")?;
    write_block(w, script, block, indent + 1, cx)?;
    Ok(())
}

fn write_expr(w: &mut impl Write, script: &Scripto, id: ExprId, cx: &WriteCx) -> fmt::Result {
    write_expr_as(w, script, id, None, cx)
}

#[allow(clippy::too_many_lines)]
fn write_expr_as(
    w: &mut impl Write,
    script: &Scripto,
    id: ExprId,
    emit_as: Option<EmitAs>,
    cx: &WriteCx,
) -> fmt::Result {
    match &script.exprs[id] {
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
        &Expr::StackDup(expr) => {
            write_expr_as(w, script, expr, emit_as, cx)?;
        }
        Expr::StackUnderflow => {
            w.write_str("@DECOMPILE ERROR stack underflow")?;
        }
        Expr::List(exprs) => {
            w.write_char('[')?;
            for (i, &expr) in exprs.iter().enumerate() {
                if i != 0 {
                    w.write_str(", ")?;
                }
                write_expr(w, script, expr, cx)?;
            }
            w.write_char(']')?;
        }
        &Expr::ArrayIndex(var, x) => {
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, script, x, cx)?;
            w.write_char(']')?;
        }
        &Expr::ArrayIndex2D(var, y, x) => {
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, script, y, cx)?;
            w.write_str("][")?;
            write_expr(w, script, x, cx)?;
            w.write_char(']')?;
        }
        &Expr::Not(expr) => {
            w.write_char('!')?;
            write_expr(w, script, expr, cx)?;
        }
        &Expr::Equal(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" == ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::NotEqual(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" != ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::Greater(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" > ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::Less(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" < ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::LessOrEqual(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" <= ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::GreaterOrEqual(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" >= ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::Add(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" + ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::Sub(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" - ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::Mul(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" * ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::Div(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" / ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::LogicalAnd(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" && ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::LogicalOr(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" || ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::BitwiseAnd(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" & ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::BitwiseOr(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" | ")?;
            write_expr(w, script, rhs, cx)?;
        }
        &Expr::In(lhs, rhs) => {
            write_expr(w, script, lhs, cx)?;
            w.write_str(" in ")?;
            write_expr(w, script, rhs, cx)?;
        }
        Expr::Call(bytecode, ins, args) => {
            write_generic(w, script, bytecode, ins, args, cx)?;
        }
        &Expr::EnumConst(enum_id, value) => {
            let name = &cx.config.enums[enum_id].values[&value];
            write!(w, "{name}/*{value}*/")?;
        }
        &Expr::DecompileError(offset, ref kind) => {
            write_decomile_error(w, script, offset, kind, cx)?;
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
            if let Some(room) = cx.scope.room() {
                if let Some(name) = cx
                    .config
                    .rooms
                    .get(usize::try_from(room).unwrap())
                    .and_then(|r| r.vars.get(usize::from(number)))
                    .and_then(|v| v.name.as_deref())
                {
                    return w.write_str(name);
                }
            }
            // room
            return write!(w, "room{number}");
        }
        _ => panic!("bad variable scope bits"),
    }
}

fn write_generic(
    w: &mut impl Write,
    script: &Scripto,
    bytecode: &[u8],
    ins: &GenericIns,
    args: &[ExprId],
    cx: &WriteCx,
) -> fmt::Result {
    GenericIns::write_name(w, ins, bytecode)?;
    for (i, &expr) in args.iter().enumerate() {
        // XXX: assume ins args are at the end of expr args
        let arg = i
            .checked_sub(args.len() - ins.args.len())
            .map(|i| &ins.args[i]);
        let emit_as = match arg {
            Some(GenericArg::IntScript) => Some(EmitAs::Script),
            _ => None,
        };
        w.write_char(' ')?;
        write_expr_as(w, script, expr, emit_as, cx)?;
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

pub fn get_script_config<'a>(cx: &WriteCx<'a>) -> Option<&'a Script> {
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

fn write_label(w: &mut impl Write, addr: usize) -> fmt::Result {
    write!(w, "label{addr:04x}")
}

fn write_decomile_error(
    w: &mut impl Write,
    script: &Scripto,
    offset: usize,
    kind: &DecompileErrorKind,
    cx: &WriteCx,
) -> fmt::Result {
    write!(w, "@DECOMPILE ERROR near 0x{offset:x} ")?;
    match *kind {
        DecompileErrorKind::StackUnderflow => {
            w.write_str("stack underflow")?;
        }
        DecompileErrorKind::StackOrphan(expr) => {
            w.write_str("stack orphan ")?;
            write_expr(w, script, expr, cx)?;
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
