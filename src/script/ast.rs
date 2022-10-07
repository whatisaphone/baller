use crate::{
    config::{Config, EnumId, Script, Type, Var},
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
            write_var_decl(w, var, cx)?;
            writeln!(w)?;
        }
        writeln!(w)?;
    }

    if !vars[params_end..].is_empty() {
        for &var in &vars[params_end..] {
            w.write_str("local variable ")?;
            write_var_decl(w, var, cx)?;
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
            write_expr(w, script, Precedence::Delimeter, min_y, cx)?;
            w.write_str("...")?;
            write_expr(w, script, Precedence::Delimeter, max_y, cx)?;
            w.write_str("][")?;
            write_expr(w, script, Precedence::Delimeter, min_x, cx)?;
            w.write_str("...")?;
            write_expr(w, script, Precedence::Delimeter, max_x, cx)?;
            w.write_str("] swap=")?;
            write_expr(w, script, Precedence::Space, swap, cx)?;
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
            write_expr(w, script, Precedence::Delimeter, min_y, cx)?;
            w.write_str("...")?;
            write_expr(w, script, Precedence::Delimeter, max_y, cx)?;
            w.write_str("][")?;
            write_expr(w, script, Precedence::Delimeter, min_x, cx)?;
            w.write_str("...")?;
            write_expr(w, script, Precedence::Delimeter, max_x, cx)?;
            w.write_char(']')?;
        }
        Stmt::Assign(var, expr) => {
            write_indent(w, indent)?;
            write_var(w, var, cx)?;
            w.write_str(" = ")?;
            write_expr(w, script, Precedence::Assign, expr, cx)?;
        }
        Stmt::SetArrayItem(var, index, value) => {
            write_indent(w, indent)?;
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, script, Precedence::Min, index, cx)?;
            w.write_str("] = ")?;
            write_expr(w, script, Precedence::Assign, value, cx)?;
        }
        Stmt::SetArrayItem2D(var, index_y, index_x, value) => {
            write_indent(w, indent)?;
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, script, Precedence::Min, index_y, cx)?;
            w.write_str("][")?;
            write_expr(w, script, Precedence::Min, index_x, cx)?;
            w.write_str("] = ")?;
            write_expr(w, script, Precedence::Assign, value, cx)?;
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
            write_expr(w, script, Precedence::Min, condition, cx)?;
            writeln!(w, ") {{")?;
            write_block(w, script, true_, indent + 1, cx)?;
            write_if_else(w, script, false_, indent, cx)?;
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::Case { value, ref cases } => {
            write_indent(w, indent)?;
            w.write_str("case ")?;
            write_expr(w, script, Precedence::Min, value, cx)?;
            writeln!(w, " {{")?;
            for case in cases {
                write_indent(w, indent + 1)?;
                match case.cond {
                    CaseCond::Eq(value) => {
                        w.write_str("of ")?;
                        write_expr(w, script, Precedence::Min, value, cx)?;
                    }
                    CaseCond::In(list) => {
                        w.write_str("in ")?;
                        write_expr(w, script, Precedence::Min, list, cx)?;
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
            write_expr(w, script, Precedence::Min, condition, cx)?;
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
                write_expr(w, script, Precedence::Min, condition, cx)?;
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
            write_expr(w, script, Precedence::Min, condition, cx)?;
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

fn write_expr(
    w: &mut impl Write,
    script: &Scripto,
    outer_prec: Precedence,
    id: ExprId,
    cx: &WriteCx,
) -> fmt::Result {
    write_expr_as(w, script, outer_prec, id, None, cx)
}

#[allow(clippy::too_many_lines)]
fn write_expr_as(
    w: &mut impl Write,
    script: &Scripto,
    outer_prec: Precedence,
    id: ExprId,
    emit_as: Option<EmitAs>,
    cx: &WriteCx,
) -> fmt::Result {
    let expr_prec = expr_precedence(script, id);
    if need_parens(outer_prec, expr_prec) {
        w.write_char('(')?;
    }
    match &script.exprs[id] {
        &Expr::Number(n) => {
            'done: loop {
                if emit_as == Some(EmitAs::Script) {
                    if let Some(name) = get_script_name(n, cx) {
                        w.write_str(name)?;
                        write_aside_value(w, n)?;
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
            write_expr_as(w, script, Precedence::Min, expr, emit_as, cx)?;
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
                write_expr(w, script, Precedence::Delimeter, expr, cx)?;
            }
            w.write_char(']')?;
        }
        &Expr::ArrayIndex(var, x) => {
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, script, Precedence::Min, x, cx)?;
            w.write_char(']')?;
        }
        &Expr::ArrayIndex2D(var, y, x) => {
            write_var(w, var, cx)?;
            w.write_char('[')?;
            write_expr(w, script, Precedence::Min, y, cx)?;
            w.write_str("][")?;
            write_expr(w, script, Precedence::Min, x, cx)?;
            w.write_char(']')?;
        }
        &Expr::Not(expr) => {
            w.write_char('!')?;
            write_expr(w, script, Precedence::Not, expr, cx)?;
        }
        &Expr::Equal(lhs, rhs) => {
            write_binop(w, script, "==", Precedence::Compare, lhs, rhs, cx)?;
        }
        &Expr::NotEqual(lhs, rhs) => {
            write_binop(w, script, "!=", Precedence::Compare, lhs, rhs, cx)?;
        }
        &Expr::Greater(lhs, rhs) => {
            write_binop(w, script, ">", Precedence::Compare, lhs, rhs, cx)?;
        }
        &Expr::Less(lhs, rhs) => {
            write_binop(w, script, "<", Precedence::Compare, lhs, rhs, cx)?;
        }
        &Expr::LessOrEqual(lhs, rhs) => {
            write_binop(w, script, "<=", Precedence::Compare, lhs, rhs, cx)?;
        }
        &Expr::GreaterOrEqual(lhs, rhs) => {
            write_binop(w, script, ">=", Precedence::Compare, lhs, rhs, cx)?;
        }
        &Expr::Add(lhs, rhs) => {
            write_binop(w, script, "+", Precedence::AddSub, lhs, rhs, cx)?;
        }
        &Expr::Sub(lhs, rhs) => {
            write_binop(w, script, "-", Precedence::AddSub, lhs, rhs, cx)?;
        }
        &Expr::Mul(lhs, rhs) => {
            write_binop(w, script, "*", Precedence::MulDiv, lhs, rhs, cx)?;
        }
        &Expr::Div(lhs, rhs) => {
            write_binop(w, script, "/", Precedence::MulDiv, lhs, rhs, cx)?;
        }
        &Expr::LogicalAnd(lhs, rhs) => {
            write_binop(w, script, "&&", Precedence::LogAnd, lhs, rhs, cx)?;
        }
        &Expr::LogicalOr(lhs, rhs) => {
            write_binop(w, script, "||", Precedence::LogOr, lhs, rhs, cx)?;
        }
        &Expr::BitwiseAnd(lhs, rhs) => {
            write_binop(w, script, "&", Precedence::BitAnd, lhs, rhs, cx)?;
        }
        &Expr::BitwiseOr(lhs, rhs) => {
            write_binop(w, script, "|", Precedence::BitOr, lhs, rhs, cx)?;
        }
        &Expr::In(lhs, rhs) => {
            write_binop(w, script, "in", Precedence::Space, lhs, rhs, cx)?;
        }
        Expr::Call(bytecode, ins, args) => {
            write_generic(w, script, bytecode, ins, args, cx)?;
        }
        &Expr::EnumConst(enum_id, value) => {
            let name = &cx.config.enums[enum_id].values[&value];
            w.write_str(name)?;
            write_aside_value(w, value)?;
        }
        &Expr::DecompileError(offset, ref kind) => {
            write_decomile_error(w, script, offset, kind, cx)?;
        }
    }
    if need_parens(outer_prec, expr_prec) {
        w.write_char(')')?;
    }
    Ok(())
}

fn expr_precedence(script: &Scripto, id: ExprId) -> Precedence {
    match script.exprs[id] {
        Expr::LogicalOr(..) => Precedence::LogOr,
        Expr::LogicalAnd(..) => Precedence::LogAnd,
        Expr::Equal(..)
        | Expr::NotEqual(..)
        | Expr::Greater(..)
        | Expr::Less(..)
        | Expr::LessOrEqual(..)
        | Expr::GreaterOrEqual(..) => Precedence::Compare,
        Expr::BitwiseOr(..) => Precedence::BitOr,
        Expr::BitwiseAnd(..) => Precedence::BitAnd,
        Expr::Add(..) | Expr::Sub(..) => Precedence::AddSub,
        Expr::Mul(..) | Expr::Div(..) => Precedence::MulDiv,
        Expr::Not(..) => Precedence::Not,
        Expr::In(..) | Expr::Call(..) | Expr::StackUnderflow | Expr::DecompileError(..) => {
            Precedence::Space
        }
        Expr::ArrayIndex(..) | Expr::ArrayIndex2D(..) => Precedence::Indexing,
        Expr::Number(..)
        | Expr::String(..)
        | Expr::Variable(..)
        | Expr::List(..)
        | Expr::EnumConst(..) => Precedence::Atom,
        Expr::StackDup(inner) => expr_precedence(script, inner),
    }
}

fn need_parens(outer: Precedence, inner: Precedence) -> bool {
    // Add clarifying parens for spaces since they often look confusing
    if inner == Precedence::Space && outer >= Precedence::LogOr {
        return true;
    }
    inner < outer
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
enum Precedence {
    Min,
    Assign,
    Delimeter, // such as `,` or `...`
    LogOr,
    LogAnd,
    Compare,
    BitOr,
    BitAnd,
    AddSub,
    MulDiv,
    Not,
    Space,
    Indexing,
    Atom,
}

fn write_binop(
    w: &mut impl Write,
    script: &Scripto,
    op: &str,
    prec: Precedence,
    lhs: ExprId,
    rhs: ExprId,
    cx: &WriteCx,
) -> fmt::Result {
    write_expr(w, script, prec, lhs, cx)?;
    write!(w, " {op} ")?;
    write_expr(w, script, prec, rhs, cx)?;
    Ok(())
}

fn write_var(w: &mut impl Write, var: Variable, cx: &WriteCx) -> fmt::Result {
    let (named, _) = resolve_variable(var, cx);
    write!(w, "{named}")
}

fn write_var_decl(w: &mut impl Write, var: Variable, cx: &WriteCx) -> fmt::Result {
    let (named, ty) = resolve_variable(var, cx);
    write!(w, "{named}")?;
    if let Some(ty) = ty {
        w.write_str(": ")?;
        write_type(w, ty, cx)?;
    }
    Ok(())
}

pub fn resolve_variable<'a>(var: Variable, cx: &WriteCx<'a>) -> (VarName<'a>, Option<&'a Type>) {
    let (scope, number) = (var.0 & 0xf000, var.0 & 0x0fff);
    match scope {
        0x0000 => {
            // global
            let name = cx
                .config
                .global_names
                .get(usize::from(number))
                .and_then(Option::as_deref);
            let ty = cx
                .config
                .global_types
                .get(usize::from(number))
                .and_then(Option::as_ref);
            let named = match name {
                Some(name) => VarName::Named(name),
                None => VarName::Numbered("global", number),
            };
            (named, ty)
        }
        0x4000 => {
            // local
            let fallback = (VarName::Numbered("local", number), None);
            let script = match get_script_config(cx) {
                Some(script) => script,
                None => return fallback,
            };
            let (name, ty) = match script.locals.get(usize::from(number)) {
                Some(var) => (var.name.as_deref(), var.ty.as_ref()),
                None => (None, None),
            };
            let params = script.params.unwrap_or(0);
            let named = match name {
                Some(name) => VarName::Named(name),
                None if number < params => VarName::Numbered("arg", number),
                None => fallback.0,
            };
            (named, ty)
        }
        0x8000 => {
            // room
            let fallback = (VarName::Numbered("room", number), None);
            let var = match get_room_var(number, cx) {
                Some(var) => var,
                None => return fallback,
            };
            let named = match &var.name {
                Some(name) => VarName::Named(name),
                None => fallback.0,
            };
            (named, var.ty.as_ref())
        }
        _ => panic!("bad variable scope bits"),
    }
}

fn get_room_var<'a>(number: u16, cx: &WriteCx<'a>) -> Option<&'a Var> {
    let room: usize = cx.scope.room()?.try_into().ok()?;
    cx.config.rooms.get(room)?.vars.get(usize::from(number))
}

pub enum VarName<'a> {
    Named(&'a str),
    Numbered(&'a str, u16),
}

impl fmt::Display for VarName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarName::Named(name) => write!(f, "{name}"),
            VarName::Numbered(kind, number) => write!(f, "{kind}{number}"),
        }
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
        write_expr_as(w, script, Precedence::Space, expr, emit_as, cx)?;
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
    let script = resolve_script(number, cx)?;
    script.name.as_deref()
}

const LOCAL_SCRIPT_CUTOFF: usize = 2048;

pub fn resolve_script<'a>(number: i32, cx: &WriteCx<'a>) -> Option<&'a Script> {
    let number: usize = number.try_into().ok()?;
    if number < LOCAL_SCRIPT_CUTOFF {
        return cx.config.scripts.get(number);
    }
    let room: usize = cx.scope.room()?.try_into().ok()?;
    cx.config.rooms.get(room)?.scripts.get(number)
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

fn write_aside_value(w: &mut impl Write, value: i32) -> fmt::Result {
    w.write_char('{')?;
    write!(w, "{value}")?;
    w.write_char('}')?;
    Ok(())
}

fn write_type(w: &mut impl Write, ty: &Type, cx: &WriteCx) -> fmt::Result {
    match ty {
        &Type::Enum(enum_id) => {
            let name = &cx.config.enums[enum_id].name;
            w.write_str(name)?;
        }
        Type::Array { item, y, x } => {
            if !matches!(**item, Type::Any) {
                write_type(w, item, cx)?;
            }
            w.write_char('[')?;
            if !matches!(**y, Type::Any) {
                write_type(w, y, cx)?;
            }
            w.write_str("][")?;
            if !matches!(**x, Type::Any) {
                write_type(w, x, cx)?;
            }
            w.write_char(']')?;
        }
        _ => todo!(),
    }
    Ok(())
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
            write_expr(w, script, Precedence::Space, expr, cx)?;
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
