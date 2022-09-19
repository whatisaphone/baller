use crate::{
    config::Config,
    script::{
        ins::{GenericIns, ItemSize, Variable},
        misc::{write_indent, AnsiStr},
    },
    utils::byte_array::ByteArray,
};
use std::{fmt, fmt::Write};

pub enum Stmt<'a> {
    DimArray {
        var: Variable,
        item_size: ItemSize,
        min1: Expr<'a>,
        max1: Expr<'a>,
        min2: Expr<'a>,
        max2: Expr<'a>,
        swap: Expr<'a>,
    },
    Assign(Variable, Expr<'a>),
    SetArrayItem(Variable, Expr<'a>, Expr<'a>),
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
    Case {
        value: Expr<'a>,
        cases: Vec<Case<'a>>,
    },
    Generic {
        bytecode: ByteArray<2>,
        ins: &'a GenericIns,
        args: Vec<Expr<'a>>,
    },
    DecompileError(usize, DecompileErrorKind),
}

#[derive(Copy, Clone, Debug)]
pub enum DecompileErrorKind {
    StackUnderflow,
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
    DecompileError(usize, DecompileErrorKind),
}

pub struct WriteCx<'a> {
    pub config: &'a Config,
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
            ref min1,
            ref max1,
            ref min2,
            ref max2,
            ref swap,
        } => {
            w.write_str("dim array ")?;
            write_var(w, var, cx)?;
            w.write_str(" ")?;
            w.write_str(format_item_size(item_size))?;
            w.write_char('[')?;
            write_expr(w, min1, cx)?;
            w.write_str("...")?;
            write_expr(w, max1, cx)?;
            w.write_str("][")?;
            write_expr(w, min2, cx)?;
            w.write_str("...")?;
            write_expr(w, max2, cx)?;
            w.write_str("] swap=")?;
            write_expr(w, swap, cx)?;
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
            writeln!(w, "}}")?;
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
        Stmt::Generic {
            ref bytecode,
            ins,
            ref args,
        } => {
            GenericIns::write_name(w, ins, bytecode)?;
            for expr in args {
                w.write_char(' ')?;
                write_expr(w, expr, cx)?;
            }
        }
        Stmt::DecompileError(offset, kind) => {
            write_decomile_error(w, offset, kind)?;
        }
    }
    Ok(())
}

#[allow(clippy::too_many_lines)]
fn write_expr(w: &mut impl Write, expr: &Expr, cx: &WriteCx) -> fmt::Result {
    match expr {
        Expr::Number(n) => {
            write!(w, "{n}")?;
        }
        Expr::String(s) => {
            write!(w, "{:?}", AnsiStr(s))?;
        }
        &Expr::Variable(var) => {
            write_var(w, var, cx)?;
        }
        Expr::StackDup(expr) => {
            write_expr(w, expr, cx)?;
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
            w.write_str(", ")?;
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
            GenericIns::write_name(w, ins, bytecode)?;
            for expr in args {
                w.write_char(' ')?;
                write_expr(w, expr, cx)?;
            }
        }
        &Expr::DecompileError(offset, kind) => {
            write_decomile_error(w, offset, kind)?;
        }
    }
    Ok(())
}

fn write_var(w: &mut impl Write, var: Variable, cx: &WriteCx) -> fmt::Result {
    let (scope, index) = (var.0 & 0xf000, var.0 & 0x0fff);
    if scope == 0x0000 {
        // global
        if let Some(name) = cx.config.globals.get(&index) {
            return w.write_str(name);
        }
    }
    write!(w, "{}", var)
}

fn format_item_size(item_size: ItemSize) -> &'static str {
    match item_size {
        ItemSize::Byte => "byte",
        ItemSize::I16 => "i16",
        ItemSize::I32 => "i32",
    }
}

fn write_decomile_error(
    w: &mut impl Write,
    offset: usize,
    kind: DecompileErrorKind,
) -> fmt::Result {
    let message = match kind {
        DecompileErrorKind::StackUnderflow => "stack underflow",
        DecompileErrorKind::WrongBlockExit => "wrong block exit",
        DecompileErrorKind::Other(msg) => msg,
    };
    write!(w, "@DECOMPILE ERROR near 0x{offset:x} {message}")
}
