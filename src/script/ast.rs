use crate::script::{
    ins::{GenericIns, ItemSize, Variable},
    misc::{write_indent, AnsiStr},
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
    CursorCharset(Expr<'a>),
    LoadScript(Expr<'a>),
    LockScript(Expr<'a>),
    LoadCharset(Expr<'a>),
    FreeArray(Variable),
    SetWindowTitle(Expr<'a>),
    If {
        condition: Expr<'a>,
        true_: Vec<Stmt<'a>>,
        false_: Vec<Stmt<'a>>,
    },
    While {
        condition: Expr<'a>,
        body: Vec<Stmt<'a>>,
    },
    Generic(&'a GenericIns, Vec<Expr<'a>>),
    Raw2([u8; 2]),
    Raw(&'a [u8]),
}

#[derive(Clone)]
pub enum Expr<'a> {
    Number(i32),
    String(&'a [u8]),
    Variable(Variable),
    List(Vec<Expr<'a>>),
    ArrayIndex(Variable, Box<Expr<'a>>),
    Not(Box<Expr<'a>>),
    Equal(Box<(Expr<'a>, Expr<'a>)>),
    NotEqual(Box<(Expr<'a>, Expr<'a>)>),
    Greater(Box<(Expr<'a>, Expr<'a>)>),
    Less(Box<(Expr<'a>, Expr<'a>)>),
    LessOrEqual(Box<(Expr<'a>, Expr<'a>)>),
    Add(Box<(Expr<'a>, Expr<'a>)>),
    Sub(Box<(Expr<'a>, Expr<'a>)>),
    LogicalAnd(Box<(Expr<'a>, Expr<'a>)>),
    LogicalOr(Box<(Expr<'a>, Expr<'a>)>),
    Call(&'a GenericIns, Vec<Expr<'a>>),
}

pub fn write_stmts(w: &mut impl Write, stmts: &[Stmt], indent: usize) -> fmt::Result {
    for stmt in stmts {
        write_indent(w, indent)?;
        write_stmt(w, stmt, indent)?;
        writeln!(w)?;
    }
    Ok(())
}

#[allow(clippy::too_many_lines)]
fn write_stmt(w: &mut impl Write, stmt: &Stmt, indent: usize) -> fmt::Result {
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
            write_var(w, var)?;
            w.write_str(" ")?;
            w.write_str(format_item_size(item_size))?;
            w.write_char('[')?;
            write_expr(w, min1)?;
            w.write_str("...")?;
            write_expr(w, max1)?;
            w.write_str("][")?;
            write_expr(w, min2)?;
            w.write_str("...")?;
            write_expr(w, max2)?;
            w.write_str("] swap=")?;
            write_expr(w, swap)?;
        }
        Stmt::Assign(var, ref expr) => {
            write_var(w, var)?;
            w.write_str(" = ")?;
            write_expr(w, expr)?;
        }
        Stmt::SetArrayItem(var, ref index, ref value) => {
            write_var(w, var)?;
            w.write_char('[')?;
            write_expr(w, index)?;
            w.write_str("] = ")?;
            write_expr(w, value)?;
        }
        Stmt::Inc(var) => {
            write_var(w, var)?;
            w.write_str("++")?;
        }
        Stmt::CursorCharset(ref expr) => {
            w.write_str("cursor-charset ")?;
            write_expr(w, expr)?;
        }
        Stmt::LoadScript(ref expr) => {
            w.write_str("load-script ")?;
            write_expr(w, expr)?;
        }
        Stmt::LockScript(ref expr) => {
            w.write_str("lock-script ")?;
            write_expr(w, expr)?;
        }
        Stmt::LoadCharset(ref expr) => {
            w.write_str("load-charset ")?;
            write_expr(w, expr)?;
        }
        Stmt::FreeArray(var) => {
            w.write_str("free-array ")?;
            write_var(w, var)?;
        }
        Stmt::If {
            ref condition,
            ref true_,
            ref false_,
        } => {
            w.write_str("if (")?;
            write_expr(w, condition)?;
            writeln!(w, ") {{")?;
            write_stmts(w, true_, indent + 1)?;
            if !false_.is_empty() {
                write_indent(w, indent)?;
                writeln!(w, "}} else {{")?;
                write_stmts(w, false_, indent + 1)?;
            }
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::While {
            ref condition,
            ref body,
        } => {
            w.write_str("while (")?;
            write_expr(w, condition)?;
            writeln!(w, ") {{")?;
            write_stmts(w, body, indent + 1)?;
            write_indent(w, indent)?;
            write!(w, "}}")?;
        }
        Stmt::SetWindowTitle(ref expr) => {
            w.write_str("set-window-title ")?;
            write_expr(w, expr)?;
        }
        Stmt::Generic(ins, ref exprs) => {
            w.write_str(ins.name)?;
            for expr in exprs {
                w.write_char(' ')?;
                write_expr(w, expr)?;
            }
        }
        Stmt::Raw2([b1, b2]) => {
            write!(w, ".db 0x{b1:02x},0x{b2:02x}")?;
        }
        Stmt::Raw(bytes) => {
            w.write_str(".db ")?;
            for (i, &b) in bytes.iter().enumerate() {
                if i != 0 {
                    w.write_char(',')?;
                }
                write!(w, "0x{b:02x}")?;
            }
        }
    }
    Ok(())
}

fn write_expr(w: &mut impl Write, expr: &Expr) -> fmt::Result {
    match expr {
        Expr::Number(n) => {
            write!(w, "{n}")?;
        }
        Expr::String(s) => {
            write!(w, "{:?}", AnsiStr(s))?;
        }
        &Expr::Variable(var) => {
            write_var(w, var)?;
        }
        Expr::List(exprs) => {
            w.write_char('[')?;
            for (i, expr) in exprs.iter().enumerate() {
                if i != 0 {
                    w.write_str(", ")?;
                }
                write_expr(w, expr)?;
            }
            w.write_char(']')?;
        }
        &Expr::ArrayIndex(var, ref index) => {
            write_var(w, var)?;
            w.write_char('[')?;
            write_expr(w, index)?;
            w.write_char(']')?;
        }
        Expr::Not(expr) => {
            w.write_char('!')?;
            write_expr(w, expr)?;
        }
        Expr::Equal(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" == ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::NotEqual(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" != ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::Greater(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" > ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::Less(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" < ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::LessOrEqual(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" <= ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::Add(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" + ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::Sub(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" - ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::LogicalAnd(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" && ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::LogicalOr(xs) => {
            write_expr(w, &xs.0)?;
            w.write_str(" || ")?;
            write_expr(w, &xs.1)?;
        }
        Expr::Call(ins, args) => {
            w.write_str(ins.name)?;
            for expr in args {
                w.write_char(' ')?;
                write_expr(w, expr)?;
            }
        }
    }
    Ok(())
}

fn write_var(w: &mut impl Write, var: Variable) -> fmt::Result {
    write!(w, "var{}", var.0)
}

fn format_item_size(item_size: ItemSize) -> &'static str {
    match item_size {
        ItemSize::Byte => "byte",
        ItemSize::I16 => "i16",
    }
}
