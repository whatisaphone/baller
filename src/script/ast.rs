use crate::script::{
    ins::{ItemSize, Variable},
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
    CursorCharset(Expr<'a>),
    LoadScript(Expr<'a>),
    LockScript(Expr<'a>),
    LoadCharset(Expr<'a>),
    FreeArray(Variable),
    SetWindowTitle(Expr<'a>),
    Raw2([u8; 2]),
    Raw(&'a [u8]),
}

#[derive(Clone)]
pub enum Expr<'a> {
    Number(i32),
    String(&'a [u8]),
    Variable(Variable),
}

pub fn write_block(w: &mut impl Write, stmts: &[Stmt], indent: usize) -> fmt::Result {
    for stmt in stmts {
        write_indent(w, indent)?;
        write_stmt(w, stmt)?;
        writeln!(w)?;
    }
    Ok(())
}

fn write_stmt(w: &mut impl Write, stmt: &Stmt) -> fmt::Result {
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
        Stmt::SetWindowTitle(ref expr) => {
            w.write_str("set-window-title ")?;
            write_expr(w, expr)?;
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
    match *expr {
        Expr::Number(n) => write!(w, "{n}"),
        Expr::String(s) => write!(w, "{:?}", AnsiStr(s)),
        Expr::Variable(var) => write_var(w, var),
    }
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
