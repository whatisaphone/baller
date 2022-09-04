use crate::script::{ins::Variable, misc::AnsiStr};
use std::{fmt, fmt::Write};

pub enum Stmt<'a> {
    Assign(Variable, Expr<'a>),
    FreeArray(Variable),
    SetWindowTitle(Expr<'a>),
    Raw2([u8; 2]),
}

pub enum Expr<'a> {
    Number(i32),
    String(&'a [u8]),
    Variable(Variable),
}

pub fn format_block(stmts: &[Stmt]) -> String {
    let mut output = String::new();
    for stmt in stmts {
        write_stmt(&mut output, stmt).unwrap();
        output.push('\n');
    }
    output
}

fn write_stmt(w: &mut impl Write, stmt: &Stmt) -> fmt::Result {
    match *stmt {
        Stmt::Assign(var, ref expr) => {
            write_var(w, var)?;
            w.write_str(" = ")?;
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
