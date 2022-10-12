use crate::script::ast::{Expr, Scripto, Stmt, StmtBlock};
use std::mem;

pub fn peep(script: &Scripto, root: &mut StmtBlock) {
    let mut i = 0;
    while i < root.stmts.len() {
        if build_while(script, root, i) {
            i = i.saturating_sub(1);
        }
        build_for(script, root, i);

        match &mut root.stmts[i] {
            Stmt::If {
                condition: _,
                true_,
                false_,
            } => {
                peep(script, true_);
                peep(script, false_);
            }
            Stmt::While { condition: _, body }
            | Stmt::Do { body, condition: _ }
            | Stmt::For { body, .. } => {
                peep(script, body);
            }
            Stmt::Case { value: _, cases } => {
                for case in cases {
                    peep(script, &mut case.body);
                }
            }
            _ => {}
        }

        i += 1;
    }
}

fn build_while(script: &Scripto, block: &mut StmtBlock, i: usize) -> bool {
    let (body, &mut condition) = match &mut block.stmts[i] {
        Stmt::Do { body, condition } => (body, condition),
        _ => return false,
    };
    if condition.is_some() {
        return false;
    }
    let (&mut condition, true_, false_) = match body.stmts.get_mut(0) {
        Some(Stmt::If {
            condition,
            true_,
            false_,
        }) => (condition, true_, false_),
        _ => return false,
    };
    let condition = match script.exprs[condition] {
        Expr::Not(e) => e,
        _ => return false,
    };
    let goto = match true_.stmts.as_slice() {
        &[Stmt::Goto(goto)] => goto,
        _ => return false,
    };
    if goto != body.end {
        return false;
    }
    if !false_.stmts.is_empty() {
        return false;
    }

    body.remove(0);
    block.stmts[i] = Stmt::While {
        condition,
        body: mem::take(body),
    };
    true
}

fn build_for(script: &Scripto, block: &mut StmtBlock, i: usize) {
    let (var, start) = match block.stmts.get(i) {
        Some(&Stmt::Assign(var, start)) => (var, start),
        _ => return,
    };
    let (&mut condition, body) = match block.stmts.get_mut(i + 1) {
        Some(Stmt::While { condition, body }) => (condition, body),
        _ => return,
    };
    let (lhs, step, end) = match script.exprs[condition] {
        Expr::LessOrEqual(lhs, rhs) => (lhs, 1, rhs),
        Expr::GreaterOrEqual(lhs, rhs) => (lhs, -1, rhs),
        _ => return,
    };
    let cond_var = match script.exprs[lhs] {
        Expr::Variable(var) => var,
        _ => return,
    };
    if cond_var != var {
        return;
    }
    let (inc_var, inc_step) = match body.stmts.last() {
        Some(&Stmt::Inc(v)) => (v, 1),
        Some(&Stmt::Dec(v)) => (v, -1),
        _ => return,
    };
    if inc_var != var || inc_step != step {
        return;
    }

    let mut body = mem::take(body);
    body.pop();
    block.remove(i);
    block.stmts[i] = Stmt::For {
        var,
        start,
        end,
        step,
        body,
    };
}
