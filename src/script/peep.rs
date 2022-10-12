use crate::script::ast::{Expr, Scripto, Stmt, StmtBlock};
use std::mem;

pub fn peep(script: &Scripto, root: &mut StmtBlock) {
    let mut i = 0;
    while i < root.stmts.len() {
        build_while(script, root, i);

        match &mut root.stmts[i] {
            Stmt::If {
                condition: _,
                true_,
                false_,
            } => {
                peep(script, true_);
                peep(script, false_);
            }
            Stmt::While { condition: _, body } | Stmt::Do { body, condition: _ } => {
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

fn build_while(script: &Scripto, block: &mut StmtBlock, i: usize) {
    let (body, &mut condition) = match &mut block.stmts[i] {
        Stmt::Do { body, condition } => (body, condition),
        _ => return,
    };
    if condition.is_some() {
        return;
    }
    let (&mut condition, true_, false_) = match body.stmts.get_mut(0) {
        Some(Stmt::If {
            condition,
            true_,
            false_,
        }) => (condition, true_, false_),
        _ => return,
    };
    let condition = match script.exprs[condition] {
        Expr::Not(e) => e,
        _ => return,
    };
    let goto = match true_.stmts.as_slice() {
        &[Stmt::Goto(goto)] => goto,
        _ => return,
    };
    if goto != body.end {
        return;
    }
    if !false_.stmts.is_empty() {
        return;
    }

    body.remove(0);
    block.stmts[i] = Stmt::While {
        condition,
        body: mem::take(body),
    };
}
