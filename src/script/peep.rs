use crate::script::ast::{Scripto, Stmt, StmtBlock};
use std::mem;

pub fn peep(script: &Scripto, root: &mut StmtBlock) {
    let mut i = 0;
    while i < root.stmts.len() {
        build_while(root, i);

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

fn build_while(block: &mut StmtBlock, i: usize) {
    let (&mut condition, true_, false_) = match &mut block.stmts[i] {
        Stmt::If {
            condition,
            true_,
            false_,
        } => (condition, true_, false_),
        _ => return,
    };
    if !false_.stmts.is_empty() {
        return;
    }
    let goto = match &true_.stmts.last() {
        Some(&Stmt::Goto(goto)) => goto,
        _ => return,
    };
    if goto != block.addrs[i] {
        return;
    }

    true_.pop();
    block.stmts[i] = Stmt::While {
        condition,
        body: mem::take(true_),
    };
}
