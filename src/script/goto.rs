use crate::script::{
    ast::{DecompileErrorKind, Scripto, Stmt, StmtBlock},
    visit::{default_visit_stmt, Visitor},
};
use tracing::trace;

pub fn add_labels_for_gotos(script: &Scripto, ast: &mut StmtBlock) {
    let mut collect = CollectGotoTargets {
        targets: Vec::new(),
    };
    collect.block(script, ast);
    let mut targets = collect.targets;
    if targets.is_empty() {
        return;
    }

    targets.sort_unstable_by_key(|n| usize::MAX - n);
    prepend_label_stmts(ast, &mut targets);

    if !targets.is_empty() {
        trace!("failed to label {:04x?}", targets);
        ast.insert(
            0,
            0,
            Stmt::DecompileError(0, DecompileErrorKind::Other("found goto without label")),
        );
    }
}

struct CollectGotoTargets {
    targets: Vec<usize>,
}

impl Visitor for CollectGotoTargets {
    fn stmt(&mut self, script: &Scripto, stmt: &Stmt) {
        if let &Stmt::Goto(target) = stmt {
            if !self.targets.contains(&target) {
                self.targets.push(target);
            }
        }
        default_visit_stmt(script, stmt, self);
    }
}

fn prepend_label_stmts(block: &mut StmtBlock, targets: &mut Vec<usize>) {
    let mut i = 0;
    while i < block.stmts.len() {
        let &next_target = match targets.last() {
            Some(target) => target,
            None => return, // no labels left means nothing more to do
        };

        if block.addrs[i] == next_target {
            targets.pop();
            block.insert(i, next_target, Stmt::Label);
            continue;
        }

        match &mut block.stmts[i] {
            Stmt::If {
                condition: _,
                true_,
                false_,
            } => {
                prepend_label_stmts(true_, targets);
                prepend_label_stmts(false_, targets);
            }
            Stmt::While { condition: _, body } => prepend_label_stmts(body, targets),
            Stmt::Do { body, condition: _ } => {
                prepend_label_stmts(body, targets);
            }
            Stmt::Case { value: _, cases } => {
                for case in cases {
                    prepend_label_stmts(&mut case.body, targets);
                }
            }
            _ => {}
        }

        i += 1;
    }
}
