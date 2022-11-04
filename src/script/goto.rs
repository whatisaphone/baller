use crate::script::{
    ast::{DecompileErrorKind, Scripto, Stmt, StmtBlock},
    visit::{default_visit_stmt, Visitor},
};
use tracing::trace;

pub fn add_labels_for_gotos(script: &mut Scripto, ast: &mut StmtBlock) {
    let mut collect = CollectGotoTargets {
        targets: Vec::new(),
    };
    collect.block(script, ast);
    let mut targets = collect.targets;
    if targets.is_empty() {
        return;
    }

    targets.sort_unstable_by_key(|n| usize::MAX - n);
    prepend_label_stmts(ast, &mut targets, None);

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
    fn stmt(&mut self, script: &mut Scripto, stmt: &mut Stmt) {
        if let &mut Stmt::Goto(target) = stmt {
            if !self.targets.contains(&target) {
                self.targets.push(target);
            }
        }
        default_visit_stmt(script, stmt, self);
    }
}

fn prepend_label_stmts(block: &mut StmtBlock, targets: &mut Vec<usize>, skip: Option<usize>) {
    let mut i = 0;
    while i < block.stmts.len() {
        if targets.is_empty() {
            return; // no targets means our work here is done
        }

        if skip != Some(block.addrs[i]) && pop_target(block.addrs[i], targets) {
            block.insert(i, block.addrs[i], Stmt::Label);
        }

        let next_skip = block.addrs.get(i + 1).copied().unwrap_or(block.end);

        match &mut block.stmts[i] {
            Stmt::If {
                condition: _,
                true_,
                false_,
            } => {
                let true_skip = false_.addrs.first().copied().unwrap_or(next_skip);
                prepend_label_stmts(true_, targets, Some(true_skip));
                prepend_label_stmts(false_, targets, Some(next_skip));
            }
            Stmt::While { condition: _, body }
            | Stmt::Do { body, .. }
            | Stmt::For { body, .. }
            | Stmt::ForList { body, .. } => {
                prepend_label_stmts(body, targets, Some(next_skip));
            }
            Stmt::Case { value: _, cases } => {
                for case in cases {
                    prepend_label_stmts(&mut case.body, targets, Some(next_skip));
                }
            }
            _ => {}
        }

        i += 1;
    }

    if skip != Some(block.end) && pop_target(block.end, targets) {
        block.push(block.end, Stmt::Label);
    }
}

fn pop_target(addr: usize, targets: &mut Vec<usize>) -> bool {
    if targets.last().copied() != Some(addr) {
        return false;
    }
    targets.pop();
    true
}
