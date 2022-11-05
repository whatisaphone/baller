use crate::script::ast::{
    Case,
    CaseCond,
    DecompileErrorKind,
    Expr,
    ExprId,
    Scripto,
    Stmt,
    StmtBlock,
};
use std::mem;

pub fn build_cases(script: &Scripto, block: &mut StmtBlock) {
    let mut i = 0;
    while i < block.stmts.len() {
        if is_case(script, &block.stmts[i])
            && matches!(
                i.checked_sub(1).and_then(|im1| block.stmts.get(im1)),
                Some(Stmt::DecompileError(_, DecompileErrorKind::StackOrphan(_)))
            )
        {
            // Cases are always paired with a stack error which we can remove once the
            // pattern is recognized.
            block.remove(i - 1);
            i -= 1;
            build_case(script, &mut block.stmts[i]);
        }

        match &mut block.stmts[i] {
            Stmt::If {
                condition: _,
                true_,
                false_,
            } => {
                build_cases(script, true_);
                build_cases(script, false_);
            }
            Stmt::While { condition: _, body } | Stmt::Do { body, .. } => {
                build_cases(script, body);
            }
            Stmt::Case { value: _, cases } => {
                for case in cases {
                    build_cases(script, &mut case.body);
                }
            }
            _ => {}
        }

        i += 1;
    }
}

fn is_case(script: &Scripto, stmt: &Stmt) -> bool {
    let &Stmt::If {
        condition,
        ref true_,
        ref false_,
    } = stmt else { return false };
    match script.exprs[condition] {
        Expr::Equal(lhs, _rhs) | Expr::In(lhs, _rhs) => {
            if !matches!(script.exprs[lhs], Expr::StackDup(_)) {
                return false;
            }
        }
        _ => return false,
    }
    if !matches!(
        true_.stmts.first(),
        Some(Stmt::DecompileError(_, DecompileErrorKind::StackUnderflow)),
    ) {
        return false;
    }
    // Check for another case
    if false_.stmts.len() == 1 && is_case(script, &false_.stmts[0]) {
        return true;
    }
    // Check for terminal else
    if matches!(
        false_.stmts.get(0),
        Some(Stmt::DecompileError(_, DecompileErrorKind::StackUnderflow)),
    ) {
        return true;
    }
    false
}

fn build_case(script: &Scripto, stmt: &mut Stmt) {
    let mut value = None;
    let mut cases = Vec::new();
    append_case(script, stmt, &mut value, &mut cases);
    *stmt = Stmt::Case {
        value: value.unwrap(),
        cases,
    };
}

fn append_case<'a>(
    script: &Scripto,
    stmt: &mut Stmt<'a>,
    value: &mut Option<ExprId>,
    cases: &mut Vec<Case<'a>>,
) {
    let &mut Stmt::If {
        condition,
        ref mut true_,
        ref mut false_,
    } = stmt else { unreachable!() };
    let cond = match script.exprs[condition] {
        Expr::Equal(lhs, rhs) => {
            debug_assert!(matches!(script.exprs[lhs], Expr::StackDup(_)));
            if value.is_none() {
                *value = Some(lhs);
            }
            CaseCond::Eq(rhs)
        }
        Expr::In(lhs, rhs) => {
            debug_assert!(matches!(script.exprs[lhs], Expr::StackDup(_)));
            if value.is_none() {
                *value = Some(lhs);
            }
            CaseCond::In(rhs)
        }
        _ => unreachable!(),
    };

    debug_assert!(matches!(
        true_.stmts.first(),
        Some(Stmt::DecompileError(_, DecompileErrorKind::StackUnderflow)),
    ));
    true_.remove(0);

    cases.push(Case {
        cond,
        body: mem::take(true_),
    });

    // Append another case
    if false_.stmts.len() == 1 && matches!(false_.stmts[0], Stmt::If { .. }) {
        append_case(script, &mut false_.stmts[0], value, cases);
        return;
    }
    // Append terminal else
    debug_assert!(matches!(
        false_.stmts[0],
        Stmt::DecompileError(_, DecompileErrorKind::StackUnderflow),
    ));
    false_.remove(0);
    if !false_.stmts.is_empty() {
        cases.push(Case {
            cond: CaseCond::Else,
            body: mem::take(false_),
        });
    }
}
