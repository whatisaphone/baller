use crate::script::ast::{Case, CaseCond, DecompileErrorKind, Expr, Stmt};
use std::mem;

pub fn build_cases(stmts: &mut Vec<Stmt>) {
    let mut i = 0;
    while i < stmts.len() {
        if is_case(&stmts[i])
            && matches!(
                i.checked_sub(1).and_then(|im1| stmts.get(im1)),
                Some(Stmt::DecompileError(_, DecompileErrorKind::StackOrphan(_)))
            )
        {
            // Cases are always paired with a stack error which we can remove once the
            // pattern is recognized.
            stmts.remove(i - 1);
            i -= 1;
            build_case(&mut stmts[i]);
        }

        match &mut stmts[i] {
            Stmt::If {
                condition: _,
                true_,
                false_,
            } => {
                build_cases(true_);
                build_cases(false_);
            }
            Stmt::While { condition: _, body } => {
                build_cases(body);
            }
            Stmt::Case { value: _, cases } => {
                for case in cases {
                    build_cases(&mut case.body);
                }
            }
            _ => {}
        }

        i += 1;
    }
}

fn is_case(stmt: &Stmt) -> bool {
    let (condition, true_, false_) = match stmt {
        Stmt::If {
            condition,
            true_,
            false_,
        } => (condition, true_, false_),
        _ => return false,
    };
    match condition {
        Expr::Equal(ops) | Expr::In(ops) => {
            let (lhs, _rhs) = &**ops;
            if !matches!(lhs, Expr::StackDup(_)) {
                return false;
            }
        }
        _ => return false,
    }
    if !matches!(
        true_.first(),
        Some(Stmt::DecompileError(_, DecompileErrorKind::StackUnderflow)),
    ) {
        return false;
    }
    // Check for another case
    if false_.len() == 1 && is_case(&false_[0]) {
        return true;
    }
    // Check for terminal else
    if matches!(
        &false_[0],
        Stmt::DecompileError(_, DecompileErrorKind::StackUnderflow),
    ) {
        return true;
    }
    false
}

fn build_case(stmt: &mut Stmt) {
    let mut value = None;
    let mut cases = Vec::new();
    append_case(stmt, &mut value, &mut cases);
    *stmt = Stmt::Case {
        value: value.unwrap(),
        cases,
    };
}

fn append_case<'a>(stmt: &mut Stmt<'a>, value: &mut Option<Expr<'a>>, cases: &mut Vec<Case<'a>>) {
    let (condition, true_, false_) = match stmt {
        Stmt::If {
            condition,
            true_,
            false_,
        } => (condition, true_, false_),
        _ => unreachable!(),
    };
    let cond = match condition {
        Expr::Equal(ops) => {
            let (lhs, rhs) = &mut **ops;
            debug_assert!(matches!(lhs, Expr::StackDup(_)));
            if value.is_none() {
                *value = Some(lhs.clone());
            }
            // leave dummy value behind
            CaseCond::Eq(mem::replace(rhs, Expr::Number(0)))
        }
        Expr::In(ops) => {
            let (lhs, rhs) = &mut **ops;
            debug_assert!(matches!(lhs, Expr::StackDup(_)));
            if value.is_none() {
                *value = Some(lhs.clone());
            }
            // leave dummy value behind
            CaseCond::In(mem::replace(rhs, Expr::Number(0)))
        }
        _ => unreachable!(),
    };

    debug_assert!(matches!(
        true_.first(),
        Some(Stmt::DecompileError(_, DecompileErrorKind::StackUnderflow)),
    ));
    true_.remove(0);

    cases.push(Case {
        cond,
        body: mem::take(true_),
    });

    // Append another case
    if false_.len() == 1 && matches!(false_[0], Stmt::If { .. }) {
        append_case(&mut false_[0], value, cases);
        return;
    }
    // Append terminal else
    debug_assert!(matches!(
        false_[0],
        Stmt::DecompileError(_, DecompileErrorKind::StackUnderflow),
    ));
    false_.remove(0);
    if !false_.is_empty() {
        cases.push(Case {
            cond: CaseCond::Else,
            body: mem::take(false_),
        });
    }
}
