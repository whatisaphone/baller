use crate::script::ast::{Expr, Scripto, Stmt, StmtBlock};
use std::mem;

pub fn peep(script: &Scripto, root: &mut StmtBlock) {
    let mut i = 0;
    while i < root.stmts.len() {
        if build_while(script, root, i) {
            i = i.saturating_sub(1);
        }
        build_for(script, root, i);
        build_for_list(script, root, i);

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
            | Stmt::Do { body, .. }
            | Stmt::For { body, .. }
            | Stmt::ForList { body, .. } => {
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
    let (body, &mut condition, &mut end) = match &mut block.stmts[i] {
        Stmt::Do {
            body,
            condition,
            end,
        } => (body, condition, end),
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
    if goto != end {
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
    body.end = *body.addrs.last().unwrap();
    body.pop();

    block.remove(i + 1);
    block.stmts[i] = Stmt::For {
        var,
        start,
        end,
        step,
        body,
    };
}

#[allow(clippy::too_many_lines)]
fn build_for_list(script: &Scripto, block: &mut StmtBlock, i: usize) -> bool {
    // array-set state [...] 1
    let (the_state, the_list, the_len) = {
        let (bytecode, _, args) = match block.stmts.get(i) {
            Some(Stmt::Generic {
                bytecode,
                ins,
                args,
            }) => (bytecode, ins, args),
            _ => return false,
        };
        if *bytecode != bytearray![0xa4, 0xd0] {
            return false;
        }
        if args.len() != 3 {
            return false;
        }
        let state = match script.exprs[args[0]] {
            Expr::Variable(var) => var,
            _ => return false,
        };
        let xs = match &script.exprs[args[1]] {
            Expr::List(xs) => xs,
            _ => return false,
        };
        let len = xs.len();
        let offset = match script.exprs[args[2]] {
            Expr::Number(n) => n,
            _ => return false,
        };
        if offset != 1 {
            return false;
        }
        (state, args[1], len)
    };

    // xe2 state (???)
    {
        let (bytecode, _, args) = match block.stmts.get(i + 1) {
            Some(Stmt::Generic {
                bytecode,
                ins,
                args,
            }) => (bytecode, ins, args),
            _ => return false,
        };
        if *bytecode != bytearray![0xe2] {
            return false;
        }
        if args.len() != 1 {
            return false;
        }
        let var = match script.exprs[args[0]] {
            Expr::Variable(var) => var,
            _ => return false,
        };
        if var != the_state {
            return false;
        }
    }

    // state[0] = 0
    {
        let (var, index, value) = match block.stmts.get(i + 2) {
            Some(&Stmt::SetArrayItem(var, index, value)) => (var, index, value),
            _ => return false,
        };
        if var != the_state {
            return false;
        }
        let index = match script.exprs[index] {
            Expr::Number(n) => n,
            _ => return false,
        };
        if index != 0 {
            return false;
        }
        let value = match script.exprs[value] {
            Expr::Number(n) => n,
            _ => return false,
        };
        if value != 0 {
            return false;
        }
    }

    // do
    let the_var = {
        let (body, &end) = match block.stmts.get(i + 3) {
            Some(Stmt::Do { body, end, .. }) => (body, end),
            _ => return false,
        };

        // inc-array-item state 0
        {
            let (bytecode, _, args) = match body.stmts.get(0) {
                Some(Stmt::Generic {
                    bytecode,
                    ins,
                    args,
                }) => (bytecode, ins, args),
                _ => return false,
            };
            // inc-array-item
            if *bytecode != bytearray![0x53] {
                return false;
            }
            if args.len() != 2 {
                return false;
            }
            let var = match script.exprs[args[0]] {
                Expr::Variable(var) => var,
                _ => return false,
            };
            if var != the_state {
                return false;
            }
            let index = match script.exprs[args[1]] {
                Expr::Number(n) => n,
                _ => return false,
            };
            if index != 0 {
                return false;
            }
        }

        // if (!(state[0] <= _)) { goto end }
        {
            let (&condition, true_, false_) = match body.stmts.get(1) {
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
            let (lhs, rhs) = match script.exprs[condition] {
                Expr::LessOrEqual(lhs, rhs) => (lhs, rhs),
                _ => return false,
            };
            let (var, index) = match script.exprs[lhs] {
                Expr::ArrayIndex(var, index) => (var, index),
                _ => return false,
            };
            if var != the_state {
                return false;
            }
            let index = match script.exprs[index] {
                Expr::Number(n) => n,
                _ => return false,
            };
            if index != 0 {
                return false;
            }
            let len = match script.exprs[rhs] {
                Expr::Number(n) => n,
                _ => return false,
            };
            if Ok(len) != the_len.try_into() {
                return false;
            }
            let goto = match true_.stmts.as_slice() {
                &[Stmt::Goto(goto)] => goto,
                _ => return false,
            };
            if goto != end {
                return false;
            }
            if !false_.stmts.is_empty() {
                return false;
            }
        }

        // var = state[state[0]]
        let the_var = {
            let (the_var, rhs) = match body.stmts.get(2) {
                Some(&Stmt::Assign(lhs, rhs)) => (lhs, rhs),
                _ => return false,
            };
            let (var, index) = match script.exprs[rhs] {
                Expr::ArrayIndex(var, index) => (var, index),
                _ => return false,
            };
            if var != the_state {
                return false;
            }
            let (var, index) = match script.exprs[index] {
                Expr::ArrayIndex(var, index) => (var, index),
                _ => return false,
            };
            if var != the_state {
                return false;
            }
            let index = match script.exprs[index] {
                Expr::Number(n) => n,
                _ => return false,
            };
            if index != 0 {
                return false;
            }
            the_var
        };

        the_var
    };

    // free-array state
    {
        let (bytecode, _, args) = match block.stmts.get(i + 4) {
            Some(Stmt::Generic {
                bytecode,
                ins,
                args,
            }) => (bytecode, ins, args),
            _ => return false,
        };
        if *bytecode != bytearray![0xbc, 0xcc] {
            return false;
        }
        if args.len() != 1 {
            return false;
        }
        let var = match script.exprs[args[0]] {
            Expr::Variable(var) => var,
            _ => return false,
        };
        if var != the_state {
            return false;
        }
    }

    // Phew!

    let body = match block.stmts.get_mut(i + 3) {
        Some(Stmt::Do { body, .. }) => body,
        _ => unreachable!(),
    };
    let mut body = mem::take(body);
    body.addrs.drain(0..3);
    body.stmts.drain(0..3);

    block.addrs.drain(i + 1..i + 5);
    block.stmts.drain(i + 1..i + 5);
    block.stmts[i] = Stmt::ForList {
        var: the_var,
        list: the_list,
        body,
    };
    true
}
