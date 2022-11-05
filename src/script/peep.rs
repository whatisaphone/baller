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
    let &mut Stmt::Do {
        ref mut body,
        condition,
        end,
    } = &mut block.stmts[i] else { return false };
    if condition.is_some() {
        return false;
    }
    let Some(&mut Stmt::If {
        condition,
        ref true_,
        ref false_,
    }) = body.stmts.get_mut(0) else { return false };
    let Expr::Not(condition) = script.exprs[condition] else { return false };
    let &[Stmt::Goto(goto)] = true_.stmts.as_slice() else { return false };
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
    let Some(&Stmt::Assign(var, start)) = block.stmts.get(i) else { return };
    let Some(&mut Stmt::While { condition, ref mut body }) = block.stmts.get_mut(i + 1) else {
        return;
    };
    let (lhs, step, end) = match script.exprs[condition] {
        Expr::LessOrEqual(lhs, rhs) => (lhs, 1, rhs),
        Expr::GreaterOrEqual(lhs, rhs) => (lhs, -1, rhs),
        _ => return,
    };
    let Expr::Variable(cond_var) = script.exprs[lhs] else { return };
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
        let Some(Stmt::Generic {
            bytecode,
            ins: _,
            args,
        }) = block.stmts.get(i) else { return false };
        if *bytecode != bytearray![0xa4, 0xd0] {
            return false;
        }
        if args.len() != 3 {
            return false;
        }
        let Expr::Variable(state) = script.exprs[args[0]] else { return false };
        let Expr::List(xs) = &script.exprs[args[1]] else { return false };
        let len = xs.len();
        let Expr::Number(offset) = script.exprs[args[2]] else { return false };
        if offset != 1 {
            return false;
        }
        (state, args[1], len)
    };

    // xe2 state (???)
    {
        let Some(Stmt::Generic {
            bytecode,
            ins: _,
            args,
        }) = block.stmts.get(i + 1) else { return false };
        if *bytecode != bytearray![0xe2] {
            return false;
        }
        if args.len() != 1 {
            return false;
        }
        let Expr::Variable(var) = script.exprs[args[0]] else { return false };
        if var != the_state {
            return false;
        }
    }

    // state[0] = 0
    {
        let Some(&Stmt::SetArrayItem(var, index, value)) = block.stmts.get(i + 2) else {
            return false;
        };
        if var != the_state {
            return false;
        }
        let Expr::Number(index) = script.exprs[index] else { return false };
        if index != 0 {
            return false;
        }
        let Expr::Number(value) = script.exprs[value] else { return false };
        if value != 0 {
            return false;
        }
    }

    // do
    let the_var = {
        let Some(&Stmt::Do { ref body, end, .. }) = block.stmts.get(i + 3) else { return false };

        // inc-array-item state 0
        {
            let Some(Stmt::Generic {
                bytecode,
                ins: _,
                args,
            }) = body.stmts.get(0) else { return false };
            // inc-array-item
            if *bytecode != bytearray![0x53] {
                return false;
            }
            if args.len() != 2 {
                return false;
            }
            let Expr::Variable(var) = script.exprs[args[0]] else { return false };
            if var != the_state {
                return false;
            }
            let Expr::Number(index) = script.exprs[args[1]] else { return false };
            if index != 0 {
                return false;
            }
        }

        // if (!(state[0] <= _)) { goto end }
        {
            let Some(&Stmt::If {
                condition,
                ref true_,
                ref false_,
            }) = body.stmts.get(1) else { return false };
            let Expr::Not(condition) = script.exprs[condition] else { return false };
            let Expr::LessOrEqual(lhs, rhs) = script.exprs[condition] else { return false };
            let Expr::ArrayIndex(var, index) = script.exprs[lhs] else { return false };
            if var != the_state {
                return false;
            }
            let Expr::Number(index) = script.exprs[index] else { return false };
            if index != 0 {
                return false;
            }
            let Expr::Number(len) = script.exprs[rhs] else { return false };
            if Ok(len) != the_len.try_into() {
                return false;
            }
            let &[Stmt::Goto(goto)] = true_.stmts.as_slice() else { return false };
            if goto != end {
                return false;
            }
            if !false_.stmts.is_empty() {
                return false;
            }
        }

        // var = state[state[0]]
        let the_var = {
            let Some(&Stmt::Assign(the_var, rhs)) = body.stmts.get(2) else { return false };
            let Expr::ArrayIndex(var, index) = script.exprs[rhs] else { return false };
            if var != the_state {
                return false;
            }
            let Expr::ArrayIndex(var, index) = script.exprs[index] else { return false };
            if var != the_state {
                return false;
            }
            let Expr::Number(index) = script.exprs[index] else { return false };
            if index != 0 {
                return false;
            }
            the_var
        };

        the_var
    };

    // free-array state
    {
        let Some(Stmt::Generic {
            bytecode,
            ins: _,
            args,
        }) = block.stmts.get(i + 4) else { return false };
        if *bytecode != bytearray![0xbc, 0xcc] {
            return false;
        }
        if args.len() != 1 {
            return false;
        }
        let Expr::Variable(var) = script.exprs[args[0]] else { return false };
        if var != the_state {
            return false;
        }
    }

    // Phew!

    let Some(Stmt::Do { body, .. }) = block.stmts.get_mut(i + 3) else { unreachable!() };
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
