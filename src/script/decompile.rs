use crate::script::{
    ast::{write_stmts, Expr, Stmt},
    decode::Decoder,
    ins::{GenericArg, Ins, Operand, Variable},
};
use arrayvec::ArrayVec;
use indexmap::IndexMap;
use std::mem;

pub fn decompile(code: &[u8]) -> Option<String> {
    let mut output = String::with_capacity(1024);
    let blocks = find_basic_blocks(code)?;
    let controls = build_control_structures(&blocks);
    let ast = build_ast(&controls, code);
    write_stmts(&mut output, &ast, 0).unwrap();
    Some(output)
}

fn find_basic_blocks(code: &[u8]) -> Option<IndexMap<usize, BasicBlock>> {
    let mut blocks = Vec::with_capacity(16);
    blocks.push(BasicBlock {
        start: 0,
        end: code.len(),
        exits: ArrayVec::new(),
    });

    let decoder = Decoder::new(code);
    while let Some((off, ins)) = decoder.next() {
        if let Some(jump_target) = ins.jump_target(off) {
            split_block(&mut blocks, decoder.pos());
            split_block(&mut blocks, jump_target);

            let block = blocks.iter_mut().find(|b| b.end == decoder.pos()).unwrap();
            block.exits.clear();
            let jump_is_conditional = match ins {
                Ins::JumpIf(_) | Ins::JumpUnless(_) => true,
                Ins::Jump(_) => false,
                _ => unreachable!(),
            };
            if jump_is_conditional {
                block.exits.push(decoder.pos());
            }
            block.exits.push(jump_target);
        }
    }
    // Cannot analyze incomplete data
    if !decoder.exhausted() {
        return None;
    }
    Some(blocks.into_iter().map(|b| (b.start, b)).collect())
}

#[derive(Debug)]
struct BasicBlock {
    start: usize,
    end: usize,
    exits: ArrayVec<usize, 2>,
}

fn split_block(blocks: &mut Vec<BasicBlock>, addr: usize) {
    for i in 0..blocks.len() {
        let block = &mut blocks[i];
        if block.start >= addr {
            break;
        }
        if addr >= block.end {
            continue;
        }
        // addr falls inside the basic block. Rend it in two
        let mut blk1 = BasicBlock {
            start: block.start,
            end: addr,
            exits: ArrayVec::new(),
        };
        blk1.exits.push(addr);
        let blk2 = BasicBlock {
            start: addr,
            end: block.end,
            exits: mem::take(&mut block.exits),
        };
        *block = blk1;
        blocks.insert(i + 1, blk2);
        break;
    }
}

fn build_control_structures(blocks: &IndexMap<usize, BasicBlock>) -> IndexMap<usize, ControlBlock> {
    // Begin with a sequence of raw basic blocks
    let mut controls = blocks
        .values()
        .map(|b| {
            (b.start, ControlBlock {
                start: b.start,
                end: b.end,
                control: Control::BasicBlock,
                exits: b.exits.clone(),
            })
        })
        .collect();

    while build_one(&mut controls) {
        // Convert groups of blocks to control structures one at a time
    }
    controls
}

fn build_one(blocks: &mut IndexMap<usize, ControlBlock>) -> bool {
    for i in (0..blocks.len()).rev() {
        if build_sequence(blocks, i) {
            return true;
        }
        if build_while(blocks, i) {
            return true;
        }
        if build_if(blocks, i) {
            return true;
        }
        if build_else(blocks, i) {
            return true;
        }
    }
    false
}

fn build_sequence(blocks: &mut IndexMap<usize, ControlBlock>, index: usize) -> bool {
    let block = &blocks[index];
    let single_exit_no_jump = block.exits.len() == 1 && block.exits[0] == block.end;
    if !single_exit_no_jump {
        return false;
    }

    let (_, next_block) = match blocks.get_index(index + 1) {
        Some(block) => block,
        None => return false,
    };
    let any_outside_jumps_to_next_block = blocks
        .values()
        .any(|b| b.start != block.start && b.exits.iter().any(|&p| p == next_block.start));
    if any_outside_jumps_to_next_block {
        return false;
    }

    let seq: Vec<_> = blocks.drain(index..=index + 1).map(|(_, b)| b).collect();
    let start = seq.first().unwrap().start;
    let end = seq.last().unwrap().end;
    let exits = seq.last().unwrap().exits.clone();
    let (temp_index, _) = blocks.insert_full(start, ControlBlock {
        start,
        end,
        control: Control::Sequence(seq),
        exits,
    });
    blocks.move_index(temp_index, index);
    true
}

fn build_if(blocks: &mut IndexMap<usize, ControlBlock>, index: usize) -> bool {
    let block = &blocks[index];
    if !matches!(block.control, Control::BasicBlock | Control::Sequence(_)) {
        return false;
    }

    let conditional_forward_jump =
        block.exits.len() == 2 && block.exits[0] == block.end && block.exits[1] > block.end;
    if !conditional_forward_jump {
        return false;
    }

    let (_, next_block) = match blocks.get_index(index + 1) {
        Some(block) => block,
        None => return false,
    };
    let jump_skips_single_block = block.exits[1] == next_block.end;
    if !jump_skips_single_block {
        return false;
    }

    let mut drain = blocks.drain(index..=index + 1).map(|(_, b)| b);
    let condition = drain.next().unwrap();
    let true_ = drain.next().unwrap();
    drop(drain);

    let mut exits = ArrayVec::new();
    exits.push(condition.exits[1]);
    assert!(true_.exits.len() == 1);
    if true_.exits[0] != exits[0] {
        exits.push(true_.exits[0]);
    }

    let (temp_index, _) = blocks.insert_full(condition.start, ControlBlock {
        start: condition.start,
        end: true_.end,
        control: Control::If(Box::new(If {
            condition,
            true_,
            false_: None,
        })),
        exits,
    });
    blocks.move_index(temp_index, index);
    true
}

fn build_else(blocks: &mut IndexMap<usize, ControlBlock>, index: usize) -> bool {
    let block = &blocks[index];
    let as_if = match &block.control {
        Control::If(c) => c,
        _ => return false,
    };
    if as_if.false_.is_some() {
        return false;
    }

    let (_, next_block) = match blocks.get_index(index + 1) {
        Some(block) => block,
        None => return false,
    };
    let next_block_single_exit_no_jump =
        next_block.exits.len() == 1 && next_block.exits[0] == next_block.end;
    if !next_block_single_exit_no_jump {
        return false;
    }

    let true_jumps_over_single_block =
        as_if.true_.exits.len() == 1 && as_if.true_.exits[0] == next_block.end;
    if !true_jumps_over_single_block {
        return false;
    }

    let (_, false_) = blocks.shift_remove_index(index + 1).unwrap();
    let block = &mut blocks[index];
    let as_if = match &mut block.control {
        Control::If(c) => c,
        _ => unreachable!(),
    };
    block.end = false_.end;
    block.exits.clear();
    block.exits.push(block.end);
    as_if.false_ = Some(false_);
    true
}

fn build_while(blocks: &mut IndexMap<usize, ControlBlock>, index: usize) -> bool {
    let block = &blocks[index];
    let conditional_forward_jump =
        block.exits.len() == 2 && block.exits[0] == block.end && block.exits[1] > block.end;
    if !conditional_forward_jump {
        return false;
    }

    let (_, next_block) = match blocks.get_index(index + 1) {
        Some(block) => block,
        None => return false,
    };
    let condition_jumps_over_single_block = block.exits[1] == next_block.end;
    if !condition_jumps_over_single_block {
        return false;
    }
    let next_block_loops_to_start =
        next_block.exits.len() == 1 && next_block.exits[0] == block.start;
    if !next_block_loops_to_start {
        return false;
    }

    let mut drain = blocks.drain(index..=index + 1).map(|(_k, v)| v);
    let condition = drain.next().unwrap();
    let body = drain.next().unwrap();
    drop(drain);

    let mut exits = ArrayVec::new();
    exits.push(condition.exits[1]);
    let (temp_index, _) = blocks.insert_full(condition.start, ControlBlock {
        start: condition.start,
        end: body.end,
        control: Control::While(Box::new(While { condition, body })),
        exits,
    });
    blocks.move_index(temp_index, index);
    true
}

#[derive(Debug)]
struct ControlBlock {
    start: usize,
    end: usize,
    control: Control,
    exits: ArrayVec<usize, 2>,
}

#[derive(Debug)]
enum Control {
    BasicBlock,
    Sequence(Vec<ControlBlock>),
    If(Box<If>),
    While(Box<While>),
}

#[derive(Debug)]
struct If {
    condition: ControlBlock,
    true_: ControlBlock,
    false_: Option<ControlBlock>,
}

#[derive(Debug)]
struct While {
    condition: ControlBlock,
    body: ControlBlock,
}

fn build_ast<'a>(blocks: &IndexMap<usize, ControlBlock>, code: &'a [u8]) -> Vec<Stmt<'a>> {
    let mut stmts = Vec::new();
    for block in blocks.values() {
        match decompile_block(block, code, &mut stmts) {
            Ok(BlockExit::Fallthrough) => {}
            _ => break, // TODO: handle error
        }
    }
    stmts
}

fn decompile_block<'a>(
    block: &ControlBlock,
    code: &'a [u8],
    stmts: &mut Vec<Stmt<'a>>,
) -> Result<BlockExit<'a>, ()> {
    match &block.control {
        Control::BasicBlock => decompile_stmts(code, block, stmts),
        Control::Sequence(blks) => {
            for block in blks {
                match decompile_block(block, code, stmts)? {
                    BlockExit::Fallthrough => {}
                    _ => return Err(()),
                }
            }
            Ok(BlockExit::Fallthrough)
        }
        Control::If(b) => {
            let condition = match decompile_stmts(code, &b.condition, stmts)? {
                BlockExit::JumpUnless(_, expr) => expr, // TODO: verify jump target?
                _ => return Err(()),
            };
            let mut true_stmts = Vec::new();
            let true_exit = decompile_block(&b.true_, code, &mut true_stmts)?;
            match (&b.false_, true_exit) {
                // TODO: verify jump target?
                (None, BlockExit::Fallthrough) | (Some(_), BlockExit::Jump(_)) => {}
                _ => return Err(()),
            }
            let mut false_stmts = Vec::new();
            if let Some(false_) = &b.false_ {
                match decompile_block(false_, code, &mut false_stmts)? {
                    BlockExit::Fallthrough => {} // TODO: verify jump target?
                    _ => return Err(()),
                }
            }
            stmts.push(Stmt::If {
                condition,
                true_: true_stmts,
                false_: false_stmts,
            });
            Ok(BlockExit::Fallthrough)
        }
        Control::While(b) => {
            let condition = match decompile_stmts(code, &b.condition, stmts)? {
                BlockExit::JumpUnless(_, expr) => expr, // TODO: verify jump target?
                _ => return Err(()),
            };
            let mut body_stmts = Vec::new();
            match decompile_block(&b.body, code, &mut body_stmts)? {
                BlockExit::Jump(_) => {} // TODO: verify jump target?
                _ => return Err(()),
            }
            stmts.push(Stmt::While {
                condition,
                body: body_stmts,
            });
            Ok(BlockExit::Fallthrough)
        }
    }
}

#[allow(clippy::too_many_lines)]
fn decompile_stmts<'a>(
    code: &'a [u8],
    block: &ControlBlock,
    output: &mut Vec<Stmt<'a>>,
) -> Result<BlockExit<'a>, ()> {
    let mut stack = Vec::new();
    let mut string_stack = Vec::new();

    let decoder = Decoder::new(code);
    decoder.set_pos(block.start);
    while decoder.pos() < block.end {
        let (off, ins) = decoder.next().ok_or(())?;
        match ins {
            Ins::Push(op) => {
                match op {
                    Operand::Byte(x) => stack.push(Expr::Number(x.into())),
                    Operand::I16(x) => stack.push(Expr::Number(x.into())),
                    Operand::I32(x) => stack.push(Expr::Number(x)),
                    Operand::Var(var) => stack.push(Expr::Variable(var)),
                }
            }
            Ins::PushString(s) => {
                string_stack.push(s);
            }
            Ins::GetArrayItem(var) => {
                let index = stack.pop().ok_or(())?;
                stack.push(Expr::ArrayIndex(var, Box::new(index)));
            }
            Ins::StackDup => {
                // TODO: only constant expressions?
                stack.push(stack.last().ok_or(())?.clone());
            }
            Ins::Not => {
                let expr = stack.pop().ok_or(())?;
                stack.push(Expr::Not(Box::new(expr)));
            }
            Ins::Equal => {
                let rhs = stack.pop().ok_or(())?;
                let lhs = stack.pop().ok_or(())?;
                stack.push(Expr::Equal(Box::new((lhs, rhs))));
            }
            Ins::NotEqual => {
                let rhs = stack.pop().ok_or(())?;
                let lhs = stack.pop().ok_or(())?;
                stack.push(Expr::NotEqual(Box::new((lhs, rhs))));
            }
            Ins::Greater => {
                let rhs = stack.pop().ok_or(())?;
                let lhs = stack.pop().ok_or(())?;
                stack.push(Expr::Greater(Box::new((lhs, rhs))));
            }
            Ins::Less => {
                let rhs = stack.pop().ok_or(())?;
                let lhs = stack.pop().ok_or(())?;
                stack.push(Expr::Less(Box::new((lhs, rhs))));
            }
            Ins::LessOrEqual => {
                let rhs = stack.pop().ok_or(())?;
                let lhs = stack.pop().ok_or(())?;
                stack.push(Expr::LessOrEqual(Box::new((lhs, rhs))));
            }
            Ins::Add => {
                let rhs = stack.pop().ok_or(())?;
                let lhs = stack.pop().ok_or(())?;
                stack.push(Expr::Add(Box::new((lhs, rhs))));
            }
            Ins::Sub => {
                let rhs = stack.pop().ok_or(())?;
                let lhs = stack.pop().ok_or(())?;
                stack.push(Expr::Sub(Box::new((lhs, rhs))));
            }
            Ins::LogicalOr => {
                let rhs = stack.pop().ok_or(())?;
                let lhs = stack.pop().ok_or(())?;
                stack.push(Expr::LogicalOr(Box::new((lhs, rhs))));
            }
            Ins::DimArray(item_size, var) => {
                let swap = stack.pop().ok_or(())?;
                let max2 = stack.pop().ok_or(())?;
                let min2 = stack.pop().ok_or(())?;
                let max1 = stack.pop().ok_or(())?;
                let min1 = stack.pop().ok_or(())?;
                output.push(Stmt::DimArray {
                    var,
                    item_size,
                    min1,
                    max1,
                    min2,
                    max2,
                    swap,
                });
            }
            Ins::Set(var) => {
                let expr = stack.pop().ok_or(())?;
                output.push(Stmt::Assign(var, expr));
            }
            Ins::SetArrayItem(var) => {
                let value = stack.pop().ok_or(())?;
                let index = stack.pop().ok_or(())?;
                output.push(Stmt::SetArrayItem(var, index, value));
            }
            Ins::Inc(var) => {
                output.push(Stmt::Inc(var));
            }
            Ins::JumpUnless(rel) => {
                if decoder.pos() != block.end {
                    return Err(());
                }
                let expr = stack.pop().ok_or(())?;
                return Ok(BlockExit::JumpUnless(rel, expr));
            }
            Ins::CursorCharset => {
                let expr = stack.pop().ok_or(())?;
                output.push(Stmt::CursorCharset(expr));
            }
            Ins::Jump(rel) => {
                if decoder.pos() != block.end {
                    return Err(());
                }
                return Ok(BlockExit::Jump(rel));
            }
            Ins::LoadScript => {
                let expr = stack.pop().ok_or(())?;
                output.push(Stmt::LoadScript(expr));
            }
            Ins::LockScript => {
                let expr = stack.pop().ok_or(())?;
                output.push(Stmt::LockScript(expr));
            }
            Ins::LoadCharset => {
                let expr = stack.pop().ok_or(())?;
                output.push(Stmt::LoadCharset(expr));
            }
            Ins::AssignString(var) => {
                let expr = pop_string(&mut stack, &mut string_stack).ok_or(())?;
                output.push(Stmt::Assign(var, expr));
            }
            Ins::FreeArray(var) => {
                output.push(Stmt::FreeArray(var));
            }
            Ins::SetWindowTitle => {
                let expr = pop_string(&mut stack, &mut string_stack).ok_or(())?;
                output.push(Stmt::SetWindowTitle(expr));
            }
            Ins::Generic2Simple(b) => {
                output.push(Stmt::Raw2(b));
            }
            Ins::Generic(_, ins) => {
                let mut args = Vec::with_capacity(ins.args.len());
                for arg in ins.args.iter().rev() {
                    let expr = match arg {
                        GenericArg::Int => stack.pop().ok_or(())?,
                        GenericArg::String => pop_string(&mut stack, &mut string_stack).ok_or(())?,
                        GenericArg::List => pop_list(&mut stack).ok_or(())?,
                    };
                    args.push(expr);
                }
                args.reverse();
                if ins.returns_value {
                    stack.push(Expr::Call(ins, args));
                } else {
                    output.push(Stmt::Generic(ins, args));
                }
            }
            _ => {
                // TODO: if stack is non-empty, this loses data
                output.push(Stmt::Raw(&code[off..block.end]));
                return Err(());
            }
        }
    }
    Ok(BlockExit::Fallthrough)
}

enum BlockExit<'a> {
    Fallthrough,
    Jump(i16),
    JumpUnless(i16, Expr<'a>),
}

fn pop_string<'a>(stack: &mut Vec<Expr>, string_stack: &mut Vec<&'a [u8]>) -> Option<Expr<'a>> {
    match stack.pop()? {
        Expr::Number(-1) => Some(Expr::String(string_stack.pop()?)),
        Expr::Number(var_id) => Some(Expr::Variable(Variable(var_id.try_into().ok()?))),
        Expr::Variable(var) => Some(Expr::Variable(var)),
        _ => None,
    }
}

fn pop_list<'a>(stack: &mut Vec<Expr<'a>>) -> Option<Expr<'a>> {
    let len = match stack.pop()? {
        Expr::Number(n) => n,
        _ => return None,
    };
    let mut list = Vec::with_capacity(len.try_into().ok()?);
    for _ in 0..len {
        list.push(stack.pop()?);
    }
    list.reverse();
    Some(Expr::List(list))
}
