use crate::script::{
    ast::{write_block, Expr, Stmt},
    decode::Decoder,
    ins::{Ins, Operand, Variable},
    misc::write_indent,
};
use arrayvec::ArrayVec;
use indexmap::IndexMap;
use std::{fmt, fmt::Write, mem};

pub fn decompile(code: &[u8]) -> Option<String> {
    let mut output = String::with_capacity(1024);
    let blocks = find_basic_blocks(code)?;
    let controls = build_control_structures(&blocks);
    for block in controls.values() {
        emit_block(&mut output, block, 0, &blocks, code).unwrap();
    }
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

fn emit_block(
    w: &mut impl Write,
    block: &ControlBlock,
    indent: usize,
    blocks: &IndexMap<usize, BasicBlock>,
    code: &[u8],
) -> fmt::Result {
    match &block.control {
        Control::BasicBlock => {
            write_indent(w, indent)?;
            writeln!(w, "L{:04x}:", block.start)?;
            let block = &blocks[&block.start];
            let mut stmts = Vec::new();
            let _ = decompile_block(code, block, &mut stmts); // TODO: handle errors
            write_block(w, &stmts, indent)?;
        }
        Control::Sequence(blks) => {
            for block in blks {
                emit_block(w, block, indent, blocks, code)?;
            }
        }
        Control::If(s) => {
            write_indent(w, indent)?;
            writeln!(w, "if")?;
            emit_block(w, &s.condition, indent + 1, blocks, code)?;
            write_indent(w, indent)?;
            writeln!(w, "then")?;
            emit_block(w, &s.true_, indent + 1, blocks, code)?;
            if let Some(false_) = &s.false_ {
                write_indent(w, indent)?;
                writeln!(w, "else")?;
                emit_block(w, false_, indent + 1, blocks, code)?;
            }
            write_indent(w, indent)?;
            writeln!(w, "end")?;
        }
        Control::While(s) => {
            write_indent(w, indent)?;
            writeln!(w, "while")?;
            emit_block(w, &s.condition, indent + 1, blocks, code)?;
            write_indent(w, indent)?;
            writeln!(w, "do")?;
            emit_block(w, &s.body, indent + 1, blocks, code)?;
            write_indent(w, indent)?;
            writeln!(w, "end")?;
        }
    }
    Ok(())
}

fn decompile_block<'a>(
    code: &'a [u8],
    block: &BasicBlock,
    output: &mut Vec<Stmt<'a>>,
) -> Option<()> {
    let mut stack = Vec::new();
    let mut string_stack = Vec::new();

    let decoder = Decoder::new(code);
    decoder.set_pos(block.start);
    while decoder.pos() < block.end {
        let (off, ins) = decoder.next()?;
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
            Ins::StackDup => {
                // TODO: only constant expressions?
                stack.push(stack.last()?.clone());
            }
            Ins::DimArray(item_size, var) => {
                let swap = stack.pop()?;
                let max2 = stack.pop()?;
                let min2 = stack.pop()?;
                let max1 = stack.pop()?;
                let min1 = stack.pop()?;
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
                let expr = stack.pop()?;
                output.push(Stmt::Assign(var, expr));
            }
            Ins::CursorCharset => {
                let expr = stack.pop()?;
                output.push(Stmt::CursorCharset(expr));
            }
            Ins::LoadScript => {
                let expr = stack.pop()?;
                output.push(Stmt::LoadScript(expr));
            }
            Ins::LockScript => {
                let expr = stack.pop()?;
                output.push(Stmt::LockScript(expr));
            }
            Ins::LoadCharset => {
                let expr = stack.pop()?;
                output.push(Stmt::LoadCharset(expr));
            }
            Ins::AssignString(var) => {
                let expr = pop_string(&mut stack, &mut string_stack)?;
                output.push(Stmt::Assign(var, expr));
            }
            Ins::FreeArray(var) => {
                output.push(Stmt::FreeArray(var));
            }
            Ins::SetWindowTitle => {
                let expr = pop_string(&mut stack, &mut string_stack)?;
                output.push(Stmt::SetWindowTitle(expr));
            }
            Ins::Generic2Simple(b) => {
                output.push(Stmt::Raw2(b));
            }
            _ => {
                // TODO: if stack is non-empty, this loses data
                output.push(Stmt::Raw(&code[off..block.end]));
                break;
            }
        }
    }
    Some(())
}

fn pop_string<'a>(stack: &mut Vec<Expr>, string_stack: &mut Vec<&'a [u8]>) -> Option<Expr<'a>> {
    match stack.pop()? {
        Expr::Number(-1) => Some(Expr::String(string_stack.pop()?)),
        Expr::Number(var_id) => Some(Expr::Variable(Variable(var_id.try_into().ok()?))),
        Expr::String(_) => None,
        Expr::Variable(var) => Some(Expr::Variable(var)),
    }
}
