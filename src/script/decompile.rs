use crate::script::{
    ast::{format_block, Expr, Stmt},
    decode::Decoder,
    ins::{Ins, Operand, Variable},
};
use arrayvec::ArrayVec;
use std::{fmt::Write, mem};

pub fn decompile(code: &[u8]) -> Option<String> {
    let mut output = String::new();
    let blocks = find_basic_blocks(code)?;
    for block in &blocks {
        let mut stmts = Vec::new();
        let _ = decompile_block(code, block, &mut stmts); // TODO: handle errors
        writeln!(output, "L{:04x}:", block.start).unwrap();
        output.push_str(&format_block(&stmts));
    }
    Some(output)
}

fn find_basic_blocks(code: &[u8]) -> Option<Vec<BasicBlock>> {
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
    Some(blocks)
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
        let (_, ins) = decoder.next()?;
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
                return None;
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
