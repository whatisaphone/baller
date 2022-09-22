use crate::{
    config::Config,
    script::{
        ast::{
            write_local_vars,
            write_stmts,
            Case,
            CaseCond,
            DecompileErrorKind,
            Expr,
            Scope,
            Stmt,
            WriteCx,
        },
        decode::Decoder,
        ins::{GenericArg, GenericIns, Ins, Operand, Variable},
        visit::Visitor,
    },
};
use arrayvec::ArrayVec;
use indexmap::IndexMap;
use std::{fmt, mem, ops::Range};
use tracing::{debug, instrument, trace};

pub fn decompile(code: &[u8], scope: Scope, config: &Config) -> String {
    if code.is_empty() {
        return String::new();
    }

    let (blocks, decode_extent) = find_basic_blocks(code);
    let controls = build_control_structures(&blocks);
    let mut ast = build_ast(&controls, code);
    build_cases(&mut ast);

    if decode_extent != code.len() {
        ast.push(Stmt::DecompileError(
            decode_extent,
            DecompileErrorKind::Other("incomplete decode"),
        ));
    }

    let locals = collect_locals(&ast);

    let mut output = String::with_capacity(1024);
    let cx = WriteCx { scope, config };
    if !config.suppress_local_variable_declarations {
        write_local_vars(&mut output, &locals, &cx).unwrap();
    }
    write_stmts(&mut output, &ast, 0, &cx).unwrap();
    output
}

fn find_basic_blocks(code: &[u8]) -> (IndexMap<usize, BasicBlock>, usize) {
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
    let blocks: IndexMap<_, _> = blocks.into_iter().map(|b| (b.start, b)).collect();
    (blocks, decoder.pos())
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

fn basic_blocks_get_index_by_end(blocks: &IndexMap<usize, BasicBlock>, addr: usize) -> usize {
    let result = 'result: loop {
        if let Some(index) = blocks.get_index_of(&addr) {
            break 'result index - 1;
        }
        break 'result blocks.len() - 1;
    };
    debug_assert!(blocks[result].end == addr);
    result
}

#[instrument(level = "debug", skip(basics))]
fn build_control_structures(basics: &IndexMap<usize, BasicBlock>) -> Vec<ControlBlock> {
    let end = basics[basics.len() - 1].end;

    let mut controls = Vec::with_capacity(16);
    controls.push(ControlBlock {
        start: 0,
        end,
        control: Control::CodeRange,
    });

    let mut work = Vec::with_capacity(16);
    work.push(0);

    while let Some(index) = work.pop() {
        scan_ctrl(index, basics, &mut controls, &mut work);
    }
    controls
}

fn scan_ctrl(
    ctrl_index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) {
    let control = &controls[ctrl_index];
    debug_assert!(matches!(control.control, Control::CodeRange));
    let start_index = basics.get_index_of(&control.start).unwrap();
    let end_index = basic_blocks_get_index_by_end(basics, control.end) + 1;
    for i in start_index..end_index {
        if build_while(ctrl_index, i, basics, controls, work) {
            return;
        }
        if build_if(ctrl_index, i, basics, controls, work) {
            return;
        }
    }
}

fn split_ctrl_range(index: usize, mid: usize, controls: &mut Vec<ControlBlock>) -> (usize, usize) {
    let parent = &controls[index];
    let start = parent.start;
    let end = parent.end;
    debug_assert!(matches!(parent.control, Control::CodeRange));
    debug_assert!(start <= mid && mid <= end);

    let out1 = controls.len();
    controls.push(ControlBlock {
        start,
        end: mid,
        control: Control::CodeRange,
    });

    let out2 = controls.len();
    controls.push(ControlBlock {
        start: mid,
        end,
        control: Control::CodeRange,
    });

    trace!("split #{index} into #{out1}+#{out2} 0x{start:x}..0x{mid:x}..0x{end:x}");

    controls[index].control = Control::Sequence(out1, out2);
    (out1, out2)
}

fn build_if(
    parent_index: usize,
    basic_index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) -> bool {
    let parent = &controls[parent_index];

    // Require conditional forward jump, still within parent block
    let cond_block = &basics[basic_index];
    if !(cond_block.exits.len() == 2
        && cond_block.exits[0] == cond_block.end
        && cond_block.exits[1] > cond_block.end
        && cond_block.exits[1] <= parent.end)
    {
        return false;
    }

    let body_start = cond_block.end;
    let body_end = cond_block.exits[1];

    debug!(
        parent = %AddrRange(parent.start..parent.end),
        cond = %AddrRange(cond_block.start..cond_block.end),
        body = %AddrRange(body_start..body_end),
        "building if",
    );

    let (_before, hereafter) = split_ctrl_range(parent_index, cond_block.start, controls);
    let (here, after) = split_ctrl_range(hereafter, body_end, controls);
    let (condition, true_) = split_ctrl_range(here, body_start, controls);

    controls[here].control = Control::If(If {
        condition,
        true_,
        false_: None,
    });

    work.push(true_);
    work.push(after);

    build_else(hereafter, here, condition, true_, basics, controls, work);

    true
}

// parent_index points to Sequence(If(cond, true, None), CodeRange).
fn build_else(
    parent_index: usize,
    if_index: usize,
    cond_index: usize,
    true_index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) {
    let parent = &controls[parent_index];

    let cond_ctrl = &controls[cond_index];
    let cond_block = &basics[&cond_ctrl.start];
    debug_assert!(
        cond_block.start == cond_ctrl.start && cond_block.end == cond_ctrl.end,
        "control block covers exactly one basic block",
    );
    debug_assert!(cond_block.exits.len() == 2 && cond_block.exits[0] == cond_block.end);

    let else_start = cond_block.exits[1];

    let true_ctrl = &controls[true_index];
    let true_end_block_index = basic_blocks_get_index_by_end(basics, true_ctrl.end);
    let true_end_block = &basics[true_end_block_index];
    // Require the then block to end at the beginning of the else block, with an
    // unconditional jump over a range of code which will later form the else block.
    if !(true_ctrl.end == else_start
        && true_end_block.exits.len() == 1
        && true_end_block.exits[0] > true_end_block.end
        && true_end_block.exits[0] <= parent.end)
    {
        return;
    }

    let else_end = true_end_block.exits[0];

    debug!(
        then = %AddrRange(true_ctrl.start..true_ctrl.end),
        "else" = %AddrRange(else_start..else_end),
        "building else",
    );

    let (first, second) = match controls[parent_index].control {
        Control::Sequence(first, second) => (first, second),
        _ => unreachable!(),
    };
    debug_assert!(first == if_index);
    debug_assert!(matches!(controls[first].control, Control::If { .. }));
    debug_assert!(matches!(controls[second].control, Control::CodeRange));
    debug_assert!(controls[second].start == else_start);
    debug_assert!(controls[second].end == parent.end);

    // Create the else block with bytes taken from the front of `second`. Then
    // modify `second` to start after the else block.
    let else_index = controls.len();
    controls.push(ControlBlock {
        start: else_start,
        end: else_end,
        control: Control::CodeRange,
    });
    let if_ = match &mut controls[if_index].control {
        Control::If(if_) => if_,
        _ => unreachable!(),
    };
    if_.false_ = Some(else_index);

    controls[second].start = else_end;

    work.push(else_index);
}

fn build_while(
    parent_index: usize,
    basic_index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) -> bool {
    let parent = &controls[parent_index];

    // Require conditional forward jump as the condition
    let cond_block = &basics[basic_index];
    if !(cond_block.exits.len() == 2
        && cond_block.exits[0] == cond_block.end
        && cond_block.exits[1] > cond_block.end
        && cond_block.exits[1] <= parent.end)
    {
        return false;
    }

    let body_start = cond_block.end;
    let body_end = cond_block.exits[1];

    let body_end_block_index = basic_blocks_get_index_by_end(basics, body_end);
    let body_end_block = &basics[body_end_block_index];
    // Require the end block to jump back to the condition
    if !(body_end_block.exits.len() == 1 && body_end_block.exits[0] == cond_block.start) {
        return false;
    }

    debug!(
        parent = %AddrRange(parent.start..parent.end),
        cond = %AddrRange(cond_block.start..cond_block.end),
        body = %AddrRange(body_start..body_end),
        "building while",
    );

    let (_before, hereafter) = split_ctrl_range(parent_index, cond_block.start, controls);
    let (here, after) = split_ctrl_range(hereafter, body_end, controls);
    let (condition, body) = split_ctrl_range(here, body_start, controls);

    controls[here].control = Control::While(While { condition, body });

    work.push(body);
    work.push(after);

    true
}

struct AddrRange(Range<usize>);

impl fmt::Display for AddrRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{:x}..0x{:x}", self.0.start, self.0.end)
    }
}

#[derive(Debug)]
struct ControlBlock {
    start: usize,
    end: usize,
    control: Control,
}

#[derive(Debug)]
enum Control {
    CodeRange,
    Sequence(usize, usize),
    If(If),
    While(While),
}

#[derive(Debug)]
struct If {
    condition: usize,
    true_: usize,
    false_: Option<usize>,
}

#[derive(Debug)]
struct While {
    condition: usize,
    body: usize,
}

fn build_ast<'a>(controls: &[ControlBlock], code: &'a [u8]) -> Vec<Stmt<'a>> {
    let mut stmts = Vec::new();
    let entry = &controls[0];
    match decompile_block(entry, code, controls, &mut stmts) {
        Ok(BlockExit::Fallthrough) => {}
        Ok(_) => {
            stmts.push(Stmt::DecompileError(
                entry.end,
                DecompileErrorKind::WrongBlockExit,
            ));
        }
        Err(DecompileError(offset, message)) => {
            stmts.push(Stmt::DecompileError(offset, message));
        }
    }
    stmts
}

struct DecompileError(usize, DecompileErrorKind);

fn decompile_block<'a>(
    block: &ControlBlock,
    code: &'a [u8],
    controls: &[ControlBlock],
    stmts: &mut Vec<Stmt<'a>>,
) -> Result<BlockExit<'a>, DecompileError> {
    match &block.control {
        Control::CodeRange => decompile_stmts(code, block.start, block.end, stmts),
        &Control::Sequence(first, second) => {
            match decompile_block(&controls[first], code, controls, stmts)? {
                BlockExit::Fallthrough => {}
                _ => {
                    stmts.push(Stmt::DecompileError(
                        controls[first].end,
                        DecompileErrorKind::WrongBlockExit,
                    ));
                }
            }
            decompile_block(&controls[second], code, controls, stmts)
        }
        Control::If(b) => {
            let cond_block = &controls[b.condition];
            debug_assert!(matches!(cond_block.control, Control::CodeRange));
            let condition = match decompile_stmts(code, cond_block.start, cond_block.end, stmts)? {
                BlockExit::JumpUnless(_, expr) => expr, // TODO: verify jump target?
                _ => Expr::DecompileError(cond_block.end, DecompileErrorKind::WrongBlockExit),
            };
            let mut true_stmts = Vec::new();
            let mut exit = decompile_block(&controls[b.true_], code, controls, &mut true_stmts)?;
            let mut false_stmts = Vec::new();
            if let Some(false_) = b.false_ {
                // If there's a false, verify the true exit, then decompile the false
                if !matches!(exit, BlockExit::Jump(_)) {
                    // TODO: verify jump target?
                    true_stmts.push(Stmt::DecompileError(
                        controls[b.true_].end,
                        DecompileErrorKind::WrongBlockExit,
                    ));
                }
                exit = decompile_block(&controls[false_], code, controls, &mut false_stmts)?;
            }
            stmts.push(Stmt::If {
                condition,
                true_: true_stmts,
                false_: false_stmts,
            });
            Ok(exit)
        }
        Control::While(b) => {
            let cond_block = &controls[b.condition];
            debug_assert!(matches!(cond_block.control, Control::CodeRange));
            let cond_expr = match decompile_stmts(code, cond_block.start, cond_block.end, stmts)? {
                BlockExit::JumpUnless(_, expr) => expr, // TODO: verify jump target?
                _ => Expr::DecompileError(cond_block.end, DecompileErrorKind::WrongBlockExit),
            };
            let mut body_stmts = Vec::new();
            match decompile_block(&controls[b.body], code, controls, &mut body_stmts)? {
                BlockExit::Jump(_) => {} // TODO: verify jump target?
                _ => {
                    stmts.push(Stmt::DecompileError(
                        cond_block.end,
                        DecompileErrorKind::WrongBlockExit,
                    ));
                }
            }
            stmts.push(Stmt::While {
                condition: cond_expr,
                body: body_stmts,
            });
            Ok(BlockExit::Fallthrough)
        }
    }
}

#[allow(clippy::too_many_lines)]
fn decompile_stmts<'a>(
    code: &'a [u8],
    block_start: usize,
    block_end: usize,
    output: &mut Vec<Stmt<'a>>,
) -> Result<BlockExit<'a>, DecompileError> {
    let mut stack = Vec::new();
    let mut string_stack = Vec::new();

    let decoder = Decoder::new(code);
    decoder.set_pos(block_start);

    macro_rules! pop {
        () => {
            stack
                .pop()
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
        (: string) => {
            pop_string(&mut stack, &mut string_stack)
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
        (: list) => {
            pop_list(&mut stack)
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
    }

    macro_rules! final_checks {
        () => {
            // TODO: enable this check once case statements are sorted
            if false && !stack.is_empty() {
                output.push(Stmt::DecompileError(
                    decoder.pos(),
                    DecompileErrorKind::Other("values remain on stack"),
                ));
            }
        };
    }

    while decoder.pos() < block_end {
        let (off, ins) = decoder.next().ok_or_else(|| {
            DecompileError(decoder.pos(), DecompileErrorKind::Other("opcode decode"))
        })?;
        match ins {
            Ins::Push(op) => {
                match op {
                    Operand::Byte(x) => stack.push(Expr::Number(x.into())),
                    Operand::I16(x) => stack.push(Expr::Number(x.into())),
                    Operand::I32(x) => stack.push(Expr::Number(x)),
                    Operand::Var(var) => stack.push(Expr::Variable(var)),
                    Operand::String(s) => string_stack.push(s),
                }
            }
            Ins::GetArrayItem(var) => {
                let x = pop!()?;
                stack.push(Expr::ArrayIndex(var, Box::new(x)));
            }
            Ins::GetArrayItem2D(var) => {
                let x = pop!()?;
                let y = pop!()?;
                stack.push(Expr::ArrayIndex2D(var, Box::new((y, x))));
            }
            Ins::StackDup => {
                // TODO: only constant expressions?
                stack.push(Expr::StackDup(Box::new(
                    stack.last().cloned().unwrap_or(Expr::StackUnderflow),
                )));
            }
            Ins::Not => {
                let expr = pop!()?;
                stack.push(Expr::Not(Box::new(expr)));
            }
            Ins::Equal => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Equal(Box::new((lhs, rhs))));
            }
            Ins::NotEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::NotEqual(Box::new((lhs, rhs))));
            }
            Ins::Greater => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Greater(Box::new((lhs, rhs))));
            }
            Ins::Less => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Less(Box::new((lhs, rhs))));
            }
            Ins::LessOrEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::LessOrEqual(Box::new((lhs, rhs))));
            }
            Ins::GreaterOrEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::GreaterOrEqual(Box::new((lhs, rhs))));
            }
            Ins::Add => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Add(Box::new((lhs, rhs))));
            }
            Ins::Sub => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Sub(Box::new((lhs, rhs))));
            }
            Ins::Mul => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Mul(Box::new((lhs, rhs))));
            }
            Ins::Div => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Div(Box::new((lhs, rhs))));
            }
            Ins::LogicalAnd => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::LogicalAnd(Box::new((lhs, rhs))));
            }
            Ins::LogicalOr => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::LogicalOr(Box::new((lhs, rhs))));
            }
            Ins::PopDiscard => {
                if pop!().is_err() {
                    output.push(Stmt::DecompileError(
                        off,
                        DecompileErrorKind::StackUnderflow,
                    ));
                }
            }
            Ins::In => {
                let list = pop!(:list)?;
                let value = pop!()?;
                stack.push(Expr::In(Box::new((value, list))));
            }
            Ins::DimArray2D(item_size, var) => {
                let swap = pop!()?;
                let max_x = pop!()?;
                let min_x = pop!()?;
                let max_y = pop!()?;
                let min_y = pop!()?;
                output.push(Stmt::DimArray {
                    var,
                    item_size,
                    min_y,
                    max_y,
                    min_x,
                    max_x,
                    swap,
                });
            }
            Ins::Set(var) => {
                let expr = pop!()?;
                output.push(Stmt::Assign(var, expr));
            }
            Ins::SetArrayItem(var) => {
                let value = pop!()?;
                let index = pop!()?;
                output.push(Stmt::SetArrayItem(var, index, value));
            }
            Ins::SetArrayItem2D(var) => {
                let value = pop!()?;
                let index_x = pop!()?;
                let index_y = pop!()?;
                output.push(Stmt::SetArrayItem2D(var, index_y, index_x, value));
            }
            Ins::Inc(var) => {
                output.push(Stmt::Inc(var));
            }
            Ins::Dec(var) => {
                output.push(Stmt::Dec(var));
            }
            Ins::JumpIf(rel) => {
                #[allow(clippy::cast_sign_loss)]
                let target = decoder.pos().wrapping_add(rel as isize as usize);
                let expr = pop!()?;
                let expr = Expr::Not(Box::new(expr));
                if decoder.pos() == block_end {
                    final_checks!();
                    return Ok(BlockExit::JumpUnless(target, expr));
                }
                // TODO: popped expr is swallowed here, emit it somehow
                output.push(Stmt::DecompileError(
                    off,
                    DecompileErrorKind::WrongBlockExit,
                ));
            }
            Ins::JumpUnless(rel) => {
                #[allow(clippy::cast_sign_loss)]
                let target = decoder.pos().wrapping_add(rel as isize as usize);
                let expr = pop!()?;
                if decoder.pos() == block_end {
                    final_checks!();
                    return Ok(BlockExit::JumpUnless(target, expr));
                }
                // TODO: popped expr is swallowed here, emit it somehow
                output.push(Stmt::DecompileError(
                    off,
                    DecompileErrorKind::WrongBlockExit,
                ));
            }
            Ins::Jump(rel) => {
                #[allow(clippy::cast_sign_loss)]
                let target = decoder.pos().wrapping_add(rel as isize as usize);
                if decoder.pos() == block_end {
                    final_checks!();
                    return Ok(BlockExit::Jump(target));
                }
                output.push(Stmt::DecompileError(
                    off,
                    DecompileErrorKind::WrongBlockExit,
                ));
            }
            Ins::AssignString(var) => {
                let expr = pop!(:string)?;
                output.push(Stmt::Assign(var, expr));
            }
            Ins::Sprintf(var) => {
                let mut args = pop!(:list)?;
                let first_arg = pop!()?;
                match &mut args {
                    Expr::List(xs) => xs.insert(0, first_arg),
                    _ => unreachable!(),
                }
                let format = pop!(:string)?;
                output.push(Stmt::Generic {
                    bytecode: bytearray![0xa4, 0xc2],
                    ins: &GenericIns {
                        name: Some("sprintf"),
                        args: &[GenericArg::String, GenericArg::Int, GenericArg::List],
                        returns_value: false,
                    },
                    args: vec![Expr::Variable(var), format, args],
                });
            }
            Ins::DimArray1DSimple(item_size, var) => {
                let max_x = pop!()?;
                output.push(Stmt::DimArray {
                    var,
                    item_size,
                    min_y: Expr::Number(0),
                    max_y: Expr::Number(0),
                    min_x: Expr::Number(0),
                    max_x,
                    swap: Expr::Number(2),
                });
            }
            Ins::DimArray2DSimple(item_size, var) => {
                let max_x = pop!()?;
                let max_y = pop!()?;
                output.push(Stmt::DimArray {
                    var,
                    item_size,
                    min_y: Expr::Number(0),
                    max_y,
                    min_x: Expr::Number(0),
                    max_x,
                    swap: Expr::Number(2),
                });
            }
            Ins::BitwiseAnd => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::BitwiseAnd(Box::new((lhs, rhs))));
            }
            Ins::BitwiseOr => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::BitwiseOr(Box::new((lhs, rhs))));
            }
            Ins::Generic(bytecode, operands, ins) => {
                let mut args = Vec::with_capacity(operands.len() + ins.args.len());
                for arg in ins.args.iter().rev() {
                    let expr = match arg {
                        GenericArg::Int | GenericArg::IntScript => pop!()?,
                        GenericArg::String => pop!(:string)?,
                        GenericArg::List => pop!(:list)?,
                    };
                    args.push(expr);
                }
                for op in operands.iter().rev() {
                    args.push(operand_to_expr(op));
                }
                args.reverse();
                if ins.returns_value {
                    stack.push(Expr::Call(bytecode, ins, args));
                } else {
                    output.push(Stmt::Generic {
                        bytecode,
                        ins,
                        args,
                    });
                }
            }
        }
    }
    if decoder.pos() != block_end {
        output.push(Stmt::DecompileError(
            decoder.pos(),
            DecompileErrorKind::WrongBlockExit,
        ));
    }
    final_checks!();
    Ok(BlockExit::Fallthrough)
}

fn operand_to_expr<'a>(operand: &Operand<'a>) -> Expr<'a> {
    match *operand {
        Operand::Byte(x) => Expr::Number(x.into()),
        Operand::I16(x) => Expr::Number(x.into()),
        Operand::I32(x) => Expr::Number(x),
        Operand::Var(v) => Expr::Variable(v),
        Operand::String(s) => Expr::String(s),
    }
}

enum BlockExit<'a> {
    Fallthrough,
    Jump(usize),
    JumpUnless(usize, Expr<'a>),
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

fn build_cases(ast: &mut [Stmt]) {
    for stmt in ast {
        if is_case(stmt) {
            build_case(stmt);
        }

        match stmt {
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

fn collect_locals(stmts: &[Stmt]) -> Vec<Variable> {
    struct CollectLocals {
        out: Vec<Variable>,
    }

    impl Visitor for CollectLocals {
        fn var(&mut self, var: Variable) {
            if applies(var) {
                self.out.push(var);
            }
        }
    }

    fn applies(var: Variable) -> bool {
        let scope = var.0 & 0xf000;
        scope == 0x4000
    }

    let mut locals = CollectLocals {
        out: Vec::with_capacity(16),
    };
    locals.stmts(stmts);

    locals.out.sort_unstable_by_key(|v| v.0);
    locals.out.dedup_by_key(|v| v.0);
    locals.out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::read_scrp;
    use std::error::Error;

    #[test]
    fn basic_if() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let out = decompile(
            &bytecode[0x23c..0x266],
            Scope::Global(1),
            &Config::default(),
        );
        assert_starts_with(
            &out,
            r#"if (read-ini-int "NoPrinting") {
    global449 = 1
}
"#,
        );
        Ok(())
    }

    #[test]
    fn basic_if_else() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let out = decompile(
            &bytecode[0x5ba..0x5d4],
            Scope::Global(1),
            &Config::default(),
        );
        assert_starts_with(
            &out,
            r#"if (!global414) {
    global414 = 1
} else {
    global414 = 0
}
"#,
        );
        Ok(())
    }

    #[test]
    fn basic_while() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let out = decompile(&bytecode[0x1b2..0x1d1], Scope::Global(1), &Config {
            suppress_local_variable_declarations: true,
            ..<_>::default()
        });
        assert_starts_with(
            &out,
            r#"while (local1 <= global105) {
    global220[local1] = local1
    local1++
}
"#,
        );
        Ok(())
    }

    #[test]
    fn case_eq() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let out = decompile(
            &bytecode[0x501..0x542],
            Scope::Global(1),
            &Config::default(),
        );
        assert_starts_with(
            &out,
            r#"case global215 {
    of 2 {
        global90[0] = 70
    }
    of 1 {
        global90[0] = 77
    }
    of 0 {
        global90[0] = 83
    }
}
"#,
        );
        Ok(())
    }

    #[test]
    fn case_range() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(415)?;
        let out = decompile(&bytecode[0x1e..0xa6], Scope::Global(1), &Config {
            suppress_local_variable_declarations: true,
            ..<_>::default()
        });
        assert_starts_with(
            &out,
            r#"case local1 {
    in [1, 3, 5] {
        local4 = 350
    }
    in [2, 4, 6] {
        local4 = 351
    }
    in [7, 9, 11] {
        local4 = 356
    }
    in [8, 10, 12] {
        local4 = 357
    }
    else {
        xb6-xfe
        xb6-x4b "Invalid kid code"
        local4 = 350
        pop-discard 0
    }
}
"#,
        );
        Ok(())
    }

    #[test]
    fn call_scripts_by_name() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let config = Config::from_ini("script.80 = test")?;
        let out = decompile(&bytecode[0x5ce..0x5d4], Scope::Global(1), &config);
        assert_eq!(out, "run-script test/*80*/ []\n");
        Ok(())
    }

    fn assert_starts_with(string: &str, prefix: &str) {
        assert!(
            string.starts_with(prefix),
            "assertion failed: {:?} starts with {:?}",
            string,
            prefix
        );
    }
}
