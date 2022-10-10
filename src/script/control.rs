use crate::script::{
    ast::{get_script_config, WriteCx},
    basic::{basic_block_get_exact, basic_blocks_get_index_by_end, BasicBlock},
};
use indexmap::IndexMap;
use std::{
    fmt,
    ops::{Range, RangeInclusive},
    slice,
};
use tracing::{debug, instrument, trace};

#[derive(Debug)]
pub struct ControlBlock {
    pub start: usize,
    pub end: usize,
    pub control: Control,
}

#[derive(Debug)]
pub enum Control {
    CodeRange,
    Sequence(Vec<usize>),
    If(If),
    While(While),
    Do(Do),
}

#[derive(Debug)]
pub struct If {
    pub condition: usize,
    pub true_: usize,
    pub false_: Option<usize>,
}

#[derive(Debug)]
pub struct While {
    pub condition: usize,
    pub body: usize,
}

#[derive(Debug)]
pub struct Do {
    pub body: usize,
    pub condition: usize,
    pub condition_kind: ConditionKind,
}

#[derive(Debug)]
pub enum ConditionKind {
    Always,
    Until,
}

#[instrument(level = "debug", skip(basics, cx))]
pub fn build_control_structures(
    basics: &IndexMap<usize, BasicBlock>,
    cx: &WriteCx,
) -> Vec<ControlBlock> {
    // Build the initial root block -- a flat sequence with every basic block in
    // order.

    let mut controls = Vec::with_capacity(basics.len() + 1);

    controls.push(ControlBlock {
        start: 0,
        end: basics[basics.len() - 1].end,
        control: Control::Sequence(Vec::new()),
    });

    let mut seq = Vec::with_capacity(basics.len());
    for basic in basics.values() {
        let block = controls.len();
        controls.push(ControlBlock {
            start: basic.start,
            end: basic.end,
            control: Control::CodeRange,
        });
        seq.push(block);
    }

    let root_seq = match &mut controls[0].control {
        Control::Sequence(root_seq) => root_seq,
        _ => unreachable!(),
    };
    *root_seq = seq;

    // Run structuring passes one by one.

    let mut work = Vec::with_capacity(16);
    work.push(0);
    while let Some(index) = work.pop() {
        scan_forward_jumps(index, basics, &mut controls, &mut work);
    }
    check_control_invariants(0, &controls, basics);

    let skip_do_blocks = get_script_config(cx).map_or(false, |c| c.skip_do_blocks);
    if !skip_do_blocks {
        work.push(0);
        while let Some(index) = work.pop() {
            scan_loops(index, basics, &mut controls, &mut work);
        }
        check_control_invariants(0, &controls, basics);
    }

    work.push(0);
    while let Some(index) = work.pop() {
        scan_elses(index, basics, &mut controls, &mut work);
    }
    check_control_invariants(0, &controls, basics);

    controls
}

fn scan_forward_jumps(
    index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) {
    match &controls[index].control {
        Control::CodeRange => {}
        Control::Sequence(_) => {
            scan_forward_jumps_in_sequence(index, basics, controls, work);
        }
        Control::If(b) => {
            work.push(b.condition);
            work.push(b.true_);
            work.extend(b.false_);
        }
        Control::While(b) => {
            work.push(b.condition);
            work.push(b.body);
        }
        Control::Do(b) => {
            work.push(b.body);
            work.push(b.condition);
        }
    }
}

fn scan_forward_jumps_in_sequence(
    parent_index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) {
    let children = match &controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };

    for i in 0..children.len() {
        match &controls[children[i]].control {
            Control::CodeRange => {
                macro_rules! done {
                    () => {
                        check_control_invariants(parent_index, controls, basics);
                        work.push(parent_index); // Come back later for the rest
                        return;
                    };
                }

                if let Some(payload) = scan_while(parent_index, i, controls, basics) {
                    build_while(parent_index, i, payload, controls);
                    done!();
                } else if let Some(payload) = scan_if(parent_index, i, controls, basics) {
                    build_if(parent_index, i, payload, controls);
                    done!();
                }
            }
            Control::Sequence(_) => unreachable!(),
            Control::If(b) => {
                work.push(b.condition);
                work.push(b.true_);
                work.extend(b.false_);
            }
            Control::While(b) => {
                work.push(b.condition);
                work.push(b.body);
            }
            Control::Do(b) => {
                work.push(b.body);
                work.push(b.condition);
            }
        }
    }
}

fn scan_if(
    parent_index: usize,
    seq_index: usize,
    controls: &[ControlBlock],
    basics: &IndexMap<usize, BasicBlock>,
) -> Option<usize> {
    let parent = &controls[parent_index];
    let children = match &parent.control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let cond_ctrl = &controls[children[seq_index]];
    let cond_block = basic_block_get_exact(basics, cond_ctrl.start, cond_ctrl.end);

    // Require conditional forward jump, still within parent block
    if !(cond_block.exits.len() == 2
        && cond_block.exits[0] == cond_block.end
        && cond_block.exits[1] >= cond_block.end
        && cond_block.exits[1] <= parent.end)
    {
        return None;
    }

    let body_end = cond_block.exits[1];
    Some(body_end)
}

fn build_if(
    parent_index: usize,
    cond_seq: usize,
    body_end: usize,
    controls: &mut Vec<ControlBlock>,
) {
    let body_seq_end = seq_index_ending_at_addr(parent_index, body_end, controls);
    debug!(
        parent = %Block(parent_index, controls),
        cond = %SeqBlock(parent_index, cond_seq, controls),
        body = %SeqRange(parent_index, (cond_seq + 1)..=body_seq_end, controls),
        "building if",
    );

    // Drain the range of blocks which will form the body.

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let mut drain = seq_blocks.drain(cond_seq..=body_seq_end);
    let condition = drain.next().unwrap();
    let body_blocks: Vec<_> = drain.collect();

    let cond_start = controls[condition].start;
    let body_start = controls[condition].end;

    // Combine the list of body blocks into one.

    let body = match body_blocks.len() {
        0 => {
            // If there are none, synthesize a zero-length block.
            let body = controls.len();
            controls.push(ControlBlock {
                start: body_start,
                end: body_start,
                control: Control::CodeRange,
            });
            body
        }
        1 => body_blocks[0],
        _ => {
            let body = controls.len();
            controls.push(ControlBlock {
                start: body_start,
                end: body_end,
                control: Control::Sequence(body_blocks),
            });
            body
        }
    };

    // Create the if block, then insert it back into the parent sequence.

    let result = controls.len();
    controls.push(ControlBlock {
        start: cond_start,
        end: body_end,
        control: Control::If(If {
            condition,
            true_: body,
            false_: None,
        }),
    });

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    seq_blocks.insert(cond_seq, result);
}

fn scan_while(
    parent_index: usize,
    seq_index: usize,
    controls: &[ControlBlock],
    basics: &IndexMap<usize, BasicBlock>,
) -> Option<usize> {
    let parent = &controls[parent_index];
    let children = match &parent.control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let cond_ctrl = &controls[children[seq_index]];
    let cond_block = basic_block_get_exact(basics, cond_ctrl.start, cond_ctrl.end);

    // Require conditional forward jump as the condition
    if !(cond_block.exits.len() == 2
        && cond_block.exits[0] == cond_block.end
        && cond_block.exits[1] > cond_block.end
        && cond_block.exits[1] <= parent.end)
    {
        return None;
    }

    let body_end = cond_block.exits[1];
    let body_end_block_index = basic_blocks_get_index_by_end(basics, body_end);
    let body_end_block = &basics[body_end_block_index];
    // Require the end block to jump back to the condition
    if !(body_end_block.exits.len() == 1 && body_end_block.exits[0] == cond_block.start) {
        return None;
    }

    let body_seq_end = children
        .iter()
        .position(|&i| controls[i].end == body_end)
        .unwrap();

    Some(body_seq_end)
}

fn build_while(
    parent_index: usize,
    cond_seq: usize,
    body_seq_end: usize,
    controls: &mut Vec<ControlBlock>,
) {
    debug!(
        parent = %Block(parent_index, controls),
        cond = %SeqBlock(parent_index, cond_seq, controls),
        body = %SeqRange(parent_index, (cond_seq + 1)..=body_seq_end, controls),
        "building while",
    );

    // Drain the range of blocks which will form the body.

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let mut drain = seq_blocks.drain(cond_seq..=body_seq_end);
    let condition = drain.next().unwrap();
    let body_blocks: Vec<_> = drain.collect();

    let cond_start = controls[condition].start;
    let body_start = controls[*body_blocks.first().unwrap()].start;
    let body_end = controls[*body_blocks.last().unwrap()].end;

    // Combine the list of body blocks into one.

    let body = match body_blocks.len() {
        0 => todo!(),
        1 => body_blocks[0],
        _ => {
            let body = controls.len();
            controls.push(ControlBlock {
                start: body_start,
                end: body_end,
                control: Control::Sequence(body_blocks),
            });
            body
        }
    };

    // Create the while block, then insert it back into the parent sequence.

    let result = controls.len();
    controls.push(ControlBlock {
        start: cond_start,
        end: body_end,
        control: Control::While(While { condition, body }),
    });

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    seq_blocks.insert(cond_seq, result);
}

fn scan_elses(
    index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) {
    match &controls[index].control {
        Control::CodeRange => {}
        Control::Sequence(_) => {
            scan_elses_in_sequence(index, basics, controls, work);
        }
        Control::If(b) => {
            work.push(b.condition);
            work.push(b.true_);
            work.extend(b.false_);
        }
        Control::While(b) => {
            work.push(b.condition);
            work.push(b.body);
        }
        Control::Do(b) => {
            work.push(b.body);
            work.push(b.condition);
        }
    }
}

fn scan_elses_in_sequence(
    parent_index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) {
    let children = match &controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };

    for i in 0..children.len() {
        match &controls[children[i]].control {
            Control::CodeRange => {}
            Control::Sequence(_) => unreachable!(),
            Control::If(b) => {
                if let Some(else_end) = scan_else(parent_index, i, basics, controls) {
                    build_else(parent_index, i, else_end, controls);
                    work.push(parent_index); // Come back later for the rest of the elses
                    return;
                }

                work.push(b.condition);
                work.push(b.true_);
                work.extend(b.false_);
            }
            Control::While(b) => {
                work.push(b.condition);
                work.push(b.body);
            }
            Control::Do(b) => {
                work.push(b.body);
                work.push(b.condition);
            }
        }
    }
}

fn scan_else(
    parent_index: usize,
    if_seq_index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &[ControlBlock],
) -> Option<usize> {
    let parent = &controls[parent_index];
    let parent_blocks = match &controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let if_index = parent_blocks[if_seq_index];
    let if_ = match &controls[if_index].control {
        Control::If(b) => b,
        _ => return None,
    };

    // An if can only have one else, of course
    if if_.false_.is_some() {
        return None;
    }

    let true_end_index = basic_blocks_get_index_by_end(basics, controls[if_.true_].end);
    let true_end = &basics[true_end_index];

    // Require an unconditional jump over a code range within the same parent, which
    // will form the else.
    if !(true_end.exits.len() == 1
        && true_end.exits[0] > true_end.end
        && true_end.exits[0] <= parent.end)
    {
        return None;
    }

    let else_end = true_end.exits[0];

    // Require the end to be on a block boundary or within a splittable block
    match seq_end_boundary(parent_index, else_end, controls) {
        (_, SeqBoundary::Exact | SeqBoundary::CanSplit) => {}
        (_, SeqBoundary::CanNotSplit) => return None,
    }

    Some(else_end)
}

fn build_else(
    parent_index: usize,
    if_seq_index: usize,
    else_end: usize,
    controls: &mut Vec<ControlBlock>,
) {
    let children = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let if_index = children[if_seq_index];
    let if_ctrl = &controls[if_index];
    let else_start = if_ctrl.end;
    debug!(
        if_ = %AddrRange(if_ctrl.start..if_ctrl.end),
        "else" = %AddrRange(else_start..else_end),
        "building else, parent=#{parent_index}",
    );

    // Drain the range of blocks which will form the else.

    let else_seq_start = if_seq_index + 1;
    let else_seq_end = seq_index_ending_at_addr(parent_index, else_end, controls);

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let else_blocks: Vec<_> = seq_blocks.drain(else_seq_start..=else_seq_end).collect();

    // Combine the list of else blocks into one.

    let else_ = match else_blocks.len() {
        0 => todo!(),
        1 => else_blocks[0],
        _ => {
            let else_ = controls.len();
            controls.push(ControlBlock {
                start: else_start,
                end: else_end,
                control: Control::Sequence(else_blocks),
            });
            else_
        }
    };

    // Grow the if block to contain the new else block.

    controls[if_index].end = else_end;
    let mut if_ = match &mut controls[if_index].control {
        Control::If(if_) => if_,
        _ => unreachable!(),
    };
    if_.false_ = Some(else_);
}

fn scan_loops(
    index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) {
    match &controls[index].control {
        Control::CodeRange | Control::Sequence(_) => {
            scan_loops_in_sequence(index, basics, controls, work);
        }
        Control::If(b) => {
            work.push(b.condition);
            work.push(b.true_);
            work.extend(b.false_);
        }
        Control::While(b) => {
            work.push(b.condition);
            work.push(b.body);
        }
        Control::Do(_) => {}
    }
}

fn scan_loops_in_sequence(
    index: usize,
    basics: &IndexMap<usize, BasicBlock>,
    controls: &mut Vec<ControlBlock>,
    work: &mut Vec<usize>,
) {
    let blocks = match &controls[index].control {
        Control::CodeRange => slice::from_ref(&index),
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };

    for &block in blocks {
        match &controls[block].control {
            Control::CodeRange => {
                if controls[block].start == controls[block].end {
                    continue;
                }
                let basic_start = basics.get_index_of(&controls[block].start).unwrap();
                let basic_end = basic_blocks_get_index_by_end(basics, controls[block].end);
                for i in basic_start..=basic_end {
                    if let Some(build) = scan_do(index, i, controls, basics) {
                        build_do(index, build, controls);
                        work.push(index); // Scan for later loops in same sequence
                        return;
                    }
                }
            }
            Control::Sequence(_) => unreachable!(),
            Control::If(b) => {
                work.push(b.condition);
                work.push(b.true_);
                work.extend(b.false_);
            }
            Control::While(b) => {
                work.push(b.condition);
                work.push(b.body);
            }
            Control::Do(_) => {}
        }
    }
}

fn scan_do(
    parent_index: usize,
    basic_index: usize,
    controls: &[ControlBlock],
    basics: &IndexMap<usize, BasicBlock>,
) -> Option<BuildDo> {
    let parent = &controls[parent_index];

    let end_block = &basics[basic_index];
    let condition_kind = match end_block.exits.len() {
        1 => ConditionKind::Always,
        2 => ConditionKind::Until,
        _ => unreachable!(),
    };
    let start = *end_block.exits.last().unwrap();
    let end = end_block.end;

    // Require backward jump within parent sequence
    if !(start < end && start >= parent.start) {
        return None;
    }

    // Require the start/end to be on block boundaries or within splittable blocks
    match &controls[parent_index].control {
        Control::CodeRange => {}
        Control::Sequence(_) => {
            match seq_start_boundary(parent_index, start, controls) {
                (_, SeqBoundary::Exact | SeqBoundary::CanSplit) => {}
                (_, SeqBoundary::CanNotSplit) => return None,
            }
            match seq_end_boundary(parent_index, end, controls) {
                (_, SeqBoundary::Exact | SeqBoundary::CanSplit) => {}
                (_, SeqBoundary::CanNotSplit) => return None,
            }
        }
        _ => unreachable!(),
    }

    Some(BuildDo {
        start,
        end,
        condition_kind,
    })
}

struct BuildDo {
    start: usize,
    end: usize,
    condition_kind: ConditionKind,
}

fn build_do(
    parent_index: usize,
    BuildDo {
        start,
        end,
        condition_kind,
    }: BuildDo,
    controls: &mut Vec<ControlBlock>,
) {
    debug!(
        parent = %Block(parent_index, controls),
        addr = %AddrRange(start..end),
        ?condition_kind,
        "building do",
    );

    // Ensure the blocks have boundaries at the start/end addresses

    let seq_start_index = seq_index_starting_at_addr(parent_index, start, controls);
    let seq_end_index = seq_index_ending_at_addr(parent_index, end, controls);

    // Drain the range of blocks which will form the loop.

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let mut drain = seq_blocks.drain(seq_start_index..=seq_end_index);
    let body_blocks: Vec<_> = drain
        .by_ref()
        .take(seq_end_index - seq_start_index)
        .collect();
    let condition = drain.next().unwrap();
    debug_assert!(drain.next().is_none());
    drop(drain);

    // Combine the list of body blocks into one.

    let body = match body_blocks.len() {
        0 => {
            // If there are none, synthesize a zero-length block.
            debug_assert!(start == controls[condition].start);
            let body = controls.len();
            controls.push(ControlBlock {
                start,
                end: start,
                control: Control::CodeRange,
            });
            body
        }
        1 => body_blocks[0],
        _ => {
            let body = controls.len();
            controls.push(ControlBlock {
                start,
                end: controls[*body_blocks.last().unwrap()].end,
                control: Control::Sequence(body_blocks),
            });
            body
        }
    };

    // Create the do block, then insert it back into the parent sequence.

    let do_ = controls.len();
    controls.push(ControlBlock {
        start,
        end,
        control: Control::Do(Do {
            body,
            condition,
            condition_kind,
        }),
    });

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    seq_blocks.insert(seq_start_index, do_);
}

fn seq_start_boundary(
    parent_index: usize,
    addr: usize,
    controls: &[ControlBlock],
) -> (usize, SeqBoundary) {
    debug_assert!(controls[parent_index].start <= addr && addr <= controls[parent_index].end);
    let seq_blocks = match &controls[parent_index].control {
        Control::Sequence(seq_blocks) => seq_blocks,
        _ => unreachable!(),
    };
    let i = seq_blocks
        .iter()
        .position(|&b| controls[b].end > addr)
        .unwrap();
    let split = if addr == controls[seq_blocks[i]].start {
        SeqBoundary::Exact
    } else if matches!(controls[seq_blocks[i]].control, Control::CodeRange) {
        SeqBoundary::CanSplit
    } else {
        SeqBoundary::CanNotSplit
    };
    (i, split)
}

fn seq_end_boundary(
    parent_index: usize,
    addr: usize,
    controls: &[ControlBlock],
) -> (usize, SeqBoundary) {
    debug_assert!(controls[parent_index].start <= addr && addr <= controls[parent_index].end);
    let seq_blocks = match &controls[parent_index].control {
        Control::Sequence(seq_blocks) => seq_blocks,
        _ => unreachable!(),
    };
    let i = seq_blocks
        .iter()
        .rposition(|&b| controls[b].start < addr)
        .unwrap();
    let split = if addr == controls[seq_blocks[i]].end {
        SeqBoundary::Exact
    } else if matches!(controls[seq_blocks[i]].control, Control::CodeRange) {
        SeqBoundary::CanSplit
    } else {
        SeqBoundary::CanNotSplit
    };
    (i, split)
}

enum SeqBoundary {
    Exact,
    CanSplit,
    CanNotSplit,
}

fn seq_index_starting_at_addr(
    parent_index: usize,
    addr: usize,
    controls: &mut Vec<ControlBlock>,
) -> usize {
    debug_assert!(controls[parent_index].start <= addr && addr <= controls[parent_index].end);

    match seq_start_boundary(parent_index, addr, controls) {
        (i, SeqBoundary::Exact) => i,
        (i, SeqBoundary::CanSplit) => {
            split_seq(parent_index, i, addr, controls);
            i + 1
        }
        (_, SeqBoundary::CanNotSplit) => unreachable!(),
    }
}

fn seq_index_ending_at_addr(
    parent_index: usize,
    addr: usize,
    controls: &mut Vec<ControlBlock>,
) -> usize {
    debug_assert!(controls[parent_index].start <= addr && addr <= controls[parent_index].end);

    match seq_end_boundary(parent_index, addr, controls) {
        (i, SeqBoundary::Exact) => i,
        (i, SeqBoundary::CanSplit) => {
            split_seq(parent_index, i, addr, controls);
            i
        }
        (_, SeqBoundary::CanNotSplit) => unreachable!(),
    }
}

fn split_seq(parent_index: usize, seq_index: usize, addr: usize, controls: &mut Vec<ControlBlock>) {
    let seq_blocks = match &controls[parent_index].control {
        Control::Sequence(seq_blocks) => seq_blocks,
        _ => unreachable!(),
    };
    let start = controls[seq_blocks[seq_index]].start;
    let end = controls[seq_blocks[seq_index]].end;
    trace!("split_seq #{parent_index}[{seq_index}] 0x{start:x}..0x{end:x} at 0x{addr:x}");
    debug_assert!(start < addr && addr < end);
    debug_assert!(matches!(
        controls[seq_blocks[seq_index]].control,
        Control::CodeRange,
    ));

    let first = seq_blocks[seq_index];
    controls[first].end = addr;

    let second = controls.len();
    controls.push(ControlBlock {
        start: addr,
        end,
        control: Control::CodeRange,
    });

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(seq_blocks) => seq_blocks,
        _ => unreachable!(),
    };
    seq_blocks.insert(seq_index + 1, second);
}

fn check_control_invariants(
    root: usize,
    controls: &[ControlBlock],
    basics: &IndexMap<usize, BasicBlock>,
) {
    #[cfg(not(debug_assertions))]
    return;

    macro_rules! asrt {
        ($cond:expr) => {
            assert!($cond, "{} invariant violation in #{}", Dump(controls), root);
        };
    }

    let ctrl = &controls[root];
    asrt!(ctrl.start <= ctrl.end);

    match &ctrl.control {
        Control::CodeRange => {
            asrt!(basics[&ctrl.start].start == ctrl.start);
            let _ = &basics[&ctrl.start]; // Ensure it starts exactly on a basic block
            // Zero-length starting blocks are okay. All others must have end exactly on
            // a basic block
            if !(ctrl.start == 0 && ctrl.end == 0) {
                let end_index = basic_blocks_get_index_by_end(basics, ctrl.end);
                asrt!(basics[end_index].end == ctrl.end);
            }
        }
        Control::Sequence(xs) => {
            asrt!(!xs.is_empty());
            asrt!(controls[*xs.first().unwrap()].start == ctrl.start);
            asrt!(controls[*xs.last().unwrap()].end == ctrl.end);
            for i in 0..xs.len() - 1 {
                asrt!(controls[xs[i]].end == controls[xs[i + 1]].start);
            }
            for &child in xs {
                check_control_invariants(child, controls, basics);
            }
        }
        Control::If(b) => {
            asrt!(ctrl.start == controls[b.condition].start);
            asrt!(controls[b.condition].end == controls[b.true_].start);
            check_control_invariants(b.condition, controls, basics);
            check_control_invariants(b.true_, controls, basics);
            match b.false_ {
                None => {
                    asrt!(ctrl.end == controls[b.true_].end);
                }
                Some(false_) => {
                    asrt!(ctrl.end == controls[false_].end);
                    asrt!(controls[b.true_].end == controls[false_].start);
                    check_control_invariants(false_, controls, basics);
                }
            }
        }
        Control::While(b) => {
            asrt!(ctrl.start == controls[b.condition].start);
            asrt!(ctrl.end == controls[b.body].end);
            asrt!(controls[b.condition].end == controls[b.body].start);
            check_control_invariants(b.condition, controls, basics);
            check_control_invariants(b.body, controls, basics);
        }
        Control::Do(b) => {
            asrt!(ctrl.start == controls[b.body].start);
            asrt!(ctrl.end == controls[b.condition].end);
            asrt!(controls[b.body].end == controls[b.condition].start);
            check_control_invariants(b.body, controls, basics);
            check_control_invariants(b.condition, controls, basics);

            let cond_block = &basics[&controls[b.condition].start];
            match b.condition_kind {
                ConditionKind::Always => {
                    asrt!(cond_block.exits.len() == 1);
                    asrt!(cond_block.exits[0] == ctrl.start);
                }
                ConditionKind::Until => {
                    asrt!(cond_block.exits.len() == 2);
                    asrt!(cond_block.exits[0] == ctrl.end);
                    asrt!(cond_block.exits[1] == ctrl.start);
                }
            }
        }
    }
}

struct AddrRange(Range<usize>);

impl fmt::Display for AddrRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{:x}..0x{:x}", self.0.start, self.0.end)
    }
}

struct Block<'a>(usize, &'a [ControlBlock]);

impl fmt::Display for Block<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self(index, controls) = self;
        let block = &controls[index];
        write!(f, "#{}(0x{:x}..0x{:x})", index, block.start, block.end)
    }
}

struct SeqBlock<'a>(usize, usize, &'a [ControlBlock]);

impl fmt::Display for SeqBlock<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self(parent_index, seq_index, controls) = self;
        let children = match &controls[parent_index].control {
            Control::Sequence(blocks) => blocks,
            _ => unreachable!(),
        };
        let block = children[seq_index];
        let ctrl = &controls[block];
        write!(
            f,
            "#{}[{}]->#{}(0x{:x}..0x{:x})",
            parent_index, seq_index, block, ctrl.start, ctrl.end,
        )
    }
}

struct SeqRange<'a>(usize, RangeInclusive<usize>, &'a [ControlBlock]);

impl fmt::Display for SeqRange<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self(parent_index, ref seq_range, controls) = self;
        let seq_start = *seq_range.start();
        let seq_end = *seq_range.end();
        let children = match &controls[parent_index].control {
            Control::Sequence(blocks) => blocks,
            _ => unreachable!(),
        };
        let start = controls[children[seq_start]].start;
        let end = controls[children[seq_end]].end;
        write!(
            f,
            "#{}[{}..={}](0x{:x}..0x{:x})",
            parent_index, seq_start, seq_end, start, end,
        )
    }
}

struct Dump<'a>(&'a [ControlBlock]);

impl fmt::Display for Dump<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, block) in self.0.iter().enumerate() {
            writeln!(
                f,
                "#{} {} {:?}",
                i,
                AddrRange(block.start..block.end),
                block.control,
            )?;
        }
        Ok(())
    }
}
