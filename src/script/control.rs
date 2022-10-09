use crate::script::basic::{basic_blocks_get_index_by_end, BasicBlock};
use indexmap::IndexMap;
use std::{fmt, mem, ops::Range, slice};
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
    pub condition: Option<usize>,
}

#[instrument(level = "debug", skip(basics))]
pub fn build_control_structures(basics: &IndexMap<usize, BasicBlock>) -> Vec<ControlBlock> {
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
    check_control_invariants(0, &controls, basics);

    work.push(0);
    while let Some(index) = work.pop() {
        flatten_sequences(index, &mut controls, &mut work);
    }
    check_control_invariants(0, &controls, basics);

    work.push(0);
    while let Some(index) = work.pop() {
        scan_elses(index, basics, &mut controls, &mut work);
    }
    check_control_invariants(0, &controls, basics);

    work.push(0);
    while let Some(index) = work.pop() {
        scan_loops(index, basics, &mut controls, &mut work);
    }
    check_control_invariants(0, &controls, basics);

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
    if control.start == control.end {
        return; // ignore empty blocks
    }

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

    controls[index].control = Control::Sequence(vec![out1, out2]);
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
        && cond_block.exits[1] >= cond_block.end
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

    true
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

fn flatten_sequences(index: usize, controls: &mut Vec<ControlBlock>, work: &mut Vec<usize>) {
    match &mut controls[index].control {
        Control::CodeRange => {}
        Control::Sequence(_) => {
            let mut sink = Vec::new();

            flatten_sequence(index, &mut sink, controls, work);

            match &mut controls[index].control {
                Control::Sequence(ch) => *ch = sink,
                _ => unreachable!(),
            }
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
            if let Some(condition) = b.condition {
                work.push(condition);
            }
        }
    }
}

fn flatten_sequence(
    index: usize,
    sink: &mut Vec<usize>,
    controls: &[ControlBlock],
    work: &mut Vec<usize>,
) {
    // Recurse into sequences

    if let Control::Sequence(children) = &controls[index].control {
        for &child in children {
            flatten_sequence(child, sink, controls, work);
        }
        return;
    }

    // All other types are stored in the current sequence, then added to the work
    // queue.

    sink.push(index);

    match &controls[index].control {
        Control::Sequence(_) => unreachable!(),
        Control::CodeRange => {}
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
            if let Some(condition) = b.condition {
                work.push(condition);
            }
        }
    }
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
            work.extend(b.condition);
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
                work.extend(b.condition);
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
    let conditional = match end_block.exits.len() {
        1 => false,
        2 => true,
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
        conditional,
    })
}

struct BuildDo {
    start: usize,
    end: usize,
    conditional: bool,
}

fn build_do(
    parent_index: usize,
    BuildDo {
        start,
        end,
        conditional,
    }: BuildDo,
    controls: &mut Vec<ControlBlock>,
) {
    trace!(
        addr = %AddrRange(start..end),
        conditional,
        "building do, parent=#{}",
        parent_index,
    );

    // This is easier if we can assume the parent is a sequence. If not, make it so.

    match controls[parent_index].control {
        Control::CodeRange => {
            // Replace the block with a sequence of length one that contains the old block.
            // This moves the old block to a new index.
            let code_index = controls.len();
            let seq_ctrl = ControlBlock {
                start: controls[parent_index].start,
                end: controls[parent_index].end,
                control: Control::Sequence(vec![code_index]),
            };
            let code_ctrl = mem::replace(&mut controls[parent_index], seq_ctrl);
            controls.push(code_ctrl);
        }
        Control::Sequence(_) => {}
        _ => unreachable!(),
    };

    // Ensure the blocks have boundaries at the start/end addresses

    let seq_start_index = seq_index_starting_at_addr(parent_index, start, controls);
    let seq_end_index = seq_index_ending_at_addr(parent_index, end, controls);

    // Drain the range of blocks which will form the loop.

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let mut drain = seq_blocks.drain(seq_start_index..=seq_end_index);
    let (body_blocks, condition) = if conditional {
        let body_blocks: Vec<_> = drain
            .by_ref()
            .take(seq_end_index - seq_start_index)
            .collect();
        let condition = drain.next().unwrap();
        debug_assert!(drain.next().is_none());
        drop(drain);
        (body_blocks, Some(condition))
    } else {
        (drain.collect(), None)
    };

    // Combine the list of body blocks into one.

    let body = match body_blocks.len() {
        0 => {
            // If there are none, synthesize a zero-length block.
            debug_assert!(start == controls[condition.unwrap()].start);
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
        control: Control::Do(Do { body, condition }),
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
            check_control_invariants(b.body, controls, basics);
            match b.condition {
                None => {
                    asrt!(ctrl.end == controls[b.body].end);
                }
                Some(cond) => {
                    asrt!(ctrl.end == controls[cond].end);
                    asrt!(controls[b.body].end == controls[cond].start);
                    check_control_invariants(cond, controls, basics);
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
