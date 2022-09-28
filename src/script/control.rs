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
    pub condition: usize,
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

    work.push(0);

    while let Some(index) = work.pop() {
        flatten_sequences(index, &mut controls, &mut work);
    }

    work.push(0);

    while let Some(index) = work.pop() {
        scan_loops(index, basics, &mut controls, &mut work);
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

    let children = match &controls[parent_index].control {
        Control::Sequence(children) => (children),
        _ => unreachable!(),
    };
    debug_assert!(children.len() == 2);
    let first = children[0];
    let second = children[1];
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

    controls[if_index].end = else_end;
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
            work.push(b.condition);
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
            work.push(b.condition);
        }
    }
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
        Control::Do(b) => {
            work.push(b.body);
            work.push(b.condition);
        }
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
            Control::Do(b) => {
                work.push(b.body);
                work.push(b.condition);
            }
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

    // Require conditional backward jump within parent sequence
    let cond_block = &basics[basic_index];
    if !(cond_block.exits.len() == 2
        && cond_block.exits[0] == cond_block.end
        && cond_block.exits[1] < cond_block.end
        && cond_block.exits[1] >= parent.start)
    {
        return None;
    }

    let body_start = cond_block.exits[1];
    let body_end = cond_block.start;
    let cond_start = cond_block.start;
    let cond_end = cond_block.end;

    Some(BuildDo {
        body_start,
        body_end,
        cond_start,
        cond_end,
    })
}

struct BuildDo {
    body_start: usize,
    body_end: usize,
    cond_start: usize,
    cond_end: usize,
}

fn build_do(
    parent_index: usize,
    BuildDo {
        body_start,
        body_end,
        cond_start,
        cond_end,
    }: BuildDo,
    controls: &mut Vec<ControlBlock>,
) {
    trace!(
        body = %AddrRange(body_start..body_end),
        condition = %AddrRange(cond_start..cond_end),
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

    let body_seq_index = seq_index_starting_at_addr(parent_index, body_start, controls);
    let cond_seq_index = seq_index_ending_at_addr(parent_index, cond_end, controls);

    // Drain the range of blocks which will form the loop.

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    let mut drain = seq_blocks.drain(body_seq_index..=cond_seq_index);
    let seq_children: Vec<_> = drain
        .by_ref()
        .take(cond_seq_index - body_seq_index)
        .collect();
    let condition = drain.next().unwrap();
    debug_assert!(drain.next().is_none());
    drop(drain);

    // Combine the list of blocks into one. If there are none, synthesize a
    // zero-length block.

    let body = match seq_children.len() {
        0 => {
            debug_assert!(body_start == body_end);
            let body = controls.len();
            controls.push(ControlBlock {
                start: body_start,
                end: body_end,
                control: Control::CodeRange,
            });
            body
        }
        1 => seq_children[0],
        _ => {
            let body = controls.len();
            controls.push(ControlBlock {
                start: body_start,
                end: body_end,
                control: Control::Sequence(seq_children),
            });
            body
        }
    };

    // Create the do block, then insert it back into the parent sequence.

    let do_ = controls.len();
    controls.push(ControlBlock {
        start: body_start,
        end: cond_end,
        control: Control::Do(Do { body, condition }),
    });

    let seq_blocks = match &mut controls[parent_index].control {
        Control::Sequence(blocks) => blocks,
        _ => unreachable!(),
    };
    seq_blocks.insert(body_seq_index, do_);
}

fn seq_index_starting_at_addr(
    parent_index: usize,
    addr: usize,
    controls: &mut Vec<ControlBlock>,
) -> usize {
    debug_assert!(controls[parent_index].start <= addr && addr <= controls[parent_index].end);
    let seq_blocks = match &controls[parent_index].control {
        Control::Sequence(seq_blocks) => seq_blocks,
        _ => unreachable!(),
    };
    let i = seq_blocks
        .iter()
        .position(|&b| controls[b].end > addr)
        .unwrap();
    if addr == controls[seq_blocks[i]].start {
        return i;
    }

    split_seq(parent_index, i, addr, controls);
    i + 1
}

fn seq_index_ending_at_addr(
    parent_index: usize,
    addr: usize,
    controls: &mut Vec<ControlBlock>,
) -> usize {
    debug_assert!(controls[parent_index].start <= addr && addr <= controls[parent_index].end);
    let seq_blocks = match &controls[parent_index].control {
        Control::Sequence(seq_blocks) => seq_blocks,
        _ => unreachable!(),
    };
    let i = seq_blocks
        .iter()
        .rposition(|&b| controls[b].start < addr)
        .unwrap();
    if addr == controls[seq_blocks[i]].end {
        return i;
    }

    split_seq(parent_index, i, addr, controls);
    i
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
