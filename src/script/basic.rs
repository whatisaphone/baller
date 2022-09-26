use crate::script::{decode::Decoder, ins::Ins};
use arrayvec::ArrayVec;
use indexmap::IndexMap;
use std::mem;

#[derive(Debug)]
pub struct BasicBlock {
    pub start: usize,
    pub end: usize,
    pub exits: ArrayVec<usize, 2>,
}

pub fn find_basic_blocks(code: &[u8]) -> (IndexMap<usize, BasicBlock>, usize) {
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

pub fn basic_blocks_get_index_by_end(blocks: &IndexMap<usize, BasicBlock>, addr: usize) -> usize {
    let result = 'result: loop {
        if let Some(index) = blocks.get_index_of(&addr) {
            break 'result index - 1;
        }
        break 'result blocks.len() - 1;
    };
    debug_assert!(blocks[result].end == addr);
    result
}
