use crate::{blocks::BlockId, compiler::loc::Loc};
use std::num::NonZeroI32;

pub type AstNodeId = u32;

pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub lists: Vec<AstNodeId>,
    pub string_table: String,
    pub root_start: u32,
    pub root_len: u32,
}

pub enum AstNode {
    RawBlockFile(RawBlockFile),
    RawBlockContainer(RawBlockContainer),
}

pub struct RawBlockFile {
    pub block_id: BlockId,
    pub glob_number: Option<NonZeroI32>,
    pub path_loc: Loc,
    pub path_offset: u32,
    pub path_len: u32,
}

pub struct RawBlockContainer {
    pub block_id: BlockId,
    pub children_start: u32,
    pub children_len: u32,
}

impl Ast {
    pub fn node(&self, id: AstNodeId) -> &AstNode {
        let index: usize = id.try_into().unwrap();
        &self.nodes[index]
    }

    pub fn list(&self, offset: u32, len: u32) -> &[AstNodeId] {
        let start: usize = offset.try_into().unwrap();
        let end: usize = (offset + len).try_into().unwrap();
        &self.lists[start..end]
    }

    pub fn string(&self, offset: u32, len: u32) -> &str {
        let start: usize = offset.try_into().unwrap();
        let end: usize = (offset + len).try_into().unwrap();
        &self.string_table[start..end]
    }
}
