use crate::{
    compiler::errors::{CompileError, CompileErrorPayload},
    FsEntry,
};
use std::error::Error;

pub struct SourceMap {
    paths: Vec<String>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            paths: Vec::with_capacity(64),
        }
    }

    pub fn add_file(&mut self, path: String) -> FileId {
        let id = FileId(self.paths.len().try_into().unwrap());
        self.paths.push(path);
        id
    }

    pub fn file_path(&self, file: FileId) -> &str {
        let file: usize = file.0.try_into().unwrap();
        &self.paths[file]
    }
}

#[derive(Copy, Clone)]
pub struct FileId(u32);

#[derive(Copy, Clone)]
pub struct Loc {
    pub file: FileId,
    pub offset: u32,
}

impl FileId {
    pub fn at(self, offset: u32) -> Loc {
        Loc { file: self, offset }
    }
}

pub fn add_source_file(
    path: String,
    reference_loc: Loc,
    map: &mut SourceMap,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(FileId, String), CompileError> {
    let data = read_file(&path, reference_loc, src_read)?;
    let source = String::from_utf8(data)
        .map_err(|_| CompileError::new(reference_loc, CompileErrorPayload::InvalidUtf8))?;
    let file = map.add_file(path);
    Ok((file, source))
}

fn read_file(
    path: &str,
    reference_loc: Loc,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy + Sized,
) -> Result<Vec<u8>, CompileError> {
    match src_read(path) {
        Ok(FsEntry::File(data)) => Ok(data),
        Ok(FsEntry::Dir(_)) | Err(_) => {
            Err(CompileError::new(
                reference_loc,
                CompileErrorPayload::CantReadFile,
            ))
        }
    }
}
