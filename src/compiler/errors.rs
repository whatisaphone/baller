use crate::compiler::loc::{Loc, SourceMap};
use std::{io, io::Write};

pub struct CompileError {
    pub loc: Option<Loc>,
    pub payload: CompileErrorPayload,
}

impl CompileError {
    pub fn new(loc: Loc, payload: CompileErrorPayload) -> Self {
        Self {
            loc: Some(loc),
            payload,
        }
    }

    pub fn new_without_loc(payload: CompileErrorPayload) -> Self {
        Self { loc: None, payload }
    }
}

pub enum CompileErrorPayload {
    InvalidChar { char: char },
    UnexpectedToken { expected: &'static str },
    Duplicate,
    BadInteger,
    CantReadFile,
    CantWriteOutput,
    InvalidUtf8,
    InvalidBlockId,
}

impl CompileError {
    pub fn write(&self, w: &mut impl Write) -> io::Result<()> {
        match &self.payload {
            CompileErrorPayload::InvalidChar { char } => {
                write!(w, "invalid character '{char}'")?;
            }
            CompileErrorPayload::UnexpectedToken { expected } => {
                write!(w, "unexpected token; expected {expected}")?;
            }
            CompileErrorPayload::Duplicate => {
                write!(w, "duplicate")?;
            }
            CompileErrorPayload::BadInteger => {
                write!(w, "integer is either invalid or out of range")?;
            }
            CompileErrorPayload::CantReadFile => {
                write!(w, "can't read file")?;
            }
            CompileErrorPayload::CantWriteOutput => {
                write!(w, "can't write output")?;
            }
            CompileErrorPayload::InvalidUtf8 => {
                write!(w, "file contains invalid UTF-8")?;
            }
            CompileErrorPayload::InvalidBlockId => {
                write!(w, "block ID must be exactly four printable characters")?;
            }
        }
        Ok(())
    }
}

pub fn report_error(map: &SourceMap, error: &CompileError) -> io::Result<()> {
    let mut w = io::stderr();
    if let Some(loc) = error.loc {
        write!(w, "{}:@{}: ", map.file_path(loc.file), loc.offset)?;
    }
    error.write(&mut w)?;
    Ok(())
}
