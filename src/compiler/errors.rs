use crate::compiler::loc::{Loc, SourceMap};
use std::{io, io::Write};

pub struct CompileError {
    pub loc: Loc,
    pub payload: CompileErrorPayload,
}

impl CompileError {
    pub fn new(loc: Loc, payload: CompileErrorPayload) -> Self {
        Self { loc, payload }
    }
}

pub enum CompileErrorPayload {
    InvalidChar { char: char },
    UnexpectedToken { expected: &'static str },
    Duplicate,
    BadInteger,
    CantReadFile,
    InvalidUtf8,
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
            CompileErrorPayload::InvalidUtf8 => {
                write!(w, "file contains invalid UTF-8")?;
            }
        }
        Ok(())
    }
}

pub fn report_error(map: &SourceMap, error: &CompileError) -> io::Result<()> {
    let mut w = io::stderr();
    write!(
        w,
        "{}:@{}: ",
        map.file_path(error.loc.file),
        error.loc.offset,
    )?;
    error.write(&mut w)?;
    Ok(())
}
