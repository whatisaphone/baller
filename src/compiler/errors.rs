use std::{io, io::Write};

pub struct CompileError {
    pub offset: u32,
    pub payload: CompileErrorPayload,
}

impl CompileError {
    pub fn new(offset: u32, payload: CompileErrorPayload) -> Self {
        Self { offset, payload }
    }
}

pub enum CompileErrorPayload {
    InvalidChar { char: char },
    UnexpectedToken { expected: &'static str },
    Duplicate,
    BadInteger,
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
        }
        Ok(())
    }
}

pub fn output_error(error: &CompileError) -> io::Result<()> {
    let mut w = io::stderr();
    write!(w, "error at offset {}: ", error.offset)?;
    error.write(&mut w)?;
    Ok(())
}
