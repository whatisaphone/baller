use std::{fmt, fmt::Write};

pub struct AnsiStr<'a>(pub &'a [u8]);

impl fmt::Display for AnsiStr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for &b in self.0 {
            if (0x20..=0x7e).contains(&b) {
                f.write_char(char::from(b))?;
            } else {
                write!(f, r"\x{b:02x}")?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for AnsiStr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('"')?;
        fmt::Display::fmt(self, f)?;
        f.write_char('"')?;
        Ok(())
    }
}
