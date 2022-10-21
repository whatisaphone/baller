use crate::compiler::{
    errors::{CompileError, CompileErrorPayload},
    loc::FileId,
    token::{Token, TokenKind, TokenPayload},
};

pub struct Lexer<'a> {
    file: FileId,
    source: &'a str,
    pos: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(file: FileId, source: &'a str) -> Self {
        Self {
            file,
            source,
            pos: 0,
        }
    }

    pub fn next(&mut self) -> Result<Token, CompileError> {
        self.eat_whitespace();
        let token_pos = self.pos;
        match self.cur_char() {
            None => Ok(Token::new(token_pos, TokenPayload::Eof)),
            Some('\n') => {
                self.pos += 1;
                Ok(Token::new(token_pos, TokenPayload::Newline))
            }
            Some('=') => {
                self.pos += 1;
                Ok(Token::new(token_pos, TokenPayload::Eq))
            }
            Some('0'..='9') => {
                let len = 1 + scan(self.source, token_pos + 1, |ch| ('0'..='9').contains(&ch));
                self.pos += len;
                let value: i32 = substr(self.source, token_pos, len).parse().map_err(|_| {
                    CompileError::new(self.file.at(token_pos), CompileErrorPayload::BadInteger)
                })?;
                Ok(Token::new(token_pos, TokenPayload::Integer { value }))
            }
            Some('"') => {
                let len = scan_string(self.file, self.source, token_pos)?;
                self.pos += len;
                Ok(Token::new(token_pos, TokenPayload::String { len }))
            }
            Some(ch) if is_ident_start(ch) => {
                let len = scan_ident(self.source, token_pos);
                self.pos += len;
                Ok(Token::new(token_pos, TokenPayload::Ident { len }))
            }
            Some(ch) => {
                Err(CompileError::new(
                    self.file.at(token_pos),
                    CompileErrorPayload::InvalidChar { char: ch },
                ))
            }
        }
    }

    fn eat_whitespace(&mut self) {
        while self.cur_char() == Some(' ') {
            self.pos += 1;
        }
    }

    fn cur_char(&self) -> Option<char> {
        self.source[self.pos.try_into().unwrap()..].chars().next()
    }

    pub fn expect_ident(&mut self, expected: &'static str) -> Result<u32, CompileError> {
        let token = self.next()?;
        match token.payload {
            TokenPayload::Ident { len } if substr(self.source, token.offset, len) == expected => {
                Ok(token.offset)
            }
            _ => {
                Err(CompileError::new(
                    self.file.at(token.offset),
                    CompileErrorPayload::UnexpectedToken { expected },
                ))
            }
        }
    }

    pub fn expect_integer(&mut self) -> Result<(u32, i32), CompileError> {
        let token = self.next()?;
        match token.payload {
            TokenPayload::Integer { value } => Ok((token.offset, value)),
            _ => {
                Err(CompileError::new(
                    self.file.at(token.offset),
                    CompileErrorPayload::UnexpectedToken {
                        expected: "integer",
                    },
                ))
            }
        }
    }

    pub fn expect_choice(
        &mut self,
        choices: &[TokenKind],
        diagnostic: &'static str,
    ) -> Result<Token, CompileError> {
        let token = self.next()?;
        if choices.contains(&token.kind()) {
            return Ok(token);
        }
        Err(CompileError::new(
            self.file.at(token.offset),
            CompileErrorPayload::UnexpectedToken {
                expected: diagnostic,
            },
        ))
    }

    pub fn expect_token(&mut self, expected: TokenKind) -> Result<(), CompileError> {
        let token = self.next()?;
        if token.kind() == expected {
            return Ok(());
        }
        Err(CompileError::new(
            self.file.at(token.offset),
            CompileErrorPayload::UnexpectedToken {
                expected: expected.describe(),
            },
        ))
    }

    pub fn expect_string(&mut self) -> Result<(u32, u32), CompileError> {
        let token = self.next()?;
        match token.payload {
            TokenPayload::String { len } => Ok((token.offset, len)),
            _ => {
                Err(CompileError::new(
                    self.file.at(token.offset),
                    CompileErrorPayload::UnexpectedToken { expected: "string" },
                ))
            }
        }
    }
}

fn scan(source: &str, start: u32, predicate: impl Fn(char) -> bool) -> u32 {
    let mut i: usize = start.try_into().unwrap();
    loop {
        match source[i..].chars().next() {
            Some(ch) if predicate(ch) => i += ch.len_utf8(),
            None | Some(_) => return u32::try_from(i).unwrap() - start,
        }
    }
}

fn scan_ident(source: &str, start: u32) -> u32 {
    let source = &source[start.try_into().unwrap()..];
    let ch = source.chars().next().unwrap();
    debug_assert!(is_ident_start(ch));
    let len0: u32 = ch.len_utf8().try_into().unwrap();
    len0 + scan(source, len0, is_ident_cont)
}

fn scan_string(file: FileId, source: &str, start: u32) -> Result<u32, CompileError> {
    debug_assert!(source[start.try_into().unwrap()..].starts_with('"'));
    let len = scan(source, start + 1, |ch| ch != '"');
    let end = start + 1 + len;
    match source[end.try_into().unwrap()..].chars().next() {
        Some('"') => Ok(1 + len + 1),
        Some(_) => unreachable!(),
        None => {
            Err(CompileError::new(
                file.at(end),
                CompileErrorPayload::UnexpectedToken {
                    expected: "string closing quote",
                },
            ))
        }
    }
}

pub fn substr(source: &str, start: u32, len: u32) -> &str {
    let start: usize = start.try_into().unwrap();
    let len: usize = len.try_into().unwrap();
    &source[start..start + len]
}

pub fn string_contents(source: &str, offset: u32, len: u32) -> &str {
    debug_assert!(substr(source, offset, 1) == "\"");
    debug_assert!(substr(source, offset + len - 1, 1) == "\"");
    // TODO: character escapes and whatnot
    &source[(offset + 1).try_into().unwrap()..(offset + len - 1).try_into().unwrap()]
}

fn is_ident_start(ch: char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z' | '_')
}

fn is_ident_cont(ch: char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z' | '_' | '0'..='9')
}
