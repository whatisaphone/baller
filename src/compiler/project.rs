use crate::{
    compiler::{
        errors::{CompileError, CompileErrorPayload},
        lexer::{string_contents, substr, Lexer},
        loc::{FileId, Loc},
        token::{TokenKind, TokenPayload},
    },
    utils::vec::{extend_insert_some, DuplicateError},
};

pub struct Project<'a> {
    pub rooms: Vec<Option<Room<'a>>>,
}

pub struct Room<'a> {
    pub name: &'a str,
    pub disk_number: u8,
    pub name_loc: Loc,
}

pub fn read_project(file: FileId, source: &str) -> Result<Project, CompileError> {
    let mut lexer = Lexer::new(file, source);
    let mut rooms = Vec::with_capacity(64);
    loop {
        let room_token = lexer.next()?;
        match room_token.payload {
            TokenPayload::Eof => break,
            TokenPayload::Ident { len } if substr(source, room_token.offset, len) == "room" => {}
            _ => {
                return Err(CompileError::new(
                    file.at(room_token.offset),
                    CompileErrorPayload::UnexpectedToken {
                        expected: "\"room\" or eof",
                    },
                ));
            }
        }

        let (room_number_offset, room_number) = lexer.expect_integer()?;
        let room_number: u8 = room_number.try_into().map_err(|_| {
            CompileError::new(file.at(room_number_offset), CompileErrorPayload::BadInteger)
        })?;

        let (name_offset, name_len) = lexer.expect_string()?;

        lexer.expect_ident("disk")?;
        lexer.expect_token(TokenKind::Eq)?;
        let (disk_number_offset, disk_number) = lexer.expect_integer()?;
        let disk_number: u8 = disk_number.try_into().map_err(|_| {
            CompileError::new(file.at(disk_number_offset), CompileErrorPayload::BadInteger)
        })?;
        lexer.expect_choice(&[TokenKind::Newline, TokenKind::Eof], "newline or eof")?;
        let room = Room {
            name: string_contents(source, name_offset, name_len),
            disk_number,
            name_loc: file.at(name_offset),
        };
        extend_insert_some(&mut rooms, room_number.into(), room).map_err(|_: DuplicateError| {
            CompileError::new(file.at(room_number_offset), CompileErrorPayload::Duplicate)
        })?;
    }
    Ok(Project { rooms })
}
