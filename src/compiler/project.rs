use crate::{
    compiler::{
        errors::{CompileError, CompileErrorPayload},
        lexer::{substr, Lexer},
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
}

pub fn read_project(source: &str) -> Result<Project, CompileError> {
    let mut lexer = Lexer::new(source);
    let mut rooms = Vec::with_capacity(64);
    loop {
        let room_token = lexer.next()?;
        match room_token.payload {
            TokenPayload::Eof => break,
            TokenPayload::Ident { len } if substr(source, room_token.offset, len) == "room" => {}
            _ => {
                return Err(CompileError::new(
                    room_token.offset,
                    CompileErrorPayload::UnexpectedToken {
                        expected: "\"room\" or eof",
                    },
                ));
            }
        }

        let (room_number_offset, room_number) = lexer.expect_integer()?;
        let room_number: u8 = room_number
            .try_into()
            .map_err(|_| CompileError::new(room_number_offset, CompileErrorPayload::BadInteger))?;

        let name = lexer.expect_string()?;

        lexer.expect_ident("disk")?;
        lexer.expect_token(TokenKind::Eq)?;
        let (disk_number_offset, disk_number) = lexer.expect_integer()?;
        let disk_number: u8 = disk_number
            .try_into()
            .map_err(|_| CompileError::new(disk_number_offset, CompileErrorPayload::BadInteger))?;
        lexer.expect_choice(&[TokenKind::Newline, TokenKind::Eof], "newline or eof")?;
        let room = Room {
            name: substr(source, name.0, name.1), // TODO
            disk_number,
        };
        extend_insert_some(&mut rooms, room_number.into(), room).map_err(|_: DuplicateError| {
            CompileError::new(room_number_offset, CompileErrorPayload::Duplicate)
        })?;
    }
    Ok(Project { rooms })
}
