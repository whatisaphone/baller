use crate::{
    blocks::{finish_block, push_disk_number, start_block, strip_disk_number, BlockId},
    compiler::{
        errors::{report_error, CompileError, CompileErrorPayload},
        lexer::{string_contents, substr, Lexer},
        loc::{add_source_file, FileId, Loc, SourceMap},
        project::{read_project, Room},
        token::{TokenKind, TokenPayload},
    },
    extract::{read_index, NICE},
    raw_build::FsEntry,
    xor::XorWriteStream,
};
use byteordered::byteorder::{WriteBytesExt, BE};
use std::{
    error::Error,
    fs::File,
    io,
    io::{BufWriter, Seek, SeekFrom, Write},
};

pub fn build_disk(
    mut path: String,
    disk_number: u8,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), Box<dyn Error>> {
    let mut f = File::open(&path)?;
    let index = read_index(&mut f)?;
    drop(f);

    let mut map = SourceMap::new();

    strip_disk_number(&mut path);
    push_disk_number(&mut path, disk_number);
    let out = File::create(&path)?;
    let out = BufWriter::new(out);
    let mut out = XorWriteStream::new(out, NICE);
    let mut fixups = Vec::with_capacity(1 << 10);

    let lecf = start_block(&mut out, *b"LECF")?;

    let project_path = "./project.txt";
    let project_source = match src_read(project_path)? {
        FsEntry::Dir(_) => return Err("not a file".into()),
        FsEntry::File(xs) => xs,
    };
    let project_source = String::from_utf8(project_source)?;
    let project_file = map.add_file(project_path.to_string());

    match compile_disk(
        project_file,
        &project_source,
        disk_number,
        &mut map,
        &mut out,
        &mut fixups,
        src_read,
    ) {
        Ok(()) => {}
        Err(error) => {
            report_error(&map, &error)?;
            return Err("compiler error reported".into()); // TODO: fix this up
        }
    }

    finish_block(&mut out, lecf, &mut fixups)?;

    for &(offset, value) in &fixups {
        out.seek(SeekFrom::Start(offset.try_into().unwrap()))?;
        out.write_i32::<BE>(value)?;
    }
    out.flush()?;

    drop(index); // TODO: update index and write to disk
    Ok(())
}

fn compile_disk(
    project_file: FileId,
    project_source: &str,
    disk_number: u8,
    map: &mut SourceMap,
    out: &mut (impl Write + Seek),
    fixups: &mut Vec<(u32, i32)>,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let project = read_project(project_file, project_source)?;

    for (room_number, room) in project.rooms.iter().enumerate() {
        let room_number: u8 = room_number.try_into().unwrap();
        let room = match room {
            Some(room) => room,
            None => continue,
        };
        if room.disk_number != disk_number {
            continue;
        }
        build_room(room_number, room, map, out, fixups, src_read)?;
    }
    Ok(())
}

fn build_room(
    room_number: u8,
    room: &Room,
    map: &mut SourceMap,
    out: &mut (impl Write + Seek),
    fixups: &mut Vec<(u32, i32)>,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let lflf = start_block(out, *b"LFLF").map_err(cant_write)?;

    let scu_path = format!("./{}/room.scu", room.name);
    let (scu_file, scu_source) = add_source_file(scu_path, room.name_loc, map, src_read)?;
    let mut lexer = Lexer::new(scu_file, &scu_source);
    loop {
        let token = lexer.next()?;
        match token.payload {
            TokenPayload::Eof => break,
            TokenPayload::Ident { len }
                if substr(&scu_source, token.offset, len) == "include-raw" =>
            {
                let (block_id_offset, block_id_len) = lexer.expect_string()?;
                let (path_offset, path_len) = lexer.expect_string()?;
                lexer.expect_choice(&[TokenKind::Newline, TokenKind::Eof], "newline or eof")?;

                let block_id = string_contents(&scu_source, block_id_offset, block_id_len);
                let block_id = parse_block_id(block_id).map_err(|_| {
                    CompileError::new(
                        scu_file.at(block_id_offset),
                        CompileErrorPayload::InvalidBlockId,
                    )
                })?;

                let path = string_contents(&scu_source, path_offset, path_len);
                let path = format!("./{}/{}", room.name, path);
                include_raw(
                    &path,
                    block_id,
                    scu_file.at(path_offset),
                    out,
                    fixups,
                    src_read,
                )?;
            }
            _ => {
                return Err(CompileError::new(
                    scu_file.at(token.offset),
                    CompileErrorPayload::UnexpectedToken {
                        expected: "declaration",
                    },
                ));
            }
        }
    }

    finish_block(out, lflf, fixups).map_err(cant_write)?;
    let _ = room_number; // TODO: needed for index
    Ok(())
}

fn include_raw(
    path: &str,
    block_id: BlockId,
    path_loc: Loc,
    out: &mut (impl Write + Seek),
    fixups: &mut Vec<(u32, i32)>,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let blob = match src_read(path) {
        Ok(FsEntry::File(blob)) => blob,
        Ok(FsEntry::Dir(_)) | Err(_) => {
            return Err(CompileError::new(
                path_loc,
                CompileErrorPayload::CantReadFile,
            ));
        }
    };

    let block = start_block(out, block_id).map_err(cant_write)?;
    out.write_all(&blob).map_err(cant_write)?;
    finish_block(out, block, fixups).map_err(cant_write)?;
    Ok(())
}

fn parse_block_id(s: &str) -> Result<BlockId, ()> {
    if !(s.len() == 4 && s.bytes().all(|b| (32..=126).contains(&b))) {
        return Err(());
    }
    Ok(s.as_bytes().try_into().unwrap())
}

fn cant_write(_: io::Error) -> CompileError {
    CompileError::new_without_loc(CompileErrorPayload::CantWriteOutput)
}
