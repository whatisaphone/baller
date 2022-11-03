use crate::{
    blocks::{finish_block, push_disk_number, start_block, strip_disk_number},
    compiler::{
        ast::{Ast, AstNode, RawBlockContainer, RawBlockFile},
        errors::{report_error, CompileError, CompileErrorPayload},
        loc::{add_source_file, FileId, SourceMap},
        parse::parse_room,
        project::{read_project, Room},
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
    let scu_path = format!("./{}/room.scu", room.name);
    let (scu_file, scu_source) = add_source_file(scu_path, room.name_loc, map, src_read)?;
    let ast = parse_room(scu_file, &scu_source)?;

    let lflf = start_block(out, *b"LFLF").map_err(cant_write)?;

    for &node_id in ast.list(ast.root_start, ast.root_len) {
        match ast.node(node_id) {
            AstNode::RawBlockFile(node) => {
                raw_block_file(room, &ast, node, out, fixups, src_read)?;
            }
            AstNode::RawBlockContainer(node) => {
                raw_block_container(room, &ast, node, out, fixups, src_read)?;
            }
        }
    }

    finish_block(out, lflf, fixups).map_err(cant_write)?;
    let _ = room_number; // TODO: needed for index
    Ok(())
}

fn raw_block_file(
    room: &Room,
    ast: &Ast,
    node: &RawBlockFile,
    out: &mut (impl Write + Seek),
    fixups: &mut Vec<(u32, i32)>,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let path = ast.string(node.path_offset, node.path_len);
    let path = format!("./{}/{}", room.name, path);
    let blob = match src_read(&path) {
        Ok(FsEntry::File(blob)) => blob,
        Ok(FsEntry::Dir(_)) | Err(_) => {
            return Err(CompileError::new(
                node.path_loc,
                CompileErrorPayload::CantReadFile,
            ));
        }
    };

    let block = start_block(out, node.block_id).map_err(cant_write)?;
    out.write_all(&blob).map_err(cant_write)?;
    finish_block(out, block, fixups).map_err(cant_write)?;
    Ok(())
}

fn raw_block_container(
    room: &Room,
    ast: &Ast,
    node: &RawBlockContainer,
    out: &mut (impl Write + Seek),
    fixups: &mut Vec<(u32, i32)>,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let block = start_block(out, node.block_id).map_err(cant_write)?;

    for &child_id in ast.list(node.children_start, node.children_len) {
        match ast.node(child_id) {
            AstNode::RawBlockFile(n) => {
                raw_block_file(room, ast, n, out, fixups, src_read)?;
            }
            AstNode::RawBlockContainer(n) => {
                raw_block_container(room, ast, n, out, fixups, src_read)?;
            }
        }
    }

    finish_block(out, block, fixups).map_err(cant_write)?;
    Ok(())
}

fn cant_write(_: io::Error) -> CompileError {
    CompileError::new_without_loc(CompileErrorPayload::CantWriteOutput)
}
