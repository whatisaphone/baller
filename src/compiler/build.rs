use crate::{
    blocks::{
        apply_fixups,
        finish_block,
        push_disk_number,
        push_index_ext,
        start_block,
        strip_disk_number,
        BlockId,
        DiskNumber,
        RoomNumber,
    },
    compiler::{
        ast::{Ast, AstNode, RawBlockContainer, RawBlockFile},
        errors::{report_error, CompileError, CompileErrorPayload},
        loc::{add_source_file, FileId, Loc, SourceMap},
        parse::parse_room,
        project::{read_project, Room},
    },
    extract::NICE,
    index::{directory_for_block_id_mut, read_index, write_index, Index},
    raw_build::FsEntry,
    utils::vec::grow_with_default,
    xor::XorWriteStream,
};
use std::{
    error::Error,
    fs::File,
    io,
    io::{BufWriter, Seek, Write},
    num::NonZeroI32,
};

pub fn build_disk(
    mut path: String,
    disk_number: DiskNumber,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), Box<dyn Error>> {
    let mut f = File::open(&path)?;
    let mut index = read_index(&mut f)?;
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
        &mut index,
        src_read,
    ) {
        Ok(()) => {}
        Err(error) => {
            report_error(&map, &error)?;
            return Err("compiler error reported".into()); // TODO: fix this up
        }
    }

    finish_block(&mut out, lecf, &mut fixups)?;

    apply_fixups(&mut out, &fixups)?;
    out.flush()?;
    drop(out);

    strip_disk_number(&mut path);
    push_index_ext(&mut path);
    write_index(&mut File::create(&path)?, &index)?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_disk(
    project_file: FileId,
    project_source: &str,
    disk_number: DiskNumber,
    map: &mut SourceMap,
    out: &mut (impl Write + Seek),
    fixups: &mut Vec<(u32, i32)>,
    index: &mut Index,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let project = read_project(project_file, project_source)?;

    for (room_number, room) in project.rooms.iter().enumerate() {
        let room_number: RoomNumber = room_number.try_into().unwrap();
        let room = match room {
            Some(room) => room,
            None => continue,
        };
        if room.disk_number != disk_number {
            continue;
        }
        index.lfl_disks[usize::from(room_number)] = disk_number;
        build_room(room_number, room, map, out, fixups, index, src_read)?;
    }
    Ok(())
}

fn build_room(
    room_number: RoomNumber,
    room: &Room,
    map: &mut SourceMap,
    out: &mut (impl Write + Seek),
    fixups: &mut Vec<(u32, i32)>,
    index: &mut Index,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let scu_path = format!("./{}/room.scu", room.name);
    let (scu_file, scu_source) = add_source_file(scu_path, room.name_loc, map, src_read)?;
    let ast = parse_room(scu_file, &scu_source)?;

    let lflf = start_block(out, *b"LFLF").map_err(cant_write)?;

    let lfl_offset = out.stream_position().map_err(cant_write)?;
    let lfl_offset: i32 = lfl_offset.try_into().unwrap();
    index.lfl_offsets[usize::from(room_number)] = lfl_offset;

    let mut cx = Cx {
        fixups,
        index,
        lfl_offset,
    };

    for &node_id in ast.list(ast.root_start, ast.root_len) {
        match ast.node(node_id) {
            AstNode::RawBlockFile(node) => {
                raw_block_file(room_number, room, &ast, node, &mut cx, out, src_read)?;
            }
            AstNode::RawBlockContainer(node) => {
                raw_block_container(room_number, room, &ast, node, &mut cx, out, src_read)?;
            }
        }
    }

    finish_block(out, lflf, fixups).map_err(cant_write)?;
    Ok(())
}

struct Cx<'a> {
    fixups: &'a mut Vec<(u32, i32)>,
    index: &'a mut Index,
    lfl_offset: i32,
}

fn raw_block_file(
    room_number: RoomNumber,
    room: &Room,
    ast: &Ast,
    node: &RawBlockFile,
    cx: &mut Cx,
    out: &mut (impl Write + Seek),
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

    let offset = out.stream_position().map_err(cant_write)?;
    let block = start_block(out, node.block_id).map_err(cant_write)?;
    out.write_all(&blob).map_err(cant_write)?;
    let size = finish_block(out, block, cx.fixups).map_err(cant_write)?;
    update_index(
        node.path_loc, // TODO: better loc
        node.block_id,
        node.glob_number,
        room_number,
        offset.try_into().unwrap(),
        size,
        cx,
    )?;
    Ok(())
}

fn raw_block_container(
    room_number: RoomNumber,
    room: &Room,
    ast: &Ast,
    node: &RawBlockContainer,
    cx: &mut Cx,
    out: &mut (impl Write + Seek),
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let block = start_block(out, node.block_id).map_err(cant_write)?;

    for &child_id in ast.list(node.children_start, node.children_len) {
        match ast.node(child_id) {
            AstNode::RawBlockFile(n) => {
                raw_block_file(room_number, room, ast, n, cx, out, src_read)?;
            }
            AstNode::RawBlockContainer(n) => {
                raw_block_container(room_number, room, ast, n, cx, out, src_read)?;
            }
        }
    }

    finish_block(out, block, cx.fixups).map_err(cant_write)?;
    Ok(())
}

fn update_index(
    loc: Loc,
    block_id: BlockId,
    glob_number: Option<NonZeroI32>,
    room_number: RoomNumber,
    offset: i32,
    size: i32,
    cx: &mut Cx,
) -> Result<(), CompileError> {
    let directory = directory_for_block_id_mut(cx.index, block_id);
    let (directory, glob_number) = match (directory, glob_number) {
        (Some(directory), Some(glob_number)) => (directory, glob_number),
        (None, None) => return Ok(()),
        (Some(_), None) => {
            return Err(CompileError::new(
                loc,
                CompileErrorPayload::GlobNumberRequired,
            ));
        }
        (None, Some(_)) => {
            return Err(CompileError::new(
                loc,
                CompileErrorPayload::GlobNumberForbidden,
            ));
        }
    };

    let i: usize = glob_number.get().try_into().unwrap();
    grow_with_default(&mut directory.room_numbers, i + 1);
    directory.room_numbers[i] = room_number;
    directory.offsets[i] = offset - cx.lfl_offset;
    directory.sizes[i] = size;
    Ok(())
}

fn cant_write(_: io::Error) -> CompileError {
    CompileError::new_without_loc(CompileErrorPayload::CantWriteOutput)
}
