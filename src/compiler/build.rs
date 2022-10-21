use crate::{
    compiler::{
        errors::{report_error, CompileError},
        loc::{add_source_file, FileId, SourceMap},
        project::{read_project, Room},
    },
    raw_build::FsEntry,
    read_index,
};
use std::{error::Error, fs::File};

pub fn build_disk(
    path: &str,
    disk_number: u8,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), Box<dyn Error>> {
    let mut f = File::open(&path)?;
    let index = read_index(&mut f)?;
    drop(f);

    let mut map = SourceMap::new();

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
        src_read,
    ) {
        Ok(()) => {}
        Err(error) => {
            report_error(&map, &error)?;
            return Err("compiler error reported".into()); // TODO: fix this up
        }
    }
    drop(index);
    Ok(())
}

fn compile_disk(
    project_file: FileId,
    project_source: &str,
    disk_number: u8,
    map: &mut SourceMap,
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
        build_room(room_number, room, map, src_read)?;
    }

    todo!();
}

fn build_room(
    room_number: u8,
    room: &Room,
    map: &mut SourceMap,
    src_read: impl Fn(&str) -> Result<FsEntry, Box<dyn Error>> + Copy,
) -> Result<(), CompileError> {
    let scu_path = format!("./{}/room.scu", room.name);
    let (scu_file, scu_source) = add_source_file(scu_path, room.name_loc, map, src_read)?;
    drop((room_number, scu_file, scu_source));
    todo!()
}
