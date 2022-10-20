use crate::{
    compiler::{errors::output_error, project::read_project},
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

    let project = src_read("./project.txt")?;
    let project = match project {
        FsEntry::Dir(_) => return Err("not a file".into()),
        FsEntry::File(xs) => xs,
    };
    let project = String::from_utf8(project)?;
    let project = match read_project(&project) {
        Ok(project) => project,
        Err(error) => {
            output_error(&error)?;
            return Err("compiler error printed".into());
        }
    };

    for (room_number, room) in project.rooms.iter().enumerate() {
        let room = match room {
            Some(room) => room,
            None => continue,
        };
        if room.disk_number != disk_number {
            continue;
        }
        eprintln!("{} {} {}", room_number, room.name, room.disk_number);
    }

    drop(index);
    todo!();
}
