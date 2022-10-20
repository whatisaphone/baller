use crate::{
    blocks::{push_disk_number, strip_disk_number, BlockScanner},
    extract::{find_lfl_number, read_index, Index, FAIL, NICE},
    utils::vec::{extend_insert_some, grow_with_default},
    xor::XorStream,
};
use std::{
    collections::HashMap,
    error::Error,
    fmt::Write,
    fs::File,
    io::{BufReader, Read, Seek, SeekFrom},
    str,
};

pub fn extract2(
    mut path: String,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
    let mut f = File::open(&path)?;
    let index = read_index(&mut f)?;
    drop(f);

    let mut rooms = Vec::with_capacity(64);

    for disk_number in 1..=2 {
        strip_disk_number(&mut path);
        push_disk_number(&mut path, disk_number);
        let mut f = File::open(&path)?;
        decompile_disk(&index, disk_number, &mut f, &mut rooms, write_file)?;
        drop(f);
    }

    let mut project = String::with_capacity(1 << 10);
    for (room_number, room) in rooms.iter().enumerate() {
        let room = match room {
            Some(room) => room,
            None => continue,
        };
        writeln!(
            project,
            "room {} {:?} disk={}",
            room_number, room.name, room.disk_number,
        )?;
    }
    write_file("./project.txt", project.as_bytes())?;
    Ok(())
}

fn decompile_disk(
    index: &Index,
    disk_number: u8,
    s: &mut (impl Read + Seek),
    rooms: &mut Vec<Option<Room>>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
    let s = XorStream::new(s, NICE);
    let mut s = BufReader::new(s);

    let len = s.seek(SeekFrom::End(0))?;
    s.rewind()?;

    let mut buf = Vec::with_capacity(64 << 10);
    let mut lflf_blocks = HashMap::with_capacity(16);
    let mut room_scu = String::with_capacity(1 << 10);

    let mut scan_root = BlockScanner::new(len);
    let lecf_len = scan_root
        .next_block_must_be(&mut s, *b"LECF")?
        .ok_or(FAIL)?;

    let mut scan_lecf = BlockScanner::new(s.stream_position()? + lecf_len);
    while let Some(lflf_len) = scan_lecf.next_block_must_be(&mut s, *b"LFLF")? {
        lflf_blocks.clear();
        room_scu.clear();

        let room_number =
            find_lfl_number(disk_number, s.stream_position()?, index).ok_or("LFL not in index")?;
        let mut room_named_by_number = String::new();
        let room_name = index
            .room_names
            .get(usize::try_from(room_number)?)
            .and_then(Option::as_deref)
            .unwrap_or_else(|| {
                room_named_by_number = format!("room{room_number}");
                &room_named_by_number
            });

        extend_insert_some(rooms, room_number.into(), Room {
            name: room_name.to_string(),
            disk_number,
        })?;

        let mut scan_lflf = BlockScanner::new(s.stream_position()? + lflf_len);

        while let Some((id, len)) = scan_lflf.next_block(&mut s)? {
            let len: usize = len.try_into()?;
            grow_with_default(&mut buf, len);
            s.read_exact(&mut buf[..len])?;

            let id_str = str::from_utf8(&id)?;
            let number = *lflf_blocks.entry(id).and_modify(|x| *x += 1).or_insert(1);
            write_file(&format!("./{room_name}/{id_str}/{number}.bin"), &buf[..len])?;
            writeln!(
                room_scu,
                r#"include-raw "{id_str}" "{id_str}/{number}.bin""#,
            )?;
        }

        scan_lflf.finish(&mut s)?;

        write_file(&format!("./{room_name}/room.scu"), room_scu.as_bytes())?;
    }

    scan_lecf.finish(&mut s)?;

    scan_root.finish(&mut s)?;

    Ok(())
}

struct Room {
    name: String,
    disk_number: u8,
}
