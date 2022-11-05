use crate::{
    blocks::{
        push_disk_number,
        strip_disk_number,
        BlockId,
        BlockScanner,
        DiskNumber,
        ObjectNumber,
        RoomNumber,
        BLOCK_HEADER_SIZE,
    },
    config::Config,
    extract_old::{find_glob_number, find_lfl_number, FAIL, NICE},
    index::{directory_for_block_id, read_index, Index},
    script::{decompile, disasm_to_string, Scope},
    utils::vec::{extend_insert_some, grow_with_default},
    xor::XorStream,
};
use std::{
    collections::HashMap,
    error::Error,
    fmt::Write,
    fs::File,
    io::{BufReader, Read, Seek, SeekFrom},
    ops::Range,
    str,
};
use tracing::info_span;

pub fn extract2(
    mut path: String,
    config: Option<&Config>,
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
        decompile_disk(&index, disk_number, config, &mut f, &mut rooms, write_file)?;
        drop(f);
    }

    let mut project = String::with_capacity(1 << 10);
    for (room_number, room) in rooms.iter().enumerate() {
        let Some(room) = room else { continue };
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
    disk_number: DiskNumber,
    config: Option<&Config>,
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
    let mut cur_path = String::with_capacity(64);
    cur_path.push_str("./");

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
            .get(usize::from(room_number))
            .and_then(Option::as_deref)
            .unwrap_or_else(|| {
                room_named_by_number = format!("room{room_number}");
                &room_named_by_number
            });

        extend_insert_some(rooms, room_number.into(), Room {
            name: room_name.to_string(),
            disk_number,
        })?;

        write!(cur_path, "{}/", room_name).unwrap();
        let cur_path_room_scu_relative = cur_path.len();

        decompile_lflf(lflf_len, &mut s, write_file, &mut Cx {
            config,
            index,
            disk_number,
            room_number,
            lflf_blocks: &mut lflf_blocks,
            buf: &mut buf,
            room_scu: &mut room_scu,
            cur_path: &mut cur_path,
            cur_path_room_scu_relative,
            indent: 0,
        })?;

        cur_path.push_str("room.scu");
        write_file(&mut cur_path, room_scu.as_bytes())?;
        pop_path_part(&mut cur_path);

        pop_path_part(&mut cur_path);
    }

    scan_lecf.finish(&mut s)?;

    scan_root.finish(&mut s)?;

    Ok(())
}

struct Cx<'a> {
    config: Option<&'a Config>,
    index: &'a Index,
    disk_number: DiskNumber,
    room_number: RoomNumber,
    lflf_blocks: &'a mut HashMap<BlockId, i32>,
    buf: &'a mut Vec<u8>,
    room_scu: &'a mut String,
    cur_path: &'a mut String,
    cur_path_room_scu_relative: usize,
    indent: i32,
}

fn decompile_lflf(
    lflf_len: u64,
    mut s: &mut BufReader<XorStream<&mut (impl Read + Seek)>>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    let mut scan_lflf = BlockScanner::new(s.stream_position()? + lflf_len);
    while let Some((id, len)) = scan_lflf.next_block(&mut s)? {
        match &id {
            b"RMDA" => decompile_rmda(len, s, write_file, cx)?,
            b"SCRP" => decompile_scrp(len, s, write_file, cx)?,
            _ => {
                decompile_raw(id, len, s, write_file, cx)?;
            }
        }
    }
    scan_lflf.finish(&mut s)?;
    Ok(())
}

fn decompile_rmda(
    block_len: u64,
    disk_read: &mut BufReader<XorStream<&mut (impl Read + Seek)>>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    write_indent(cx.room_scu, cx.indent);
    cx.room_scu.push_str("raw-block \"RMDA\" {\n");
    cx.indent += 1;

    cx.cur_path.push_str("RMDA/");

    let mut scan = BlockScanner::new(disk_read.stream_position()? + block_len);
    while let Some((id, len)) = scan.next_block(disk_read)? {
        match &id {
            b"OBCD" => decompile_obcd(len, disk_read, write_file, cx)?,
            b"ENCD" | b"EXCD" => decompile_enex(id, len, disk_read, write_file, cx)?,
            b"LSC2" => decompile_lsc2(len, disk_read, write_file, cx)?,
            _ => {
                decompile_raw(id, len, disk_read, write_file, cx)?;
            }
        }
    }
    scan.finish(disk_read)?;

    pop_path_part(cx.cur_path);

    cx.indent -= 1;
    write_indent(cx.room_scu, cx.indent);
    cx.room_scu.push_str("}\n");
    Ok(())
}

fn decompile_scrp(
    block_len: u64,
    disk_read: &mut BufReader<XorStream<&mut (impl Read + Seek)>>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    let number = decompile_raw(*b"SCRP", block_len, disk_read, write_file, cx)?;

    write!(cx.cur_path, "SCRP/{number}").unwrap();
    decompile_script(
        0..block_len.try_into().unwrap(),
        Scope::Global(number),
        write_file,
        cx,
    )?;
    pop_path_part(cx.cur_path);
    pop_path_part(cx.cur_path);
    Ok(())
}

fn decompile_enex(
    block_id: BlockId,
    block_len: u64,
    disk_read: &mut BufReader<XorStream<&mut (impl Read + Seek)>>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    let len = read_into_buf(block_len, disk_read, cx)?;

    match &block_id {
        b"ENCD" => cx.cur_path.push_str("ENCD"),
        b"EXCD" => cx.cur_path.push_str("EXCD"),
        _ => unreachable!(),
    }
    cx.cur_path.push_str(".bin");
    emit_raw_block(block_id, 0..len, write_file, cx)?;
    cx.cur_path.truncate(cx.cur_path.len() - 4);

    let scope = match &block_id {
        b"ENCD" => Scope::RoomEnter(cx.room_number),
        b"EXCD" => Scope::RoomExit(cx.room_number),
        _ => unreachable!(),
    };
    decompile_script(0..block_len.try_into().unwrap(), scope, write_file, cx)?;

    pop_path_part(cx.cur_path);
    Ok(())
}

fn decompile_lsc2(
    block_len: u64,
    disk_read: &mut BufReader<XorStream<&mut (impl Read + Seek)>>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    let number = decompile_raw(*b"LSC2", block_len, disk_read, write_file, cx)?;

    write!(cx.cur_path, "LSC2/{number}").unwrap();
    decompile_script(
        4..block_len.try_into().unwrap(),
        Scope::RoomLocal(cx.room_number, number),
        write_file,
        cx,
    )?;
    pop_path_part(cx.cur_path);
    pop_path_part(cx.cur_path);
    Ok(())
}

fn decompile_obcd(
    block_len: u64,
    disk_read: &mut BufReader<XorStream<&mut (impl Read + Seek)>>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    write_indent(cx.room_scu, cx.indent);
    cx.room_scu.push_str("raw-block \"OBCD\" {\n");
    cx.indent += 1;

    let mut scan = BlockScanner::new(disk_read.stream_position()? + block_len);

    // CDHD

    let len = scan.next_block_must_be(disk_read, *b"CDHD")?.ok_or(FAIL)?;
    let len = read_into_buf(len, disk_read, cx)?;
    let object_number = ObjectNumber::from_le_bytes(cx.buf[0..2].try_into().unwrap());

    write!(cx.cur_path, "OBCD/{object_number}/CDHD.bin").unwrap();
    emit_raw_block(*b"CDHD", 0..len, write_file, cx)?;
    pop_path_part(cx.cur_path); // NB this is asymmetrical

    // VERB

    let len = scan.next_block_must_be(disk_read, *b"VERB")?.ok_or(FAIL)?;
    let len = read_into_buf(len, disk_read, cx)?;

    cx.cur_path.push_str("VERB.bin");
    emit_raw_block(*b"VERB", 0..len, write_file, cx)?;
    pop_path_part(cx.cur_path);

    decompile_verb(object_number, len, write_file, cx)?;

    // OBNA

    let len = scan.next_block_must_be(disk_read, *b"OBNA")?.ok_or(FAIL)?;
    let len = read_into_buf(len, disk_read, cx)?;

    cx.cur_path.push_str("OBNA.bin");
    emit_raw_block(*b"OBNA", 0..len, write_file, cx)?;
    pop_path_part(cx.cur_path);

    // Finish up

    pop_path_part(cx.cur_path);
    pop_path_part(cx.cur_path);

    scan.finish(disk_read)?;

    cx.indent -= 1;
    write_indent(cx.room_scu, cx.indent);
    cx.room_scu.push_str("}\n");
    Ok(())
}

fn decompile_verb(
    object_number: ObjectNumber,
    block_len: usize,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    #[allow(clippy::cast_possible_truncation)]
    const BLK_HD_SZ: u16 = BLOCK_HEADER_SIZE as u16;

    let mut pos = 0;
    while let Some(verb) = read_verb_header(&cx.buf[pos..])? {
        pos += VERB_HEADER_SIZE;
        let start: usize = (verb.offset - BLK_HD_SZ).try_into()?; // relative to block header
        let end: usize = match read_verb_header(&cx.buf[pos..])? {
            Some(next) => (next.offset - BLK_HD_SZ).try_into()?,
            None => block_len,
        };

        if !(pos <= block_len && start <= block_len && end <= block_len && start < end) {
            return Err(FAIL.into());
        }

        write!(cx.cur_path, "VERB_{:02}", verb.number).unwrap();
        decompile_script(
            start..end,
            Scope::Verb(cx.room_number, object_number),
            write_file,
            cx,
        )?;
        pop_path_part(cx.cur_path);
    }
    Ok(())
}

fn read_verb_header(buf: &[u8]) -> Result<Option<VerbHeader>, Box<dyn Error>> {
    #[allow(clippy::get_first)]
    let number = *buf.get(0).ok_or(FAIL)?;
    if number == 0 {
        return Ok(None);
    }
    let offset = u16::from_le_bytes(buf.get(1..3).ok_or(FAIL)?.try_into().unwrap());
    Ok(Some(VerbHeader { number, offset }))
}

const VERB_HEADER_SIZE: usize = 3;

struct VerbHeader {
    number: u8,
    offset: u16,
}

fn decompile_script(
    code_range: Range<usize>,
    scope: Scope,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    let Some(config) = cx.config else { return Ok(()) };

    let code = &cx.buf[code_range];

    let disasm = disasm_to_string(code);
    cx.cur_path.push_str(".s");
    write_file(cx.cur_path, disasm.as_bytes())?;
    cx.cur_path.truncate(cx.cur_path.len() - 2);

    let decomp = {
        let _span = info_span!("decompile", cx.cur_path).entered();
        decompile(code, scope, config)
    };
    cx.cur_path.push_str(".scu");
    write_file(cx.cur_path, decomp.as_bytes())?;
    Ok(())
}

fn decompile_raw(
    id: BlockId,
    len: u64,
    disk_read: &mut BufReader<XorStream<&mut (impl Read + Seek)>>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<i32, Box<dyn Error>> {
    let offset = disk_read.stream_position()?;

    let len = read_into_buf(len, disk_read, cx)?;
    let buf = &cx.buf[..len];

    let id_str = str::from_utf8(&id)?;
    let glob_number = find_block_glob_number(cx.index, id, cx.disk_number, offset)?;
    let number = if let Some(n) = glob_number {
        n
    } else if let Some(n) = get_block_local_number(id, buf)? {
        n
    } else {
        let n = cx.lflf_blocks.entry(id).or_insert(0);
        *n += 1;
        *n
    };
    write!(cx.cur_path, "{id_str}/{number}.bin").unwrap();

    write_file(cx.cur_path, buf)?;

    write_indent(cx.room_scu, cx.indent);
    write!(cx.room_scu, r#"raw-block "{id_str}""#)?;
    if let Some(glob_number) = glob_number {
        write!(cx.room_scu, " {glob_number}")?;
    }
    writeln!(
        cx.room_scu,
        r#" "{}""#,
        &cx.cur_path[cx.cur_path_room_scu_relative..],
    )?;

    pop_path_part(cx.cur_path);
    pop_path_part(cx.cur_path);

    Ok(number)
}

fn emit_raw_block(
    id: BlockId,
    data_range: Range<usize>,
    write_file: &mut impl FnMut(&str, &[u8]) -> Result<(), Box<dyn Error>>,
    cx: &mut Cx,
) -> Result<(), Box<dyn Error>> {
    debug_assert!(directory_for_block_id(cx.index, id).is_none());

    let buf = &cx.buf[data_range];
    write_file(cx.cur_path, buf)?;

    let id_str = str::from_utf8(&id)?;
    write_indent(cx.room_scu, cx.indent);
    write!(cx.room_scu, r#"raw-block "{id_str}""#)?;
    writeln!(
        cx.room_scu,
        r#" "{}""#,
        &cx.cur_path[cx.cur_path_room_scu_relative..],
    )?;
    Ok(())
}

fn find_block_glob_number(
    index: &Index,
    id: BlockId,
    disk_number: DiskNumber,
    offset: u64,
) -> Result<Option<i32>, Box<dyn Error>> {
    let Some(directory) = directory_for_block_id(index, id) else { return Ok(None) };
    Ok(Some(
        find_glob_number(index, directory, disk_number, offset - BLOCK_HEADER_SIZE)
            .ok_or("missing from index")?,
    ))
}

fn get_block_local_number(id: BlockId, data: &[u8]) -> Result<Option<i32>, Box<dyn Error>> {
    match &id {
        b"LSC2" => {
            let number_bytes = data.get(0..4).ok_or("local script too short")?;
            Ok(Some(i32::from_le_bytes(number_bytes.try_into().unwrap())))
        }
        _ => Ok(None),
    }
}

fn read_into_buf(
    len: u64,
    disk_read: &mut BufReader<XorStream<&mut (impl Read + Seek)>>,
    cx: &mut Cx,
) -> Result<usize, Box<dyn Error>> {
    let len: usize = len.try_into()?;
    grow_with_default(cx.buf, len);
    let buf = &mut cx.buf[..len];
    disk_read.read_exact(buf)?;
    Ok(len)
}

fn write_indent(out: &mut String, indent: i32) {
    for _ in 0..indent * 4 {
        out.push(' ');
    }
}

struct Room {
    name: String,
    disk_number: DiskNumber,
}

fn pop_path_part(s: &mut String) {
    s.pop();
    while *s.as_bytes().last().unwrap() != b'/' {
        s.pop();
    }
}
