use crate::{
    blocks::{apply_fixups, write_block, BlockId, BlockScanner},
    extract::{FAIL, NICE},
    utils::vec::extend_insert_some,
    xor::{XorStream, XorWriteStream},
};
use byteordered::byteorder::{ReadBytesExt, WriteBytesExt, LE};
use std::{
    error::Error,
    fmt,
    io,
    io::{BufReader, BufWriter, Read, Seek, SeekFrom, Write},
};

pub struct Index {
    pub maxs: Maxs,
    pub room_images: Directory,
    pub rooms: Directory,
    pub scripts: Directory,
    pub sounds: Directory,
    pub costumes: Directory,
    pub charsets: Directory,
    pub images: Directory,
    pub talkies: Directory,
    pub lfl_offsets: Vec<i32>,
    pub lfl_disks: Vec<u8>,
    pub room_names: Vec<Option<String>>,
    pub objects: Objects,
}

pub struct Maxs {
    pub variables: i16,
    pub unknown1: i16,
    pub room_variables: i16,
    pub objects_in_room: i16,
    pub arrays: i16,
    pub unknown2: i16,
    pub unknown3: i16,
    pub flobjects: i16,
    pub inventory_objects: i16,
    pub rooms: i16,
    pub scripts: i16,
    pub sounds: i16,
    pub charsets: i16,
    pub costumes: i16,
    pub objects: i16,
    pub images: i16,
    pub sprites: i16,
    pub local_scripts: i16,
    pub heap: i16,
    pub palettes: i16,
    pub unknown4: i16,
    pub talkies: i16,
}

pub struct Directory {
    pub room_numbers: Vec<u8>,
    pub offsets: Vec<i32>,
    pub sizes: Vec<i32>,
}

pub struct Objects {
    pub states: Vec<u8>,
    pub owners: Vec<u8>,
    pub rooms: Vec<u8>,
    pub classes: Vec<i32>,
}

pub fn read_index(r: &mut (impl Read + Seek)) -> Result<Index, Box<dyn Error>> {
    let r = XorStream::new(r, NICE);
    let mut r = BufReader::new(r);

    let len = r.seek(SeekFrom::End(0))?;
    r.rewind()?;

    let mut scan = BlockScanner::new(len);
    scan.next_block_must_be(&mut r, *b"MAXS")?.ok_or(FAIL)?;
    let maxs = read_maxs(&mut r)?;

    let mut dir = |&id: &BlockId| {
        scan.next_block_must_be(&mut r, id)?.ok_or(FAIL)?;
        read_directory(&mut r)
    };
    let room_images = dir(b"DIRI")?;
    let rooms = dir(b"DIRR")?;
    let scripts = dir(b"DIRS")?;
    let sounds = dir(b"DIRN")?;
    let costumes = dir(b"DIRC")?;
    let charsets = dir(b"DIRF")?;
    let images = dir(b"DIRM")?;
    let talkies = dir(b"DIRT")?;

    scan.next_block_must_be(&mut r, *b"DLFL")?.ok_or(FAIL)?;
    let len = r.read_i16::<LE>()?;
    let mut lfl_offsets = vec![0; len.try_into()?];
    r.read_i32_into::<LE>(&mut lfl_offsets)?;

    scan.next_block_must_be(&mut r, *b"DISK")?.ok_or(FAIL)?;
    let len = r.read_i16::<LE>()?;
    let mut lfl_disks = vec![0; len.try_into()?];
    r.read_exact(&mut lfl_disks)?;

    let len = scan.next_block_must_be(&mut r, *b"RNAM")?.ok_or(FAIL)?;
    let mut buf = vec![0; len.try_into()?];
    r.read_exact(&mut buf)?;
    let mut room_names = Vec::with_capacity(maxs.rooms.try_into()?);
    let mut i = 0;
    loop {
        let number = i16::from_le_bytes(buf.get(i..i + 2).ok_or(FAIL)?.try_into().unwrap());
        i += 2;
        let number: usize = number.try_into()?;
        if number == 0 {
            break;
        }
        let len = buf[i..].iter().position(|&b| b == 0).ok_or(FAIL)?;
        let name = String::from_utf8(buf[i..i + len].to_vec())?;
        i += len + 1;
        extend_insert_some(&mut room_names, number, name)?;
    }

    scan.next_block_must_be(&mut r, *b"DOBJ")?.ok_or(FAIL)?;
    let len = r.read_i16::<LE>()?;
    let mut objects = Objects {
        states: vec![0; len.try_into()?],
        owners: vec![0; len.try_into()?],
        rooms: vec![0; len.try_into()?],
        classes: vec![0; len.try_into()?],
    };
    r.read_exact(&mut objects.states)?;
    r.read_exact(&mut objects.owners)?;
    r.read_exact(&mut objects.rooms)?;
    r.read_i32_into::<LE>(&mut objects.classes)?;

    Ok(Index {
        maxs,
        room_images,
        rooms,
        scripts,
        sounds,
        costumes,
        charsets,
        images,
        talkies,
        lfl_offsets,
        lfl_disks,
        room_names,
        objects,
    })
}

fn read_maxs(r: &mut impl Read) -> Result<Maxs, Box<dyn Error>> {
    Ok(Maxs {
        variables: r.read_i16::<LE>()?,
        unknown1: r.read_i16::<LE>()?,
        room_variables: r.read_i16::<LE>()?,
        objects_in_room: r.read_i16::<LE>()?,
        arrays: r.read_i16::<LE>()?,
        unknown2: r.read_i16::<LE>()?,
        unknown3: r.read_i16::<LE>()?,
        flobjects: r.read_i16::<LE>()?,
        inventory_objects: r.read_i16::<LE>()?,
        rooms: r.read_i16::<LE>()?,
        scripts: r.read_i16::<LE>()?,
        sounds: r.read_i16::<LE>()?,
        charsets: r.read_i16::<LE>()?,
        costumes: r.read_i16::<LE>()?,
        objects: r.read_i16::<LE>()?,
        images: r.read_i16::<LE>()?,
        sprites: r.read_i16::<LE>()?,
        local_scripts: r.read_i16::<LE>()?,
        heap: r.read_i16::<LE>()?,
        palettes: r.read_i16::<LE>()?,
        unknown4: r.read_i16::<LE>()?,
        talkies: r.read_i16::<LE>()?,
    })
}

fn read_directory(r: &mut impl Read) -> Result<Directory, Box<dyn Error>> {
    let len = r.read_i16::<LE>()?;
    let mut result = Directory {
        room_numbers: vec![0; len.try_into()?],
        offsets: vec![0; len.try_into()?],
        sizes: vec![0; len.try_into()?],
    };
    r.read_exact(&mut result.room_numbers)?;
    r.read_i32_into::<LE>(&mut result.offsets)?;
    r.read_i32_into::<LE>(&mut result.sizes)?;
    Ok(result)
}

pub fn write_index(w: &mut (impl Write + Seek), index: &Index) -> io::Result<()> {
    let w = XorWriteStream::new(w, NICE);
    let mut w = BufWriter::new(w);

    let mut fixups = Vec::with_capacity(64);

    write_block(&mut w, *b"MAXS", &mut fixups, |w, _| {
        write_maxs(w, &index.maxs)
    })?;

    let mut dir = |&id, dir| write_block(&mut w, id, &mut fixups, |w, _| write_directory(w, dir));
    dir(b"DIRI", &index.room_images)?;
    dir(b"DIRR", &index.rooms)?;
    dir(b"DIRS", &index.scripts)?;
    dir(b"DIRN", &index.sounds)?;
    dir(b"DIRC", &index.costumes)?;
    dir(b"DIRF", &index.charsets)?;
    dir(b"DIRM", &index.images)?;
    dir(b"DIRT", &index.talkies)?;

    write_block(&mut w, *b"DLFL", &mut fixups, |w, _| {
        w.write_i16::<LE>(index.lfl_disks.len().try_into().unwrap())?;
        for &x in &index.lfl_offsets {
            w.write_i32::<LE>(x)?;
        }
        Ok(())
    })?;

    write_block(&mut w, *b"DISK", &mut fixups, |w, _| {
        w.write_i16::<LE>(index.lfl_disks.len().try_into().unwrap())?;
        w.write_all(&index.lfl_disks)?;
        Ok(())
    })?;

    write_block(&mut w, *b"RNAM", &mut fixups, |w, _| {
        for (i, name) in index.room_names.iter().enumerate() {
            let name = match name {
                Some(name) => name,
                None => continue,
            };
            w.write_i16::<LE>(i.try_into().unwrap())?;
            w.write_all(name.as_bytes())?;
            w.write_all(b"\0")?;
        }
        w.write_i16::<LE>(0)?;
        Ok(())
    })?;

    write_block(&mut w, *b"DOBJ", &mut fixups, |w, _| {
        w.write_i16::<LE>(index.objects.states.len().try_into().unwrap())?;
        w.write_all(&index.objects.states)?;
        w.write_all(&index.objects.owners)?;
        w.write_all(&index.objects.rooms)?;
        for &x in &index.objects.classes {
            w.write_i32::<LE>(x)?;
        }
        Ok(())
    })?;

    write_block(&mut w, *b"AARY", &mut fixups, |w, _| w.write_i16::<LE>(0))?;

    write_block(&mut w, *b"INIB", &mut fixups, |w, fixups| {
        write_block(w, *b"NOTE", fixups, |w, _| w.write_i16::<LE>(0))
    })?;

    apply_fixups(&mut w, &fixups)?;
    Ok(())
}

fn write_maxs(w: &mut impl Write, maxs: &Maxs) -> io::Result<()> {
    w.write_i16::<LE>(maxs.variables)?;
    w.write_i16::<LE>(maxs.unknown1)?;
    w.write_i16::<LE>(maxs.room_variables)?;
    w.write_i16::<LE>(maxs.objects_in_room)?;
    w.write_i16::<LE>(maxs.arrays)?;
    w.write_i16::<LE>(maxs.unknown2)?;
    w.write_i16::<LE>(maxs.unknown3)?;
    w.write_i16::<LE>(maxs.flobjects)?;
    w.write_i16::<LE>(maxs.inventory_objects)?;
    w.write_i16::<LE>(maxs.rooms)?;
    w.write_i16::<LE>(maxs.scripts)?;
    w.write_i16::<LE>(maxs.sounds)?;
    w.write_i16::<LE>(maxs.charsets)?;
    w.write_i16::<LE>(maxs.costumes)?;
    w.write_i16::<LE>(maxs.objects)?;
    w.write_i16::<LE>(maxs.images)?;
    w.write_i16::<LE>(maxs.sprites)?;
    w.write_i16::<LE>(maxs.local_scripts)?;
    w.write_i16::<LE>(maxs.heap)?;
    w.write_i16::<LE>(maxs.palettes)?;
    w.write_i16::<LE>(maxs.unknown4)?;
    w.write_i16::<LE>(maxs.talkies)?;
    Ok(())
}

fn write_directory(w: &mut impl Write, directory: &Directory) -> io::Result<()> {
    w.write_i16::<LE>(directory.room_numbers.len().try_into().unwrap())?;
    w.write_all(&directory.room_numbers)?;
    for &x in &directory.offsets {
        w.write_i32::<LE>(x)?;
    }
    for &x in &directory.sizes {
        w.write_i32::<LE>(x)?;
    }
    Ok(())
}

pub fn dump_index(w: &mut impl fmt::Write, index: &Index) -> fmt::Result {
    w.write_str("lfl_disks:\n")?;
    for (i, x) in index.lfl_disks.iter().enumerate() {
        writeln!(w, "\t{i}\t{x}")?;
    }
    w.write_str("lfl_offsets:\n")?;
    for (i, x) in index.lfl_offsets.iter().enumerate() {
        writeln!(w, "\t{i}\t{x}")?;
    }
    w.write_str("room_images:\n")?;
    dump_directory(w, index, &index.room_images)?;
    w.write_str("rooms:\n")?;
    dump_directory(w, index, &index.rooms)?;
    w.write_str("scripts:\n")?;
    dump_directory(w, index, &index.scripts)?;
    w.write_str("sounds:\n")?;
    dump_directory(w, index, &index.sounds)?;
    w.write_str("costumes:\n")?;
    dump_directory(w, index, &index.costumes)?;
    w.write_str("charsets:\n")?;
    dump_directory(w, index, &index.charsets)?;
    w.write_str("images:\n")?;
    dump_directory(w, index, &index.images)?;
    w.write_str("talkies:\n")?;
    dump_directory(w, index, &index.talkies)?;

    w.write_str("incomplete dump\n")?;
    Ok(())
}

fn dump_directory(w: &mut impl fmt::Write, index: &Index, dir: &Directory) -> fmt::Result {
    for i in 0..dir.room_numbers.len() {
        let room: usize = dir.room_numbers[i].into();
        writeln!(
            w,
            "\t{}\t{}\t{}\t{}\t{}\t{}",
            i,
            dir.room_numbers[i],
            dir.offsets[i],
            dir.sizes[i],
            index.lfl_disks[room],
            index.lfl_offsets[room] + dir.offsets[i],
        )?;
    }
    Ok(())
}

pub fn directory_for_block_id(index: &Index, id: BlockId) -> Option<&Directory> {
    match &id {
        b"RMIM" => Some(&index.room_images),
        b"RMDA" => Some(&index.rooms),
        b"SCRP" => Some(&index.scripts),
        b"DIGI" => Some(&index.sounds),
        b"AKOS" => Some(&index.costumes),
        b"AWIZ" | b"MULT" => Some(&index.images),
        b"TLKE" => Some(&index.talkies),
        _ => None,
    }
}

pub fn directory_for_block_id_mut(index: &mut Index, id: BlockId) -> Option<&mut Directory> {
    match &id {
        b"RMIM" => Some(&mut index.room_images),
        b"RMDA" => Some(&mut index.rooms),
        b"SCRP" => Some(&mut index.scripts),
        b"DIGI" => Some(&mut index.sounds),
        b"AKOS" => Some(&mut index.costumes),
        b"AWIZ" | b"MULT" => Some(&mut index.images),
        b"TLKE" => Some(&mut index.talkies),
        _ => None,
    }
}
