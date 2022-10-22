use inside_baseball::{build_disk, extract, extract2, raw_build, read_index, Config, FsEntry};
use std::{
    collections::HashMap,
    env,
    error::Error,
    fs,
    fs::File,
    io,
    io::{Read, Seek, SeekFrom},
    path::{Path, PathBuf},
    thread,
};

#[test]
fn raw_round_trip() {
    // In debug builds, the stack overflows in `decompile_blocks` for deeply nested
    // scripts. Run in a thread with a larger stack.
    thread::Builder::new()
        .stack_size(8 << 20)
        .spawn(|| raw_round_trip_thread().unwrap())
        .unwrap()
        .join()
        .unwrap();
}

fn raw_round_trip_thread() -> Result<(), Box<dyn Error>> {
    let input_path = fixture_path("baseball 2001.he0");
    let mut input = File::open(&input_path)?;
    let index = read_index(&mut input)?;
    drop(input);

    let disk_number = 2;
    let input_path = fixture_path("baseball 2001.(b)");
    let mut input = File::open(&input_path)?;
    let mut fs = HashMap::with_capacity(1 << 10);
    extract(
        &index,
        disk_number,
        &Config::default(),
        false,
        &mut input,
        &mut |path, data| fs_write(&mut fs, path, data.to_vec()),
    )?;
    drop(input);

    let mut output = Vec::new();
    raw_build(&mut io::Cursor::new(&mut output), |path| fs_read(&fs, path))?;
    assert_stream_eq(&input_path, &mut io::Cursor::new(&mut output))?;
    Ok(())
}

#[test]
fn round_trip2() -> Result<(), Box<dyn Error>> {
    let mut fs = HashMap::with_capacity(1 << 10);

    extract2(
        fixture_path("baseball 2001.he0")
            .into_os_string()
            .into_string()
            .unwrap(),
        &mut |path, data| fs_write(&mut fs, path, data.to_vec()),
    )?;

    let root = env::temp_dir().join("inside-baseball-test-round-trip");
    if root.exists() {
        fs::remove_dir_all(&root)?;
    }
    fs::create_dir(&root)?;
    let index_path = root.join("baseball 2001.he0");
    fs::copy(&fixture_path("baseball 2001.he0"), &index_path)?;

    let disk_number = 2;
    build_disk(
        index_path.into_os_string().into_string().unwrap(),
        disk_number,
        |path| fs_read(&fs, path),
    )?;

    assert_stream_eq(
        &root.join("baseball 2001.(b)"),
        &mut File::open(&fixture_path("baseball 2001.(b)"))?,
    )?;
    Ok(())
}

fn assert_stream_eq(path: &Path, s2: &mut (impl Read + Seek)) -> Result<(), io::Error> {
    let mut s1 = File::open(path)?;
    if streams_eq(&mut s1, s2)? {
        return Ok(());
    }
    // Streams don't match. Dump for debugging, then panic
    s2.seek(SeekFrom::Start(0))?;
    let dest = env::temp_dir().join(path.file_name().unwrap());
    io::copy(s2, &mut File::create(&dest)?)?;
    assert_eq!(path, dest);
    unreachable!()
}

fn streams_eq(s1: &mut impl Read, s2: &mut impl Read) -> Result<bool, io::Error> {
    let mut buf1 = [0; 8 << 10];
    let mut buf2 = [0; 8 << 10];
    loop {
        let n = s1.read(&mut buf1)?;
        // 0 is EOF
        if n == 0 {
            let n = s2.read(&mut buf2)?;
            return Ok(n == 0);
        }

        // Pedantically, EOF is not handled here, so this might return an error instead
        // of Ok(false). That's fine for testing.
        s2.read_exact(&mut buf2[..n])?;
        if buf1[..n] != buf2[..n] {
            return Ok(false);
        }
    }
}

fn fixture_path(name: &str) -> PathBuf {
    let mut result = PathBuf::from(file!());
    result.pop();
    result.push("fixtures");
    result.push("baseball2001");
    result.push(name);
    result
}

enum Entry {
    Dir(HashMap<String, Entry>),
    File(Vec<u8>),
}

fn fs_read(fs: &HashMap<String, Entry>, path: &str) -> Result<FsEntry, Box<dyn Error>> {
    let (dir, file) = match path.rsplit_once('/') {
        Some((dir, file)) => (dir, file),
        None => ("", path),
    };
    match get_dir(fs, dir)?.get(file) {
        Some(Entry::Dir(children)) => Ok(FsEntry::Dir(children.keys().cloned().collect())),
        Some(Entry::File(data)) => Ok(FsEntry::File(data.clone())),
        _ => Err("not found".into()),
    }
}

fn fs_write(
    fs: &mut HashMap<String, Entry>,
    path: &str,
    data: Vec<u8>,
) -> Result<(), Box<dyn Error>> {
    let (dir, file) = path.rsplit_once('/').unwrap();
    mkdirs(fs, dir)?.insert(file.to_string(), Entry::File(data));
    Ok(())
}

fn mkdirs<'a>(
    mut fs: &'a mut HashMap<String, Entry>,
    path: &str,
) -> Result<&'a mut HashMap<String, Entry>, Box<dyn Error>> {
    for part in path.split('/') {
        let child = if fs.contains_key(part) {
            fs.get_mut(part).unwrap()
        } else {
            fs.entry(part.to_string())
                .or_insert_with(|| Entry::Dir(HashMap::new()))
        };
        fs = match child {
            Entry::Dir(dir) => dir,
            Entry::File(_) => return Err("not a dir".into()),
        };
    }
    Ok(fs)
}

fn get_dir<'a>(
    mut fs: &'a HashMap<String, Entry>,
    path: &str,
) -> Result<&'a HashMap<String, Entry>, Box<dyn Error>> {
    if path.is_empty() {
        return Ok(fs);
    }
    for part in path.split('/') {
        let child = fs.get(part).ok_or("dir not found")?;
        fs = match child {
            Entry::Dir(dir) => dir,
            Entry::File(_) => return Err("not a dir".into()),
        };
    }
    Ok(fs)
}
