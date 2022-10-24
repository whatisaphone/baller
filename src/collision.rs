#![allow(unaligned_references, clippy::cast_ptr_alignment)] // major TODO here, drop the mmap

use memmap2::Mmap;
use std::{
    error::Error,
    fmt,
    fs::File,
    io::{BufWriter, Seek, SeekFrom, Write},
    mem,
    path::{Path, PathBuf},
    slice,
};

#[repr(packed)]
struct Field {
    box_count: i16,
    boxes: [CollideBox; 64],
    #[allow(dead_code)]
    slices: [[i32; 16]; 16],
}

struct CollideBox {
    p1: Vec3,
    unused: i32,
    p2: Vec3,
    p3: Vec3,
    p4: Vec3,
    n1: Vec3,
    n2: Vec3,
    n3: Vec3,
    n4: Vec3,
    n5: Vec3,
    bounce: i32,
    material: i32,
    eat_ball: i32,
    end_play: i32,
}

struct Vec3 {
    x: i32,
    y: i32,
    z: i32,
}

pub fn extract(input: &Path, mut output: PathBuf) -> Result<(), Box<dyn Error>> {
    debug_assert!(mem::size_of::<Field>() == 9218);

    let mut f = File::open(input)?;

    let len: usize = f.seek(SeekFrom::End(0))?.try_into().unwrap();
    let num_fields = len / mem::size_of::<Field>();

    let f = unsafe { Mmap::map(&f) }?;
    let fields = unsafe { slice::from_raw_parts(f.as_ptr().cast::<Field>(), num_fields) };

    output.push("dump.csv");
    dump_csv(fields, &output)?;
    output.pop();

    for (fi, field) in fields.iter().enumerate() {
        output.push(&format!("field{fi}.scad")); // yikes
        write_scad(field, &output)?;
        output.pop();
    }

    Ok(())
}

fn dump_csv(fields: &[Field], out_path: &Path) -> Result<(), Box<dyn Error>> {
    let mut out = BufWriter::new(File::create(out_path)?);
    writeln!(
        out,
        "field,box,p1x,p1y,p1z,unused,p2x,p2y,p2z,p3x,p3y,p3z,p4x,p4y,p4z,n1x,n1y,n1z,n2x,n2y,n2z,\
         n3x,n3y,n3z,n4x,n4y,n4z,n5x,n5y,n5z,bounce,material,eat_ball,end_play"
    )?;
    for (fi, field) in fields.iter().enumerate() {
        let boxes = &field.boxes[..field.box_count.try_into()?];
        for (bi, b) in boxes.iter().enumerate() {
            write!(out, "{fi},{bi}")?;
            macro_rules! writ {
                ($($f:tt)+) => {
                    out.write_all(b",")?;
                    write!(out, "{}", b.$($f)+)?;
                };
            }
            writ!(p1.x);
            writ!(p1.y);
            writ!(p1.z);
            writ!(unused);
            writ!(p2.x);
            writ!(p2.y);
            writ!(p2.z);
            writ!(p3.x);
            writ!(p3.y);
            writ!(p3.z);
            writ!(p4.x);
            writ!(p4.y);
            writ!(p4.z);
            writ!(n1.x);
            writ!(n1.y);
            writ!(n1.z);
            writ!(n2.x);
            writ!(n2.y);
            writ!(n2.z);
            writ!(n3.x);
            writ!(n3.y);
            writ!(n3.z);
            writ!(n4.x);
            writ!(n4.y);
            writ!(n4.z);
            writ!(n5.x);
            writ!(n5.y);
            writ!(n5.z);
            writ!(bounce);
            writ!(material);
            writ!(eat_ball);
            writ!(end_play);
            writeln!(out)?;
        }
    }
    out.flush()?;
    Ok(())
}

fn write_scad(field: &Field, path: &Path) -> Result<(), Box<dyn Error>> {
    let mut out = BufWriter::new(File::create(path)?);
    out.write_all(
        b"\
$fs = 120;

$vpt = [0,1080,480];
$vpr = [-34,0,0];
$vpf = 45;

foul_line = 6;
plain_color = [1,1,1,0.8];
out_color = [1,0.75,0.75,0.8];
eat_color = [0.5,0.75,1,0.8];

// home, first, second, third, rubber
color([1,1,1]) scale([1,-1,-1]) rotate([90,0,0]) linear_extrude(1) {
    polygon([[-30,30*2+300],[30,30*2+300],[30,30+300],[0,300],[-30,30+300]]);
    polygon([[552-36*2,852],[552-36,852+36],[552,852],[552-36,852-36]]);
    polygon([[0,1404+36],[36,1404],[0,1404-36],[-36,1404]]);
    polygon([[-552+36*2,852],[-552+36,852+36],[-552,852],[-552+36,852-36]]);
    polygon([[-19,852],[19,852],[19,9+852],[-19,9+852]]);
}

// foul lines
color([1,1,1,0.5]) scale([1,-1,-1]) rotate([90,0,0]) linear_extrude(1) {
    polygon([[0,300],[-5000,300+5000],[-5000,300+5000+foul_line],[0,300+foul_line]]);
    polygon([[0,300],[5000,300+5000],[5000,300+5000+foul_line],[0,300+foul_line]]);
}

scale([0.1,0.1,-0.1]) translate([-23200,0,0]) {
",
    )?;
    for b in &field.boxes {
        let color = if b.eat_ball != 0 {
            "eat_color"
        } else if b.end_play != 0 {
            "out_color"
        } else {
            "plain_color"
        };
        // face 1, p1-p3
        writeln!(
            out,
            "    color({}) polyhedron([{},{},{},{}], [[0,1,3,2]]);",
            color,
            SVec3(b.p1.x, 0, b.p1.z),
            SVec3(b.p3.x, 0, b.p3.z),
            SVec3(b.p1.x, b.p3.y, b.p1.z),
            SVec3(b.p3.x, b.p3.y, b.p3.z),
        )?;
        // face 2, p1-p2
        writeln!(
            out,
            "    color({}) polyhedron([{},{},{},{}], [[0,1,3,2]]);",
            color,
            SVec3(b.p1.x, 0, b.p1.z),
            SVec3(b.p2.x, 0, b.p2.z),
            SVec3(b.p1.x, b.p3.y, b.p1.z),
            SVec3(b.p2.x, b.p4.y, b.p2.z),
        )?;
        // face 3, p3-p4
        writeln!(
            out,
            "    color({}) polyhedron([{},{},{},{}], [[0,1,3,2]]);",
            color,
            SVec3(b.p3.x, 0, b.p3.z),
            SVec3(b.p4.x, 0, b.p4.z),
            SVec3(b.p3.x, b.p3.y, b.p3.z),
            SVec3(b.p4.x, b.p4.y, b.p4.z),
        )?;
        // face 4, p2-p4
        writeln!(
            out,
            "    color({}) polyhedron([{},{},{},{}], [[0,1,3,2]]);",
            color,
            SVec3(b.p2.x, 0, b.p2.z),
            SVec3(b.p4.x, 0, b.p4.z),
            SVec3(b.p2.x, b.p4.y, b.p2.z),
            SVec3(b.p4.x, b.p4.y, b.p4.z),
        )?;
        // face 5, p1-p2-p3-p4
        writeln!(
            out,
            "    color({}) polyhedron([{},{},{},{}], [[0,1,3,2]]);",
            color,
            SVec3(b.p1.x, b.p3.y, b.p1.z),
            SVec3(b.p2.x, b.p4.y, b.p2.z),
            SVec3(b.p3.x, b.p3.y, b.p3.z),
            SVec3(b.p4.x, b.p4.y, b.p4.z),
        )?;
    }
    out.write_all(b"}\n")?;
    out.flush()?;
    Ok(())
}

struct SVec3(i32, i32, i32);

impl fmt::Display for SVec3 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{},{},{}]", self.0, self.1, self.2)
    }
}
