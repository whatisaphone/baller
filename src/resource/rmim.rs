use crate::{blocks::BlockScanner, utils::bit_stream::BitStream};
use byteordered::byteorder::{ReadBytesExt, WriteBytesExt, LE};
use std::{
    error::Error,
    io::{BufWriter, Cursor, Seek, SeekFrom, Write},
};

pub fn decode(palette: &[u8], rmim_raw: &[u8]) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut r = Cursor::new(rmim_raw);
    let mut rmih_scanner = BlockScanner::new(rmim_raw.len().try_into().unwrap());
    let rmih_len = rmih_scanner
        .next_block_must_be(&mut r, *b"RMIH")?
        .ok_or("expected RMIH")?
        .try_into() // TODO: why does next_block return u64?
        .unwrap();
    r.seek(SeekFrom::Current(rmih_len))?; // skip for now

    let im00_len = rmih_scanner
        .next_block_must_be(&mut r, *b"IM00")?
        .ok_or("expected IM00")?;
    let mut im00_scanner = BlockScanner::new(r.position() + im00_len);

    let bmap_len = im00_scanner
        .next_block_must_be(&mut r, *b"BMAP")?
        .ok_or("expected BMAP")?;
    let bmap_end = r.position() + bmap_len;

    let mut out = Vec::with_capacity(1024);
    let mut w = BufWriter::new(&mut out);
    let width = 640; // TODO
    let height = 480;
    write_bmp_header(&mut w, width, height)?;
    write_bmp_palette(&mut w, palette)?;

    let compression = r.read_u8()?;
    // for now, only supporting BMCOMP_NMAJMIN_H8
    if compression != 0x8a {
        return Err(format!("unsupported compression type 0x{:02x}", compression).into());
    }

    let delta: [i16; 8] = [-4, -3, -2, -1, 1, 2, 3, 4];

    let mut color = r.read_u8()?;
    let mut bits = BitStream::new();
    while !(r.position() == bmap_end && bits.buf_remaining() == 0) {
        w.write_u8(color)?;
        if bits.read_bit(&mut r)? {
            if bits.read_bit(&mut r)? {
                let d: usize = bits.read_bits(&mut r, 3)?.into();
                color = u8::try_from(i16::from(color) + delta[d])?;
            } else {
                color = bits.read_bits(&mut r, 8)?;
            }
        }
    }

    w.flush()?;
    drop(w);
    Ok(out)
}

fn write_bmp_header(w: &mut impl Write, width: u32, height: u32) -> Result<(), Box<dyn Error>> {
    const BITMAP_FILE_HEADER_SIZE: u32 = 14;
    const BITMAP_INFO_HEADER_SIZE: u32 = 40;
    const FULL_HEADER_SIZE: u32 = BITMAP_FILE_HEADER_SIZE + BITMAP_INFO_HEADER_SIZE;

    const PALETTE_SIZE: u32 = 0x400;

    // BITMAPFILEHEADER
    w.write_all(b"BM")?;
    w.write_u32::<LE>(FULL_HEADER_SIZE + PALETTE_SIZE + width * height)?;
    w.write_u16::<LE>(0)?;
    w.write_u16::<LE>(0)?;
    w.write_u32::<LE>(FULL_HEADER_SIZE + PALETTE_SIZE)?;

    // BITMAPINFOHEADER
    w.write_u32::<LE>(BITMAP_INFO_HEADER_SIZE)?;
    w.write_i32::<LE>(width.try_into().unwrap())?;
    w.write_i32::<LE>(-i32::try_from(height).unwrap())?;
    w.write_u16::<LE>(1)?;
    w.write_u16::<LE>(8)?;
    w.write_u32::<LE>(0)?; // BI_RGB
    w.write_u32::<LE>(0)?;
    w.write_i32::<LE>(0)?;
    w.write_i32::<LE>(0)?;
    w.write_u32::<LE>(256)?;
    w.write_u32::<LE>(0)?;
    Ok(())
}

fn write_bmp_palette(mut w: impl Write, palette: &[u8]) -> Result<(), Box<dyn Error>> {
    for i in 0..0x100 {
        w.write_all(&palette[i * 3..i * 3 + 3])?;
        w.write_all(&[0])?;
    }
    Ok(())
}
