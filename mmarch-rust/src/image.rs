//! The image formats inside MM and Heroes archives, and the .bmp files RSPak
//! turns them into.
//!
//! `TRSLod.DoExtract` (RSLod.pas) does not hand a texture, sprite or PCX out as
//! stored bytes — it decodes each one into a Windows bitmap and saves that. The
//! three source formats differ in where the palette comes from:
//!
//! * **textures / icons** (`TMMLodFile` with `BmpSize != 0`) carry their own
//!   768-byte palette right behind the pixel data.
//! * **sprites** only name a palette; the 768 bytes live in a `palNNN` entry of
//!   a `bitmaps.lod` sitting next to the archive.
//! * **Heroes PCX** carries its palette behind the pixels, or is 24-bit.

use std::io;

/// One decoded 8-bit image: rows top-down, plus the 256*3 RGB palette.
pub struct Indexed {
    pub width: u32,
    pub height: u32,
    /// `height` rows of `width` palette indices, first row at the top.
    pub rows: Vec<Vec<u8>>,
    /// 768 bytes, R,G,B per entry.
    pub palette: Vec<u8>,
}

const FILE_HEADER: usize = 14;
const INFO_HEADER: usize = 40;
const PALETTE_BYTES: usize = 256 * 4;

fn row_stride(width: u32, bits_per_pixel: u32) -> usize {
    (((width * bits_per_pixel + 31) / 32) * 4) as usize
}

/// Write an 8-bit indexed BMP: the file Delphi's `TBitmap.SaveToStream`
/// produces for a `pf8bit` bitmap, and byte for byte what MMArchive writes.
pub fn write_bmp8(img: &Indexed) -> Vec<u8> {
    let stride = row_stride(img.width, 8);
    let padding = stride - img.width as usize;
    let image_size = stride * img.height as usize;
    let mut out = Vec::with_capacity(FILE_HEADER + INFO_HEADER + PALETTE_BYTES + image_size);

    // BITMAPFILEHEADER
    out.extend_from_slice(b"BM");
    out.extend_from_slice(
        &((FILE_HEADER + INFO_HEADER + PALETTE_BYTES + image_size) as u32).to_le_bytes());
    out.extend_from_slice(&0u16.to_le_bytes());   // reserved
    out.extend_from_slice(&0u16.to_le_bytes());   // reserved
    out.extend_from_slice(&((FILE_HEADER + INFO_HEADER + PALETTE_BYTES) as u32).to_le_bytes());

    // BITMAPINFOHEADER
    out.extend_from_slice(&(INFO_HEADER as u32).to_le_bytes());
    out.extend_from_slice(&(img.width as i32).to_le_bytes());
    out.extend_from_slice(&(img.height as i32).to_le_bytes());  // positive: bottom-up
    out.extend_from_slice(&1u16.to_le_bytes());   // planes
    out.extend_from_slice(&8u16.to_le_bytes());   // bits per pixel
    out.extend_from_slice(&0u32.to_le_bytes());   // BI_RGB
    out.extend_from_slice(&(image_size as u32).to_le_bytes());
    out.extend_from_slice(&0i32.to_le_bytes());   // x pixels per metre
    out.extend_from_slice(&0i32.to_le_bytes());   // y pixels per metre
    out.extend_from_slice(&256u32.to_le_bytes()); // colours used
    out.extend_from_slice(&0u32.to_le_bytes());   // colours important

    // Colour table, BGRA
    for i in 0..256 {
        let (r, g, b) = (
            img.palette.get(i * 3).copied().unwrap_or(0),
            img.palette.get(i * 3 + 1).copied().unwrap_or(0),
            img.palette.get(i * 3 + 2).copied().unwrap_or(0),
        );
        out.extend_from_slice(&[b, g, r, 0]);
    }

    // Rows, bottom first
    for row in img.rows.iter().rev() {
        out.extend_from_slice(row);
        out.extend(std::iter::repeat(0u8).take(padding));
    }
    out
}

/// Write a 24-bit BMP from top-down BGR rows (Heroes PCX without a palette).
pub fn write_bmp24(width: u32, height: u32, rows_bgr: &[Vec<u8>]) -> Vec<u8> {
    let stride = row_stride(width, 24);
    let padding = stride - (width as usize) * 3;
    let image_size = stride * height as usize;
    let mut out = Vec::with_capacity(FILE_HEADER + INFO_HEADER + image_size);
    out.extend_from_slice(b"BM");
    out.extend_from_slice(&((FILE_HEADER + INFO_HEADER + image_size) as u32).to_le_bytes());
    out.extend_from_slice(&0u16.to_le_bytes());
    out.extend_from_slice(&0u16.to_le_bytes());
    out.extend_from_slice(&((FILE_HEADER + INFO_HEADER) as u32).to_le_bytes());
    out.extend_from_slice(&(INFO_HEADER as u32).to_le_bytes());
    out.extend_from_slice(&(width as i32).to_le_bytes());
    out.extend_from_slice(&(height as i32).to_le_bytes());
    out.extend_from_slice(&1u16.to_le_bytes());
    out.extend_from_slice(&24u16.to_le_bytes());
    out.extend_from_slice(&0u32.to_le_bytes());
    out.extend_from_slice(&(image_size as u32).to_le_bytes());
    out.extend_from_slice(&0i32.to_le_bytes());
    out.extend_from_slice(&0i32.to_le_bytes());
    out.extend_from_slice(&0u32.to_le_bytes());
    out.extend_from_slice(&0u32.to_le_bytes());
    for row in rows_bgr.iter().rev() {
        out.extend_from_slice(row);
        out.extend(std::iter::repeat(0u8).take(padding));
    }
    out
}

/// A BMP read back off disk: either palette indices or plain BGR pixels.
/// Heroes archives hold both kinds.
pub enum AnyBitmap {
    Indexed(Indexed),
    /// Rows top-down, three bytes per pixel in B, G, R order.
    Bgr { width: u32, height: u32, rows: Vec<Vec<u8>> },
}

/// Read a BMP, 8-bit or 24-bit.
pub fn read_bmp(data: &[u8]) -> io::Result<AnyBitmap> {
    if data.len() >= 30 && u16::from_le_bytes([data[28], data[29]]) == 24 {
        return read_bmp24(data).map(|(w, h, rows)| AnyBitmap::Bgr { width: w, height: h, rows });
    }
    read_bmp8(data).map(AnyBitmap::Indexed)
}

fn read_bmp24(data: &[u8]) -> io::Result<(u32, u32, Vec<Vec<u8>>)> {
    let bad = |what: &str| {
        io::Error::new(io::ErrorKind::InvalidData, format!("not a usable 24-bit BMP: {}", what))
    };
    if data.len() < FILE_HEADER + INFO_HEADER || &data[0..2] != b"BM" {
        return Err(bad("no BM signature"));
    }
    let u32_at = |o: usize| u32::from_le_bytes([data[o], data[o + 1], data[o + 2], data[o + 3]]);
    let i32_at = |o: usize| i32::from_le_bytes([data[o], data[o + 1], data[o + 2], data[o + 3]]);
    let pixel_offset = u32_at(10) as usize;
    let width = i32_at(18);
    let height = i32_at(22);
    if u32_at(30) != 0 {
        return Err(bad("compressed BMPs are not supported"));
    }
    if width <= 0 || height == 0 {
        return Err(bad("zero-sized image"));
    }
    let top_down = height < 0;
    let (w, h) = (width as u32, height.unsigned_abs());
    let stride = row_stride(w, 24);
    if pixel_offset + stride * h as usize > data.len() {
        return Err(bad("truncated pixel data"));
    }
    let mut rows = Vec::with_capacity(h as usize);
    for y in 0..h as usize {
        let at = pixel_offset + y * stride;
        rows.push(data[at..at + (w as usize) * 3].to_vec());
    }
    if !top_down {
        rows.reverse();
    }
    Ok((w, h, rows))
}

/// Read back an 8-bit indexed BMP (what `add` receives after an `extract`).
pub fn read_bmp8(data: &[u8]) -> io::Result<Indexed> {
    let bad = |what: &str| {
        io::Error::new(io::ErrorKind::InvalidData, format!("not a usable 8-bit BMP: {}", what))
    };
    if data.len() < FILE_HEADER + INFO_HEADER || &data[0..2] != b"BM" {
        return Err(bad("no BM signature"));
    }
    let u32_at = |o: usize| u32::from_le_bytes([data[o], data[o + 1], data[o + 2], data[o + 3]]);
    let i32_at = |o: usize| i32::from_le_bytes([data[o], data[o + 1], data[o + 2], data[o + 3]]);
    let u16_at = |o: usize| u16::from_le_bytes([data[o], data[o + 1]]);

    let pixel_offset = u32_at(10) as usize;
    let info_size = u32_at(14) as usize;
    let width = i32_at(18);
    let height = i32_at(22);
    let bpp = u16_at(28);
    let compression = u32_at(30);
    if bpp != 8 {
        return Err(bad("only 8-bit images can go into an MM archive"));
    }
    if compression != 0 {
        return Err(bad("compressed BMPs are not supported"));
    }
    if width <= 0 || height == 0 {
        return Err(bad("zero-sized image"));
    }
    let top_down = height < 0;
    let (width, height) = (width as u32, height.unsigned_abs());

    let mut colours = u32_at(46) as usize;
    if colours == 0 {
        colours = 256;
    }
    let table_at = FILE_HEADER + info_size;
    if table_at + colours * 4 > data.len() || pixel_offset > data.len() {
        return Err(bad("truncated colour table"));
    }
    let mut palette = vec![0u8; 768];
    for i in 0..colours.min(256) {
        let o = table_at + i * 4;
        palette[i * 3] = data[o + 2];      // R
        palette[i * 3 + 1] = data[o + 1];  // G
        palette[i * 3 + 2] = data[o];      // B
    }

    let stride = row_stride(width, 8);
    if pixel_offset + stride * height as usize > data.len() {
        return Err(bad("truncated pixel data"));
    }
    let mut rows = Vec::with_capacity(height as usize);
    for y in 0..height as usize {
        let src = pixel_offset + y * stride;
        rows.push(data[src..src + width as usize].to_vec());
    }
    if !top_down {
        rows.reverse();
    }
    Ok(Indexed { width, height, rows, palette })
}

// ---------------------------------------------------------------- sprites --

/// RSLod.pas `TSprite`, the header of a sprites.lod entry.
#[derive(Clone, Copy, Debug)]
pub struct SpriteHeader {
    pub data_size: u32,
    pub width: i16,
    pub height: i16,
    pub palette: i16,
    pub y_skip: i16,
    pub unpacked_size: u32,
}

/// The 20 bytes of TSprite, as they sit in the entry (after the 12-byte name).
pub const SPRITE_HEADER_SIZE: usize = 20;
const SPRITE_LINE_SIZE: usize = 8;

pub fn read_sprite_header(raw: &[u8], name_size: usize) -> io::Result<(SpriteHeader, usize)> {
    // TRSLod.DoExtract seeks NameSize - 4 before reading the header.
    let at = name_size - 4;
    if raw.len() < at + SPRITE_HEADER_SIZE {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "sprite entry is too small"));
    }
    let u32_at = |o: usize| u32::from_le_bytes([raw[o], raw[o + 1], raw[o + 2], raw[o + 3]]);
    let i16_at = |o: usize| i16::from_le_bytes([raw[o], raw[o + 1]]);
    Ok((
        SpriteHeader {
            data_size: u32_at(at),
            width: i16_at(at + 4),
            height: i16_at(at + 6),
            palette: i16_at(at + 8),
            y_skip: i16_at(at + 12),
            unpacked_size: u32_at(at + 16),
        },
        at,
    ))
}

/// Decode a sprites.lod entry into palette indices (RSLod.pas
/// TRSLod.UnpackSprite). The palette itself is not here — the entry only names
/// one, and the caller has to find it in a bitmaps.lod.
pub fn decode_sprite(raw: &[u8], name_size: usize) -> io::Result<(SpriteHeader, Vec<Vec<u8>>)> {
    let (hdr, at) = read_sprite_header(raw, name_size)?;
    if hdr.width < 0 || hdr.height < 0 {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "sprite has a negative size"));
    }
    let (w, h) = (hdr.width as usize, hdr.height as usize);
    let lines_at = at + SPRITE_HEADER_SIZE;
    let pixels_at = lines_at + h * SPRITE_LINE_SIZE;
    if pixels_at > raw.len() {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "sprite line table is truncated"));
    }

    let pixels = if hdr.unpacked_size != 0 {
        crate::lod::zlib_decompress_bounded(&raw[pixels_at..], hdr.unpacked_size as usize)?
    } else {
        raw[pixels_at..].to_vec()
    };

    // Each line names the span it actually covers; everything outside it is
    // the transparent colour, index 0.
    let mut rows = Vec::with_capacity(h);
    for y in 0..h {
        let o = lines_at + y * SPRITE_LINE_SIZE;
        let a1 = i16::from_le_bytes([raw[o], raw[o + 1]]) as i32;
        let a2 = i16::from_le_bytes([raw[o + 2], raw[o + 3]]) as i32;
        let pos = i32::from_le_bytes([raw[o + 4], raw[o + 5], raw[o + 6], raw[o + 7]]);
        let mut row = vec![0u8; w];
        if a1 >= 0 && a2 >= a1 && pos >= 0 {
            let start = (a1 as usize).min(w);
            let count = ((a2 - a1 + 1) as usize).min(w - start);
            let src = pos as usize;
            if src + count <= pixels.len() {
                row[start..start + count].copy_from_slice(&pixels[src..src + count]);
            }
        }
        rows.push(row);
    }
    Ok((hdr, rows))
}

// --------------------------------------------------------- known bad palettes

/// Sprites that name a palette that is wrong or missing.
///
/// MM6's bats point at palette 422, which exists in `bitmaps.lod` but belongs
/// to something else; MM7's swamp trees point at 940, which is not in
/// `bitmaps.lod` at all. MMArchive carries this table (`PalFix` in
/// RSLodEdt.pas) and so does mmarch: name, the palette to use instead, and the
/// 20 header bytes that identify the resource, so a modified sprite of the same
/// name is left alone.
pub const PALETTE_FIXUPS: &[(&str, i16, [u8; SPRITE_HEADER_SIZE])] = &[
    ("BATATA0", 156, [0xD3, 0x1A, 0x00, 0x00, 0xE2, 0x00, 0xE2, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x3C, 0x00, 0xFF, 0xFF, 0x4B, 0x65, 0x00, 0x00]),
    ("BATATB0", 156, [0xBC, 0x25, 0x00, 0x00, 0xFE, 0x00, 0x11, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xDA, 0x71, 0x00, 0x00]),
    ("BATATC0", 156, [0x4B, 0x33, 0x00, 0x00, 0x3E, 0x01, 0x2C, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0xA9, 0x94, 0x00, 0x00]),
    ("BATATD0", 156, [0xD4, 0x32, 0x00, 0x00, 0x17, 0x01, 0x1D, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0xD1, 0x91, 0x00, 0x00]),
    ("BATATE0", 156, [0xA7, 0x1B, 0x00, 0x00, 0x72, 0x01, 0xB0, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0xEC, 0x6B, 0x00, 0x00]),
    ("BATATF0", 156, [0xE1, 0x16, 0x00, 0x00, 0x72, 0x01, 0x55, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0x29, 0x37, 0x00, 0x00]),
    ("BATDEA0", 156, [0xAD, 0x4A, 0x00, 0x00, 0xCA, 0x00, 0xC8, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x65, 0x67, 0x00, 0x00]),
    ("BATDEB0", 156, [0x22, 0x2F, 0x00, 0x00, 0x0D, 0x01, 0x77, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x9B, 0x40, 0x00, 0x00]),
    ("BATDEC0", 156, [0xB3, 0x36, 0x00, 0x00, 0x28, 0x01, 0x98, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0x53, 0x5E, 0x00, 0x00]),
    ("BATDED0", 156, [0x1D, 0x46, 0x00, 0x00, 0x03, 0x01, 0xDB, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x23, 0x00, 0xFF, 0xFF, 0x28, 0x5F, 0x00, 0x00]),
    ("BATDEE0", 156, [0xDB, 0x1C, 0x00, 0x00, 0xC5, 0x00, 0x6E, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x07, 0x00, 0xFF, 0xFF, 0x9B, 0x21, 0x00, 0x00]),
    ("BATDEF0", 156, [0x49, 0x13, 0x00, 0x00, 0xC9, 0x00, 0x64, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0xD7, 0x19, 0x00, 0x00]),
    ("BATWAA0", 156, [0x28, 0x19, 0x00, 0x00, 0x2D, 0x01, 0xE4, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x45, 0x00, 0xFF, 0xFF, 0xCA, 0x5B, 0x00, 0x00]),
    ("BATWAA1", 156, [0xCF, 0x3D, 0x00, 0x00, 0x3C, 0x01, 0x0C, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x5B, 0x00, 0xFF, 0xFF, 0x89, 0x62, 0x00, 0x00]),
    ("BATWAA2", 156, [0x45, 0x35, 0x00, 0x00, 0xB8, 0x00, 0x16, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x5D, 0x00, 0xFF, 0xFF, 0x44, 0x45, 0x00, 0x00]),
    ("BATWAA3", 156, [0x50, 0x35, 0x00, 0x00, 0x12, 0x01, 0x06, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x4D, 0x00, 0xFF, 0xFF, 0xF6, 0x66, 0x00, 0x00]),
    ("BATWAA4", 156, [0x1C, 0x16, 0x00, 0x00, 0x54, 0x01, 0xEA, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x36, 0x00, 0xFF, 0xFF, 0xF7, 0x69, 0x00, 0x00]),
    ("BATWAB0", 156, [0x13, 0x1A, 0x00, 0x00, 0x2D, 0x01, 0xE4, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x43, 0x00, 0xFF, 0xFF, 0x38, 0x4F, 0x00, 0x00]),
    ("BATWAB1", 156, [0x5B, 0x35, 0x00, 0x00, 0x3C, 0x01, 0x0C, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x5A, 0x00, 0xFF, 0xFF, 0xFB, 0x57, 0x00, 0x00]),
    ("BATWAB2", 156, [0x55, 0x31, 0x00, 0x00, 0xB8, 0x00, 0x16, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x5C, 0x00, 0xFF, 0xFF, 0x64, 0x3C, 0x00, 0x00]),
    ("BATWAB3", 156, [0xB5, 0x3A, 0x00, 0x00, 0x12, 0x01, 0x06, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x4C, 0x00, 0xFF, 0xFF, 0x95, 0x61, 0x00, 0x00]),
    ("BATWAB4", 156, [0x2B, 0x18, 0x00, 0x00, 0x54, 0x01, 0xEA, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x35, 0x00, 0xFF, 0xFF, 0x7B, 0x60, 0x00, 0x00]),
    ("BATWAC0", 156, [0x68, 0x15, 0x00, 0x00, 0x2D, 0x01, 0xE4, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x44, 0x00, 0xFF, 0xFF, 0x19, 0x47, 0x00, 0x00]),
    ("BATWAC1", 156, [0x30, 0x27, 0x00, 0x00, 0x3C, 0x01, 0x0C, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x5A, 0x00, 0xFF, 0xFF, 0x47, 0x4B, 0x00, 0x00]),
    ("BATWAC2", 156, [0x34, 0x27, 0x00, 0x00, 0xB8, 0x00, 0x16, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x5D, 0x00, 0xFF, 0xFF, 0x8F, 0x2F, 0x00, 0x00]),
    ("BATWAC3", 156, [0xB0, 0x2E, 0x00, 0x00, 0x12, 0x01, 0x06, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x4C, 0x00, 0xFF, 0xFF, 0x6D, 0x52, 0x00, 0x00]),
    ("BATWAC4", 156, [0x48, 0x16, 0x00, 0x00, 0x54, 0x01, 0xEA, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x35, 0x00, 0xFF, 0xFF, 0xA8, 0x59, 0x00, 0x00]),
    ("BATWAD0", 156, [0x33, 0x15, 0x00, 0x00, 0x2D, 0x01, 0xE4, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x23, 0x00, 0xFF, 0xFF, 0x6C, 0x4C, 0x00, 0x00]),
    ("BATWAD1", 156, [0xFA, 0x28, 0x00, 0x00, 0x3C, 0x01, 0x0C, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x2F, 0x00, 0xFF, 0xFF, 0x30, 0x47, 0x00, 0x00]),
    ("BATWAD2", 156, [0xCF, 0x1E, 0x00, 0x00, 0xB8, 0x00, 0x16, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x2D, 0x00, 0xFF, 0xFF, 0x84, 0x26, 0x00, 0x00]),
    ("BATWAD3", 156, [0x62, 0x21, 0x00, 0x00, 0x12, 0x01, 0x06, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x22, 0x00, 0xFF, 0xFF, 0xBA, 0x45, 0x00, 0x00]),
    ("BATWAD4", 156, [0xDF, 0x18, 0x00, 0x00, 0x54, 0x01, 0xEA, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x14, 0x00, 0xFF, 0xFF, 0x4A, 0x5B, 0x00, 0x00]),
    ("BATWAE0", 156, [0xED, 0x30, 0x00, 0x00, 0x2D, 0x01, 0xE4, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x38, 0x00, 0xFF, 0xFF, 0xEF, 0x4C, 0x00, 0x00]),
    ("BATWAE1", 156, [0xF0, 0x2E, 0x00, 0x00, 0x3C, 0x01, 0x0C, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x4E, 0x00, 0xFF, 0xFF, 0x60, 0x45, 0x00, 0x00]),
    ("BATWAE2", 156, [0xD5, 0x14, 0x00, 0x00, 0xB8, 0x00, 0x16, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x50, 0x00, 0xFF, 0xFF, 0x89, 0x19, 0x00, 0x00]),
    ("BATWAE3", 156, [0xB5, 0x27, 0x00, 0x00, 0x12, 0x01, 0x06, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x40, 0x00, 0xFF, 0xFF, 0x08, 0x3E, 0x00, 0x00]),
    ("BATWAE4", 156, [0xEB, 0x37, 0x00, 0x00, 0x54, 0x01, 0xEA, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x28, 0x00, 0xFF, 0xFF, 0xBD, 0x59, 0x00, 0x00]),
    ("BATWAF0", 156, [0xB6, 0x1B, 0x00, 0x00, 0x2D, 0x01, 0xE4, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x3D, 0x00, 0xFF, 0xFF, 0x12, 0x3D, 0x00, 0x00]),
    ("BATWAF1", 156, [0x8B, 0x2B, 0x00, 0x00, 0x3C, 0x01, 0x0C, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x53, 0x00, 0xFF, 0xFF, 0xA7, 0x41, 0x00, 0x00]),
    ("BATWAF2", 156, [0x8E, 0x28, 0x00, 0x00, 0xB8, 0x00, 0x16, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x55, 0x00, 0xFF, 0xFF, 0x12, 0x32, 0x00, 0x00]),
    ("BATWAF3", 156, [0xA2, 0x2B, 0x00, 0x00, 0x12, 0x01, 0x06, 0x01, 0xA6, 0x01, 0x00, 0x00, 0x45, 0x00, 0xFF, 0xFF, 0x5C, 0x3E, 0x00, 0x00]),
    ("BATWAF4", 156, [0x3F, 0x22, 0x00, 0x00, 0x54, 0x01, 0xEA, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x2D, 0x00, 0xFF, 0xFF, 0x52, 0x49, 0x00, 0x00]),
    ("BATWIA0", 156, [0x82, 0x17, 0x00, 0x00, 0xDF, 0x00, 0xCD, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x2F, 0x00, 0xFF, 0xFF, 0xF0, 0x59, 0x00, 0x00]),
    ("BATWIB0", 156, [0xE1, 0x5F, 0x00, 0x00, 0x4E, 0x01, 0xB4, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0x10, 0x8E, 0x00, 0x00]),
    ("BATWIC0", 156, [0xE7, 0x6C, 0x00, 0x00, 0x38, 0x01, 0xD0, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0x86, 0xA1, 0x00, 0x00]),
    ("BATWID0", 156, [0x9D, 0x65, 0x00, 0x00, 0x52, 0x01, 0xD9, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x02, 0x00, 0xFF, 0xFF, 0x0D, 0x97, 0x00, 0x00]),
    ("BATWIE0", 156, [0x46, 0x2D, 0x00, 0x00, 0x3D, 0x01, 0x9B, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0x92, 0x48, 0x00, 0x00]),
    ("BATWIF0", 156, [0x82, 0x1C, 0x00, 0x00, 0xC1, 0x00, 0xFB, 0x00, 0xA6, 0x01, 0x00, 0x00, 0x01, 0x00, 0xFF, 0xFF, 0xEE, 0x3D, 0x00, 0x00]),
    ("swptree1", 120, [0xD9, 0x22, 0x00, 0x00, 0x70, 0x00, 0xD3, 0x00, 0xAC, 0x03, 0x00, 0x00, 0x00, 0x00, 0x12, 0x00, 0xCB, 0x32, 0x00, 0x00]),
    ("swptree2", 120, [0xB8, 0x47, 0x00, 0x00, 0x88, 0x00, 0x49, 0x01, 0xAC, 0x03, 0x00, 0x00, 0x00, 0x00, 0x12, 0x00, 0xDC, 0x69, 0x00, 0x00]),
    ("swptree3", 120, [0x62, 0x4C, 0x00, 0x00, 0xD2, 0x00, 0xC1, 0x00, 0xAC, 0x03, 0x00, 0x00, 0x00, 0x00, 0x12, 0x00, 0x89, 0x7A, 0x00, 0x00]),
    ("swptree4", 120, [0x1E, 0x44, 0x00, 0x00, 0x19, 0x01, 0x47, 0x01, 0xAC, 0x03, 0x00, 0x00, 0x00, 0x00, 0x12, 0x00, 0xA3, 0xD5, 0x00, 0x00]),
];

/// Apply the table above (RSLodEdit.SpritePaletteFixup).
pub fn fix_sprite_palette(name: &str, header_bytes: &[u8], palette: i16) -> i16 {
    for (fix_name, fix_palette, fingerprint) in PALETTE_FIXUPS {
        if fix_name.eq_ignore_ascii_case(name) && header_bytes == fingerprint {
            return *fix_palette;
        }
    }
    palette
}

// -------------------------------------------------------------- Heroes PCX --

/// RSLod.pas TRSLod.UnpackPcx. Nothing to do with the real PCX format: a
/// 12-byte header, raw pixels, and a palette when the pixels are 8-bit.
pub fn decode_pcx(data: &[u8]) -> io::Result<Vec<u8>> {
    let bad = |what: &str| io::Error::new(io::ErrorKind::InvalidData, format!("bad PCX: {}", what));
    if data.len() < 12 {
        return Err(bad("shorter than its header"));
    }
    let i32_at = |o: usize| i32::from_le_bytes([data[o], data[o + 1], data[o + 2], data[o + 3]]);
    let image_size = i32_at(0);
    let width = i32_at(4);
    let height = i32_at(8);
    if width <= 0 || height <= 0 || image_size <= 0 {
        return Err(bad("zero-sized image"));
    }
    let (w, h, len) = (width as usize, height as usize, image_size as usize);
    let bytes_per_pixel = len / (w * h);
    if bytes_per_pixel * w * h != len || !(bytes_per_pixel == 1 || bytes_per_pixel == 3) {
        return Err(bad("pixel data is neither 8-bit nor 24-bit"));
    }
    if 12 + len > data.len() {
        return Err(bad("truncated pixel data"));
    }
    let pixels = &data[12..12 + len];
    if bytes_per_pixel == 1 {
        if 12 + len + 768 > data.len() {
            return Err(bad("missing palette"));
        }
        let rows = (0..h).map(|y| pixels[y * w..(y + 1) * w].to_vec()).collect();
        Ok(write_bmp8(&Indexed {
            width: w as u32,
            height: h as u32,
            rows,
            palette: data[12 + len..12 + len + 768].to_vec(),
        }))
    } else {
        let rows = (0..h).map(|y| pixels[y * w * 3..(y + 1) * w * 3].to_vec()).collect::<Vec<_>>();
        Ok(write_bmp24(w as u32, h as u32, &rows))
    }
}


// ------------------------------------------------------------ packing ------

/// Windows' GetNearestPaletteIndex: the entry closest in RGB space, ties going
/// to the lower index.
pub fn nearest_palette_index(palette: &[u8], r: u8, g: u8, b: u8) -> u8 {
    let (mut best, mut best_d) = (0usize, i32::MAX);
    for i in 0..256 {
        let dr = palette[i * 3] as i32 - r as i32;
        let dg = palette[i * 3 + 1] as i32 - g as i32;
        let db = palette[i * 3 + 2] as i32 - b as i32;
        let d = dr * dr + dg * dg + db * db;
        if d < best_d {
            best_d = d;
            best = i;
            if d == 0 {
                break;
            }
        }
    }
    best as u8
}

/// RSLod.pas MixCl: average four colours channel by channel, with rounding,
/// all three bytes at once.
fn mix_colours(c: [u32; 4]) -> u32 {
    let high: u32 = c.iter().map(|v| v & 0x00FC_FCFC).fold(0, u32::wrapping_add);
    let low: u32 = (c.iter().map(|v| v & 0x0003_0303).fold(0, u32::wrapping_add)
        + 0x0002_0202)
        & 0x000C_0C0C;
    high.wrapping_add(low) >> 2
}

/// RSLod.pas MixClTr: the same average, but only over the pixels that are not
/// the transparent index, and the result is transparent again when fewer than
/// three of the four were opaque.
fn mix_colours_transparent(c: [u32; 4], opaque: [bool; 4]) -> (u32, bool) {
    const MUL: [u32; 5] = [0, 0x100, 0x80, 1, 0x40];
    let (mut a, mut b) = (0u32, 0u32);
    for i in 0..4 {
        if opaque[i] {
            a = a.wrapping_add(c[i] & 0x00FF_00FF);
            b = b.wrapping_add((c[i] & 0x0000_FF00).wrapping_add(0x0100_0000));
        }
    }
    let transparent = b < 0x0300_0000;
    let mut m = MUL[(b >> 24) as usize];
    if m == 1 {
        // three of four: multiply by 0x55 to divide by three
        a = a.wrapping_mul(0x55).wrapping_add((a >> 2) & 0x01FF_01FF);
        b = b.wrapping_mul(0x55).wrapping_add((b >> 2) & 0x0001_FF00);
        m = 1;
    }
    let mixed = ((a.wrapping_mul(m).wrapping_add(0x0080_0080) & 0xFF00_FF00)
        .wrapping_add(b.wrapping_mul(m).wrapping_add(0x0000_8000) & 0x00FF_0000))
        >> 8;
    (mixed, transparent)
}

/// Whether palette entry 0 is one of the colours MM treats as transparent
/// (RSLod.pas FillBitmapZooms / DoPackBitmap look for exactly these).
pub fn palette_is_transparent(palette: &[u8]) -> bool {
    let c = ((palette[0] as u32) << 16) | ((palette[1] as u32) << 8) | palette[2] as u32;
    matches!(c, 0x00FF_FF00 | 0x00FF_00FF | 0x00FC_00FC | 0x00FC_FC00)
}

/// Build the three half-size levels MM textures carry after the base image
/// (RSLod.pas DoFillBitmapZooms). Returns them concatenated, in order.
///
/// The averaging is a faithful port; picking the palette entry for a mixed
/// colour is not something Windows documents exactly, so a level may differ
/// from MMArchive's by a rounding tie. The base image and every header field
/// are exact.
pub fn build_mipmaps(rows: &[Vec<u8>], width: usize, height: usize, palette: &[u8]) -> Vec<u8> {
    let transparent = palette_is_transparent(palette);
    let colour_of = |index: u8| -> u32 {
        let i = index as usize * 3;
        ((palette[i] as u32) << 16) | ((palette[i + 1] as u32) << 8) | palette[i + 2] as u32
    };

    // Work on colours, halving each time; `indices` keeps the transparency mask.
    let mut w = width;
    let mut h = height;
    let mut colours: Vec<u32> = rows
        .iter()
        .flat_map(|r| r.iter().map(|&i| colour_of(i)))
        .collect();
    let mut indices: Vec<u8> = rows.iter().flat_map(|r| r.iter().copied()).collect();

    let mut out = Vec::new();
    for _ in 0..3 {
        let (nw, nh) = (w / 2, h / 2);
        let mut next_colours = Vec::with_capacity(nw * nh);
        let mut next_indices = Vec::with_capacity(nw * nh);
        for y in 0..nh {
            for x in 0..nw {
                let at = [
                    (y * 2) * w + x * 2,
                    (y * 2) * w + x * 2 + 1,
                    (y * 2 + 1) * w + x * 2,
                    (y * 2 + 1) * w + x * 2 + 1,
                ];
                let c = [colours[at[0]], colours[at[1]], colours[at[2]], colours[at[3]]];
                let (mixed, index) = if transparent {
                    let opaque = [
                        indices[at[0]] != 0,
                        indices[at[1]] != 0,
                        indices[at[2]] != 0,
                        indices[at[3]] != 0,
                    ];
                    let (mixed, is_transparent) = mix_colours_transparent(c, opaque);
                    if is_transparent {
                        (mixed, 0u8)
                    } else {
                        (mixed, nearest_of(palette, mixed))
                    }
                } else {
                    let mixed = mix_colours(c);
                    (mixed, nearest_of(palette, mixed))
                };
                next_colours.push(mixed);
                next_indices.push(index);
                out.push(index);
            }
        }
        colours = next_colours;
        indices = next_indices;
        w = nw;
        h = nh;
        if w == 0 || h == 0 {
            break;
        }
    }
    out
}

fn nearest_of(palette: &[u8], colour: u32) -> u8 {
    nearest_palette_index(
        palette,
        ((colour >> 16) & 0xFF) as u8,
        ((colour >> 8) & 0xFF) as u8,
        (colour & 0xFF) as u8,
    )
}

/// The size of the pixel buffer a mipmapped texture needs
/// (RSLod.pas DoPackBitmap).
pub fn mipmapped_buffer_size(base: usize) -> usize {
    base + ((base / 4 + base) / 4 + base) / 4
}

/// RSLod.pas TRSLod.PackSprite: turn palette indices into the per-row spans a
/// sprites.lod entry stores. Returns (spans, line records, y_skip).
pub fn pack_sprite_rows(rows: &[Vec<u8>], width: usize) -> (Vec<u8>, Vec<[u8; 8]>, i16) {
    let mut spans: Vec<u8> = Vec::new();
    let mut lines: Vec<[u8; 8]> = Vec::with_capacity(rows.len());
    let mut y_skip = rows.len() as i16;
    for (y, row) in rows.iter().enumerate() {
        // trailing then leading transparent pixels
        let last = (0..width).rev().find(|&x| row[x] != 0);
        let mut rec = [0u8; 8];
        match last {
            Some(a2) => {
                y_skip = (rows.len() - y - 1) as i16;
                let a1 = (0..width).find(|&x| row[x] != 0).unwrap_or(0);
                rec[0..2].copy_from_slice(&(a1 as i16).to_le_bytes());
                rec[2..4].copy_from_slice(&(a2 as i16).to_le_bytes());
                rec[4..8].copy_from_slice(&(spans.len() as i32).to_le_bytes());
                spans.extend_from_slice(&row[a1..=a2]);
            }
            None => {
                rec[0..2].copy_from_slice(&(-1i16).to_le_bytes());
                rec[2..4].copy_from_slice(&(-1i16).to_le_bytes());
            }
        }
        lines.push(rec);
    }
    (spans, lines, y_skip)
}

/// RSLod.pas TRSLod.PackPcx: the Heroes "PCX" wrapper. 8-bit images keep a
/// palette behind the pixels; 24-bit ones do not have one.
pub fn pack_pcx(bmp: &AnyBitmap) -> Vec<u8> {
    let (width, height, bytes_per_pixel, rows, palette): (u32, u32, usize, &Vec<Vec<u8>>, Option<&[u8]>) =
        match bmp {
            AnyBitmap::Indexed(i) => (i.width, i.height, 1, &i.rows, Some(&i.palette)),
            AnyBitmap::Bgr { width, height, rows } => (*width, *height, 3, rows, None),
        };
    let image_size = width as usize * height as usize * bytes_per_pixel;
    let mut out = Vec::with_capacity(12 + image_size + 768);
    out.extend_from_slice(&(image_size as i32).to_le_bytes());
    out.extend_from_slice(&(width as i32).to_le_bytes());
    out.extend_from_slice(&(height as i32).to_le_bytes());
    for row in rows {
        out.extend_from_slice(row);
    }
    if let Some(p) = palette {
        out.extend_from_slice(&p[..768]);
    }
    out
}
