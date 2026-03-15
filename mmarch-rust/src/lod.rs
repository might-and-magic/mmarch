use crate::archive::{Archive, ArchiveEntry, ArchiveKind};
use miniz_oxide::deflate::compress_to_vec_zlib;
use miniz_oxide::inflate::decompress_to_vec_zlib;
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom, Write};

/// LOD archive (Heroes III and MM6/7/8 formats).
pub struct LodArchive {
    pub path: String,
    pub kind: ArchiveKind,
    pub entries: Vec<ArchiveEntry>,
    // Header fields needed for writing
    pub header_data: Vec<u8>, // raw header bytes for faithful rewrite
    pub data_start: u64,
    pub item_size: u32, // 32 for most, 76 for MM8
}

// Heroes III LOD header: 92 bytes
const H3_HEADER_SIZE: u64 = 92;
const H3_ENTRY_SIZE: u32 = 32;
const H3_NAME_SIZE: usize = 16;

// MM6+ LOD header: 288 bytes (0x120)
const MM_HEADER_SIZE: u64 = 288;
const MM_ENTRY_SIZE: u32 = 32;
const MM8_ENTRY_SIZE: u32 = 76;
const MM_NAME_SIZE: usize = 16;
const MM8_NAME_SIZE: usize = 64;

// TMMLodFile header size (used in bitmaps/icons/MM8 LODs)
const TMMLODFILE_HEADER_SIZE: usize = 32;
// Name repeated before TMMLodFile header
const TMMLODFILE_NAME_SIZE: usize = 16;

// Games LOD header sizes
const GAMES_HEADER_SIZE: usize = 8;
const GAMES7_HEADER_SIZE: usize = 16;
const GAMES7_SIG1: u32 = 0x16741;
const GAMES7_SIG2: u32 = 0x6969766D;

fn read_i16_le(data: &[u8], offset: usize) -> i16 {
    i16::from_le_bytes([data[offset], data[offset + 1]])
}

fn read_i32_le(data: &[u8], offset: usize) -> i32 {
    i32::from_le_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]])
}

fn read_u16_le(data: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([data[offset], data[offset + 1]])
}

fn read_u32_le(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]])
}

fn write_u32_le(data: &mut [u8], offset: usize, val: u32) {
    let bytes = val.to_le_bytes();
    data[offset..offset + 4].copy_from_slice(&bytes);
}

fn write_i32_le(data: &mut [u8], offset: usize, val: i32) {
    let bytes = val.to_le_bytes();
    data[offset..offset + 4].copy_from_slice(&bytes);
}

fn write_u16_le(data: &mut [u8], offset: usize, val: u16) {
    let bytes = val.to_le_bytes();
    data[offset..offset + 2].copy_from_slice(&bytes);
}

fn write_i16_le(data: &mut [u8], offset: usize, val: i16) {
    let bytes = val.to_le_bytes();
    data[offset..offset + 2].copy_from_slice(&bytes);
}

fn read_fixed_string(data: &[u8], offset: usize, max_len: usize) -> String {
    let slice = &data[offset..offset + max_len];
    let end = slice.iter().position(|&b| b == 0).unwrap_or(max_len);
    String::from_utf8_lossy(&slice[..end]).to_string()
}

fn write_fixed_string(data: &mut [u8], offset: usize, max_len: usize, s: &str) {
    let bytes = s.as_bytes();
    let copy_len = bytes.len().min(max_len - 1);
    data[offset..offset + max_len].fill(0);
    data[offset..offset + copy_len].copy_from_slice(&bytes[..copy_len]);
}

/// Check if a kind uses TMMLodFile headers (bitmaps/icons/MM8).
fn kind_has_tmmlodfile_header(kind: ArchiveKind) -> bool {
    matches!(
        kind,
        ArchiveKind::LodBitmaps | ArchiveKind::LodIcons | ArchiveKind::LodMM8
    )
}

/// Check if a kind is a games/chapter LOD.
fn kind_is_games_lod(kind: ArchiveKind) -> bool {
    matches!(
        kind,
        ArchiveKind::LodGames
            | ArchiveKind::LodGames7
            | ArchiveKind::LodChapter
            | ArchiveKind::LodChapter7
    )
}

/// Check if a kind uses the MM7+ games header (16-byte with signatures).
fn kind_is_games7(kind: ArchiveKind) -> bool {
    matches!(kind, ArchiveKind::LodGames7 | ArchiveKind::LodChapter7)
}

/// Check if filename is a games-LOD compressed type (.blv, .odm, .dlv, .ddm).
fn is_games_compressed_ext(name: &str) -> bool {
    let ext = crate::path_utils::get_file_ext(name).to_lowercase();
    matches!(ext.as_str(), ".blv" | ".odm" | ".dlv" | ".ddm")
}

impl LodArchive {
    pub fn load(path: &str) -> io::Result<Self> {
        let data = fs::read(path)?;
        if data.len() < 8 {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "File too small"));
        }

        // Check signature
        if &data[0..4] != b"LOD\0" {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "Not a LOD file"));
        }

        let version_u32 = read_u32_le(&data, 4);

        if version_u32 <= 0xFFFF {
            // Heroes III format
            Self::load_h3(path, &data)
        } else {
            // MM6+ format
            Self::load_mm(path, &data)
        }
    }

    fn load_h3(path: &str, data: &[u8]) -> io::Result<Self> {
        if data.len() < H3_HEADER_SIZE as usize {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "H3 LOD header too small"));
        }
        let count = read_u32_le(data, 8) as usize;
        let header_data = data[..H3_HEADER_SIZE as usize].to_vec();
        let data_start = H3_HEADER_SIZE + (count as u64) * (H3_ENTRY_SIZE as u64);

        let mut entries = Vec::with_capacity(count);
        for i in 0..count {
            let entry_offset = H3_HEADER_SIZE as usize + i * H3_ENTRY_SIZE as usize;
            if entry_offset + H3_ENTRY_SIZE as usize > data.len() {
                break;
            }
            let name = read_fixed_string(data, entry_offset, H3_NAME_SIZE);
            let addr = read_u32_le(data, entry_offset + 16);
            let unpacked = read_u32_le(data, entry_offset + 20);
            // H3 entry layout: name[16], offset(4), unpackedSize(4), _unk(4), packedSize(4)
            // offset+16 = address, offset+20 = unpackedSize, offset+24 = _unk, offset+28 = packedSize
            let packed_size = read_u32_le(data, entry_offset + 28);

            // If packedSize != 0, data is compressed and packedSize is the stored size
            // If packedSize == 0, stored size = unpackedSize and data is not compressed
            let (size, unpacked_size) = if packed_size != 0 {
                (packed_size, unpacked)
            } else {
                (unpacked, 0)
            };

            entries.push(ArchiveEntry {
                name,
                offset: addr as u64,
                size,
                unpacked_size,
                data: None,
                original_data: None,
            });
        }

        Ok(LodArchive {
            path: path.to_string(),
            kind: ArchiveKind::LodHeroes,
            entries,
            header_data,
            data_start,
            item_size: H3_ENTRY_SIZE,
        })
    }

    fn load_mm(path: &str, data: &[u8]) -> io::Result<Self> {
        if data.len() < MM_HEADER_SIZE as usize {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "MM LOD header too small"));
        }

        let version_str = read_fixed_string(data, 4, 80);
        let lod_type = read_fixed_string(data, 256, 16).to_lowercase();
        let archive_start = read_u32_le(data, 272) as u64;
        let count = read_u16_le(data, 284) as usize;

        // Determine kind
        let mut kind = detect_mm_kind(&version_str, &lod_type);

        // For games/chapter LODs, check first entry's data for Games7 signatures
        // The Delphi tool writes "GameMMVI" header but uses Games7 (16-byte) data format
        if (kind == ArchiveKind::LodGames || kind == ArchiveKind::LodChapter) && count > 0 {
            let first_entry_offset = archive_start as usize;
            if first_entry_offset + 32 <= data.len() {
                let first_addr = read_u32_le(data, first_entry_offset + 16);
                let first_abs = archive_start as usize + first_addr as usize;
                if first_abs + 8 <= data.len() {
                    let sig1 = read_u32_le(data, first_abs);
                    let sig2 = read_u32_le(data, first_abs + 4);
                    if sig1 == GAMES7_SIG1 && sig2 == GAMES7_SIG2 {
                        kind = if kind == ArchiveKind::LodGames {
                            ArchiveKind::LodGames7
                        } else {
                            ArchiveKind::LodChapter7
                        };
                    }
                }
            }
        }

        let (item_size, name_size) = if kind == ArchiveKind::LodMM8 {
            (MM8_ENTRY_SIZE, MM8_NAME_SIZE)
        } else {
            (MM_ENTRY_SIZE, MM_NAME_SIZE)
        };

        let header_data = data[..MM_HEADER_SIZE as usize].to_vec();
        let data_start = archive_start + (count as u64) * (item_size as u64);
        let file_size = data.len() as u64;

        // For bitmaps/icons/MM8 LODs, the entry table does NOT contain a reliable
        // size field. The "size" field in the entry is actually meaningless for these
        // formats. The real stored size must be computed from address gaps.
        // For games/chapter LODs, the size field at offset name_size+4 is also not
        // the stored size — unpacked_size is at name_size+8. The stored data includes
        // a header before the compressed payload.
        //
        // We read addresses first, then compute sizes from gaps.

        struct RawEntry {
            name: String,
            addr: u64,       // absolute address in file
            entry_size: u32, // the "size" field from the entry table
            unpacked: u32,   // the "unpacked_size" field from the entry table
        }

        let mut raw_entries: Vec<RawEntry> = Vec::with_capacity(count);
        for i in 0..count {
            let entry_offset = archive_start as usize + i * item_size as usize;
            if entry_offset + item_size as usize > data.len() {
                break;
            }
            let name = read_fixed_string(data, entry_offset, name_size);
            let addr_off = name_size;
            let addr = read_u32_le(data, entry_offset + addr_off);
            let entry_size_field = read_u32_le(data, entry_offset + addr_off + 4);
            let unpacked = read_u32_le(data, entry_offset + addr_off + 8);

            raw_entries.push(RawEntry {
                name,
                addr: archive_start + addr as u64,
                entry_size: entry_size_field,
                unpacked,
            });
        }

        let mut entries = Vec::with_capacity(raw_entries.len());

        if kind_has_tmmlodfile_header(kind) {
            // For bitmaps/icons/MM8: compute size from address gaps
            for i in 0..raw_entries.len() {
                let re = &raw_entries[i];
                let next_addr = if i + 1 < raw_entries.len() {
                    raw_entries[i + 1].addr
                } else {
                    file_size
                };
                let stored_size = (next_addr - re.addr) as u32;
                entries.push(ArchiveEntry {
                    name: re.name.clone(),
                    offset: re.addr,
                    size: stored_size,
                    unpacked_size: re.unpacked,
                    data: None,
                original_data: None,
                });
            }
        } else if kind_is_games_lod(kind) {
            // For games LODs: compute size from address gaps too, since the
            // entry's size field is not the stored size for compressed files
            for i in 0..raw_entries.len() {
                let re = &raw_entries[i];
                let next_addr = if i + 1 < raw_entries.len() {
                    raw_entries[i + 1].addr
                } else {
                    file_size
                };
                let stored_size = (next_addr - re.addr) as u32;
                entries.push(ArchiveEntry {
                    name: re.name.clone(),
                    offset: re.addr,
                    size: stored_size,
                    unpacked_size: re.unpacked,
                    data: None,
                original_data: None,
                });
            }
        } else {
            // Sprites or unknown — use entry fields directly
            for re in &raw_entries {
                entries.push(ArchiveEntry {
                    name: re.name.clone(),
                    offset: re.addr,
                    size: re.entry_size,
                    unpacked_size: re.unpacked,
                    data: None,
                original_data: None,
                });
            }
        }

        Ok(LodArchive {
            path: path.to_string(),
            kind,
            entries,
            header_data,
            data_start,
            item_size,
        })
    }

    fn name_size(&self) -> usize {
        if self.item_size == MM8_ENTRY_SIZE {
            MM8_NAME_SIZE
        } else {
            if self.kind == ArchiveKind::LodHeroes {
                H3_NAME_SIZE
            } else {
                MM_NAME_SIZE
            }
        }
    }
}

fn detect_mm_kind(version_str: &str, lod_type: &str) -> ArchiveKind {
    let ver = version_str.to_uppercase();
    let lt = lod_type.to_lowercase();

    if ver.contains("MMVIII") {
        return ArchiveKind::LodMM8;
    }

    if lt.starts_with("maps") {
        // Check for MM7 games LOD: version contains "GAMEMMVII" or just "MMVII"
        if ver.contains("GAMEMMVII") || ver.contains("GAMEMMVIII") {
            return ArchiveKind::LodGames7;
        }
        if ver.contains("MMVII") && !ver.contains("MMVIII") {
            return ArchiveKind::LodGames7;
        }
        if ver.contains("GAMEMMVI") {
            return ArchiveKind::LodGames;
        }
        if ver.contains("MMVI") && !ver.contains("MMVII") {
            return ArchiveKind::LodGames;
        }
        // Fallback for maps: if version has VII, use Games7
        if ver.contains("VII") && !ver.contains("VIII") {
            return ArchiveKind::LodGames7;
        }
        return ArchiveKind::LodGames;
    }

    if lt.starts_with("chapter") {
        if ver.contains("VII") || ver.contains("VIII") {
            return ArchiveKind::LodChapter7;
        }
        return ArchiveKind::LodChapter;
    }

    if lt.starts_with("bitmaps") {
        return ArchiveKind::LodBitmaps;
    }
    if lt.starts_with("icons") {
        return ArchiveKind::LodIcons;
    }
    if lt.starts_with("sprites") {
        return ArchiveKind::LodSprites;
    }

    // Default to games
    ArchiveKind::LodGames
}

/// Read raw bytes from the archive file for a given entry.
fn read_raw_entry(path: &str, entry: &ArchiveEntry) -> io::Result<Vec<u8>> {
    let mut f = File::open(path)?;
    f.seek(SeekFrom::Start(entry.offset))?;
    let mut buf = vec![0u8; entry.size as usize];
    f.read_exact(&mut buf)?;
    Ok(buf)
}

/// Extract file data from a bitmaps/icons/MM8 LOD entry.
/// The stored format is: 16-byte name + 32-byte TMMLodFile header + compressed data.
/// For non-BMP files (BmpSize==0): decompress DataSize bytes using zlib.
/// For BMP files (BmpSize!=0): return raw pixel data + palette (treated as raw).
fn extract_tmmlodfile(raw: &[u8]) -> io::Result<Vec<u8>> {
    if raw.len() < TMMLODFILE_NAME_SIZE + TMMLODFILE_HEADER_SIZE {
        // Too small for header — return as-is
        return Ok(raw.to_vec());
    }

    let hdr_start = TMMLODFILE_NAME_SIZE;
    let bmp_size = read_i32_le(raw, hdr_start);
    let data_size = read_i32_le(raw, hdr_start + 4) as usize;
    let unp_size = read_i32_le(raw, hdr_start + 24) as usize;

    let payload_start = TMMLODFILE_NAME_SIZE + TMMLODFILE_HEADER_SIZE;

    if bmp_size == 0 {
        // Non-image file
        if data_size == 0 {
            // Palette file (.act) — 768 bytes after header
            if raw.len() >= payload_start + 768 {
                return Ok(raw[payload_start..payload_start + 768].to_vec());
            }
            // Just return whatever is after the header
            return Ok(raw[payload_start..].to_vec());
        }
        // Compressed non-image data
        if payload_start + data_size > raw.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "TMMLodFile data_size exceeds available data",
            ));
        }
        let compressed = &raw[payload_start..payload_start + data_size];
        if unp_size > 0 && data_size != unp_size {
            // Decompress
            zlib_decompress(compressed)
        } else {
            // Not actually compressed
            Ok(compressed.to_vec())
        }
    } else {
        // Bitmap image: pixel data (possibly compressed) + 768-byte palette
        // For simplicity, we store/extract BMP files the same as non-BMP:
        // the raw payload after header is DataSize bytes of pixel data + 768 palette.
        // We return the compressed pixel data decompressed + palette appended.
        if data_size == 0 {
            return Ok(raw[payload_start..].to_vec());
        }
        if payload_start + data_size > raw.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "TMMLodFile bitmap data_size exceeds available data",
            ));
        }
        let pixel_data_compressed = &raw[payload_start..payload_start + data_size];
        let palette_start = payload_start + data_size;

        let pixel_data = if unp_size > 0 && data_size != unp_size as usize {
            zlib_decompress(pixel_data_compressed)?
        } else {
            pixel_data_compressed.to_vec()
        };

        // Append palette if present
        if palette_start + 768 <= raw.len() {
            let palette = &raw[palette_start..palette_start + 768];
            let mut result = pixel_data;
            result.extend_from_slice(palette);
            Ok(result)
        } else {
            Ok(pixel_data)
        }
    }
}

/// Extract file data from a games/chapter LOD entry.
/// For .blv/.odm/.dlv/.ddm: has 8 or 16-byte header before compressed data.
/// For other files: stored as-is.
fn extract_games_lod(raw: &[u8], kind: ArchiveKind, name: &str) -> io::Result<Vec<u8>> {
    if !is_games_compressed_ext(name) {
        // Not a compressed type — return as-is
        return Ok(raw.to_vec());
    }

    let is7 = kind_is_games7(kind);
    let hdr_size = if is7 { GAMES7_HEADER_SIZE } else { GAMES_HEADER_SIZE };

    if raw.len() < hdr_size {
        return Ok(raw.to_vec());
    }

    let (data_size, unpacked_size) = if is7 {
        // Verify signatures
        let sig1 = read_u32_le(raw, 0);
        let sig2 = read_u32_le(raw, 4);
        if sig1 != GAMES7_SIG1 || sig2 != GAMES7_SIG2 {
            // Signatures don't match — treat as raw
            return Ok(raw.to_vec());
        }
        (read_u32_le(raw, 8) as usize, read_u32_le(raw, 12) as usize)
    } else {
        (read_u32_le(raw, 0) as usize, read_u32_le(raw, 4) as usize)
    };

    if data_size == 0 || unpacked_size == 0 {
        // Uncompressed — data after header is the file
        // (UnpackedSize=0 means not compressed in Delphi convention)
        let end = if data_size > 0 && hdr_size + data_size <= raw.len() {
            hdr_size + data_size
        } else {
            raw.len()
        };
        return Ok(raw[hdr_size..end].to_vec());
    }

    if hdr_size + data_size > raw.len() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Games LOD compressed data_size exceeds available data",
        ));
    }

    let compressed = &raw[hdr_size..hdr_size + data_size];
    let decompressed = zlib_decompress(compressed)?;

    if unpacked_size > 0 && decompressed.len() != unpacked_size {
        // Size mismatch but we still return what we got
    }

    Ok(decompressed)
}

/// Build the stored blob for a file being added to a bitmaps/icons/MM8 LOD.
/// Wraps the file data with 16-byte name + 32-byte TMMLodFile header.
/// All files are stored as non-BMP (BmpSize=0) with zlib compression.
fn wrap_tmmlodfile(name: &str, file_data: &[u8]) -> io::Result<Vec<u8>> {
    let compressed = zlib_compress(file_data)?;
    let (stored_payload, _data_size, unp_size) = if compressed.len() < file_data.len() {
        (compressed, file_data.len(), file_data.len())
    } else {
        // Compression didn't help — store uncompressed
        // When not compressed: DataSize = data length, UnpSize = 0 (signals not compressed)
        (file_data.to_vec(), file_data.len(), 0usize)
    };

    let total = TMMLODFILE_NAME_SIZE + TMMLODFILE_HEADER_SIZE + stored_payload.len();
    let mut buf = vec![0u8; total];

    // Write 16-byte name
    write_fixed_string(&mut buf, 0, TMMLODFILE_NAME_SIZE, name);

    // Write TMMLodFile header (32 bytes) at offset 16
    let h = TMMLODFILE_NAME_SIZE;
    // BmpSize = 0 (not a bitmap)
    write_i32_le(&mut buf, h, 0);
    // DataSize = compressed size (or raw size if not compressed)
    write_i32_le(&mut buf, h + 4, stored_payload.len() as i32);
    // BmpWidth, BmpHeight, etc. all 0
    write_i16_le(&mut buf, h + 8, 0);
    write_i16_le(&mut buf, h + 10, 0);
    write_i16_le(&mut buf, h + 12, 0);
    write_i16_le(&mut buf, h + 14, 0);
    write_i16_le(&mut buf, h + 16, 0);
    write_i16_le(&mut buf, h + 18, 0);
    write_i16_le(&mut buf, h + 20, 0); // Palette
    write_i16_le(&mut buf, h + 22, 0); // _unk
    // UnpSize = original size (0 if not compressed)
    write_i32_le(&mut buf, h + 24, unp_size as i32);
    // Bits = 0
    write_i32_le(&mut buf, h + 28, 0);

    // Write payload
    buf[TMMLODFILE_NAME_SIZE + TMMLODFILE_HEADER_SIZE..].copy_from_slice(&stored_payload);

    Ok(buf)
}

/// Build the stored blob for a file being added to a games/chapter LOD.
/// For .blv/.odm/.dlv/.ddm: compress with zlib and prepend 8 or 16-byte header.
/// For other files: store as-is.
fn wrap_games_lod(file_data: &[u8], kind: ArchiveKind, name: &str) -> io::Result<(Vec<u8>, u32)> {
    if !is_games_compressed_ext(name) {
        // Not a compressed type — store raw, entry unpacked_size = 0
        return Ok((file_data.to_vec(), 0));
    }

    let compressed = zlib_compress(file_data)?;
    let is7 = kind_is_games7(kind);
    let hdr_size = if is7 { GAMES7_HEADER_SIZE } else { GAMES_HEADER_SIZE };

    // Entry's UnpackedSize must be 0 for games LODs (compression info is in data header)
    // This matches Delphi behavior where IsPacked = (Size != UnpackedSize)
    // and we want IsPacked to be determined by the data header, not the entry.

    let (payload, _data_size_val) = if compressed.len() < file_data.len() {
        (compressed, file_data.len())
    } else {
        // Compression didn't help — store uncompressed
        // Convention: DataSize = actual size, UnpackedSize = 0 (signals not compressed)
        let mut buf = vec![0u8; hdr_size + file_data.len()];
        if is7 {
            write_u32_le(&mut buf, 0, GAMES7_SIG1);
            write_u32_le(&mut buf, 4, GAMES7_SIG2);
            write_u32_le(&mut buf, 8, file_data.len() as u32); // DataSize = actual size
            write_u32_le(&mut buf, 12, 0); // UnpackedSize = 0 (not compressed)
        } else {
            write_u32_le(&mut buf, 0, file_data.len() as u32); // DataSize = actual size
            write_u32_le(&mut buf, 4, 0); // UnpackedSize = 0 (not compressed)
        }
        buf[hdr_size..].copy_from_slice(file_data);
        return Ok((buf, 0));
    };

    let mut buf = vec![0u8; hdr_size + payload.len()];
    if is7 {
        write_u32_le(&mut buf, 0, GAMES7_SIG1);
        write_u32_le(&mut buf, 4, GAMES7_SIG2);
        write_u32_le(&mut buf, 8, payload.len() as u32);
        write_u32_le(&mut buf, 12, file_data.len() as u32);
    } else {
        write_u32_le(&mut buf, 0, payload.len() as u32);
        write_u32_le(&mut buf, 4, file_data.len() as u32);
    }
    buf[hdr_size..].copy_from_slice(&payload);

    Ok((buf, 0))
}

impl Archive for LodArchive {
    fn kind(&self) -> ArchiveKind {
        self.kind
    }

    fn file_path(&self) -> &str {
        &self.path
    }

    fn entries(&self) -> &[ArchiveEntry] {
        &self.entries
    }

    fn entries_mut(&mut self) -> &mut Vec<ArchiveEntry> {
        &mut self.entries
    }

    fn read_entry_data(&self, index: usize) -> io::Result<Vec<u8>> {
        let entry = &self.entries[index];

        // If this entry has original (unwrapped) data, return it
        if let Some(ref orig) = entry.original_data {
            return Ok(orig.clone());
        }

        // If this entry has in-memory stored data but no original_data,
        // it's already the final file content (e.g. from merge)
        if let Some(ref data) = entry.data {
            return Ok(data.clone());
        }

        // Read raw stored bytes from the archive file
        let raw = read_raw_entry(&self.path, entry)?;

        // Format-specific extraction
        if kind_has_tmmlodfile_header(self.kind) {
            extract_tmmlodfile(&raw)
        } else if kind_is_games_lod(self.kind) {
            extract_games_lod(&raw, self.kind, &entry.name)
        } else if self.kind == ArchiveKind::LodHeroes {
            // H3: if packed, decompress
            if entry.is_packed() {
                zlib_decompress(&raw)
            } else {
                Ok(raw)
            }
        } else {
            // Sprites or other: simple decompress if packed
            if entry.is_packed() {
                zlib_decompress(&raw)
            } else {
                Ok(raw)
            }
        }
    }

    fn rebuild(&mut self) -> io::Result<()> {
        let tmp_path = format!("{}.tmp", self.path);
        {
            let mut src = File::open(&self.path).ok();
            let mut out = File::create(&tmp_path)?;

            let count = self.entries.len();

            if self.kind == ArchiveKind::LodHeroes {
                // Write H3 header
                let mut header = self.header_data.clone();
                if header.len() < H3_HEADER_SIZE as usize {
                    header.resize(H3_HEADER_SIZE as usize, 0);
                }
                write_u32_le(&mut header, 8, count as u32);
                out.write_all(&header)?;

                // Calculate data start
                let new_data_start = H3_HEADER_SIZE + (count as u64) * (H3_ENTRY_SIZE as u64);

                // Write entries and data
                let mut current_addr: u32 = 0;
                let mut entry_table = vec![0u8; count * H3_ENTRY_SIZE as usize];
                let mut data_buf = Vec::new();

                for (i, entry) in self.entries.iter().enumerate() {
                    let eoff = i * H3_ENTRY_SIZE as usize;
                    write_fixed_string(&mut entry_table, eoff, H3_NAME_SIZE, &entry.name);
                    // H3: stored address is absolute file offset
                    write_u32_le(&mut entry_table, eoff + 16, (new_data_start as u32) + current_addr);

                    // Read data
                    let file_data = if let Some(ref d) = entry.data {
                        d.clone()
                    } else if let Some(ref mut f) = src {
                        f.seek(SeekFrom::Start(entry.offset))?;
                        let mut buf = vec![0u8; entry.size as usize];
                        f.read_exact(&mut buf)?;
                        buf
                    } else {
                        vec![0u8; entry.size as usize]
                    };

                    // H3 entry: name[16], address(4), unpackedSize(4), _unk(4), packedSize(4)
                    // For packed: unpackedSize = unpacked, packedSize = stored size
                    // For unpacked: unpackedSize = size, packedSize = 0
                    if entry.is_packed() {
                        write_u32_le(&mut entry_table, eoff + 20, entry.unpacked_size);
                        write_u32_le(&mut entry_table, eoff + 24, 0); // _unk
                        write_u32_le(&mut entry_table, eoff + 28, file_data.len() as u32);
                    } else {
                        write_u32_le(&mut entry_table, eoff + 20, file_data.len() as u32);
                        write_u32_le(&mut entry_table, eoff + 24, 0); // _unk
                        write_u32_le(&mut entry_table, eoff + 28, 0); // packedSize = 0
                    }

                    data_buf.extend_from_slice(&file_data);
                    current_addr += file_data.len() as u32;
                }

                out.write_all(&entry_table)?;
                out.write_all(&data_buf)?;

                self.data_start = new_data_start;

                // Update entries with new sizes and offsets
                let mut offset = new_data_start;
                let mut new_entries = Vec::with_capacity(self.entries.len());
                for entry in &self.entries {
                    let raw_size = if let Some(ref d) = entry.data {
                        d.len() as u32
                    } else {
                        entry.size
                    };
                    new_entries.push(ArchiveEntry {
                        name: entry.name.clone(),
                        offset,
                        size: raw_size,
                        unpacked_size: entry.unpacked_size,
                        data: None,
                original_data: None,
                    });
                    offset += raw_size as u64;
                }
                self.entries = new_entries;
            } else {
                // MM6+ format
                let mut header = self.header_data.clone();
                if header.len() < MM_HEADER_SIZE as usize {
                    header.resize(MM_HEADER_SIZE as usize, 0);
                }
                write_u16_le(&mut header, 284, count as u16);

                let archive_start = read_u32_le(&header, 272) as u64;
                let new_data_start = archive_start + (count as u64) * (self.item_size as u64);

                out.write_all(&header)?;

                // Pad if archive_start > header size
                if archive_start > MM_HEADER_SIZE {
                    let pad = vec![0u8; (archive_start - MM_HEADER_SIZE) as usize];
                    out.write_all(&pad)?;
                }

                let name_size = self.name_size();
                let mut current_addr: u32 = 0;
                let mut entry_table = vec![0u8; count * self.item_size as usize];
                let mut data_buf = Vec::new();

                let entries_size = (count as u32) * (self.item_size as u32);

                // Collect stored data and compute sizes
                let mut stored_sizes: Vec<u32> = Vec::with_capacity(count);
                let mut stored_datas: Vec<Vec<u8>> = Vec::with_capacity(count);

                for entry in self.entries.iter() {
                    let file_data = if let Some(ref d) = entry.data {
                        d.clone()
                    } else if let Some(ref mut f) = src {
                        f.seek(SeekFrom::Start(entry.offset))?;
                        let mut buf = vec![0u8; entry.size as usize];
                        f.read_exact(&mut buf)?;
                        buf
                    } else {
                        vec![0u8; entry.size as usize]
                    };
                    stored_sizes.push(file_data.len() as u32);
                    stored_datas.push(file_data);
                }

                for (i, entry) in self.entries.iter().enumerate() {
                    let eoff = i * self.item_size as usize;
                    write_fixed_string(&mut entry_table, eoff, name_size, &entry.name);
                    // MM: stored address relative to archiveStart
                    write_u32_le(&mut entry_table, eoff + name_size, entries_size + current_addr);
                    // For bitmaps/icons/MM8/games: the "size" field in the entry table
                    // is not really used for reading (size comes from address gaps), but
                    // we write it for compatibility. For games LODs with non-compressed
                    // files, we write the actual size. For compressed files, we write 0.
                    // The unpacked_size field is important for games LODs.
                    write_u32_le(&mut entry_table, eoff + name_size + 4, stored_sizes[i]);
                    write_u32_le(&mut entry_table, eoff + name_size + 8, entry.unpacked_size);

                    data_buf.extend_from_slice(&stored_datas[i]);
                    current_addr += stored_sizes[i];
                }

                // Update archive size in header
                let archive_size = (count as u64) * (self.item_size as u64) + data_buf.len() as u64;
                out.seek(SeekFrom::Start(276))?;
                out.write_all(&(archive_size as u32).to_le_bytes())?;

                out.seek(SeekFrom::Start(archive_start))?;
                out.write_all(&entry_table)?;
                out.write_all(&data_buf)?;

                self.data_start = new_data_start;

                // Update offsets
                let mut offset = new_data_start;
                for (i, entry) in self.entries.iter_mut().enumerate() {
                    entry.offset = offset;
                    entry.size = stored_sizes[i];
                    entry.data = None;
                    entry.original_data = None;
                    offset += stored_sizes[i] as u64;
                }
            }
        }

        // Replace original
        fs::remove_file(&self.path)?;
        fs::rename(&tmp_path, &self.path)?;

        Ok(())
    }

    fn create_new(path: &str, kind: ArchiveKind) -> io::Result<Self> {
        let (header_data, data_start, item_size) = match kind {
            ArchiveKind::LodHeroes => {
                let mut h = vec![0u8; H3_HEADER_SIZE as usize];
                h[0..4].copy_from_slice(b"LOD\0");
                write_u32_le(&mut h, 4, 200); // version (typical H3 LOD version)
                write_u32_le(&mut h, 8, 0); // count
                (h, H3_HEADER_SIZE, H3_ENTRY_SIZE)
            }
            _ => {
                let mut h = vec![0u8; MM_HEADER_SIZE as usize];
                h[0..4].copy_from_slice(b"LOD\0");

                let (ver_str, lod_type_str) = kind_to_mm_strings(kind);
                write_fixed_string(&mut h, 4, 80, ver_str);
                write_fixed_string(&mut h, 256, 16, lod_type_str);

                write_u32_le(&mut h, 164, 100); // unk1
                write_u32_le(&mut h, 172, 1);   // archives count
                write_u32_le(&mut h, 272, MM_HEADER_SIZE as u32); // archive start
                write_u16_le(&mut h, 284, 0); // count

                let is = if kind == ArchiveKind::LodMM8 { MM8_ENTRY_SIZE } else { MM_ENTRY_SIZE };
                (h, MM_HEADER_SIZE, is)
            }
        };

        // Write initial empty archive
        let mut f = File::create(path)?;
        f.write_all(&header_data)?;

        Ok(LodArchive {
            path: path.to_string(),
            kind,
            entries: Vec::new(),
            header_data,
            data_start,
            item_size,
        })
    }
}

fn kind_to_mm_strings(kind: ArchiveKind) -> (&'static str, &'static str) {
    match kind {
        ArchiveKind::LodBitmaps => ("MMVI", "bitmaps"),
        ArchiveKind::LodIcons => ("MMVI", "icons"),
        ArchiveKind::LodSprites => ("MMVI", "sprites08"),
        ArchiveKind::LodMM8 => ("MMVIII", "bitmaps"),
        ArchiveKind::LodGames7 => ("GameMMVI", "maps"),
        ArchiveKind::LodGames => ("GameMMVI", "maps"),
        ArchiveKind::LodChapter7 => ("MMVI", "chapter"),
        ArchiveKind::LodChapter => ("MMVI", "chapter"),
        _ => ("MMVI", "bitmaps"),
    }
}

/// Compress data with zlib.
pub fn zlib_compress(data: &[u8]) -> io::Result<Vec<u8>> {
    Ok(compress_to_vec_zlib(data, 6))
}

/// Decompress zlib data.
pub fn zlib_decompress(data: &[u8]) -> io::Result<Vec<u8>> {
    decompress_to_vec_zlib(data)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("zlib decompress error: {:?}", e)))
}

/// Add a file to the LOD archive. The file is read from disk and stored
/// with the appropriate format-specific wrapping.
pub fn lod_add_file(archive: &mut LodArchive, file_path: &str) -> io::Result<()> {
    let file_data = fs::read(file_path)?;
    let file_name = crate::path_utils::get_file_name(file_path);

    // Determine in-archive name
    let ext = crate::path_utils::get_file_ext(&file_name);
    let in_name = if let Some(in_ext) = archive.kind.in_archive_ext(&ext) {
        if in_ext.is_empty() {
            crate::path_utils::get_file_stem(&file_name)
        } else {
            format!("{}{}", crate::path_utils::get_file_stem(&file_name), in_ext)
        }
    } else {
        file_name.clone()
    };

    // Remove existing entry with same name (case-insensitive)
    archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&in_name));

    // Format-specific wrapping
    if kind_has_tmmlodfile_header(archive.kind) {
        // Wrap with TMMLodFile header
        let stored = wrap_tmmlodfile(&in_name, &file_data)?;
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: stored.len() as u32,
            unpacked_size: 0, // not used in entry table for bitmaps/icons
            data: Some(stored),
            original_data: Some(file_data),
        });
    } else if kind_is_games_lod(archive.kind) {
        // Wrap with games header for compressed types
        let (stored, unpacked_size) = wrap_games_lod(&file_data, archive.kind, &in_name)?;
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: stored.len() as u32,
            unpacked_size,
            data: Some(stored),
            original_data: Some(file_data),
        });
    } else if archive.kind == ArchiveKind::LodHeroes {
        // H3: compress if beneficial, store packed data directly
        let compressed = zlib_compress(&file_data)?;
        let (stored_data, unpacked_size) = if compressed.len() < file_data.len() {
            (compressed, file_data.len() as u32)
        } else {
            (file_data.clone(), 0u32)
        };
        let sz = stored_data.len() as u32;
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: sz,
            unpacked_size,
            data: Some(stored_data),
            original_data: Some(file_data),
        });
    } else {
        // Sprites or other: store as-is
        let sz = file_data.len() as u32;
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: sz,
            unpacked_size: 0,
            original_data: Some(file_data.clone()),
            data: Some(file_data),
        });
    }

    Ok(())
}
