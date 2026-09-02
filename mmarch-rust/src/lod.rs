use crate::archive::{Archive, ArchiveEntry, ArchiveKind, TmmClass};
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
    /// What each entry's TMMLodFile header says it is. Worked out in one pass
    /// the first time anyone asks, because both naming and extraction want it
    /// and re-opening the archive per entry costs more than decoding does.
    tmm_classes: OnceLock<Vec<TmmClass>>,
}

// Heroes III LOD header: 92 bytes
const H3_HEADER_SIZE: u64 = 92;
const H3_ENTRY_SIZE: u32 = 32;
const H3_NAME_SIZE: usize = 16;
// RSPak's Options.MinFileSize for Heroes III LODs (RSLod.pas,
// TRSLodBase.InitOptions): 92 + 10000*32, the entry table the shipped H3
// archives reserve. TRSMMFiles.SaveAsNoBlock starts writing file data at
// max(MinFileSize, DataStart + tableSize), so an H3 LOD written by MMArchive
// or the Delphi mmarch always leaves that much room before the first file.
const H3_MIN_FILE_SIZE: u64 = 320092;

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

/// RSPak's `Options.NameSize` (RSLod.pas, TRSLodBase.InitOptions): the name is
/// repeated in front of the TMMLodFile header inside every stored blob, and
/// extraction seeks past exactly this many bytes. MM8 uses $40, everything
/// else $10 — reading an MM8 blob at offset $10 lands in the zero padding of
/// the name, which makes every file look like a 768-byte palette.
fn kind_name_size(kind: ArchiveKind) -> usize {
    if kind == ArchiveKind::LodMM8 {
        MM8_NAME_SIZE
    } else {
        MM_NAME_SIZE
    }
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
                // GetIsPacked for a Heroes LOD: the packed-size field is set.
                packed: packed_size != 0,
            });
        }

        Ok(LodArchive {
            path: path.to_string(),
            kind: ArchiveKind::LodHeroes,
            entries,
            header_data,
            data_start,
            item_size: H3_ENTRY_SIZE,
            tmm_classes: OnceLock::new(),
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

        // For games/chapter LODs, check entries' data for Games7 signatures.
        // The Delphi tool writes "GameMMVI" header but uses Games7 (16-byte) data format.
        // We scan all entries (not just the first) because non-compressed files
        // (e.g. .bin, .txt) are stored without a Games7 header. Only compressed
        // files (.blv, .odm, .dlv, .ddm) have the signature.
        if (kind == ArchiveKind::LodGames || kind == ArchiveKind::LodChapter) && count > 0 {
            let item_sz = if kind == ArchiveKind::LodMM8 { MM8_ENTRY_SIZE } else { MM_ENTRY_SIZE };
            let ns = if kind == ArchiveKind::LodMM8 { MM8_NAME_SIZE } else { MM_NAME_SIZE };
            'sig_scan: for i in 0..count {
                let eo = archive_start as usize + i * item_sz as usize;
                if eo + item_sz as usize > data.len() { break; }
                let entry_name = read_fixed_string(data, eo, ns);
                // Only check entries that have compressed extensions
                if !is_games_compressed_ext(&entry_name) { continue; }
                let addr = read_u32_le(data, eo + ns);
                let abs_addr = archive_start as usize + addr as usize;
                if abs_addr + 8 <= data.len() {
                    let sig1 = read_u32_le(data, abs_addr);
                    let sig2 = read_u32_le(data, abs_addr + 4);
                    if sig1 == GAMES7_SIG1 && sig2 == GAMES7_SIG2 {
                        kind = if kind == ArchiveKind::LodGames {
                            ArchiveKind::LodGames7
                        } else {
                            ArchiveKind::LodChapter7
                        };
                    }
                    break 'sig_scan;
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

        // Entry layout, as RSPak describes it through TRSMMFilesOptions
        // (RSLod.pas, TRSLodBase.InitOptions):
        //
        //     name[NameSize]        NameSize = $40 for MM8, $10 otherwise
        //     addr   at NameSize    relative to ArchiveStart (AddrStart)
        //     size   at NameSize+4  the stored (on-disk) size of the entry
        //     unused at NameSize+8  always 0 in real archives
        //
        // RSPak names that second field `UnpackedSizeOffset` and leaves
        // `SizeOffset` at -1, so TRSMMFiles.GetSize falls through to it: for
        // every MM LOD it IS the stored size, and IsPacked is always false
        // (both PackedSizeOffset and SizeOffset are -1 — see GetIsPacked).
        // Compression is described by the per-entry data header instead
        // (TMMLodFile / TMM6GamesFile), not by the entry table.
        let mut entries: Vec<ArchiveEntry> = Vec::with_capacity(count);
        for i in 0..count {
            let entry_offset = archive_start as usize + i * item_size as usize;
            if entry_offset + item_size as usize > data.len() {
                break;
            }
            let name = read_fixed_string(data, entry_offset, name_size);
            let addr_off = name_size;
            let addr = read_u32_le(data, entry_offset + addr_off);
            let stored_size = read_u32_le(data, entry_offset + addr_off + 4);
            let unpacked = read_u32_le(data, entry_offset + addr_off + 8);

            entries.push(ArchiveEntry {
                name,
                offset: archive_start + addr as u64,
                size: stored_size,
                unpacked_size: unpacked,
                data: None,
                original_data: None,
                packed: false,
            });
        }

        Ok(LodArchive {
            path: path.to_string(),
            kind,
            entries,
            header_data,
            data_start,
            item_size,
            tmm_classes: OnceLock::new(),
        })
    }

    /// Read every entry's TMMLodFile header in one pass over the archive.
    fn read_tmm_classes(&self) -> Vec<TmmClass> {
        let name_size = kind_name_size(self.kind);
        let want = name_size + TMMLODFILE_HEADER_SIZE;
        let mut file = File::open(&self.path).ok();
        let mut head = vec![0u8; want];
        self.entries
            .iter()
            .map(|entry| {
                let bytes: &[u8] = match entry.data.as_deref() {
                    Some(d) => d,
                    None => {
                        let f = match file.as_mut() {
                            Some(f) => f,
                            None => return TmmClass::Plain,
                        };
                        if (entry.size as usize) < want
                            || f.seek(SeekFrom::Start(entry.offset)).is_err()
                            || f.read_exact(&mut head).is_err()
                        {
                            return TmmClass::Plain;
                        }
                        &head
                    }
                };
                if bytes.len() < want {
                    return TmmClass::Plain;
                }
                if read_i32_le(bytes, name_size) != 0 {
                    TmmClass::Bmp
                } else if read_i32_le(bytes, name_size + 4) == 0
                    && entry.size as usize >= 768 + want
                {
                    TmmClass::Act
                } else {
                    TmmClass::Plain
                }
            })
            .collect()
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
    let file_len = f.metadata()?.len();
    // Check before allocating: a corrupt entry table would otherwise reserve
    // its bogus size (up to 4 GB) only to fail the read afterwards.
    if entry.offset > file_len || entry.size as u64 > file_len - entry.offset {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "entry claims {} bytes at offset {}, past the end of the {}-byte archive",
                entry.size, entry.offset, file_len
            ),
        ));
    }
    f.seek(SeekFrom::Start(entry.offset))?;
    let mut buf = vec![0u8; entry.size as usize];
    f.read_exact(&mut buf)?;
    Ok(buf)
}

/// Palettes from the `bitmaps.lod` archives sitting next to `archive_path`.
///
/// RSLod.pas TRSLod.LoadBitmapsLods collects `bitmaps.lod` itself plus every
/// `*.bitmaps.lod` in the same directory, and RSMMArchivesFind then searches
/// them from the last one backwards — so a patch archive's palette wins over
/// the base game's. Sprites need this because their entry only names a
/// palette; the 768 bytes are somewhere else entirely.
fn sibling_palettes(archive_path: &str) -> std::sync::Arc<HashMap<u16, Vec<u8>>> {
    static CACHE: OnceLock<Mutex<HashMap<String, std::sync::Arc<HashMap<u16, Vec<u8>>>>>> =
        OnceLock::new();
    let dir = crate::path_utils::get_file_dir(archive_path);
    let cache = CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    if let Some(hit) = cache.lock().unwrap().get(&dir) {
        return hit.clone();
    }
    // Built outside the lock: loading an archive re-enters this module.
    let built = std::sync::Arc::new(load_sibling_palettes(&dir));
    cache
        .lock()
        .unwrap()
        .entry(dir)
        .or_insert(built)
        .clone()
}

fn load_sibling_palettes(dir: &str) -> HashMap<u16, Vec<u8>> {
    let mut out = HashMap::new();
    let read_dir = match fs::read_dir(if dir.is_empty() { "." } else { dir }) {
        Ok(d) => d,
        Err(_) => return out,
    };
    let mut lods: Vec<String> = Vec::new();
    let mut base: Option<String> = None;
    for e in read_dir.flatten() {
        let name = e.file_name().to_string_lossy().to_string();
        let lower = name.to_lowercase();
        if lower == "bitmaps.lod" {
            base = Some(name);
        } else if lower.ends_with(".bitmaps.lod") {
            lods.push(name);
        }
    }
    lods.sort();
    // base first, patches after, so the later insert wins
    let ordered = base.into_iter().chain(lods);
    for name in ordered {
        let path = format!("{}{}", crate::path_utils::with_trailing_slash(dir), name);
        let lod = match LodArchive::load(&path) {
            Ok(l) => l,
            Err(_) => continue,
        };
        for i in 0..lod.entries.len() {
            let index = match palette_index_of(&lod.entries[i].name) {
                Some(v) => v,
                None => continue,
            };
            if let Ok(data) = lod.read_entry_data(i) {
                if data.len() >= 768 {
                    out.insert(index, data[..768].to_vec());
                }
            }
        }
    }
    out
}

/// Turn a sprites.lod entry into a .bmp, the way TRSLod.DoExtract does.
/// `Ok(None)` means the palette the sprite names could not be found, which is
/// not an error: the Delphi CLI ends up writing the stored bytes in that case
/// too, because its extract loop falls back to a raw extraction.
fn extract_sprite(raw: &[u8], name: &str, archive_path: &str) -> io::Result<Option<Vec<u8>>> {
    let (hdr, rows) = crate::image::decode_sprite(raw, MM_NAME_SIZE)?;
    // RSLodEdit.SpritePaletteFixup, applied before the palette is looked up.
    let at = MM_NAME_SIZE - 4;
    let palette_index =
        crate::image::fix_sprite_palette(name, &raw[at..at + crate::image::SPRITE_HEADER_SIZE], hdr.palette);
    if palette_index < 0 {
        return Ok(None);
    }
    let palettes = sibling_palettes(archive_path);
    let palette = match palettes.get(&(palette_index as u16)) {
        Some(p) => p.clone(),
        None => return Ok(None),
    };
    Ok(Some(crate::image::write_bmp8(&crate::image::Indexed {
        width: hdr.width as u32,
        height: hdr.height as u32,
        rows,
        palette,
    })))
}

/// Extract file data from a bitmaps/icons/MM8 LOD entry.
/// The stored format is: 16-byte name + 32-byte TMMLodFile header + compressed data.
/// For non-BMP files (BmpSize==0): decompress DataSize bytes using zlib.
/// For BMP files (BmpSize!=0): return raw pixel data + palette (treated as raw).
fn extract_tmmlodfile(raw: &[u8], name_size: usize, name: &str) -> io::Result<Vec<u8>> {
    if raw.len() < name_size + TMMLODFILE_HEADER_SIZE {
        // Too small for header — return as-is
        return Ok(raw.to_vec());
    }

    let hdr_start = name_size;
    let bmp_size = read_i32_le(raw, hdr_start);
    // TMMLodFile stores these as signed Int32. A negative one is nonsense, and
    // casting it to usize would turn it into a request for exabytes.
    let (data_size, unp_size) = match (
        usize::try_from(read_i32_le(raw, hdr_start + 4)),
        usize::try_from(read_i32_le(raw, hdr_start + 24)),
    ) {
        (Ok(d), Ok(u)) => (d, u),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "TMMLodFile header has a negative size field",
            ))
        }
    };

    let payload_start = name_size + TMMLODFILE_HEADER_SIZE;

    if bmp_size == 0 {
        // Palette (.act): RSLod.pas TRSLod.DoExtract takes the 768 bytes right
        // after the header when DataSize is 0 and the blob is big enough.
        if data_size == 0 {
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
        // RSLod.pas TRSLod.Unzip: UnpSize == 0 is the "stored raw" marker;
        // anything else is a zlib stream read until UnpSize bytes come out.
        let out = if unp_size > 0 {
            zlib_decompress_bounded(&raw[payload_start..], unp_size)?
        } else {
            raw[payload_start..payload_start + data_size].to_vec()
        };
        // .str entries hold NUL-separated lines; TRSLod.UnpackStr turns each
        // NUL into a CRLF on the way out (PackStr reverses it on the way in).
        if crate::path_utils::get_file_ext(name).eq_ignore_ascii_case(".str") {
            Ok(str_nul_to_crlf(&out))
        } else {
            Ok(out)
        }
    } else {
        // A texture or icon. RSLod.pas TRSLod.UnpackBitmap: the pixels are
        // BmpWidth x BmpHeight palette indices behind the header, and the
        // 768-byte palette sits right after the compressed data. Textures with
        // mipmaps decompress to more than BmpWidth*BmpHeight bytes — the extra
        // levels follow the base image and are not part of the picture.
        if data_size == 0 {
            return Ok(raw[payload_start..].to_vec());
        }
        if payload_start + data_size > raw.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "TMMLodFile bitmap data_size exceeds available data",
            ));
        }
        let width = read_i16_le(raw, hdr_start + 8) as i32;
        let height = read_i16_le(raw, hdr_start + 10) as i32;
        let palette_start = payload_start + data_size;

        let pixel_data = if unp_size > 0 {
            zlib_decompress_bounded(&raw[payload_start..], unp_size)?
        } else {
            raw[payload_start..payload_start + data_size].to_vec()
        };

        let pixels_needed = (width as i64) * (height as i64);
        if width <= 0
            || height <= 0
            || bmp_size as i64 != pixels_needed
            || (pixel_data.len() as i64) < pixels_needed
            || palette_start + 768 > raw.len()
        {
            // Not a picture we can describe; hand back what we decoded.
            let mut result = pixel_data;
            if palette_start + 768 <= raw.len() {
                result.extend_from_slice(&raw[palette_start..palette_start + 768]);
            }
            return Ok(result);
        }

        let (w, h) = (width as usize, height as usize);
        let rows = (0..h).map(|y| pixel_data[y * w..(y + 1) * w].to_vec()).collect();
        Ok(crate::image::write_bmp8(&crate::image::Indexed {
            width: w as u32,
            height: h as u32,
            rows,
            palette: raw[palette_start..palette_start + 768].to_vec(),
        }))
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

    // RSLod.pas TRSLod.DoExtract reads the header only for UnpackedSize and
    // then calls Unzip with `FFiles.Size[i] - sz` as the compressed length —
    // the header's own DataSize is never used. It cannot be: MM6 chapter LODs
    // (new.lod, .mm6 saves) ship .ddm/.dlv entries whose DataSize counts the
    // 8-byte header as well, so trusting it overruns the entry by 8 bytes.
    let unpacked_size = if is7 {
        // Verify signatures
        let sig1 = read_u32_le(raw, 0);
        let sig2 = read_u32_le(raw, 4);
        if sig1 != GAMES7_SIG1 || sig2 != GAMES7_SIG2 {
            // Signatures don't match — treat as raw
            return Ok(raw.to_vec());
        }
        read_u32_le(raw, 12) as usize
    } else {
        read_u32_le(raw, 4) as usize
    };

    // UnpackedSize == 0 means "stored raw" in the Delphi convention.
    if unpacked_size == 0 {
        return Ok(raw[hdr_size..].to_vec());
    }

    zlib_decompress_bounded(&raw[hdr_size..], unpacked_size)
}

/// Turn the NUL separators of a `.str` entry into CRLF (RSLod.pas
/// TRSLod.UnpackStr).
fn str_nul_to_crlf(data: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(data.len());
    for &b in data {
        if b == 0 {
            out.extend_from_slice(b"\r\n");
        } else {
            out.push(b);
        }
    }
    out
}

/// Reverse of [`str_nul_to_crlf`] (RSLod.pas TRSLod.PackStr): a `.str` file
/// that still contains a NUL anywhere is taken to be in archive form already
/// and left alone.
fn str_crlf_to_nul(data: &[u8]) -> Vec<u8> {
    if data.contains(&0) {
        return data.to_vec();
    }
    let mut out = Vec::with_capacity(data.len());
    let mut i = 0;
    while i < data.len() {
        if data[i] == b'\r' && i + 1 < data.len() && data[i + 1] == b'\n' {
            out.push(0);
            i += 2;
        } else {
            out.push(data[i]);
            i += 1;
        }
    }
    out
}

/// Build the stored blob for a file being added to a bitmaps/icons/MM8 LOD.
/// Wraps the file data with a NameSize-byte name + 32-byte TMMLodFile header.
/// All files are stored as non-BMP (BmpSize=0) with zlib compression.
fn wrap_tmmlodfile(
    name: &str,
    file_data: &[u8],
    name_size: usize,
    is_palette: bool,
) -> io::Result<Vec<u8>> {
    // .str entries are stored NUL-separated (RSLod.pas TRSLod.PackStr).
    let owned;
    let file_data: &[u8] =
        if crate::path_utils::get_file_ext(name).eq_ignore_ascii_case(".str") {
            owned = str_crlf_to_nul(file_data);
            &owned
        } else {
            file_data
        };

    // A palette goes in as 768 raw bytes behind a header whose DataSize is 0 —
    // RSLod.pas TRSLod.Add calls Zip with pk = -1 for .act, which skips both
    // the compression attempt and the DataSize write. That zero is the only
    // thing marking the entry as a palette on the way out, so getting it wrong
    // silently turns pal*.act into a nameless blob in MMArchive.
    if is_palette {
        if file_data.len() != 768 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("a palette must be exactly 768 bytes, {} has {}", name, file_data.len()),
            ));
        }
        let mut buf = vec![0u8; name_size + TMMLODFILE_HEADER_SIZE + 768];
        write_fixed_string(&mut buf, 0, name_size, name);
        buf[name_size + TMMLODFILE_HEADER_SIZE..].copy_from_slice(file_data);
        return Ok(buf);
    }

    // RSLod.pas TRSLod.Zip only tries zlib above 256 bytes, and keeps the raw
    // bytes whenever the compressed stream isn't actually smaller.
    let compressed = if file_data.len() > 256 {
        Some(zlib_compress_cached(file_data)?)
    } else {
        None
    };
    let (stored_payload, unp_size) = match compressed {
        Some(c) if c.len() < file_data.len() => (c, file_data.len()),
        // Not compressed: DataSize = data length, UnpSize = 0 (the marker)
        _ => (file_data.to_vec(), 0usize),
    };

    let total = name_size + TMMLODFILE_HEADER_SIZE + stored_payload.len();
    let mut buf = vec![0u8; total];

    // Write the name, repeated in front of the header
    write_fixed_string(&mut buf, 0, name_size, name);

    // Write TMMLodFile header (32 bytes) right after the name
    let h = name_size;
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
    buf[name_size + TMMLODFILE_HEADER_SIZE..].copy_from_slice(&stored_payload);

    Ok(buf)
}

/// Wrap a picture the way RSLod.pas TRSLod.PackBitmap does: the name, a
/// TMMLodFile header describing the image, the zlib-compressed pixels and the
/// 768-byte palette.
fn wrap_tmmlodfile_bitmap(
    name: &str,
    img: &crate::image::Indexed,
    name_size: usize,
    kind: ArchiveKind,
    palette_index: i16,
    bits: i32,
) -> io::Result<Vec<u8>> {
    let (w, h) = (img.width as usize, img.height as usize);
    let bmp_size = w * h;
    let mipmapped = bits & 2 != 0;

    let mut buffer: Vec<u8> = Vec::with_capacity(if mipmapped {
        crate::image::mipmapped_buffer_size(bmp_size)
    } else {
        bmp_size
    });
    for row in &img.rows {
        buffer.extend_from_slice(row);
    }
    if mipmapped {
        buffer.extend_from_slice(&crate::image::build_mipmaps(&img.rows, w, h, &img.palette));
        buffer.resize(crate::image::mipmapped_buffer_size(bmp_size), 0);
    }
    let unpacked = buffer.len();

    // TRSLod.Zip: only try zlib above 256 bytes, and keep the raw bytes when
    // the compressed stream is not actually smaller. UnpSize == 0 is the
    // "stored as is" marker.
    let compressed = if unpacked > 256 {
        Some(zlib_compress_cached(&buffer)?)
    } else {
        None
    };
    let (payload, unp_size) = match compressed {
        Some(c) if c.len() < unpacked => (c, unpacked),
        _ => (buffer, 0usize),
    };

    let mut out = vec![0u8; name_size + TMMLODFILE_HEADER_SIZE + payload.len() + 768];
    write_fixed_string(&mut out, 0, name_size, name);
    let hdr = name_size;
    write_i32_le(&mut out, hdr, bmp_size as i32);
    write_i32_le(&mut out, hdr + 4, payload.len() as i32);
    write_i16_le(&mut out, hdr + 8, w as i16);
    write_i16_le(&mut out, hdr + 10, h as i16);
    if kind == ArchiveKind::LodBitmaps && w != 0 {
        write_i16_le(&mut out, hdr + 20, palette_index);
        if mipmapped {
            let ln2 = |v: usize| -> io::Result<i16> {
                if v.is_power_of_two() && v >= 4 {
                    Ok(v.trailing_zeros() as i16)
                } else {
                    Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!("{}: a bitmaps.lod texture must be a power of 2, at least 4", name),
                    ))
                }
            };
            write_i16_le(&mut out, hdr + 12, ln2(w)?);
            write_i16_le(&mut out, hdr + 14, ln2(h)?);
            write_i16_le(&mut out, hdr + 16, (w - 1) as i16);
            write_i16_le(&mut out, hdr + 18, (h - 1) as i16);
        }
    }
    write_i32_le(&mut out, hdr + 24, unp_size as i32);
    write_i32_le(&mut out, hdr + 28, bits);
    let at = name_size + TMMLODFILE_HEADER_SIZE;
    out[at..at + payload.len()].copy_from_slice(&payload);
    out[at + payload.len()..].copy_from_slice(&img.palette[..768]);
    Ok(out)
}

/// Wrap a picture as a sprites.lod entry (RSLod.pas TRSLod.PackSprite).
fn wrap_sprite(name: &str, img: &crate::image::Indexed, palette_index: i16) -> io::Result<Vec<u8>> {
    if palette_index <= 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "{}: failed to find matching palette in bitmaps.lod (give one with /p, or \
                 put the palette in a [*.]bitmaps.lod next to the archive)",
                name
            ),
        ));
    }
    let (w, h) = (img.width as usize, img.height as usize);
    let (spans, lines, y_skip) = crate::image::pack_sprite_rows(&img.rows, w);

    let compressed = if spans.len() > 256 {
        Some(zlib_compress_cached(&spans)?)
    } else {
        None
    };
    let (payload, unp_size) = match compressed {
        Some(c) if c.len() < spans.len() => (c, spans.len()),
        _ => (spans, 0usize),
    };

    // 12 bytes of name, TSprite, one line record per row, then the spans.
    let head = MM_NAME_SIZE - 4;
    let mut out = vec![0u8; head + crate::image::SPRITE_HEADER_SIZE + lines.len() * 8 + payload.len()];
    write_fixed_string(&mut out, 0, head, name);
    write_u32_le(&mut out, head, payload.len() as u32);
    write_i16_le(&mut out, head + 4, w as i16);
    write_i16_le(&mut out, head + 6, h as i16);
    write_i16_le(&mut out, head + 8, palette_index);
    write_i16_le(&mut out, head + 12, y_skip);
    write_u32_le(&mut out, head + 16, unp_size as u32);
    let mut at = head + crate::image::SPRITE_HEADER_SIZE;
    for rec in &lines {
        out[at..at + 8].copy_from_slice(rec);
        at += 8;
    }
    out[at..].copy_from_slice(&payload);
    Ok(out)
}

/// Is this one of the files TRSLod.LoadBitmapsLods would collect?
fn is_bitmaps_lod_name(path: &str) -> bool {
    let name = crate::path_utils::get_file_name(path).to_lowercase();
    name == "bitmaps.lod" || name.ends_with(".bitmaps.lod")
}

/// The palette index whose 768 bytes match this picture's (MMArchMain.pas
/// getPalette -> RSMMArchivesFindSamePalette).
///
/// RSLod.pas TRSLod.LoadBitmapsLods gathers `bitmaps.lod` and every
/// `*.bitmaps.lod` next to the archive — **including the archive being written**
/// when its own name matches. That is what makes
/// `create x.bitmaps.lod mmbitmapslod . pal994.act picture.bmp` work: by the
/// time the picture is added the palette is already in the archive, in memory,
/// and nowhere on disk yet.
fn match_palette(archive: &LodArchive, palette: &[u8]) -> Option<i16> {
    let wanted = &palette[..768];
    if is_bitmaps_lod_name(&archive.path) {
        for (i, entry) in archive.entries.iter().enumerate() {
            if let Some(index) = palette_index_of(&entry.name) {
                if let Ok(data) = archive.read_entry_data(i) {
                    if data.len() >= 768 && &data[..768] == wanted {
                        return Some(index as i16);
                    }
                }
            }
        }
    }
    let table = sibling_palettes(&archive.path);
    let mut found: Option<i16> = None;
    for (index, bytes) in table.iter() {
        if bytes.as_slice() == wanted {
            // highest index wins, matching RSMMArchivesFind's backwards search
            found = Some(found.map_or(*index as i16, |f: i16| f.max(*index as i16)));
        }
    }
    found
}

/// `pal023` -> 23. RSPak looks these up with a case-insensitive FindFile, and
/// it has to: MM8's bitmaps.lod spells them pal005, Pal586 and PAL123 alike.
fn palette_index_of(name: &str) -> Option<u16> {
    let lower = name.to_ascii_lowercase();
    let digits = lower.strip_prefix("pal")?;
    if digits.is_empty() || !digits.bytes().all(|c| c.is_ascii_digit()) {
        return None;
    }
    digits.parse().ok()
}

/// Build the stored blob for a file being added to a games/chapter LOD.
/// For .blv/.odm/.dlv/.ddm: compress with zlib and prepend 8 or 16-byte header.
/// For other files: store as-is.
fn wrap_games_lod(file_data: &[u8], kind: ArchiveKind, name: &str) -> io::Result<(Vec<u8>, u32)> {
    if !is_games_compressed_ext(name) {
        // Not a compressed type — store raw, entry unpacked_size = 0
        return Ok((file_data.to_vec(), 0));
    }

    let compressed = zlib_compress_cached(file_data)?;
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

    fn tmmlodfile_class(&self, index: usize) -> TmmClass {
        if !kind_has_tmmlodfile_header(self.kind) {
            return TmmClass::Plain;
        }
        self.tmm_classes
            .get_or_init(|| self.read_tmm_classes())
            .get(index)
            .copied()
            .unwrap_or(TmmClass::Plain)
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
            extract_tmmlodfile(&raw, kind_name_size(self.kind), &entry.name)
        } else if kind_is_games_lod(self.kind) {
            extract_games_lod(&raw, self.kind, &entry.name)
        } else if self.kind == ArchiveKind::LodHeroes {
            // H3: if packed, decompress (RawExtract copies UnpackedSize bytes)
            let data = if entry.is_packed() {
                zlib_decompress_bounded(&raw, entry.unpacked_size as usize)?
            } else {
                raw
            };
            // TRSLod.DoExtract turns a .pcx into a .bmp. Heroes' "PCX" is its
            // own thing: a 12-byte header, raw pixels, then a palette.
            if crate::path_utils::get_file_ext(&entry.name).eq_ignore_ascii_case(".pcx") {
                if let Ok(bmp) = crate::image::decode_pcx(&data) {
                    return Ok(bmp);
                }
            }
            Ok(data)
        } else if self.kind == ArchiveKind::LodSprites {
            // No palette to colour it with, or not a sprite at all — mmarch
            // lets any file into a sprites.lod, unlike the Delphi version — so
            // hand back the stored bytes, which is also what the Delphi CLI
            // ends up writing when its extract loop catches the failure.
            match extract_sprite(&raw, &entry.name, &self.path) {
                Ok(Some(bmp)) => Ok(bmp),
                Ok(None) | Err(_) => Ok(raw),
            }
        } else {
            // Anything else: simple decompress if packed
            if entry.is_packed() {
                zlib_decompress_bounded(&raw, entry.unpacked_size as usize)
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
                let new_data_start = (H3_HEADER_SIZE + (count as u64) * (H3_ENTRY_SIZE as u64))
                    .max(H3_MIN_FILE_SIZE);

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
                if !data_buf.is_empty() {
                    // Leave the reserved entry-table room MinFileSize asks for.
                    let written = H3_HEADER_SIZE + entry_table.len() as u64;
                    if new_data_start > written {
                        out.write_all(&vec![0u8; (new_data_start - written) as usize])?;
                    }
                }
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
                        packed: entry.packed,
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
                write_fixed_string(&mut h, 84, 80, kind_to_mm_description(kind));
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
            tmm_classes: OnceLock::new(),
        })
    }
}

// Version/LodType strings as the real game files (and RSLod's LodTypes
// table) have them. Note: Games7 uses the SAME "GameMMVI" header as
// Games — MM7/8-format games LODs are told apart by the per-entry data
// signature, not the header (see the sig scan in LodArchive::load).
// GitHub issue #2: writing a made-up "GameMMVII" made MMEditor/RSLod
// reject the archive with "Unknown LOD version".
fn kind_to_mm_strings(kind: ArchiveKind) -> (&'static str, &'static str) {
    match kind {
        ArchiveKind::LodBitmaps => ("MMVI", "bitmaps"),
        ArchiveKind::LodIcons => ("MMVI", "icons"),
        ArchiveKind::LodSprites => ("MMVI", "sprites08"),
        ArchiveKind::LodMM8 => ("MMVIII", "language"),
        ArchiveKind::LodGames7 => ("GameMMVI", "maps"),
        ArchiveKind::LodGames => ("GameMMVI", "maps"),
        ArchiveKind::LodChapter7 => ("MMVII", "chapter"),
        ArchiveKind::LodChapter => ("MMVI", "chapter"),
        _ => ("MMVI", "bitmaps"),
    }
}

// Description field as in the real game files / RSLod's LodDescriptions.
fn kind_to_mm_description(kind: ArchiveKind) -> &'static str {
    match kind {
        ArchiveKind::LodBitmaps => "Bitmaps for MMVI.",
        ArchiveKind::LodIcons => "Icons for MMVI.",
        ArchiveKind::LodSprites => "Sprites for MMVI.",
        ArchiveKind::LodMM8 => "Language for MMVIII.",
        ArchiveKind::LodGames7 | ArchiveKind::LodGames => "Maps for MMVI",
        ArchiveKind::LodChapter7 => "newmaps for MMVII",
        ArchiveKind::LodChapter => "newmaps for MMVI",
        _ => "",
    }
}

/// Compress data with zlib.
// ---- parallel pre-compression ----------------------------------------
// Compressing entries one-by-one serializes the dominant cost of archive
// creation. precompress_files() zlib-compresses every input file on all
// cores up front (rayon); the per-file add paths then take the ready
// blob from this content-keyed cache via zlib_compress_cached(), so the
// archive bytes are identical to a serial run.

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

static PRECOMP: OnceLock<Mutex<HashMap<Vec<u8>, Vec<u8>>>> = OnceLock::new();

pub fn precompress_files(paths: &[String]) {
    use rayon::prelude::*;
    let computed: Vec<(Vec<u8>, Vec<u8>)> = paths
        .par_iter()
        .filter_map(|p| {
            let data = fs::read(p).ok()?;
            let compressed = zlib_compress(&data).ok()?;
            Some((data, compressed))
        })
        .collect();
    let m = PRECOMP.get_or_init(|| Mutex::new(HashMap::new()));
    m.lock().unwrap().extend(computed);
}

pub fn zlib_compress_cached(data: &[u8]) -> io::Result<Vec<u8>> {
    if let Some(m) = PRECOMP.get() {
        if let Some(c) = m.lock().unwrap().get(data) {
            return Ok(c.clone());
        }
    }
    zlib_compress(data)
}

/// Archive kinds whose add path zlib-compresses the raw file bytes —
/// the ones precompress_files() can help.
pub fn kind_precompressible(kind: ArchiveKind) -> bool {
    kind_has_tmmlodfile_header(kind) || kind_is_games_lod(kind) || kind == ArchiveKind::LodHeroes
}
// -----------------------------------------------------------------------

pub fn zlib_compress(data: &[u8]) -> io::Result<Vec<u8>> {
    Ok(compress_to_vec_zlib(data, 6))
}

/// Decompress zlib data.
pub fn zlib_decompress(data: &[u8]) -> io::Result<Vec<u8>> {
    decompress_to_vec_zlib(data)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("zlib decompress error: {:?}", e)))
}

/// Decompress a zlib stream, stopping as soon as `expected` bytes have come
/// out.
///
/// This is what RSPak does everywhere an entry header carries an unpacked size:
/// TRSLod.Unzip and TRSMMFiles.RawExtract wrap the archive stream in a
/// TDecompressionStream and copy exactly UnpackedSize bytes out of it. Whatever
/// follows the deflate stream inside the entry — a bitmap palette, padding, a
/// DataSize field that counts its own header, leftovers of a longer entry that
/// a patcher overwrote in place — is never looked at. Handing the whole
/// remainder of the entry to a one-shot decompressor instead rejects such
/// entries outright.
///
/// The unpacked size the entry promises is the integrity check here, so the
/// trailing Adler-32 is not verified: GOG's MM6 `Games.lod` ships `cd3.blv` and
/// `d08.blv` with a stale checksum and ~2 KB of stale bytes behind an otherwise
/// intact stream, and the game reads them.
pub fn zlib_decompress_bounded(data: &[u8], expected: usize) -> io::Result<Vec<u8>> {
    use miniz_oxide::inflate::stream::{inflate, InflateState};
    use miniz_oxide::{DataFormat, MZFlush};

    if expected == 0 {
        return Ok(Vec::new());
    }
    // Never reserve a buffer the input could not possibly fill. deflate tops
    // out around 1032:1, so anything past that is a corrupt or hostile header
    // (a negative Int32 size field reads back as ~18 EB) and reserving it
    // aborts the process instead of skipping the one bad entry.
    let ceiling = data.len().saturating_mul(1032).saturating_add(1024);
    if expected > ceiling {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "entry declares {} unpacked bytes, more than {} compressed bytes can produce",
                expected,
                data.len()
            ),
        ));
    }
    let truncated = |got: usize| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "zlib stream ended after {} of the {} bytes the entry declares",
                got, expected
            ),
        )
    };

    // The whole entry is already in memory and the output size is known, so a
    // single Finish call is enough. That is also the only path miniz_oxide
    // decompresses straight into the caller's buffer on, instead of through
    // its ring buffer.
    let mut out = vec![0u8; expected];
    let mut state = InflateState::new_boxed(DataFormat::ZLibIgnoreChecksum);
    let r = inflate(&mut state, data, &mut out, MZFlush::Finish);
    if r.bytes_written >= expected {
        // Done, or the stream had more to give and we stopped at `expected` —
        // either way the entry's bytes are all here.
        return Ok(out);
    }
    // Short of what the entry promised. How many bytes did come out is the
    // useful part of the message — a stream that stops early and one that is
    // malformed halfway both land here.
    if r.bytes_written > 0 {
        return Err(truncated(r.bytes_written));
    }
    match r.status {
        Err(e) => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("not a usable zlib stream: {:?}", e),
        )),
        Ok(_) => Err(truncated(0)),
    }
}

/// Add a file to the LOD archive. The file is read from disk and stored
/// with the appropriate format-specific wrapping.
pub fn lod_add_file(archive: &mut LodArchive, file_path: &str) -> io::Result<()> {
    lod_add_file_with_palette(archive, file_path, None)
}

pub fn lod_add_file_with_palette(
    archive: &mut LodArchive,
    file_path: &str,
    palette_index: Option<i32>,
) -> io::Result<()> {
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

    // Note: the entry being replaced is only removed once the new blob exists.
    // Dropping it first means a conversion that fails — a sprite whose palette
    // cannot be found, say — takes the old resource with it.

    // RSLod.pas TRSLod.Add: a .bmp going into an archive that stores pictures is
    // converted into that archive's image format, not filed away as bytes.
    let is_bmp = ext.eq_ignore_ascii_case(".bmp");
    if is_bmp
        && matches!(
            archive.kind,
            ArchiveKind::LodBitmaps
                | ArchiveKind::LodIcons
                | ArchiveKind::LodMM8
                | ArchiveKind::LodSprites
                | ArchiveKind::LodHeroes
        )
    {
        // Heroes archives hold 24-bit pictures too; everything else is
        // palette indices.
        if archive.kind == ArchiveKind::LodHeroes {
            let stored = crate::image::pack_pcx(&crate::image::read_bmp(&file_data)?);
            archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&in_name));
            archive.entries.push(ArchiveEntry {
                name: in_name,
                offset: 0,
                size: stored.len() as u32,
                unpacked_size: 0,
                data: Some(stored),
                original_data: Some(file_data),
                packed: false,
            });
            return Ok(());
        }
        let img = crate::image::read_bmp8(&file_data)?;
        let stored = match archive.kind {
            ArchiveKind::LodSprites => {
                // TRSLod.AddBitmap: an explicit index, else the one the entry
                // being replaced already used, else a matching palette in a
                // neighbouring bitmaps.lod.
                let index = palette_index
                    .map(|p| p as i16)
                    .or_else(|| {
                        archive
                            .entries
                            .iter()
                            .find(|e| e.name.eq_ignore_ascii_case(&in_name))
                            .and_then(|e| read_raw_entry(&archive.path, e).ok())
                            .and_then(|raw| {
                                crate::image::read_sprite_header(&raw, MM_NAME_SIZE)
                                    .ok()
                                    .map(|(h, _)| h.palette)
                            })
                    })
                    .or_else(|| match_palette(archive, &img.palette))
                    .unwrap_or(0);
                wrap_sprite(&in_name, &img, index)?
            }
            _ => {
                // FindBitmapPalette: only bitmaps.lod carries a palette index
                // and mipmaps; icons and MM8 store the picture plainly.
                let (index, bits) = if archive.kind == ArchiveKind::LodBitmaps {
                    let existing = archive
                        .entries
                        .iter()
                        .find(|e| e.name.eq_ignore_ascii_case(&in_name))
                        .and_then(|e| read_raw_entry(&archive.path, e).ok());
                    let old_palette = existing.as_ref().and_then(|raw| {
                        (raw.len() >= MM_NAME_SIZE + TMMLODFILE_HEADER_SIZE)
                            .then(|| read_i16_le(raw, MM_NAME_SIZE + 20))
                    });
                    let old_bits = existing.as_ref().and_then(|raw| {
                        (raw.len() >= MM_NAME_SIZE + TMMLODFILE_HEADER_SIZE)
                            .then(|| read_i32_le(raw, MM_NAME_SIZE + 28))
                    });
                    let index = match palette_index
                        .map(|p| p as i16)
                        .or(old_palette)
                        .or_else(|| match_palette(archive, &img.palette))
                    {
                        Some(i) => i,
                        // MMArchMain.pas getPalette raises here rather than
                        // storing a texture under palette 0, which would be a
                        // picture that renders in the wrong colours.
                        None => {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidInput,
                                format!(
                                    "{}: failed to find matching palette in bitmaps.lod \
                                     (give one with /p, or put the palette in a \
                                     [*.]bitmaps.lod next to the archive)",
                                    file_name
                                ),
                            ))
                        }
                    };
                    (index, old_bits.unwrap_or(0) | 0x12)
                } else if crate::image::palette_is_transparent(&img.palette) {
                    (0, 512)
                } else {
                    (0, 0)
                };
                wrap_tmmlodfile_bitmap(
                    &in_name,
                    &img,
                    kind_name_size(archive.kind),
                    archive.kind,
                    index,
                    bits,
                )?
            }
        };
        archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&in_name));
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: stored.len() as u32,
            unpacked_size: 0,
            data: Some(stored),
            original_data: Some(file_data),
            packed: false,
        });
        return Ok(());
    }

    // Format-specific wrapping
    if kind_has_tmmlodfile_header(archive.kind) {
        // RSLod.pas TRSLod.Add treats a .act file as a palette, and so does a
        // 768-byte extensionless `pal*` going into a bitmaps LOD.
        let is_palette = ext.eq_ignore_ascii_case(".act")
            || (ext.is_empty()
                && archive.kind == ArchiveKind::LodBitmaps
                && file_name.len() >= 3
                && file_name[..3].eq_ignore_ascii_case("pal")
                && file_data.len() == 768);
        // Wrap with TMMLodFile header
        let stored =
            wrap_tmmlodfile(&in_name, &file_data, kind_name_size(archive.kind), is_palette)?;
        archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&in_name));
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: stored.len() as u32,
            unpacked_size: 0, // not used in entry table for bitmaps/icons
            data: Some(stored),
            original_data: Some(file_data),
            packed: false,
        });
    } else if kind_is_games_lod(archive.kind) {
        // Wrap with games header for compressed types
        let (stored, unpacked_size) = wrap_games_lod(&file_data, archive.kind, &in_name)?;
        archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&in_name));
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: stored.len() as u32,
            unpacked_size,
            data: Some(stored),
            original_data: Some(file_data),
            packed: false,
        });
    } else if archive.kind == ArchiveKind::LodHeroes {
        // H3: compress if beneficial, store packed data directly
        let compressed = zlib_compress_cached(&file_data)?;
        let (stored_data, unpacked_size) = if compressed.len() < file_data.len() {
            (compressed, file_data.len() as u32)
        } else {
            (file_data.clone(), 0u32)
        };
        let sz = stored_data.len() as u32;
        archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&in_name));
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: sz,
            unpacked_size,
            data: Some(stored_data),
            original_data: Some(file_data),
            packed: unpacked_size != 0,
        });
    } else {
        // Sprites or other: store as-is
        let sz = file_data.len() as u32;
        archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&in_name));
        archive.entries.push(ArchiveEntry {
            name: in_name,
            offset: 0,
            size: sz,
            unpacked_size: 0,
            original_data: Some(file_data.clone()),
            packed: false,
            data: Some(file_data),
        });
    }

    Ok(())
}
