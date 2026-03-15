use crate::archive::{Archive, ArchiveEntry, ArchiveKind};
use crate::lod::{zlib_compress, zlib_decompress};
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom, Write};

/// SND archive (Heroes III and MM6/7/8 formats).
pub struct SndArchive {
    pub path: String,
    pub kind: ArchiveKind,
    pub entries: Vec<ArchiveEntry>,
}

// H3 SND: 48 bytes per entry (name[40], address u32, size u32)
const H3_SND_ENTRY_SIZE: u32 = 48;
const H3_SND_NAME_SIZE: usize = 40;

// MM SND: 52 bytes per entry (name[40], address u32, size u32, unpacked_size u32)
const MM_SND_ENTRY_SIZE: u32 = 52;
const MM_SND_NAME_SIZE: usize = 40;

fn read_u32_le(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]])
}

fn write_u32_le(buf: &mut Vec<u8>, val: u32) {
    buf.extend_from_slice(&val.to_le_bytes());
}

fn read_fixed_string(data: &[u8], offset: usize, max_len: usize) -> String {
    let slice = &data[offset..offset + max_len];
    let end = slice.iter().position(|&b| b == 0).unwrap_or(max_len);
    String::from_utf8_lossy(&slice[..end]).to_string()
}

fn write_fixed_string(buf: &mut Vec<u8>, s: &str, max_len: usize) {
    let bytes = s.as_bytes();
    let copy_len = bytes.len().min(max_len - 1);
    let mut fixed = vec![0u8; max_len];
    fixed[..copy_len].copy_from_slice(&bytes[..copy_len]);
    buf.extend_from_slice(&fixed);
}

impl SndArchive {
    pub fn load(path: &str) -> io::Result<Self> {
        let data = fs::read(path)?;
        if data.len() < 4 {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "File too small for SND"));
        }

        let count = read_u32_le(&data, 0) as usize;
        if count == 0 {
            return Ok(SndArchive {
                path: path.to_string(),
                kind: ArchiveKind::SndHeroes,
                entries: Vec::new(),
            });
        }

        // Detect H3 vs MM: read first file's data and check for zlib header
        // H3 SND entries: 48 bytes, MM SND entries: 52 bytes
        // Try H3 first
        let h3_first_entry_offset = 4;
        if data.len() < h3_first_entry_offset + H3_SND_ENTRY_SIZE as usize {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "SND too small"));
        }

        let h3_first_addr = read_u32_le(&data, h3_first_entry_offset + 40) as usize;
        let is_mm = if h3_first_addr < data.len() && h3_first_addr + 2 <= data.len() {
            // Check if data at that offset starts with zlib magic (0x78 0x9C or similar)
            let b0 = data[h3_first_addr];
            let b1 = data[h3_first_addr + 1];
            // zlib header: first byte is 0x78 (deflate, 32K window)
            b0 == 0x78 && (b1 == 0x9C || b1 == 0x01 || b1 == 0xDA || b1 == 0x5E)
        } else {
            false
        };

        // Also check: if not zlib, check if it's RIFF (WAV) -> H3
        let _is_h3 = if !is_mm && h3_first_addr + 4 <= data.len() {
            &data[h3_first_addr..h3_first_addr + 4] == b"RIFF"
        } else {
            !is_mm
        };

        if is_mm {
            Self::load_mm(path, &data, count)
        } else {
            Self::load_h3(path, &data, count)
        }
    }

    fn load_h3(path: &str, data: &[u8], count: usize) -> io::Result<Self> {
        let mut entries = Vec::with_capacity(count);
        for i in 0..count {
            let off = 4 + i * H3_SND_ENTRY_SIZE as usize;
            if off + H3_SND_ENTRY_SIZE as usize > data.len() {
                break;
            }
            let name = read_fixed_string(data, off, H3_SND_NAME_SIZE);
            let addr = read_u32_le(data, off + 40);
            let size = read_u32_le(data, off + 44);
            entries.push(ArchiveEntry {
                name,
                offset: addr as u64,
                size,
                unpacked_size: 0,
                data: None,
                original_data: None,
            });
        }
        Ok(SndArchive {
            path: path.to_string(),
            kind: ArchiveKind::SndHeroes,
            entries,
        })
    }

    fn load_mm(path: &str, data: &[u8], count: usize) -> io::Result<Self> {
        let mut entries = Vec::with_capacity(count);
        for i in 0..count {
            let off = 4 + i * MM_SND_ENTRY_SIZE as usize;
            if off + MM_SND_ENTRY_SIZE as usize > data.len() {
                break;
            }
            let name = read_fixed_string(data, off, MM_SND_NAME_SIZE);
            let addr = read_u32_le(data, off + 40);
            let size = read_u32_le(data, off + 44);
            let unpacked = read_u32_le(data, off + 48);
            entries.push(ArchiveEntry {
                name,
                offset: addr as u64,
                size,
                unpacked_size: unpacked,
                data: None,
                original_data: None,
            });
        }
        Ok(SndArchive {
            path: path.to_string(),
            kind: ArchiveKind::SndMM,
            entries,
        })
    }
}

impl Archive for SndArchive {
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
        if let Some(ref orig) = entry.original_data {
            return Ok(orig.clone());
        }
        if let Some(ref data) = entry.data {
            return Ok(data.clone());
        }
        let mut f = File::open(&self.path)?;
        f.seek(SeekFrom::Start(entry.offset))?;
        let mut buf = vec![0u8; entry.size as usize];
        f.read_exact(&mut buf)?;

        if entry.is_packed() {
            zlib_decompress(&buf)
        } else {
            Ok(buf)
        }
    }

    fn rebuild(&mut self) -> io::Result<()> {
        let tmp_path = format!("{}.tmp", self.path);
        {
            let mut src = File::open(&self.path).ok();
            let mut out = File::create(&tmp_path)?;

            let count = self.entries.len() as u32;
            let is_mm = self.kind == ArchiveKind::SndMM;
            let entry_size = if is_mm { MM_SND_ENTRY_SIZE } else { H3_SND_ENTRY_SIZE };
            let data_start = 4 + (count as u64) * (entry_size as u64);

            // Write count
            out.write_all(&count.to_le_bytes())?;

            // Collect all data first to compute addresses
            let mut all_data = Vec::new();
            let mut addrs = Vec::new();
            for entry in &self.entries {
                let file_data = if let Some(ref d) = entry.data {
                    d.clone()
                } else if let Some(ref mut f) = src {
                    f.seek(SeekFrom::Start(entry.offset))?;
                    let mut buf = vec![0u8; entry.size as usize];
                    f.read_exact(&mut buf)?;
                    buf
                } else {
                    Vec::new()
                };
                addrs.push(data_start as u32 + all_data.len() as u32);
                all_data.extend_from_slice(&file_data);
            }

            // Write entries
            for (i, entry) in self.entries.iter().enumerate() {
                let mut entry_buf = Vec::new();
                write_fixed_string(&mut entry_buf, &entry.name, H3_SND_NAME_SIZE);
                write_u32_le(&mut entry_buf, addrs[i]);
                write_u32_le(&mut entry_buf, entry.size);
                if is_mm {
                    write_u32_le(&mut entry_buf, entry.unpacked_size);
                }
                out.write_all(&entry_buf)?;
            }

            // Write data
            out.write_all(&all_data)?;
        }

        fs::remove_file(&self.path)?;
        fs::rename(&tmp_path, &self.path)?;

        // Reload to update offsets
        let reloaded = SndArchive::load(&self.path)?;
        self.entries = reloaded.entries;

        Ok(())
    }

    fn create_new(path: &str, kind: ArchiveKind) -> io::Result<Self> {
        let mut f = File::create(path)?;
        f.write_all(&0u32.to_le_bytes())?;
        Ok(SndArchive {
            path: path.to_string(),
            kind,
            entries: Vec::new(),
        })
    }
}

pub fn snd_add_file(archive: &mut SndArchive, file_path: &str) -> io::Result<()> {
    let file_data = fs::read(file_path)?;
    let file_name = crate::path_utils::get_file_stem(
        &crate::path_utils::get_file_name(file_path),
    );

    // Remove existing
    archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&file_name));

    let is_mm = archive.kind == ArchiveKind::SndMM;
    let (stored_data, size, unpacked_size) = if is_mm {
        let compressed = zlib_compress(&file_data)?;
        if compressed.len() < file_data.len() {
            (compressed.clone(), compressed.len() as u32, file_data.len() as u32)
        } else {
            (file_data.clone(), file_data.len() as u32, file_data.len() as u32)
        }
    } else {
        (file_data.clone(), file_data.len() as u32, 0)
    };

    archive.entries.push(ArchiveEntry {
        name: file_name,
        offset: 0,
        size,
        unpacked_size,
        data: Some(stored_data),
        original_data: Some(file_data),
    });

    Ok(())
}
