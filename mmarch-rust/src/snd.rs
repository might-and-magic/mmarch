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
            // Empty archive: try to preserve the kind based on file size.
            // An MM SND that was created and optimized with 0 entries is just 4 bytes,
            // same as H3 SND. We default to SndMM since that's the more common format
            // and avoids degrading mm snd archives that have been emptied.
            // The actual kind will be corrected by the rebuild() which reloads.
            return Ok(SndArchive {
                path: path.to_string(),
                kind: ArchiveKind::SndMM,
                entries: Vec::new(),
            });
        }

        // Detect H3 vs MM by validating both interpretations.
        // H3 SND entries: 48 bytes each. MM SND entries: 52 bytes each.
        // We try both and pick the one where addresses are valid.
        let is_mm = Self::detect_mm_snd(&data, count);

        if is_mm {
            Self::load_mm(path, &data, count)
        } else {
            Self::load_h3(path, &data, count)
        }
    }

    /// Detect whether this is an MM SND (52-byte entries) or H3 SND (48-byte entries).
    /// We try both interpretations and validate addresses against file size.
    fn detect_mm_snd(data: &[u8], count: usize) -> bool {
        let file_size = data.len();

        // Validate H3 interpretation (48-byte entries)
        let h3_data_start = 4 + count * H3_SND_ENTRY_SIZE as usize;
        let h3_valid = if h3_data_start <= file_size {
            (0..count).all(|i| {
                let off = 4 + i * H3_SND_ENTRY_SIZE as usize;
                if off + H3_SND_ENTRY_SIZE as usize > file_size { return false; }
                let addr = read_u32_le(data, off + 40) as usize;
                let size = read_u32_le(data, off + 44) as usize;
                addr >= h3_data_start && addr + size <= file_size
            })
        } else {
            false
        };

        // Validate MM interpretation (52-byte entries)
        let mm_data_start = 4 + count * MM_SND_ENTRY_SIZE as usize;
        let mm_valid = if mm_data_start <= file_size {
            (0..count).all(|i| {
                let off = 4 + i * MM_SND_ENTRY_SIZE as usize;
                if off + MM_SND_ENTRY_SIZE as usize > file_size { return false; }
                let addr = read_u32_le(data, off + 40) as usize;
                let size = read_u32_le(data, off + 44) as usize;
                addr >= mm_data_start && addr + size <= file_size
            })
        } else {
            false
        };

        if mm_valid && !h3_valid {
            return true;
        }
        if h3_valid && !mm_valid {
            return false;
        }
        // Both valid (or neither): fall back to zlib check on first entry
        if h3_valid {
            let off = 4 + 40; // first entry addr offset in H3 format
            let addr = read_u32_le(data, off) as usize;
            if addr + 2 <= file_size {
                let b0 = data[addr];
                return b0 == 0x78; // zlib magic
            }
        }
        false
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
