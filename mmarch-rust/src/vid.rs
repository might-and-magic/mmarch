use crate::archive::{Archive, ArchiveEntry, ArchiveKind};
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom, Write};

/// VID archive (Heroes III and MM6 formats).
pub struct VidArchive {
    pub path: String,
    pub kind: ArchiveKind,
    pub entries: Vec<ArchiveEntry>,
}

const VID_ENTRY_SIZE: u32 = 44;
const VID_NAME_SIZE: usize = 40;

fn read_u32_le(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([data[offset], data[offset + 1], data[offset + 2], data[offset + 3]])
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

fn write_u32_le_vec(buf: &mut Vec<u8>, val: u32) {
    buf.extend_from_slice(&val.to_le_bytes());
}

impl VidArchive {
    pub fn load(path: &str) -> io::Result<Self> {
        let data = fs::read(path)?;
        if data.len() < 4 {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "File too small for VID"));
        }

        let count = read_u32_le(&data, 0) as usize;
        let file_size = data.len() as u64;

        let mut entries = Vec::with_capacity(count);
        for i in 0..count {
            let off = 4 + i * VID_ENTRY_SIZE as usize;
            if off + VID_ENTRY_SIZE as usize > data.len() {
                break;
            }
            let name = read_fixed_string(&data, off, VID_NAME_SIZE);
            let addr = read_u32_le(&data, off + 40);

            // Size: next entry's address - this entry's address, or file_end - last address
            let next_addr = if i + 1 < count {
                let next_off = 4 + (i + 1) * VID_ENTRY_SIZE as usize;
                read_u32_le(&data, next_off + 40) as u64
            } else {
                file_size
            };

            let size = (next_addr - addr as u64) as u32;

            entries.push(ArchiveEntry {
                name,
                offset: addr as u64,
                size,
                unpacked_size: 0,
                data: None,
                original_data: None,
            });
        }

        // Detect kind: H3 or MM6 - we default to H3, but create sets it appropriately
        Ok(VidArchive {
            path: path.to_string(),
            kind: ArchiveKind::VidHeroes,
            entries,
        })
    }
}

impl Archive for VidArchive {
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
        Ok(buf)
    }

    fn rebuild(&mut self) -> io::Result<()> {
        let tmp_path = format!("{}.tmp", self.path);
        {
            let mut src = File::open(&self.path).ok();
            let mut out = File::create(&tmp_path)?;

            let count = self.entries.len() as u32;
            let data_start = 4 + (count as u64) * (VID_ENTRY_SIZE as u64);

            out.write_all(&count.to_le_bytes())?;

            // Collect data
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
                write_fixed_string(&mut entry_buf, &entry.name, VID_NAME_SIZE);
                write_u32_le_vec(&mut entry_buf, addrs[i]);
                out.write_all(&entry_buf)?;
            }

            out.write_all(&all_data)?;
        }

        fs::remove_file(&self.path)?;
        fs::rename(&tmp_path, &self.path)?;

        let reloaded = VidArchive::load(&self.path)?;
        self.entries = reloaded.entries;

        Ok(())
    }

    fn create_new(path: &str, kind: ArchiveKind) -> io::Result<Self> {
        let mut f = File::create(path)?;
        f.write_all(&0u32.to_le_bytes())?;
        Ok(VidArchive {
            path: path.to_string(),
            kind,
            entries: Vec::new(),
        })
    }
}

pub fn vid_add_file(archive: &mut VidArchive, file_path: &str) -> io::Result<()> {
    let file_data = fs::read(file_path)?;
    let file_name = crate::path_utils::get_file_stem(
        &crate::path_utils::get_file_name(file_path),
    );

    // Remove existing
    archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&file_name));

    archive.entries.push(ArchiveEntry {
        name: file_name,
        offset: 0,
        size: file_data.len() as u32,
        unpacked_size: 0,
        original_data: Some(file_data.clone()),
        data: Some(file_data),
    });

    Ok(())
}
