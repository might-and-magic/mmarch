use crate::archive::{Archive, ArchiveEntry, ArchiveKind};
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom, Write};

/// VID archive (Heroes III and MM6 formats).
pub struct VidArchive {
    pub path: String,
    pub kind: ArchiveKind,
    pub entries: Vec<ArchiveEntry>,
    /// RSLod.pas TRSVid.FNoExtension — the archive stores names without the
    /// `.smk` extension, so `add` has to strip it again.
    pub no_extension: bool,
}

const VID_ENTRY_SIZE: u32 = 44;
const VID_NAME_SIZE: usize = 40;

// Trailer signatures MMArchive appends to a .vid (RSLod.pas).
const VID_SIG_SIZE: usize = 16;
const VID_SIG_OLD: [u8; VID_SIG_SIZE] = [
    0x3E, 0xB9, 0xC5, 0xC5, 0x79, 0x47, 0x48, 0xBD, 0x91, 0x3A, 0xAC, 0xEB, 0x28, 0xEB, 0xE0, 0x15,
];
const VID_SIG_START: [u8; VID_SIG_SIZE] = [
    0x87, 0x03, 0xC2, 0x4E, 0x26, 0xCF, 0x4C, 0xC6, 0x97, 0xDD, 0xE2, 0xEC, 0xAE, 0xBE, 0xCD, 0xB4,
];
const VID_SIG_END: [u8; VID_SIG_SIZE] = [
    0x0B, 0x74, 0x52, 0x46, 0x76, 0x09, 0x4D, 0x9F, 0xAF, 0xE5, 0x3F, 0x7E, 0x9B, 0x23, 0x78, 0x0E,
];
const VID_SIG_NOEXT: [u8; VID_SIG_SIZE] = [
    0x3F, 0x78, 0xDE, 0x47, 0xE9, 0x2E, 0x40, 0x65, 0x9A, 0xF1, 0x74, 0xBB, 0xAE, 0x9D, 0x77, 0xD7,
];

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

        let mut addrs: Vec<u64> = Vec::with_capacity(count);
        let mut names: Vec<String> = Vec::with_capacity(count);
        for i in 0..count {
            let off = 4 + i * VID_ENTRY_SIZE as usize;
            if off + VID_ENTRY_SIZE as usize > data.len() {
                break;
            }
            names.push(read_fixed_string(&data, off, VID_NAME_SIZE));
            addrs.push(read_u32_le(&data, off + 40) as u64);
        }

        // RSLod.pas TRSVid.ReadHeader: MMArchive may append a table of sizes
        // after the data, marked by one of two signature layouts.
        let (size_table, mut no_extension) = Self::read_trailer(&data, names.len());

        // TRSVid.Load: names alone also settle the no-extension flag — the
        // first entry decides, an extension-less one turning it on.
        for n in &names {
            let ext = crate::path_utils::get_file_ext(n);
            if ext.is_empty() {
                no_extension = true;
                break;
            }
            if ext.eq_ignore_ascii_case(".smk") {
                break;
            }
        }

        // TRSVid.GetFileSize: a VID entry table has no size field, so an entry
        // runs until the *nearest following* address of ANY other entry (not
        // just the next one in the table — real archives are not stored in
        // entry order), bounded by the size table when there is one.
        let mut entries = Vec::with_capacity(names.len());
        for (i, name) in names.iter().enumerate() {
            let start = addrs[i];
            let mut end = match &size_table {
                Some(t) => start + t[i] as u64,
                None => file_size,
            };
            for (j, &other) in addrs.iter().enumerate() {
                if j != i && other >= start && other < end {
                    end = other;
                }
            }
            entries.push(ArchiveEntry {
                name: name.clone(),
                offset: start,
                size: end.saturating_sub(start) as u32,
                unpacked_size: 0,
                data: None,
                original_data: None,
                packed: false,
            });
        }

        Ok(VidArchive {
            path: path.to_string(),
            // TRSVid has a single on-disk layout; the kind only records
            // whether names keep their extension (mm6vid vs h3mm78vid).
            kind: if no_extension { ArchiveKind::VidMM6 } else { ArchiveKind::VidHeroes },
            entries,
            no_extension,
        })
    }

    /// Read the optional trailing size table, returning it plus the
    /// no-extension marker (RSLod.pas TRSVid.ReadHeader).
    fn read_trailer(data: &[u8], count: usize) -> (Option<Vec<u32>>, bool) {
        let take_sig = |end: usize| -> Option<&[u8]> {
            end.checked_sub(VID_SIG_SIZE).map(|s| &data[s..end])
        };
        let read_table = |start: usize| -> Option<Vec<u32>> {
            let need = count.checked_mul(4)?;
            if start + need > data.len() {
                return None;
            }
            Some((0..count).map(|i| read_u32_le(data, start + i * 4)).collect())
        };

        let tail = match take_sig(data.len()) {
            Some(t) => t,
            None => return (None, false),
        };
        if tail == VID_SIG_OLD {
            if count == 0 {
                return (None, false);
            }
            let start = match data.len().checked_sub(VID_SIG_SIZE + count * 4) {
                Some(v) => v,
                None => return (None, false),
            };
            return (read_table(start), false);
        }
        if tail == VID_SIG_END {
            let start_sig_at = match data.len().checked_sub(VID_SIG_SIZE * 2 + count * 4) {
                Some(v) => v,
                None => return (None, false),
            };
            if take_sig(start_sig_at + VID_SIG_SIZE) != Some(&VID_SIG_START[..]) {
                return (None, false);
            }
            let table = if count != 0 {
                read_table(start_sig_at + VID_SIG_SIZE)
            } else {
                None
            };
            let no_ext = take_sig(start_sig_at) == Some(&VID_SIG_NOEXT[..]);
            return (table, no_ext);
        }
        (None, tail == VID_SIG_NOEXT)
    }

    /// TRSVid.NeedNoExtSig — the marker is only needed when the names alone
    /// would not reveal that this is a no-extension archive.
    fn need_no_ext_sig(&self) -> bool {
        self.no_extension
            && !self
                .entries
                .iter()
                .any(|e| crate::path_utils::get_file_ext(&e.name).is_empty())
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

            // TRSVid.WriteHeader: the data is contiguous after a rebuild, so a
            // size table is never needed; the no-extension marker still is when
            // the names alone would not give it away.
            if self.need_no_ext_sig() {
                out.write_all(&VID_SIG_NOEXT)?;
            }
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
            // MMArchMain.pas: `mm6vid` creates the archive with NoExtension
            // set, `h3mm78vid` without it.
            no_extension: kind == ArchiveKind::VidMM6,
        })
    }
}

pub fn vid_add_file(archive: &mut VidArchive, file_path: &str) -> io::Result<()> {
    let file_data = fs::read(file_path)?;
    let base = crate::path_utils::get_file_name(file_path);
    // RSLod.pas TRSVid.Add: only a no-extension archive drops the `.smk`;
    // otherwise the name is stored as it is (H3 .vid entries keep `.bik`).
    let file_name = if archive.no_extension
        && crate::path_utils::get_file_ext(&base).eq_ignore_ascii_case(".smk")
    {
        crate::path_utils::get_file_stem(&base)
    } else {
        base.clone()
    };

    // Remove existing
    archive.entries.retain(|e| !e.name.eq_ignore_ascii_case(&file_name));

    archive.entries.push(ArchiveEntry {
        name: file_name,
        offset: 0,
        size: file_data.len() as u32,
        unpacked_size: 0,
        original_data: Some(file_data.clone()),
        packed: false,
        data: Some(file_data),
    });

    Ok(())
}
