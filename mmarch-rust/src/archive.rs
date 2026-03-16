#![allow(dead_code)]
use std::io;

/// Represents one file entry within an archive.
#[derive(Clone, Debug)]
pub struct ArchiveEntry {
    pub name: String,
    pub offset: u64,       // absolute offset in archive file
    pub size: u32,         // packed/stored size
    pub unpacked_size: u32,
    pub data: Option<Vec<u8>>, // in-memory stored/wrapped data for new/modified entries (used by rebuild)
    pub original_data: Option<Vec<u8>>, // original unwrapped file data (used by read_entry_data)
}

impl ArchiveEntry {
    pub fn is_packed(&self) -> bool {
        self.unpacked_size != 0 && self.unpacked_size != self.size
    }
}

/// The kind of archive, determines binary layout.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ArchiveKind {
    LodHeroes,
    LodBitmaps,
    LodIcons,
    LodSprites,
    LodGames,
    LodGames7,
    LodChapter,
    LodChapter7,
    LodMM8,
    SndHeroes,
    SndMM,
    VidHeroes,
    VidMM6,
}

impl ArchiveKind {
    pub fn is_lod(&self) -> bool {
        matches!(
            self,
            ArchiveKind::LodHeroes
                | ArchiveKind::LodBitmaps
                | ArchiveKind::LodIcons
                | ArchiveKind::LodSprites
                | ArchiveKind::LodGames
                | ArchiveKind::LodGames7
                | ArchiveKind::LodChapter
                | ArchiveKind::LodChapter7
                | ArchiveKind::LodMM8
        )
    }

    pub fn is_snd(&self) -> bool {
        matches!(self, ArchiveKind::SndHeroes | ArchiveKind::SndMM)
    }

    pub fn is_vid(&self) -> bool {
        matches!(self, ArchiveKind::VidHeroes | ArchiveKind::VidMM6)
    }

    /// What extension does this kind add when extracting files?
    pub fn extracted_ext_for(&self, in_archive_name: &str) -> Option<&'static str> {
        let ext = crate::path_utils::get_file_ext(in_archive_name).to_lowercase();
        match self {
            ArchiveKind::SndHeroes | ArchiveKind::SndMM => Some(".wav"),
            ArchiveKind::VidHeroes | ArchiveKind::VidMM6 => Some(".smk"),
            ArchiveKind::LodBitmaps | ArchiveKind::LodIcons => {
                // files without extension get .bmp, palette files get .act
                None // handled specially
            }
            ArchiveKind::LodSprites | ArchiveKind::LodMM8 => None,
            ArchiveKind::LodHeroes => {
                if ext == ".pcx" {
                    Some(".bmp")
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Map extracted extension back to in-archive extension.
    pub fn in_archive_ext(&self, extracted_ext: &str) -> Option<&'static str> {
        let ext = extracted_ext.to_lowercase();
        match self {
            ArchiveKind::LodHeroes => {
                if ext == ".bmp" {
                    Some(".pcx")
                } else if ext == ".wav" {
                    Some("")
                } else {
                    None
                }
            }
            ArchiveKind::LodBitmaps | ArchiveKind::LodIcons => {
                if ext == ".bmp" || ext == ".act" {
                    Some("")
                } else {
                    None
                }
            }
            ArchiveKind::LodSprites | ArchiveKind::LodMM8 => {
                if ext == ".bmp" {
                    Some("")
                } else {
                    None
                }
            }
            ArchiveKind::LodGames | ArchiveKind::LodGames7 => {
                if ext == ".wav" || ext == ".smk" {
                    Some("")
                } else {
                    None
                }
            }
            ArchiveKind::SndHeroes | ArchiveKind::SndMM => {
                if ext == ".wav" {
                    Some("")
                } else {
                    None
                }
            }
            ArchiveKind::VidHeroes | ArchiveKind::VidMM6 => {
                if ext == ".smk" {
                    Some("")
                } else {
                    None
                }
            }
            ArchiveKind::LodChapter | ArchiveKind::LodChapter7 => None,
        }
    }
}

/// Trait for archive format implementations.
pub trait Archive {
    fn kind(&self) -> ArchiveKind;
    fn file_path(&self) -> &str;
    fn entries(&self) -> &[ArchiveEntry];
    fn entries_mut(&mut self) -> &mut Vec<ArchiveEntry>;

    /// Read file data for the entry at the given index.
    fn read_entry_data(&self, index: usize) -> io::Result<Vec<u8>>;

    /// Rebuild (optimize) the archive: write all data sequentially.
    fn rebuild(&mut self) -> io::Result<()>;

    /// Create a new empty archive file.
    fn create_new(path: &str, kind: ArchiveKind) -> io::Result<Self>
    where
        Self: Sized;

    fn find_entry(&self, name: &str) -> Option<usize> {
        let entries = self.entries();
        // Try exact case-insensitive match first
        for (i, e) in entries.iter().enumerate() {
            if e.name.eq_ignore_ascii_case(name) {
                return Some(i);
            }
        }
        // Try with extension mapping (e.g., .bmp/.act -> extensionless, .wav -> extensionless)
        let ext = crate::path_utils::get_file_ext(name);
        if let Some(in_arch_ext) = self.kind().in_archive_ext(&ext) {
            let stem = &name[..name.len() - ext.len()];
            let mapped_name = format!("{}{}", stem, in_arch_ext);
            for (i, e) in entries.iter().enumerate() {
                if e.name.eq_ignore_ascii_case(&mapped_name) {
                    // Verify the extracted extension matches (for .bmp/.act disambiguation)
                    if self.verify_extracted_ext(i, &ext) {
                        return Some(i);
                    }
                }
            }
        }
        None
    }

    /// Check whether the entry at the given index, when extracted, would have
    /// the given extension. Needed to distinguish .bmp from .act in bitmaps/icons LODs.
    fn verify_extracted_ext(&self, index: usize, requested_ext: &str) -> bool {
        let kind = self.kind();
        let entry = &self.entries()[index];
        let in_ext = crate::path_utils::get_file_ext(&entry.name);

        match kind {
            // For bitmaps/icons/MM8: extensionless entries are .act if 768 bytes (palette), .bmp otherwise
            ArchiveKind::LodBitmaps | ArchiveKind::LodIcons | ArchiveKind::LodMM8 => {
                if !in_ext.is_empty() {
                    // Entry has an extension — extracted name keeps it
                    return in_ext.eq_ignore_ascii_case(requested_ext);
                }
                // Extensionless entry: check data size to determine .bmp vs .act
                // We read the data to check, but only if disambiguation is needed
                let req = requested_ext.to_lowercase();
                if req == ".act" {
                    // .act = palette file = exactly 768 bytes extracted
                    if let Ok(data) = self.read_entry_data(index) {
                        data.len() == 768
                    } else {
                        false
                    }
                } else if req == ".bmp" {
                    // .bmp = anything that's NOT 768 bytes
                    if let Ok(data) = self.read_entry_data(index) {
                        data.len() != 768
                    } else {
                        true
                    }
                } else {
                    true
                }
            }
            // For sprites: extensionless -> .bmp, no ambiguity
            ArchiveKind::LodSprites => true,
            // For SND: extensionless -> .wav, no ambiguity
            ArchiveKind::SndHeroes | ArchiveKind::SndMM => true,
            // For VID: extensionless -> .smk, no ambiguity
            ArchiveKind::VidHeroes | ArchiveKind::VidMM6 => true,
            // For H3 LOD: .pcx -> .bmp
            ArchiveKind::LodHeroes => true,
            // Default: accept
            _ => true,
        }
    }

    /// Get the extracted file name for an entry (with proper extension mapping).
    fn get_extracted_name(&self, index: usize) -> String {
        let entry = &self.entries()[index];
        let kind = self.kind();
        let in_name = &entry.name;
        let in_ext = crate::path_utils::get_file_ext(in_name);

        match kind {
            ArchiveKind::SndHeroes | ArchiveKind::SndMM => {
                if in_ext.is_empty() {
                    format!("{}.wav", in_name)
                } else {
                    in_name.clone()
                }
            }
            ArchiveKind::VidHeroes | ArchiveKind::VidMM6 => {
                if in_ext.is_empty() {
                    format!("{}.smk", in_name)
                } else {
                    in_name.clone()
                }
            }
            ArchiveKind::LodHeroes => {
                if in_ext.eq_ignore_ascii_case(".pcx") {
                    let stem = crate::path_utils::get_file_stem(in_name);
                    format!("{}.bmp", stem)
                } else {
                    in_name.clone()
                }
            }
            ArchiveKind::LodBitmaps | ArchiveKind::LodIcons | ArchiveKind::LodMM8 => {
                if in_ext.is_empty() {
                    // Check if palette (768 bytes) -> .act, otherwise .bmp
                    let is_palette = self.read_entry_data(index)
                        .map(|d| d.len() == 768)
                        .unwrap_or(false);
                    if is_palette {
                        format!("{}.act", in_name)
                    } else {
                        format!("{}.bmp", in_name)
                    }
                } else {
                    in_name.clone()
                }
            }
            ArchiveKind::LodSprites => {
                if in_ext.is_empty() {
                    format!("{}.bmp", in_name)
                } else {
                    in_name.clone()
                }
            }
            _ => in_name.clone(),
        }
    }

    fn list(&self, separator: &str) -> String {
        let entries = self.entries();
        let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();
        names.join(separator)
    }

    fn delete_entry(&mut self, index: usize) {
        self.entries_mut().remove(index);
    }

    fn rename_entry(&mut self, index: usize, new_name: &str) {
        self.entries_mut()[index].name = new_name.to_string();
    }
}
