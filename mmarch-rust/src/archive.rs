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
                if ext == ".wav" || ext == ".bmp" {
                    if ext == ".bmp" {
                        Some(".pcx")
                    } else {
                        Some("") // no extension for wav in H3 snd
                    }
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
            _ => None,
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
        // Try with extension mapping
        let ext = crate::path_utils::get_file_ext(name);
        if let Some(in_arch_ext) = self.kind().in_archive_ext(&ext) {
            let stem = &name[..name.len() - ext.len()];
            let mapped_name = format!("{}{}", stem, in_arch_ext);
            for (i, e) in entries.iter().enumerate() {
                if e.name.eq_ignore_ascii_case(&mapped_name) {
                    return Some(i);
                }
            }
        }
        None
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
