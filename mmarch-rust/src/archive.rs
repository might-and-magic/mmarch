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
    /// RSPak's `IsPacked`, as the format defines it rather than as the two size
    /// fields happen to compare.
    ///
    /// For a Heroes LOD it is "the packed-size field is non-zero"
    /// (`TRSMMFiles.GetIsPacked` with `PackedSizeOffset >= 0`), and that is not
    /// the same as "the sizes differ": Heroes archives really do contain
    /// resources that compress to exactly their original length —
    /// `Lcdesc.txt` in `H3ab_bmp.lod` and `AVLXsu12.def` in `H3ab_spr.lod` are
    /// both 1683 and 54 bytes either way — and guessing left those two stored
    /// compressed on disk.
    ///
    /// For an MM SND it is `Size <> UnpackedSize`, and for every MM LOD it is
    /// false: what is compressed there is described by the per-resource data
    /// header, not by the entry table.
    pub packed: bool,
}

impl ArchiveEntry {
    pub fn is_packed(&self) -> bool {
        self.packed
    }
}

/// How RSPak's TRSLod.DoExtract classifies one entry of a bitmaps/icons/MM8
/// LOD, and therefore which extension the extracted file gets. The TMMLodFile
/// header decides this, not the size of the decoded output: MM6 patch archives
/// store palettes as `BmpSize = 0, DataSize = 768` rather than
/// `DataSize = 0`, and RSPak leaves those entries' names alone.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TmmClass {
    /// BmpSize != 0 — an image; RSPak appends `.bmp`.
    Bmp,
    /// DataSize == 0 and the entry is big enough — a palette; `.act`.
    Act,
    /// Anything else — the name is used as it stands.
    Plain,
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
// Sync supertrait: archives are plain data + on-demand file reads, and
// extract_all() decompresses entries on all cores (rayon)
pub trait Archive: Sync {
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
        // Last resort, matching MMArchMain.pas getIndexByFileName: compare
        // against the name the entry would be extracted under, with or without
        // its extension. This is what lets `delete clip2` reach an entry stored
        // as `clip2.smk` in a .vid, or `Azurattk` reach `Azurattk` in a .snd
        // that extracts as `Azurattk.wav`.
        for i in 0..entries.len() {
            let extracted = self.get_extracted_name(i);
            if extracted.eq_ignore_ascii_case(name) {
                return Some(i);
            }
            let stem = &extracted[..extracted.len() - crate::path_utils::get_file_ext(&extracted).len()];
            if stem.eq_ignore_ascii_case(name) {
                return Some(i);
            }
        }
        None
    }

    /// Classify a bitmaps/icons/MM8 entry the way TRSLod.DoExtract does.
    /// Only LodArchive can answer this; everything else has no TMMLodFile
    /// header to read.
    fn tmmlodfile_class(&self, _index: usize) -> TmmClass {
        TmmClass::Plain
    }

    /// Check whether the entry at the given index, when extracted, would have
    /// the given extension. Needed to distinguish .bmp from .act in bitmaps/icons LODs.
    fn verify_extracted_ext(&self, index: usize, requested_ext: &str) -> bool {
        let kind = self.kind();
        let entry = &self.entries()[index];
        let in_ext = crate::path_utils::get_file_ext(&entry.name);

        match kind {
            // bitmaps/icons/MM8: the TMMLodFile header decides.
            ArchiveKind::LodBitmaps | ArchiveKind::LodIcons | ArchiveKind::LodMM8 => {
                let ext = crate::path_utils::get_file_ext(&self.get_extracted_name(index));
                let _ = in_ext;
                ext.eq_ignore_ascii_case(requested_ext)
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
            // RSLod.pas TRSSnd.GetExtractName appends `.wav` unconditionally,
            // so an entry stored as `death.old` comes out `death.old.wav`.
            ArchiveKind::SndHeroes | ArchiveKind::SndMM => format!("{}.wav", in_name),
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
            // TRSLod.DoExtract appends the suffix to the *whole* stored name,
            // driven by the TMMLodFile header.
            //
            // For an entry the header calls neither a bitmap nor a palette,
            // RSPak leaves the name alone. mmarch instead keeps its documented
            // "no extension -> .bmp / .act" mapping (README, "In-archive and
            // extracted extension difference"), because the Rust port stores
            // added .bmp files as generic data (BmpSize = 0) and they have to
            // come back out under the name they went in with. The two only
            // disagree on entries no shipped archive contains: MM6 fan patches
            // store a few palettes as DataSize = 768 rather than 0, and mmarch
            // names those pal165.act where RSPak would say pal165.
            ArchiveKind::LodBitmaps | ArchiveKind::LodIcons | ArchiveKind::LodMM8 => {
                match self.tmmlodfile_class(index) {
                    TmmClass::Bmp => format!("{}.bmp", in_name),
                    TmmClass::Act => format!("{}.act", in_name),
                    TmmClass::Plain if in_ext.is_empty() => {
                        let is_palette = self
                            .read_entry_data(index)
                            .map(|d| d.len() == 768)
                            .unwrap_or(false);
                        if is_palette {
                            format!("{}.act", in_name)
                        } else {
                            format!("{}.bmp", in_name)
                        }
                    }
                    TmmClass::Plain => in_name.clone(),
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
