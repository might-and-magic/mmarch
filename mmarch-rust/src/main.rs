#![allow(dead_code)]
mod archive;
mod checksum;
mod compare;
mod lod;
mod path_utils;
mod script_gen;
mod snd;
mod vid;

use archive::{Archive, ArchiveEntry, ArchiveKind};
use path_utils::*;
use std::fs;
use std::io;

const MMARCH_VERSION: &str = "4.0.0";
const MMARCH_URL: &str = "https://github.com/might-and-magic/mmarch";

// ============================================================
// Dynamic archive loading
// ============================================================

enum DynArchive {
    Lod(lod::LodArchive),
    Snd(snd::SndArchive),
    Vid(vid::VidArchive),
}

impl DynArchive {
    fn load(path: &str) -> io::Result<Self> {
        let ext = get_file_ext(path).to_lowercase();
        match ext.as_str() {
            ".snd" => Ok(DynArchive::Snd(snd::SndArchive::load(path)?)),
            ".vid" => Ok(DynArchive::Vid(vid::VidArchive::load(path)?)),
            _ => Ok(DynArchive::Lod(lod::LodArchive::load(path)?)),
        }
    }

    fn as_archive(&self) -> &dyn Archive {
        match self {
            DynArchive::Lod(a) => a,
            DynArchive::Snd(a) => a,
            DynArchive::Vid(a) => a,
        }
    }

    fn as_archive_mut(&mut self) -> &mut dyn Archive {
        match self {
            DynArchive::Lod(a) => a,
            DynArchive::Snd(a) => a,
            DynArchive::Vid(a) => a,
        }
    }

    fn add_file(&mut self, file_path: &str) -> io::Result<()> {
        match self {
            DynArchive::Lod(a) => lod::lod_add_file(a, file_path),
            DynArchive::Snd(a) => snd::snd_add_file(a, file_path),
            DynArchive::Vid(a) => vid::vid_add_file(a, file_path),
        }
    }

    fn rebuild(&mut self) -> io::Result<()> {
        self.as_archive_mut().rebuild()
    }

    fn create_new(path: &str, kind: ArchiveKind) -> io::Result<Self> {
        let ext = get_file_ext(path).to_lowercase();
        match ext.as_str() {
            ".snd" => Ok(DynArchive::Snd(snd::SndArchive::create_new(path, kind)?)),
            ".vid" => Ok(DynArchive::Vid(vid::VidArchive::create_new(path, kind)?)),
            _ => Ok(DynArchive::Lod(lod::LodArchive::create_new(path, kind)?)),
        }
    }
}

// ============================================================
// CLI
// ============================================================

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        help(false);
        return;
    }

    let method = trim_char_left(&args[1], '-');
    let method_str = method.as_str();

    let result = match method_str {
        "extract" | "e" => check_archive_arg(&args).and_then(|_| cmd_extract(&args)),
        "list" | "l" => check_archive_arg(&args).and_then(|_| cmd_list(&args)),
        "add" | "a" => check_archive_arg(&args).and_then(|_| cmd_add(&args)),
        "delete" | "d" => check_archive_arg(&args).and_then(|_| cmd_delete(&args)),
        "rename" | "r" => check_archive_arg(&args).and_then(|_| cmd_rename(&args)),
        "create" | "c" => check_archive_arg(&args).and_then(|_| cmd_create(&args)),
        "merge" | "m" => check_archive_arg(&args).and_then(|_| cmd_merge(&args)),
        "checksum" | "s" => check_archive_arg(&args).and_then(|_| cmd_checksum(&args)),
        "compare" | "k" => check_archive_arg(&args).and_then(|_| cmd_compare(&args)),
        "optimize" | "o" => check_archive_arg(&args).and_then(|_| cmd_optimize(&args)),
        "diff-files-to-nsis" | "df2n" => cmd_diff_files_to_any(&args, true),
        "diff-files-to-batch" | "df2b" => cmd_diff_files_to_any(&args, false),
        "diff-add-keep" | "dak" => check_archive_arg(&args).and_then(|_| cmd_diff_add_keep(&args)),
        "help" | "h" | "" => {
            help(false);
            Ok(())
        }
        _ => {
            eprintln!("Error: Unknown method: `{}`", method_str);
            println!();
            help(true);
            Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Unknown method: `{}`", method_str),
            ))
        }
    };

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        println!();
        help(true);
        std::process::exit(1);
    }
}

fn check_archive_arg(args: &[String]) -> Result<(), io::Error> {
    if args.len() < 3 || args[2].is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "You must specify an archive file",
        ));
    }
    Ok(())
}

fn get_arg(args: &[String], index: usize) -> String {
    args.get(index).cloned().unwrap_or_default()
}

// ============================================================
// Commands
// ============================================================

fn cmd_extract(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    let extract_to_base = get_arg(args, 3);
    if extract_to_base.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "You must specify a folder (use `.` for current folder)",
        ));
    }

    if archive_file.contains('*') {
        // Batch extraction
        let archive_list = wildcard_archive_name_to_archive_list(archive_file);
        for (name, folder) in &archive_list {
            let full_path = format!("{}{}", with_trailing_slash(folder), name);
            match DynArchive::load(&full_path) {
                Ok(arch) => {
                    let extract_to = format!(
                        "{}{}{}{}",
                        with_trailing_slash(&extract_to_base),
                        with_trailing_slash(folder),
                        name,
                        MMARCHIVE_EXT
                    );
                    let file_filter = get_arg(args, 4);
                    if file_filter.is_empty() {
                        extract_all(arch.as_archive(), &extract_to, "*");
                    } else {
                        for i in 4..args.len() {
                            let fname = &args[i];
                            if fname.contains('*') {
                                extract_all(arch.as_archive(), &extract_to, &wildcard_filename_to_ext(fname));
                            } else {
                                if let Err(e) = extract_single(arch.as_archive(), &extract_to, fname) {
                                    eprintln!(
                                        "File `{}` in archive `{}` error:",
                                        beautify_path(fname),
                                        beautify_path(&full_path)
                                    );
                                    eprintln!("{}", e);
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!(
                        "Archive file `{}` (or perhaps not an MM Archive file) error:",
                        beautify_path(&full_path)
                    );
                    eprintln!("{}", e);
                }
            }
        }
    } else {
        let arch = DynArchive::load(archive_file)?;
        let file_filter = get_arg(args, 4);
        if file_filter.is_empty() {
            extract_all(arch.as_archive(), &extract_to_base, "*");
        } else {
            let mut not_found_count = 0u32;
            for i in 4..args.len() {
                let fname = &args[i];
                if fname.contains('*') {
                    extract_all(arch.as_archive(), &extract_to_base, &wildcard_filename_to_ext(fname));
                } else {
                    if let Err(e) = extract_single(arch.as_archive(), &extract_to_base, fname) {
                        eprintln!(
                            "File `{}` in archive `{}` error:",
                            beautify_path(fname),
                            beautify_path(archive_file)
                        );
                        eprintln!("{}", e);
                        not_found_count += 1;
                    }
                }
            }
            if not_found_count > 0 {
                return Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("{} file(s) not found in the archive", not_found_count),
                ));
            }
        }
    }
    Ok(())
}

fn extract_all(arch: &dyn Archive, folder: &str, ext: &str) {
    let _ = create_dir_recur(folder);
    let entries = arch.entries();
    for i in 0..entries.len() {
        let entry = &entries[i];
        let entry_ext = get_file_ext(&entry.name);

        let matches = if ext == "*" {
            true
        } else if ext.is_empty() {
            entry_ext.is_empty()
        } else {
            entry_ext.eq_ignore_ascii_case(ext)
                || check_extracted_ext_match(arch.kind(), &entry.name, ext)
        };

        if matches {
            if let Err(e) = extract_entry_to_folder(arch, i, folder) {
                eprintln!(
                    "File `{}` in archive `{}` error:",
                    beautify_path(&entry.name),
                    beautify_path(arch.file_path())
                );
                eprintln!("{}", e);
            }
        }
    }
}

fn check_extracted_ext_match(kind: ArchiveKind, in_archive_name: &str, requested_ext: &str) -> bool {
    // Check if the requested extracted ext maps to this in-archive name
    if let Some(in_ext) = kind.in_archive_ext(requested_ext) {
        let in_archive_ext = get_file_ext(in_archive_name);
        in_archive_ext.eq_ignore_ascii_case(in_ext)
    } else {
        false
    }
}

fn extract_entry_to_folder(arch: &dyn Archive, index: usize, folder: &str) -> io::Result<()> {
    let _ = create_dir_recur(folder);
    let entry = &arch.entries()[index];
    let data = arch.read_entry_data(index)?;

    // Determine extracted file name (may depend on data for palette detection)
    let extracted_name = get_extracted_name(arch.kind(), &entry.name, Some(&data));
    let dest_path = format!("{}{}", with_trailing_slash(folder), extracted_name);
    fs::write(&dest_path, &data)?;
    Ok(())
}

fn get_extracted_name(kind: ArchiveKind, in_archive_name: &str, data: Option<&[u8]>) -> String {
    match kind {
        ArchiveKind::SndHeroes | ArchiveKind::SndMM => {
            let stem = get_file_stem(in_archive_name);
            if get_file_ext(in_archive_name).is_empty() {
                format!("{}.wav", stem)
            } else {
                in_archive_name.to_string()
            }
        }
        ArchiveKind::VidHeroes | ArchiveKind::VidMM6 => {
            let stem = get_file_stem(in_archive_name);
            if get_file_ext(in_archive_name).is_empty() {
                format!("{}.smk", stem)
            } else {
                in_archive_name.to_string()
            }
        }
        ArchiveKind::LodHeroes => {
            let ext = get_file_ext(in_archive_name).to_lowercase();
            if ext == ".pcx" {
                format!("{}.bmp", get_file_stem(in_archive_name))
            } else {
                in_archive_name.to_string()
            }
        }
        ArchiveKind::LodBitmaps | ArchiveKind::LodIcons | ArchiveKind::LodMM8 => {
            // Files without extension: .act if palette (768 bytes), .bmp otherwise
            if get_file_ext(in_archive_name).is_empty() {
                let is_palette = data.map_or(false, |d| d.len() == 768);
                if is_palette {
                    format!("{}.act", in_archive_name)
                } else {
                    format!("{}.bmp", in_archive_name)
                }
            } else {
                in_archive_name.to_string()
            }
        }
        ArchiveKind::LodSprites => {
            // Sprites: files without extension get .bmp
            if get_file_ext(in_archive_name).is_empty() {
                format!("{}.bmp", in_archive_name)
            } else {
                in_archive_name.to_string()
            }
        }
        _ => in_archive_name.to_string(),
    }
}

fn extract_single(arch: &dyn Archive, folder: &str, file_to_extract: &str) -> io::Result<()> {
    match arch.find_entry(file_to_extract) {
        Some(idx) => extract_entry_to_folder(arch, idx, folder),
        None => Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("File `{}` is not found in the archive", file_to_extract),
        )),
    }
}

fn cmd_list(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    let separator = if args.len() > 3 && !args[3].is_empty() {
        args[3].clone()
    } else {
        "\r\n".to_string()
    };
    let arch = DynArchive::load(archive_file)?;
    print!("{}", arch.as_archive().list(&separator));
    Ok(())
}

fn cmd_add(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    if args.len() < 4 || args[3].is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "You must specify at least one file to add",
        ));
    }
    let mut arch = DynArchive::load(archive_file)?;
    add_proc(&mut arch, args, 3)?;
    Ok(())
}

fn add_proc(arch: &mut DynArchive, args: &[String], from: usize) -> io::Result<()> {
    let mut i = from;
    while i < args.len() {
        let file_path = &args[i];
        if !file_path.is_empty() {
            let file_name = get_file_name(file_path);
            if file_name.contains('*') {
                let folder = get_file_dir(file_path);
                let folder = if folder.is_empty() { ".".to_string() } else { folder };
                let ext = wildcard_filename_to_ext(&file_name);
                add_all(arch, &folder, &ext)?;
            } else {
                // Check for /p PALETTE_INDEX
                if i + 2 < args.len() && args[i + 1].eq_ignore_ascii_case("/p") {
                    // Palette index specified - just add the file (palette support is a no-op in basic impl)
                    let _palette_index: i32 = args[i + 2].parse().unwrap_or(0);
                    if let Err(e) = arch.add_file(file_path) {
                        eprintln!("File `{}` error: {}", beautify_path(file_path), e);
                    }
                    i += 2;
                } else {
                    if let Err(e) = arch.add_file(file_path) {
                        eprintln!("File `{}` error: {}", beautify_path(file_path), e);
                    }
                }
            }
        }
        i += 1;
    }
    arch.rebuild()?;
    Ok(())
}

fn add_all(arch: &mut DynArchive, folder: &str, ext: &str) -> io::Result<()> {
    let files = get_all_files_in_folder(folder, ext, false);
    for fname in &files {
        let full_path = format!("{}{}", with_trailing_slash(folder), fname);
        if let Err(e) = arch.add_file(&full_path) {
            eprintln!("File `{}` error: {}", beautify_path(fname), e);
        }
    }
    Ok(())
}

fn cmd_delete(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    if args.len() < 4 || args[3].is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "You must specify at least one file to delete",
        ));
    }
    let mut arch = DynArchive::load(archive_file)?;
    let mut not_found_count = 0u32;

    for i in 3..args.len() {
        let fname = &args[i];
        if !fname.is_empty() {
            if fname.contains('*') {
                let ext = wildcard_filename_to_ext(fname);
                delete_all(&mut arch, &ext);
            } else {
                match arch.as_archive().find_entry(fname) {
                    Some(idx) => {
                        arch.as_archive_mut().delete_entry(idx);
                    }
                    None => {
                        eprintln!(
                            "File `{}` error: File `{}` is not found in the archive",
                            beautify_path(fname),
                            fname
                        );
                        not_found_count += 1;
                    }
                }
            }
        }
    }
    arch.rebuild()?;
    if not_found_count > 0 {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("{} file(s) not found in the archive", not_found_count),
        ));
    }
    Ok(())
}

fn delete_all(arch: &mut DynArchive, ext: &str) {
    let entries = arch.as_archive().entries();
    let mut to_remove: Vec<usize> = Vec::new();
    for (i, entry) in entries.iter().enumerate() {
        let entry_ext = get_file_ext(&entry.name);
        let matches = if ext == "*" {
            true
        } else if ext.is_empty() {
            entry_ext.is_empty()
        } else {
            entry_ext.eq_ignore_ascii_case(ext)
        };
        if matches {
            to_remove.push(i);
        }
    }
    // Remove in reverse order
    for idx in to_remove.into_iter().rev() {
        arch.as_archive_mut().delete_entry(idx);
    }
}

fn cmd_rename(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    let old_name = get_arg(args, 3);
    let new_name = get_arg(args, 4);
    if old_name.is_empty() || new_name.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "You must specify a file to rename, and a file name that the file will be renamed to",
        ));
    }
    let mut arch = DynArchive::load(archive_file)?;
    match arch.as_archive().find_entry(&old_name) {
        Some(idx) => {
            arch.as_archive_mut().rename_entry(idx, &new_name);
            arch.rebuild()?;
        }
        None => {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("File `{}` is not found in the archive", old_name),
            ));
        }
    }
    Ok(())
}

fn cmd_create(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    let archive_type = get_arg(args, 3);
    let folder = get_arg(args, 4);
    if archive_type.is_empty() || folder.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "You must specify a type of your archive file that will be created and a folder (use `.` for current folder)",
        ));
    }

    let kind = match archive_type.to_lowercase().as_str() {
        "h3lod" => ArchiveKind::LodHeroes,
        "h3snd" => ArchiveKind::SndHeroes,
        "mmsnd" => ArchiveKind::SndMM,
        "h3mm78vid" => ArchiveKind::VidHeroes,
        "mm6vid" => ArchiveKind::VidMM6,
        "mmbitmapslod" => ArchiveKind::LodBitmaps,
        "mmiconslod" => ArchiveKind::LodIcons,
        "mmspriteslod" => ArchiveKind::LodSprites,
        "mm8loclod" => ArchiveKind::LodMM8,
        "mm78gameslod" => ArchiveKind::LodGames7,
        "mm6gameslod" => ArchiveKind::LodGames,
        "mm78save" => ArchiveKind::LodChapter7,
        "mm6save" => ArchiveKind::LodChapter,
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Unknown archive type: {}", archive_type),
            ));
        }
    };

    let full_path = format!("{}{}", with_trailing_slash(&folder), archive_file);
    let mut arch = DynArchive::create_new(&full_path, kind)?;
    if args.len() > 5 {
        add_proc(&mut arch, args, 5)?;
    } else {
        arch.rebuild()?;
    }
    Ok(())
}

fn cmd_merge(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    let archive_file2 = get_arg(args, 3);
    if archive_file2.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "You must specify two archive files to be merged together",
        ));
    }

    let mut arch1 = DynArchive::load(archive_file)?;
    let arch2 = DynArchive::load(&archive_file2)?;

    // For each file in archive2, add/replace in archive1
    let entries2 = arch2.as_archive().entries().to_vec();
    for (i, entry) in entries2.iter().enumerate() {
        let data = arch2.as_archive().read_entry_data(i)?;

        // Remove existing with same name
        if let Some(idx) = arch1.as_archive().find_entry(&entry.name) {
            arch1.as_archive_mut().delete_entry(idx);
        }

        arch1.as_archive_mut().entries_mut().push(ArchiveEntry {
            name: entry.name.clone(),
            offset: 0,
            size: entry.size,
            unpacked_size: entry.unpacked_size,
            data: Some(if entry.is_packed() {
                // Store the raw (packed) data
                let mut f = std::fs::File::open(arch2.as_archive().file_path())?;
                use std::io::{Read, Seek, SeekFrom};
                f.seek(SeekFrom::Start(entry.offset))?;
                let mut buf = vec![0u8; entry.size as usize];
                f.read_exact(&mut buf)?;
                buf
            } else {
                data.clone()
            }),
            original_data: Some(data),
        });
    }

    arch1.rebuild()?;
    Ok(())
}

fn cmd_compare(args: &[String]) -> io::Result<()> {
    let path1 = &args[2];
    let path2 = get_arg(args, 3);
    if path2.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "You must specify two archive files or folders to be compared",
        ));
    }

    let option = get_arg(args, 4);

    match option.as_str() {
        "nsis" => {
            let script_path = get_arg(args, 5);
            let diff_folder_name = get_arg(args, 6);
            if script_path.is_empty() || diff_folder_name.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Insufficient parameters",
                ));
            }
            let result = compare::compare_base(
                path1,
                &path2,
                &format!("{}{}", with_trailing_slash(&get_file_dir(&script_path)), &diff_folder_name),
                true,
            );
            if !result.same {
                script_gen::generate_script(
                    &result.deleted_folders,
                    &result.deleted_non_res_files,
                    &result.deleted_res_files,
                    &result.modified_archives,
                    &script_path,
                    &diff_folder_name,
                    true,
                );
            }
        }
        "batch" => {
            let script_path = get_arg(args, 5);
            let diff_folder_name = get_arg(args, 6);
            if script_path.is_empty() || diff_folder_name.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Insufficient parameters",
                ));
            }
            let result = compare::compare_base(
                path1,
                &path2,
                &format!("{}{}", with_trailing_slash(&get_file_dir(&script_path)), &diff_folder_name),
                true,
            );
            if !result.same {
                script_gen::generate_script(
                    &result.deleted_folders,
                    &result.deleted_non_res_files,
                    &result.deleted_res_files,
                    &result.modified_archives,
                    &script_path,
                    &diff_folder_name,
                    false,
                );
            }
        }
        "filesonly" => {
            let diff_folder = get_arg(args, 5);
            if diff_folder.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Insufficient parameters",
                ));
            }
            compare::compare_base(path1, &path2, &diff_folder, false);
        }
        "" => {
            compare::compare_base(path1, &path2, "", false);
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Unknown compare option: `{}`", option),
            ));
        }
    }

    Ok(())
}

fn cmd_optimize(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    let mut arch = DynArchive::load(archive_file)?;
    arch.rebuild()?;
    Ok(())
}

fn cmd_diff_files_to_any(args: &[String], is_nsis: bool) -> io::Result<()> {
    let old_diff_folder = get_arg(args, 2);
    let script_file_path = get_arg(args, 3);
    let diff_folder_name = get_arg(args, 4);

    if old_diff_folder.is_empty() || script_file_path.is_empty() || diff_folder_name.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Insufficient parameters",
        ));
    }

    let old_diff_folder = beautify_path(&old_diff_folder);

    // Check if folder exists and is non-empty
    if !std::path::Path::new(&old_diff_folder).is_dir() {
        println!("Folder `{}` is empty", old_diff_folder);
        return Ok(());
    }

    let files = get_all_files_in_folder(&old_diff_folder, "*", false);
    let dirs = get_all_files_in_folder(&old_diff_folder, "*", true);
    if files.is_empty() && dirs.is_empty() {
        println!("Folder `{}` is empty", old_diff_folder);
        return Ok(());
    }

    let script_file_path = beautify_path(&script_file_path);
    let script_file_folder = get_file_dir(&script_file_path);

    let mut deleted_folders = Vec::new();
    let mut deleted_non_res = Vec::new();
    let mut deleted_res = Vec::new();
    let mut modified_archives = Vec::new();

    compare::get_list_from_diff_files(
        &old_diff_folder,
        &mut deleted_folders,
        &mut deleted_non_res,
        &mut deleted_res,
        &mut modified_archives,
    );

    let new_diff_folder = format!(
        "{}{}",
        with_trailing_slash(&script_file_folder),
        beautify_path(&diff_folder_name)
    );

    let mut should_generate = true;
    if !old_diff_folder.eq_ignore_ascii_case(&new_diff_folder) {
        should_generate = move_dir(&old_diff_folder, &new_diff_folder);
    }

    if should_generate {
        script_gen::generate_script(
            &deleted_folders,
            &deleted_non_res,
            &deleted_res,
            &modified_archives,
            &script_file_path,
            &diff_folder_name,
            is_nsis,
        );
    }

    Ok(())
}

fn cmd_diff_add_keep(args: &[String]) -> io::Result<()> {
    let folder = &args[2];
    add_keep_to_all_empty_folders_recur(folder);
    Ok(())
}

fn cmd_checksum(args: &[String]) -> io::Result<()> {
    let archive_file = &args[2];
    let arg3 = get_arg(args, 3);

    // Check for verify mode: /v or /vall
    if arg3.eq_ignore_ascii_case("/v") || arg3.eq_ignore_ascii_case("/vall") {
        let verify_all = arg3.eq_ignore_ascii_case("/vall");
        let remaining: Vec<String> = args[4..].to_vec();
        if remaining.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "You must specify a checksum file or inline name:hash pairs",
            ));
        }

        // Detect inline mode: first arg contains ":" but is not a file path.
        // A Windows drive letter like "C:\..." has ":" at position 1, so we
        // check that ":" exists and is NOT at position 1.
        let is_inline = {
            let first = &remaining[0];
            match first.find(':') {
                Some(pos) => pos != 1,
                None => false,
            }
        };
        let pairs = if is_inline {
            checksum::parse_inline_pairs(&remaining)
        } else {
            // File mode
            let content = fs::read_to_string(&remaining[0])?;
            checksum::parse_checksum_file(&content)
        };

        let arch = DynArchive::load(archive_file)?;
        let ok = checksum::verify_checksums(arch.as_archive(), &pairs, verify_all);
        if !ok {
            std::process::exit(1);
        }
        return Ok(());
    }

    // Generate mode
    if arg3.is_empty() {
        // CRC32 of the archive file itself
        let output = checksum::format_archive_checksum(archive_file)?;
        print!("{}", output);
        return Ok(());
    }

    // CRC32 of resources
    let filters: Vec<String> = args[3..].to_vec();
    let arch = DynArchive::load(archive_file)?;
    let results = checksum::checksum_resources(arch.as_archive(), &filters)?;
    for (name, crc) in &results {
        print!("{}", checksum::format_checksum_line(name, *crc));
    }
    Ok(())
}

// ============================================================
// Help
// ============================================================

fn help(short: bool) {
    println!("mmarch Version {} Usage:", MMARCH_VERSION);
    println!();
    println!("mmarch extract <ARCHIVE_FILE> <FOLDER> [FILE_TO_EXTRACT_1] [FILE_TO_EXTRACT_2] [...]");
    println!("mmarch list <ARCHIVE_FILE> [SEPARATOR]");
    println!("mmarch add <ARCHIVE_FILE> <FILE_TO_ADD_1> [FILE_TO_ADD_2] [...]");
    println!("mmarch delete <ARCHIVE_FILE> <FILE_TO_DELETE_1> [FILE_TO_DELETE_2] [...]");
    println!("mmarch rename <ARCHIVE_FILE> <OLD_FILE_NAME> <NEW_FILE_NAME>");
    println!("mmarch create <ARCHIVE_FILE> <ARCHIVE_FILE_TYPE> <FOLDER> [FILE_TO_ADD_1] [FILE_TO_ADD_2] [...]");
    println!("mmarch merge <ARCHIVE_FILE> <ARCHIVE_FILE_2>");
    println!("mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2>");
    println!("mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2> {{nsis|batch}} <SCRIPT_FILE> <DIFF_FOLDER_NAME>");
    println!("mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2> filesonly <DIFF_FOLDER>");
    println!("mmarch diff-files-to-{{nsis|batch}} <OLD_DIFF_FOLDER> <SCRIPT_FILE> <DIFF_FOLDER_NAME>");
    println!("mmarch diff-add-keep <DIFF_FOLDER>");
    println!("mmarch checksum <ARCHIVE_FILE>");
    println!("mmarch checksum <ARCHIVE_FILE> [FILE_1] [FILE_2] [...]");
    println!("mmarch checksum <ARCHIVE_FILE> /v[all] <CRC32_FILE>");
    println!("mmarch checksum <ARCHIVE_FILE> /v[all] <name1:HASH1> [name2:HASH2] [...]");
    println!("mmarch optimize <ARCHIVE_FILE>");
    println!("mmarch help");

    if !short {
        println!();
        println!("(`<>`: required; `[]`: optional; `|`: or):");
        println!();
        println!("- Initial letter of the first argument can be used (e.g. `e` for `extract`)");
        println!("- File names are case-insensitive");
        println!("- You can use the following notations");
        println!();
        println!("FOLDER: ");
        println!("            .    current folder");
        println!();
        println!("FILE_TO_XX_?: ");
        println!("*.*   |     *    all files");
        println!("        *.txt    all files with specified extension");
        println!("           *.    all files without extension");
        println!();
        println!("ARCHIVE_FILE in `mmarch extract`: ");
        println!("File path:");
        println!("           **    all directories recursively");
        println!("            *    any ONE directory");
        println!("File name:");
        println!("*.*   |     *    all supported archive files");
        println!("        *.lod    all supported archive files with specified extension");
        println!("*.lod|lwd|vid    all supported archive files with any of specified extensions");
        println!();
        println!("Read README.md file or go to the following page for more details and examples:");
        println!("{}", MMARCH_URL);
    }
}
