use crate::archive::Archive;
use crate::path_utils::*;
use std::fs;
use std::io;

const COLOR_GREEN: u16 = 0x0A;
const COLOR_RED: u16 = 0x0C;
const COLOR_YELLOW: u16 = 0x0E;
const COLOR_BG_BLUE: u16 = 0x10;

fn color_writeln(msg: &str, color: u16) {
    #[cfg(windows)]
    {
        let stdout = io::stdout();
        let _handle = stdout.lock();
        unsafe {
            let h = windows_get_std_handle();
            let mut info: CONSOLE_SCREEN_BUFFER_INFO = std::mem::zeroed();
            GetConsoleScreenBufferInfo(h, &mut info);
            SetConsoleTextAttribute(h, color);
            println!("{}", msg);
            SetConsoleTextAttribute(h, info.wAttributes);
        }
    }
    #[cfg(not(windows))]
    {
        let ansi = match color & 0x0F {
            0x0A => "\x1b[32;1m",
            0x0C => "\x1b[31;1m",
            0x0E => "\x1b[33;1m",
            _ => "\x1b[0m",
        };
        let bg = if color & 0x10 != 0 { "\x1b[44m" } else { "" };
        println!("{}{}{}\x1b[0m", bg, ansi, msg);
    }
}

#[cfg(windows)]
#[repr(C)]
#[allow(non_snake_case)]
struct COORD {
    X: i16,
    Y: i16,
}

#[cfg(windows)]
#[repr(C)]
#[allow(non_snake_case)]
struct SMALL_RECT {
    Left: i16,
    Top: i16,
    Right: i16,
    Bottom: i16,
}

#[cfg(windows)]
#[repr(C)]
#[allow(non_snake_case)]
struct CONSOLE_SCREEN_BUFFER_INFO {
    dwSize: COORD,
    dwCursorPosition: COORD,
    wAttributes: u16,
    srWindow: SMALL_RECT,
    dwMaximumWindowSize: COORD,
}

#[cfg(windows)]
extern "system" {
    fn GetStdHandle(nStdHandle: u32) -> *mut std::ffi::c_void;
    fn GetConsoleScreenBufferInfo(
        hConsoleOutput: *mut std::ffi::c_void,
        lpConsoleScreenBufferInfo: *mut CONSOLE_SCREEN_BUFFER_INFO,
    ) -> i32;
    fn SetConsoleTextAttribute(
        hConsoleOutput: *mut std::ffi::c_void,
        wAttributes: u16,
    ) -> i32;
}

#[cfg(windows)]
unsafe fn windows_get_std_handle() -> *mut std::ffi::c_void {
    GetStdHandle(0xFFFFFFF5u32) // STD_OUTPUT_HANDLE
}

fn color_print_file_list(
    added: &[String],
    modified: &[String],
    deleted: &[String],
    added_folders: Option<&[String]>,
    deleted_folders: Option<&[String]>,
) {
    let mut all: Vec<String> = Vec::new();

    if let Some(af) = added_folders {
        for f in af {
            all.push(format!("{}{} +", f, SLASH));
        }
    }
    if let Some(df) = deleted_folders {
        for f in df {
            all.push(format!("{}{} -", f, SLASH));
        }
    }
    for f in added {
        all.push(format!("{} +", f));
    }
    for f in modified {
        all.push(format!("{} m", f));
    }
    for f in deleted {
        all.push(format!("{} -", f));
    }

    all.sort();

    // Reformat
    for item in &mut all {
        let last_char = item.chars().last().unwrap_or(' ');
        let prefix = &item[..item.len() - 2];
        *item = format!("[{}] {}", last_char, prefix);
    }

    println!();
    for item in &all {
        let mut color: u16 = if item.starts_with("[+]") {
            COLOR_GREEN
        } else if item.starts_with("[-]") {
            COLOR_RED
        } else {
            COLOR_YELLOW
        };

        if item.contains(ARCH_RES_SEPARATOR) {
            color |= COLOR_BG_BLUE;
        }

        color_writeln(item, color);
    }
}

fn load_archive_dyn(path: &str) -> io::Result<Box<dyn Archive>> {
    let ext = get_file_ext(path).to_lowercase();
    match ext.as_str() {
        ".snd" => Ok(Box::new(crate::snd::SndArchive::load(path)?)),
        ".vid" => Ok(Box::new(crate::vid::VidArchive::load(path)?)),
        _ => Ok(Box::new(crate::lod::LodArchive::load(path)?)),
    }
}

fn compare_in_archive_data(
    old_archive: &dyn Archive,
    new_archive: &dyn Archive,
    old_idx: usize,
    new_idx: usize,
) -> bool {
    let old_entry = &old_archive.entries()[old_idx];
    let new_entry = &new_archive.entries()[new_idx];

    // Delphi logic: first compare raw (packed) sizes
    if old_entry.size != new_entry.size {
        // Raw sizes differ. If neither is packed, they're different.
        if !old_entry.is_packed() && !new_entry.is_packed() {
            return false;
        }
        // At least one is packed — decompress and compare unpacked data
        if let (Ok(old_data), Ok(new_data)) = (
            old_archive.read_entry_data(old_idx),
            new_archive.read_entry_data(new_idx),
        ) {
            return old_data == new_data;
        }
        return false;
    }

    // Raw sizes match — compare raw bytes from disk
    let old_raw = read_raw_bytes(old_archive, old_idx);
    let new_raw = read_raw_bytes(new_archive, new_idx);
    if let (Ok(old_raw), Ok(new_raw)) = (old_raw, new_raw) {
        if old_raw == new_raw {
            return true;
        }
        // Raw bytes differ but sizes match. If either is packed, compare unpacked.
        if old_entry.is_packed() || new_entry.is_packed() {
            if let (Ok(old_data), Ok(new_data)) = (
                old_archive.read_entry_data(old_idx),
                new_archive.read_entry_data(new_idx),
            ) {
                return old_data == new_data;
            }
        }
        return false;
    }

    false
}

/// Read raw (packed) bytes for an entry directly from the archive file.
fn read_raw_bytes(archive: &dyn Archive, index: usize) -> io::Result<Vec<u8>> {
    let entry = &archive.entries()[index];
    let mut f = std::fs::File::open(archive.file_path())?;
    use std::io::{Read, Seek, SeekFrom};
    f.seek(SeekFrom::Start(entry.offset))?;
    let mut buf = vec![0u8; entry.size as usize];
    f.read_exact(&mut buf)?;
    Ok(buf)
}

fn compare_archive_entries(
    old_path: &str,
    new_path: &str,
    added: &mut Vec<String>,
    modified: &mut Vec<String>,
    deleted: &mut Vec<String>,
) -> io::Result<()> {
    let old_arch = load_archive_dyn(old_path)?;
    let new_arch = load_archive_dyn(new_path)?;

    let old_entries = old_arch.entries();
    let new_entries = new_arch.entries();

    let mut old_names: Vec<String> = old_entries.iter().map(|e| e.name.clone()).collect();
    let old_names_orig = old_names.clone();

    for (ni, ne) in new_entries.iter().enumerate() {
        if let Some(pos) = old_names.iter().position(|n| n.eq_ignore_ascii_case(&ne.name)) {
            let old_name = old_names.remove(pos);
            let oi = old_names_orig.iter().position(|n| n == &old_name).unwrap();
            if !compare_in_archive_data(old_arch.as_ref(), new_arch.as_ref(), oi, ni) {
                modified.push(ne.name.clone());
            }
        } else {
            added.push(ne.name.clone());
        }
    }

    for name in &old_names {
        deleted.push(name.clone());
    }

    Ok(())
}

pub struct CompareResult {
    pub same: bool,
    pub deleted_folders: Vec<String>,
    pub deleted_non_res_files: Vec<String>,
    pub deleted_res_files: Vec<String>,
    pub modified_archives: Vec<String>,
}

pub fn compare_base(
    old_path: &str,
    new_path: &str,
    copy_to_folder: &str,
    collect_lists: bool,
) -> CompareResult {
    let old_path = beautify_path(old_path);
    let new_path = beautify_path(new_path);

    let old_is_dir = std::path::Path::new(&old_path).is_dir();
    let new_is_dir = std::path::Path::new(&new_path).is_dir();

    let copy_to_folder_exists = !copy_to_folder.is_empty() && std::path::Path::new(copy_to_folder).exists();

    if old_is_dir && new_is_dir {
        compare_folders(&old_path, &new_path, copy_to_folder, collect_lists, copy_to_folder_exists)
    } else if std::path::Path::new(&old_path).is_file()
        && std::path::Path::new(&new_path).is_file()
        && is_supported_ext(&get_file_ext(&old_path))
        && is_supported_ext(&get_file_ext(&new_path))
    {
        compare_archive_files(&old_path, &new_path, copy_to_folder, collect_lists, copy_to_folder_exists)
    } else {
        eprintln!("Please specify two folders, or two MM Archive files");
        CompareResult {
            same: true,
            deleted_folders: Vec::new(),
            deleted_non_res_files: Vec::new(),
            deleted_res_files: Vec::new(),
            modified_archives: Vec::new(),
        }
    }
}

/// Clean up old diff folder entries that conflict with new entries.
/// This enables incremental version merging (V1->V2 then V2->V3 to same folder).
fn cleanup_old_diff_add_file(copy_to_folder: &str, rel_path: &str) {
    let base = with_trailing_slash(copy_to_folder);
    // Delete corresponding .todelete marker
    let todelete = format!("{}{}{}", base, rel_path, TODELETE_EXT);
    let _ = fs::remove_file(&todelete);
    // Delete .mmarchive folder if it exists (file was previously an archive)
    let mmarchive = format!("{}{}{}", base, rel_path, MMARCHIVE_EXT);
    let _ = fs::remove_dir_all(&mmarchive);
}

fn cleanup_old_diff_delete_file(copy_to_folder: &str, rel_path: &str) {
    let base = with_trailing_slash(copy_to_folder);
    // Delete the actual file from diff folder
    let file_path = format!("{}{}", base, rel_path);
    let _ = fs::remove_file(&file_path);
    // Delete .mmarchive folder if the file was an archive
    let mmarchive = format!("{}{}{}", base, rel_path, MMARCHIVE_EXT);
    let _ = fs::remove_dir_all(&mmarchive);
}

fn cleanup_old_diff_delete_folder(copy_to_folder: &str, rel_path: &str) {
    let base = with_trailing_slash(copy_to_folder);
    // Delete the folder from diff
    let folder_path = format!("{}{}", base, rel_path);
    let _ = fs::remove_dir_all(&folder_path);
}

fn cleanup_old_diff_add_resource(copy_to_folder: &str, archive_rel: &str, res_name: &str) {
    let base = with_trailing_slash(copy_to_folder);
    // Delete .todelete marker for this resource
    let todelete = format!("{}{}{}{}{}{}", base, archive_rel, MMARCHIVE_EXT, SLASH, res_name, TODELETE_EXT);
    let _ = fs::remove_file(&todelete);
}

fn cleanup_old_diff_delete_resource(copy_to_folder: &str, archive_rel: &str, res_name: &str) {
    let base = with_trailing_slash(copy_to_folder);
    // Delete the .todelete marker for this resource
    let todelete = format!("{}{}{}{}{}{}", base, archive_rel, MMARCHIVE_EXT, SLASH, res_name, TODELETE_EXT);
    let _ = fs::remove_file(&todelete);
    // Delete the resource file itself from .mmarchive folder
    let res_file = format!("{}{}{}{}{}", base, archive_rel, MMARCHIVE_EXT, SLASH, res_name);
    let _ = fs::remove_file(&res_file);
    // Delete the parent archive's .todelete marker
    let arch_todelete = format!("{}{}{}", base, archive_rel, TODELETE_EXT);
    let _ = fs::remove_file(&arch_todelete);
}

fn compare_folders(
    old_folder: &str,
    new_folder: &str,
    copy_to_folder: &str,
    collect_lists: bool,
    copy_to_folder_exists: bool,
) -> CompareResult {
    let old_prefix = with_trailing_slash(old_folder);
    let new_prefix = with_trailing_slash(new_folder);

    // Get all folders recursively
    let mut old_folder_list = Vec::new();
    let mut new_folder_list = Vec::new();
    add_all_files_default(&mut old_folder_list, old_folder, 1, true, false);
    add_all_files_default(&mut new_folder_list, new_folder, 1, true, false);

    // Strip root prefix
    let old_folder_list: Vec<String> = old_folder_list
        .iter()
        .map(|f| f.strip_prefix(&old_prefix).unwrap_or(f).to_string())
        .collect();
    let new_folder_list: Vec<String> = new_folder_list
        .iter()
        .map(|f| f.strip_prefix(&new_prefix).unwrap_or(f).to_string())
        .collect();

    // Get all files recursively
    let mut old_file_list_full = Vec::new();
    let mut new_file_list_full = Vec::new();
    add_all_files_default(&mut old_file_list_full, old_folder, 1, false, false);
    add_all_files_default(&mut new_file_list_full, new_folder, 1, false, false);

    let old_file_list: Vec<String> = old_file_list_full
        .iter()
        .map(|f| f.strip_prefix(&old_prefix).unwrap_or(f).to_string())
        .collect();
    let new_file_list: Vec<String> = new_file_list_full
        .iter()
        .map(|f| f.strip_prefix(&new_prefix).unwrap_or(f).to_string())
        .collect();

    let mut added_folders = Vec::new();
    let mut deleted_folders = Vec::new();
    let mut remaining_old_folders = old_folder_list.clone();

    for nf in &new_folder_list {
        if let Some(pos) = remaining_old_folders.iter().position(|f| f == nf) {
            remaining_old_folders.remove(pos);
        } else {
            added_folders.push(nf.clone());
            if !copy_to_folder.is_empty() {
                // No cleanup needed for adding a folder
                let _ = create_dir_recur(&format!("{}{}", with_trailing_slash(copy_to_folder), nf));
            }
        }
    }

    for of in &remaining_old_folders {
        if !has_todelete_parent_folder(of, &deleted_folders) {
            deleted_folders.push(of.clone());
        }
        if !copy_to_folder.is_empty() && !has_todelete_parent_folder(of, &deleted_folders) {
            if copy_to_folder_exists {
                cleanup_old_diff_delete_folder(copy_to_folder, of);
            }
            let _ = create_dir_recur(&format!(
                "{}{}{}",
                with_trailing_slash(copy_to_folder),
                of,
                TODELETE_EXT
            ));
        }
    }

    let mut added_files = Vec::new();
    let mut modified_files = Vec::new();
    let mut deleted_files = Vec::new();
    let mut remaining_old_files = old_file_list.clone();

    for nf in &new_file_list {
        if let Some(pos) = remaining_old_files.iter().position(|f| f == nf) {
            remaining_old_files.remove(pos);

            let old_full = format!("{}{}", old_prefix, nf);
            let new_full = format!("{}{}", new_prefix, nf);

            match compare_file_bytes(&old_full, &new_full) {
                Ok(true) => {} // same, skip
                Ok(false) => {
                    // Files differ - check if it's an archive
                    let ext = get_file_ext(nf).to_lowercase();
                    if is_supported_ext(&ext) {
                        // Try as archive
                        let mut arch_added = Vec::new();
                        let mut arch_modified = Vec::new();
                        let mut arch_deleted = Vec::new();

                        match compare_archive_entries(
                            &old_full,
                            &new_full,
                            &mut arch_added,
                            &mut arch_modified,
                            &mut arch_deleted,
                        ) {
                            Ok(()) => {
                                // Extract modified/added resources
                                if !copy_to_folder.is_empty() {
                                    if let Ok(new_arch) = load_archive_dyn(&new_full) {
                                        for res_name in arch_added.iter().chain(arch_modified.iter()) {
                                            if copy_to_folder_exists {
                                                cleanup_old_diff_add_resource(copy_to_folder, nf, res_name);
                                            }
                                            if let Some(idx) = new_arch.find_entry(res_name) {
                                                let dest = format!(
                                                    "{}{}{}",
                                                    with_trailing_slash(copy_to_folder),
                                                    nf,
                                                    MMARCHIVE_EXT
                                                );
                                                let _ = extract_single_entry(new_arch.as_ref(), idx, &dest);
                                            }
                                        }
                                        for res_name in &arch_deleted {
                                            if copy_to_folder_exists {
                                                cleanup_old_diff_delete_resource(copy_to_folder, nf, res_name);
                                            }
                                            let dest_file = format!(
                                                "{}{}{}{}{}{}",
                                                with_trailing_slash(copy_to_folder),
                                                nf,
                                                MMARCHIVE_EXT,
                                                SLASH,
                                                res_name,
                                                TODELETE_EXT
                                            );
                                            let _ = create_empty_file(&dest_file);
                                        }
                                    }
                                }

                                let sep = ARCH_RES_SEPARATOR.to_string();
                                for a in &arch_added {
                                    added_files.push(format!("{}{}{}", nf, sep, a));
                                }
                                for m in &arch_modified {
                                    modified_files.push(format!("{}{}{}", nf, sep, m));
                                }
                                for d in &arch_deleted {
                                    deleted_files.push(format!("{}{}{}", nf, sep, d));
                                }
                                // Mark archive as modified overall
                                modified_files.push(format!("{}{}", nf, sep));
                            }
                            Err(_) => {
                                // Not a valid archive, treat as regular file
                                modified_files.push(nf.clone());
                                if !copy_to_folder.is_empty() {
                                    if copy_to_folder_exists {
                                        cleanup_old_diff_add_file(copy_to_folder, nf);
                                    }
                                    let _ = copy_file0(
                                        &new_full,
                                        &format!("{}{}", with_trailing_slash(copy_to_folder), nf),
                                    );
                                }
                            }
                        }
                    } else {
                        modified_files.push(nf.clone());
                        if !copy_to_folder.is_empty() {
                            if copy_to_folder_exists {
                                cleanup_old_diff_add_file(copy_to_folder, nf);
                            }
                            let _ = copy_file0(
                                &new_full,
                                &format!("{}{}", with_trailing_slash(copy_to_folder), nf),
                            );
                        }
                    }
                }
                Err(_) => {}
            }
        } else {
            added_files.push(nf.clone());
            if !copy_to_folder.is_empty() {
                if copy_to_folder_exists {
                    cleanup_old_diff_add_file(copy_to_folder, nf);
                }
                let new_full = format!("{}{}", new_prefix, nf);
                let _ = copy_file0(
                    &new_full,
                    &format!("{}{}", with_trailing_slash(copy_to_folder), nf),
                );
            }
        }
    }

    for of in &remaining_old_files {
        if !has_todelete_parent_folder(of, &deleted_folders) {
            deleted_files.push(of.clone());
        }
        if !copy_to_folder.is_empty() && !has_todelete_parent_folder(of, &deleted_folders) {
            if copy_to_folder_exists {
                cleanup_old_diff_delete_file(copy_to_folder, of);
            }
            let _ = create_empty_file(&format!(
                "{}{}{}",
                with_trailing_slash(copy_to_folder),
                of,
                TODELETE_EXT
            ));
        }
    }

    let same = added_files.is_empty()
        && modified_files.is_empty()
        && deleted_files.is_empty()
        && added_folders.is_empty()
        && deleted_folders.is_empty();

    if same {
        println!("Folders are exactly the same");
    } else {
        color_print_file_list(
            &added_files,
            &modified_files,
            &deleted_files,
            Some(&added_folders),
            Some(&deleted_folders),
        );
    }

    // Build result lists
    let result_deleted_folders = deleted_folders.clone();
    let mut result_deleted_non_res = Vec::new();
    let mut result_deleted_res = Vec::new();
    let mut result_modified_archives = Vec::new();

    if collect_lists {
        for f in &deleted_files {
            if f.contains(ARCH_RES_SEPARATOR) {
                result_deleted_res.push(f.clone());
            } else if !deleted_folders.iter().any(|df| {
                let dir = trim_chars_right(&get_file_dir(f), '\\', '/');
                dir == *df
            }) {
                result_deleted_non_res.push(f.clone());
            }
        }
        for f in &modified_files {
            if f.ends_with(ARCH_RES_SEPARATOR) {
                result_modified_archives.push(f.clone());
            }
        }
    }

    CompareResult {
        same,
        deleted_folders: result_deleted_folders,
        deleted_non_res_files: result_deleted_non_res,
        deleted_res_files: result_deleted_res,
        modified_archives: result_modified_archives,
    }
}

fn compare_archive_files(
    old_path: &str,
    new_path: &str,
    copy_to_folder: &str,
    collect_lists: bool,
    copy_to_folder_exists: bool,
) -> CompareResult {
    // First check if files are identical
    match compare_file_bytes(old_path, new_path) {
        Ok(true) => {
            println!("Files are exactly the same");
            return CompareResult {
                same: true,
                deleted_folders: Vec::new(),
                deleted_non_res_files: Vec::new(),
                deleted_res_files: Vec::new(),
                modified_archives: Vec::new(),
            };
        }
        _ => {}
    }

    let mut added = Vec::new();
    let mut modified = Vec::new();
    let mut deleted = Vec::new();

    match compare_archive_entries(old_path, new_path, &mut added, &mut modified, &mut deleted) {
        Ok(()) => {
            let name = get_file_name(new_path);

            if !copy_to_folder.is_empty() {
                if let Ok(new_arch) = load_archive_dyn(new_path) {
                    for res_name in added.iter().chain(modified.iter()) {
                        if copy_to_folder_exists {
                            cleanup_old_diff_add_resource(copy_to_folder, &name, res_name);
                        }
                        if let Some(idx) = new_arch.find_entry(res_name) {
                            let dest = format!(
                                "{}{}{}",
                                with_trailing_slash(copy_to_folder),
                                name,
                                MMARCHIVE_EXT
                            );
                            let _ = extract_single_entry(new_arch.as_ref(), idx, &dest);
                        }
                    }
                    for res_name in &deleted {
                        if copy_to_folder_exists {
                            cleanup_old_diff_delete_resource(copy_to_folder, &name, res_name);
                        }
                        let dest_file = format!(
                            "{}{}{}{}{}{}",
                            with_trailing_slash(copy_to_folder),
                            name,
                            MMARCHIVE_EXT,
                            SLASH,
                            res_name,
                            TODELETE_EXT
                        );
                        let _ = create_empty_file(&dest_file);
                    }
                }
            }

            color_print_file_list(&added, &modified, &deleted, None, None);

            let mut result_deleted_res = Vec::new();
            let mut result_modified_archives = Vec::new();

            if collect_lists {
                for d in &deleted {
                    result_deleted_res.push(format!("{}{}{}", name, ARCH_RES_SEPARATOR, d));
                }
                if !added.is_empty() && !modified.is_empty() {
                    result_modified_archives.push(format!("{}{}", name, ARCH_RES_SEPARATOR));
                }
            }

            CompareResult {
                same: false,
                deleted_folders: Vec::new(),
                deleted_non_res_files: Vec::new(),
                deleted_res_files: result_deleted_res,
                modified_archives: result_modified_archives,
            }
        }
        Err(e) => {
            eprintln!("Incorrect MM Archive files: {}", e);
            CompareResult {
                same: true,
                deleted_folders: Vec::new(),
                deleted_non_res_files: Vec::new(),
                deleted_res_files: Vec::new(),
                modified_archives: Vec::new(),
            }
        }
    }
}

fn extract_single_entry(archive: &dyn Archive, index: usize, dest_folder: &str) -> io::Result<()> {
    let _ = create_dir_recur(dest_folder);
    let entry = &archive.entries()[index];
    let data = archive.read_entry_data(index)?;
    let extracted_name = crate::get_extracted_name(archive.kind(), &entry.name, Some(&data));
    let dest_path = format!("{}{}", with_trailing_slash(dest_folder), extracted_name);
    fs::write(&dest_path, &data)?;
    Ok(())
}

pub fn get_list_from_diff_files(
    old_diff_folder: &str,
    deleted_folders: &mut Vec<String>,
    deleted_non_res: &mut Vec<String>,
    deleted_res: &mut Vec<String>,
    modified_archives: &mut Vec<String>,
) {
    let prefix = with_trailing_slash(old_diff_folder);

    // .todelete folders
    let todelete_ext = vec![TODELETE_EXT.to_string()];
    let mut raw = Vec::new();
    add_all_files_to_file_list(&mut raw, old_diff_folder, 1, true, false, &todelete_ext);
    for f in &raw {
        let mut rel = f.strip_prefix(&prefix).unwrap_or(f).to_string();
        // Remove .todelete from end
        if rel.ends_with(TODELETE_EXT) {
            rel = rel[..rel.len() - TODELETE_EXT.len()].to_string();
        }
        deleted_folders.push(rel);
    }

    // .todelete files
    raw.clear();
    add_all_files_to_file_list(&mut raw, old_diff_folder, 1, false, false, &todelete_ext);
    for f in &raw {
        let mut rel = f.strip_prefix(&prefix).unwrap_or(f).to_string();
        if rel.ends_with(TODELETE_EXT) {
            rel = rel[..rel.len() - TODELETE_EXT.len()].to_string();
        }

        let mmarch_marker = format!("{}{}", MMARCHIVE_EXT, SLASH);
        if rel.contains(&mmarch_marker) {
            // Resource file in .mmarchive folder
            let converted = rel.replace(&mmarch_marker, &ARCH_RES_SEPARATOR.to_string());
            deleted_res.push(converted);
        } else {
            deleted_non_res.push(rel);
        }
    }

    // .mmarchive folders
    let mmarchive_ext = vec![MMARCHIVE_EXT.to_string()];
    raw.clear();
    add_all_files_to_file_list(&mut raw, old_diff_folder, 1, true, false, &mmarchive_ext);
    for f in &raw {
        let mut rel = f.strip_prefix(&prefix).unwrap_or(f).to_string();
        if rel.ends_with(MMARCHIVE_EXT) {
            rel = rel[..rel.len() - MMARCHIVE_EXT.len()].to_string();
        }
        modified_archives.push(format!("{}{}", rel, ARCH_RES_SEPARATOR));
    }
}
