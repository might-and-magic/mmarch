use std::fs;
use std::io;
use std::path::Path;

pub const SUPPORTED_EXTS: &[&str] = &[".lod", ".pac", ".snd", ".vid", ".lwd", ".mm7", ".dod", ".mm6"];
pub const TODELETE_EXT: &str = ".todelete";
pub const MMARCHIVE_EXT: &str = ".mmarchive";
pub const EMPTY_FOLDER_KEEP: &str = ".mmarchkeep";
pub const ARCH_RES_SEPARATOR: char = ':';
#[cfg(windows)]
pub const SLASH: char = '\\';
#[cfg(not(windows))]
pub const SLASH: char = '/';

pub fn beautify_path(old_str: &str) -> String {
    if old_str.is_empty() {
        return String::new();
    }
    let chars: Vec<char> = old_str.chars().collect();
    let mut result = String::new();
    let start;
    if chars[0] == '.' {
        result.push('.');
        start = 1;
    } else {
        start = 0;
    }
    for i in start..chars.len() {
        let c = chars[i];
        if c == '\\' || c == '/' {
            if result.is_empty() || result.chars().last() != Some(SLASH) {
                result.push(SLASH);
            }
        } else {
            result.push(c);
        }
    }
    let prefix = format!(".{}", SLASH);
    if result.starts_with(&prefix) {
        result = result[prefix.len()..].to_string();
    }
    result
}

pub fn with_trailing_slash(path: &str) -> String {
    if path.is_empty() {
        return String::new();
    }
    if path.ends_with('\\') || path.ends_with('/') {
        path.to_string()
    } else {
        format!("{}{}", path, SLASH)
    }
}

pub fn trim_char_left(s: &str, c: char) -> String {
    s.trim_start_matches(c).to_string()
}

pub fn trim_chars_right(s: &str, c1: char, c2: char) -> String {
    let mut s = s.to_string();
    while s.ends_with(c1) || s.ends_with(c2) {
        s.pop();
    }
    s
}

pub fn wildcard_filename_to_ext(filename: &str) -> String {
    if filename == "*" || filename == "*.*" {
        "*".to_string()
    } else if filename == "*." {
        String::new()
    } else {
        trim_char_left(filename, '*')
    }
}

pub fn is_supported_ext(ext: &str) -> bool {
    let ext_lower = ext.to_lowercase();
    SUPPORTED_EXTS.iter().any(|e| *e == ext_lower)
}

pub fn get_file_ext(path: &str) -> String {
    if let Some(pos) = path.rfind('.') {
        let after_sep = path.rfind(|c: char| c == '\\' || c == '/').unwrap_or(0);
        if pos > after_sep {
            return path[pos..].to_string();
        }
    }
    String::new()
}

pub fn get_file_stem(path: &str) -> String {
    let name = get_file_name(path);
    let ext = get_file_ext(&name);
    if ext.is_empty() {
        name
    } else {
        name[..name.len() - ext.len()].to_string()
    }
}

pub fn get_file_name(path: &str) -> String {
    let p = path.replace('/', "\\");
    if let Some(pos) = p.rfind('\\') {
        p[pos + 1..].to_string()
    } else {
        p
    }
}

pub fn get_file_dir(path: &str) -> String {
    let p = path.replace('/', &SLASH.to_string());
    if let Some(pos) = p.rfind(SLASH) {
        p[..pos + 1].to_string()
    } else {
        String::new()
    }
}

pub fn create_dir_recur(dir: &str) -> io::Result<()> {
    let p = Path::new(dir);
    if !p.exists() {
        fs::create_dir_all(p)?;
    }
    Ok(())
}

pub fn create_empty_file(file_path: &str) -> io::Result<()> {
    if let Some(parent) = Path::new(file_path).parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(file_path, b"")?;
    Ok(())
}

pub fn copy_file0(old_file: &str, new_file: &str) -> io::Result<()> {
    let new_path = Path::new(new_file);
    if let Some(parent) = new_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::copy(old_file, new_file)?;
    Ok(())
}

pub fn str_to_file(file_path: &str, content: &str) -> io::Result<()> {
    let p = Path::new(file_path);
    if let Some(parent) = p.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(p, content.as_bytes())?;
    Ok(())
}

pub fn del_dir(folder: &str) {
    let _ = fs::remove_dir_all(folder);
}

pub fn move_dir(from: &str, to: &str) -> bool {
    let from = trim_chars_right(&beautify_path(from), '\\', '/');
    let to = trim_chars_right(&beautify_path(to), '\\', '/');

    if is_subfolder(&from, &to) {
        let tmp = format!("{}.mmarchtmp", from);
        if !move_dir(&from, &tmp) {
            return false;
        }
        return move_dir(&tmp, &to);
    }

    if from == to {
        return false;
    }

    if let Some(parent) = Path::new(&to).parent() {
        let _ = fs::create_dir_all(parent);
    }

    // Try rename first (fast path, same filesystem)
    if fs::rename(&from, &to).is_ok() {
        return true;
    }

    // Fall back to recursive copy + delete (cross-filesystem, like Delphi's ShFileOperation)
    if copy_dir_recursive(&from, &to).is_ok() {
        let _ = fs::remove_dir_all(&from);
        return true;
    }

    false
}

fn copy_dir_recursive(from: &str, to: &str) -> io::Result<()> {
    fs::create_dir_all(to)?;
    for entry in fs::read_dir(from)? {
        let entry = entry?;
        let dest = format!("{}{}{}", with_trailing_slash(to), entry.file_name().to_string_lossy(), "");
        if entry.file_type()?.is_dir() {
            copy_dir_recursive(&entry.path().to_string_lossy(), &dest)?;
        } else {
            fs::copy(entry.path(), &dest)?;
        }
    }
    Ok(())
}

pub fn is_subfolder(folder: &str, potential_subfolder: &str) -> bool {
    let folder = trim_chars_right(&beautify_path(folder), '\\', '/');
    let mut parent = trim_chars_right(&beautify_path(potential_subfolder), '\\', '/');
    loop {
        let last = parent.clone();
        parent = trim_chars_right(&get_file_dir(&parent), '\\', '/');
        if parent == folder {
            return true;
        }
        if parent.is_empty() || parent == last {
            return false;
        }
    }
}

/// Get all files (or dirs) in a folder, optionally filtering by extension.
/// ext: "*" = all, "" = no extension, ".xyz" = specific extension
/// Returns just the file/dir names (not full paths).
pub fn get_all_files_in_folder(path: &str, ext: &str, is_dir: bool) -> io::Result<Vec<String>> {
    let mut result = Vec::new();
    let path = if path.is_empty() { "." } else { path };
    // Match Delphi: raise exception if directory does not exist (MMArchPath.pas:207-209)
    if !Path::new(path).is_dir() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("Directory {} is not found", path),
        ));
    }
    let dir = fs::read_dir(path)?;
    for entry in dir.flatten() {
        let ft = match entry.file_type() {
            Ok(ft) => ft,
            Err(_) => continue,
        };
        let name = entry.file_name().to_string_lossy().to_string();
        if name == "." || name == ".." {
            continue;
        }
        if name == EMPTY_FOLDER_KEEP {
            continue;
        }
        if is_dir {
            if !ft.is_dir() {
                continue;
            }
        } else {
            if ft.is_dir() {
                continue;
            }
        }
        if ext == "*" {
            result.push(name);
        } else if ext.is_empty() {
            // files without extension
            if get_file_ext(&name).is_empty() {
                result.push(name);
            }
        } else {
            if get_file_ext(&name).eq_ignore_ascii_case(ext) {
                result.push(name);
            }
        }
    }
    result.sort();
    Ok(result)
}

/// Recursively collect files or dirs.
/// recursive: 1=recursive, 2=one level of subdirs, 3=current folder only
/// If use_path_filename_pair, returns "filename:path" pairs.
/// Otherwise returns full relative paths.
pub fn add_all_files_to_file_list(
    file_list: &mut Vec<String>,
    path: &str,
    recursive: u32,
    is_dir: bool,
    use_path_filename_pair: bool,
    ext_list: &[String],
) -> io::Result<()> {
    if recursive == 1 || recursive == 3 {
        for ext in ext_list {
            for name in get_all_files_in_folder(path, ext, is_dir)? {
                if use_path_filename_pair {
                    file_list.push(format!("{}:{}", name, beautify_path(path)));
                } else {
                    file_list.push(format!("{}{}", with_trailing_slash(&beautify_path(path)), name));
                }
            }
        }
    }
    if recursive != 3 {
        let dirs = get_all_files_in_folder(path, "*", true)?;
        for dir in dirs {
            let sub = format!("{}{}", with_trailing_slash(path), dir);
            if recursive == 1 {
                add_all_files_to_file_list(file_list, &sub, 1, is_dir, use_path_filename_pair, ext_list)?;
            } else if recursive == 2 {
                for ext in ext_list {
                    for name in get_all_files_in_folder(&sub, ext, is_dir)? {
                        if use_path_filename_pair {
                            file_list.push(format!("{}:{}", name, beautify_path(&sub)));
                        } else {
                            file_list.push(format!("{}{}", with_trailing_slash(&beautify_path(&sub)), name));
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn add_all_files_default(
    file_list: &mut Vec<String>,
    path: &str,
    recursive: u32,
    is_dir: bool,
    use_path_filename_pair: bool,
) -> io::Result<()> {
    let ext_list = vec!["*".to_string()];
    add_all_files_to_file_list(file_list, path, recursive, is_dir, use_path_filename_pair, &ext_list)
}

pub fn wildcard_archive_name_to_archive_list(archive_name: &str) -> io::Result<Vec<(String, String)>> {
    let path = trim_chars_right(&get_file_dir(archive_name), '\\', '/');
    let filename = get_file_name(archive_name);

    let ext_list = file_name_to_ext_list(&filename);

    let mut raw_list = Vec::new();

    if path.ends_with("**") {
        let path_temp = &path[..path.len() - 2];
        let path_temp = trim_chars_right(path_temp, '\\', '/');
        add_all_files_to_file_list(&mut raw_list, &path_temp, 1, false, true, &ext_list)?;
    } else if path.ends_with('*') {
        let path_temp = &path[..path.len() - 1];
        let path_temp = trim_chars_right(path_temp, '\\', '/');
        add_all_files_to_file_list(&mut raw_list, &path_temp, 2, false, true, &ext_list)?;
    } else {
        add_all_files_to_file_list(&mut raw_list, &path, 3, false, true, &ext_list)?;
    }

    // Parse "name:path" pairs
    Ok(raw_list
        .iter()
        .map(|s| {
            if let Some(pos) = s.find(':') {
                (s[..pos].to_string(), s[pos + 1..].to_string())
            } else {
                (s.clone(), ".".to_string())
            }
        })
        .collect())
}

fn file_name_to_ext_list(filename: &str) -> Vec<String> {
    if filename == "*" || filename == "*.*" {
        return SUPPORTED_EXTS.iter().map(|s| s.to_string()).collect();
    }
    let ext = get_file_ext(filename);
    if ext.contains('|') {
        let ext_no_dot = trim_char_left(&ext, '.');
        ext_no_dot
            .split('|')
            .filter_map(|e| {
                let full = format!(".{}", e.to_lowercase());
                if SUPPORTED_EXTS.contains(&full.as_str()) {
                    Some(full)
                } else {
                    None
                }
            })
            .collect()
    } else {
        vec![ext]
    }
}

pub fn add_keep_to_all_empty_folders_recur(folder: &str) -> io::Result<()> {
    let folder = trim_chars_right(&beautify_path(folder), '\\', '/');
    let mut all_folders = vec![folder.clone()];
    let mut tmp = Vec::new();
    add_all_files_default(&mut tmp, &folder, 1, true, false)?;
    all_folders.extend(tmp);

    for f in &all_folders {
        let dirs = get_all_files_in_folder(f, "*", true)?;
        let files = get_all_files_in_folder(f, "*", false)?;
        if dirs.is_empty() && files.is_empty() {
            let _ = create_empty_file(&format!("{}{}{}", with_trailing_slash(f), "", EMPTY_FOLDER_KEEP));
        }
    }
    Ok(())
}

pub fn compare_file_bytes(old_file: &str, new_file: &str) -> io::Result<bool> {
    let old_data = fs::read(old_file)?;
    let new_data = fs::read(new_file)?;
    Ok(old_data == new_data)
}

pub fn has_todelete_parent_folder(file_or_folder: &str, deleted_folders: &[String]) -> bool {
    let mut parent = file_or_folder.to_string();
    loop {
        parent = trim_chars_right(&get_file_dir(&parent), '\\', '/');
        if parent.is_empty() {
            return false;
        }
        if deleted_folders.iter().any(|f| f.eq_ignore_ascii_case(&parent)) {
            return true;
        }
    }
}
