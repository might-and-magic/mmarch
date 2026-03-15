use crate::archive::Archive;
use crate::path_utils::{get_file_ext, get_file_name, wildcard_filename_to_ext};
use std::fs;
use std::io;

// CRC-32 lookup table (polynomial 0xEDB88320, reflected)
const CRC32_TABLE: [u32; 256] = {
    let mut table = [0u32; 256];
    let mut i = 0u32;
    while i < 256 {
        let mut crc = i;
        let mut j = 0;
        while j < 8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ 0xEDB88320;
            } else {
                crc >>= 1;
            }
            j += 1;
        }
        table[i as usize] = crc;
        i += 1;
    }
    table
};

pub fn crc32(data: &[u8]) -> u32 {
    let mut crc: u32 = 0xFFFFFFFF;
    for &byte in data {
        crc = (crc >> 8) ^ CRC32_TABLE[((crc ^ byte as u32) & 0xFF) as usize];
    }
    crc ^ 0xFFFFFFFF
}

pub fn format_crc(crc: u32) -> String {
    format!("{:08X}", crc)
}

/// CRC32 of an entire file on disk.
pub fn checksum_file(path: &str) -> io::Result<u32> {
    let data = fs::read(path)?;
    Ok(crc32(&data))
}

/// CRC32 of resources inside an archive matching the given filter.
/// filter: "*" = all, ".ext" = extension, "" = no extension, or specific name.
pub fn checksum_resources(
    archive: &dyn Archive,
    filters: &[String],
) -> io::Result<Vec<(String, u32)>> {
    let entries = archive.entries();
    let mut results = Vec::new();

    for i in 0..entries.len() {
        let name = &entries[i].name;
        if matches_any_filter(name, filters) {
            let data = archive.read_entry_data(i)?;
            results.push((name.clone(), crc32(&data)));
        }
    }

    Ok(results)
}

fn matches_any_filter(name: &str, filters: &[String]) -> bool {
    for filter in filters {
        if filter == "*" || filter == "*.*" {
            return true;
        }
        if filter.contains('*') {
            let ext = wildcard_filename_to_ext(filter);
            if ext == "*" {
                return true;
            }
            if ext.is_empty() {
                if get_file_ext(name).is_empty() {
                    return true;
                }
            } else {
                if get_file_ext(name).eq_ignore_ascii_case(&ext) {
                    return true;
                }
            }
        } else {
            if name.eq_ignore_ascii_case(filter) {
                return true;
            }
        }
    }
    false
}

/// Parse a checksum file. Each line: "HEXHASH  filename"
pub fn parse_checksum_file(content: &str) -> Vec<(String, u32)> {
    let mut pairs = Vec::new();
    for line in content.lines() {
        let line = line.trim_end_matches('\r');
        if line.is_empty() {
            continue;
        }
        if let Some(pos) = line.find("  ") {
            let hash_str = &line[..pos];
            let name = &line[pos + 2..];
            if let Ok(hash) = u32::from_str_radix(hash_str, 16) {
                pairs.push((name.to_string(), hash));
            }
        }
    }
    pairs
}

/// Parse inline pairs: "name:HASH"
pub fn parse_inline_pairs(args: &[String]) -> Vec<(String, u32)> {
    let mut pairs = Vec::new();
    for arg in args {
        if let Some(pos) = arg.find(':') {
            let name = &arg[..pos];
            let hash_str = &arg[pos + 1..];
            if let Ok(hash) = u32::from_str_radix(hash_str, 16) {
                pairs.push((name.to_string(), hash));
            }
        }
    }
    pairs
}

/// Verify checksums. Returns true if all pass.
pub fn verify_checksums(
    archive: &dyn Archive,
    pairs: &[(String, u32)],
    verify_all: bool,
) -> bool {
    let mut failures = 0u32;
    let entries = archive.entries();

    for (name, expected) in pairs {
        match archive.find_entry(name) {
            Some(idx) => {
                match archive.read_entry_data(idx) {
                    Ok(data) => {
                        let actual = crc32(&data);
                        if actual == *expected {
                            println!("{}: OK", name);
                        } else {
                            println!("{}: FAILED", name);
                            failures += 1;
                        }
                    }
                    Err(_) => {
                        println!("{}: FAILED", name);
                        failures += 1;
                    }
                }
            }
            None => {
                println!("{}: FAILED", name);
                failures += 1;
            }
        }
    }

    if verify_all {
        // Check for files in archive not listed
        let listed: Vec<String> = pairs.iter().map(|(n, _)| n.to_lowercase()).collect();
        let mut unlisted = 0u32;
        for entry in entries {
            if !listed.iter().any(|n| n.eq_ignore_ascii_case(&entry.name)) {
                unlisted += 1;
            }
        }
        if unlisted > 0 {
            eprintln!(
                "WARNING: {} files in archive not listed in checksum",
                unlisted
            );
            failures += unlisted;
        }
    }

    if failures > 0 {
        eprintln!("WARNING: {} computed checksums did NOT match", failures);
        false
    } else {
        true
    }
}

/// Generate output line in checksum format.
pub fn format_checksum_line(name: &str, crc: u32) -> String {
    format!("{:08X}  {}\r\n", crc, name)
}

/// Generate checksum output for archive file itself.
pub fn format_archive_checksum(path: &str) -> io::Result<String> {
    let crc = checksum_file(path)?;
    let name = get_file_name(path);
    Ok(format_checksum_line(&name, crc))
}
