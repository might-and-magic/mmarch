//! Comprehensive integration tests for mmarch.
//!
//! Every test runs against BOTH the Rust binary and the Delphi binary (if present).
//! All commands are run with CWD set to the test's temp directory, using relative
//! paths to avoid drive-letter colon issues with the Delphi binary.
//!
//! # Where the expected values come from
//!
//! Never from running mmarch and writing down what came out. Every CRC below is
//! derived from the archive format as GrayFace's RSPak defines it
//! (`mmarch-delphi/RSPak/Extra/RSLod.pas`) — decode the entry by hand, then
//! assert mmarch agrees. A golden value recorded from the implementation only
//! asserts that today's behaviour equals today's behaviour: `real_mm8loc.T.lod`
//! carried `382A0F77` for `D07.EVT` for two releases, and that was the CRC of
//! 768 bytes of undecoded archive bytes — the test passed for as long as the
//! bug lasted, and failed the moment it was fixed.
//!
//! That includes the pictures. A texture, icon, sprite or Heroes PCX comes out
//! as a real .bmp, and `mmarch-rust/tests/reference/rspak.py` builds the same
//! file independently from the format — for the MM6 bats it comes out byte for
//! byte identical to the .bmp MMArchive itself produced, kept in
//! `tests/reference/mmarchive/`.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

static COUNTER: AtomicU64 = AtomicU64::new(0);

fn cargo_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn test_general_dir() -> PathBuf {
    cargo_dir().join("tests").join("data_general")
}

fn test_compare_dir() -> PathBuf {
    cargo_dir().join("tests").join("data_compare")
}

/// Return a unique temp directory.
fn temp_dir(prefix: &str) -> PathBuf {
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    let tid = std::thread::current().id();
    let dir = std::env::temp_dir().join(format!("mmarch_test_{}_{}_{:?}", prefix, id, tid));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn cleanup(dir: &Path) {
    let _ = fs::remove_dir_all(dir);
}


/// Get list of binaries to test.
/// The binaries under test.
///
/// Rust only. `mmarch-delphi/mmarch.exe` used to be run through the same suite
/// on Windows, but the Delphi build is frozen at 5.0.0 and cannot regress, so
/// there is nothing for CI to catch there; asserting the same numbers for both
/// only meant that this port could never fix anything the Delphi CLI got
/// wrong. What the two do differently is recorded in DEVNOTES.md
/// § Deliberate differences from the Delphi version, checked by hand against
/// the shipped 5.0.0 binary rather than on every push.
fn get_binaries() -> Vec<(&'static str, PathBuf)> {
    let rust_bin = PathBuf::from(env!("CARGO_BIN_EXE_mmarch"));
    assert!(rust_bin.exists(), "Rust binary not found at {:?}", rust_bin);
    vec![("rust", rust_bin)]
}

/// Run binary with CWD. All paths in args should be relative to cwd.
fn run_in(bin: &Path, cwd: &Path, args: &[&str]) -> (String, String, bool) {
    let output = Command::new(bin)
        .current_dir(cwd)
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("failed to execute {:?}: {}", bin, e));
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    (stdout, stderr, output.status.success())
}

fn run_ok_in(bin: &Path, cwd: &Path, args: &[&str]) -> String {
    let (stdout, stderr, ok) = run_in(bin, cwd, args);
    assert!(ok, "[{:?}] {:?} in {:?} failed.\nstdout: {}\nstderr: {}",
            bin.file_name().unwrap(), args, cwd, stdout, stderr);
    stdout
}

fn write_test_file(path: &Path, content: &[u8]) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, content).unwrap();
}

fn create_wav(path: &Path) {
    let num_samples: u32 = 16;
    let sample_rate: u32 = 22050;
    let bits_per_sample: u16 = 8;
    let num_channels: u16 = 1;
    let byte_rate = sample_rate * (num_channels as u32) * (bits_per_sample as u32 / 8);
    let block_align = num_channels * (bits_per_sample / 8);
    let data_size = num_samples;
    let file_size = 36 + data_size;

    let mut buf: Vec<u8> = Vec::with_capacity(44 + num_samples as usize);
    buf.extend_from_slice(b"RIFF");
    buf.extend_from_slice(&file_size.to_le_bytes());
    buf.extend_from_slice(b"WAVE");
    buf.extend_from_slice(b"fmt ");
    buf.extend_from_slice(&16u32.to_le_bytes());
    buf.extend_from_slice(&1u16.to_le_bytes());
    buf.extend_from_slice(&num_channels.to_le_bytes());
    buf.extend_from_slice(&sample_rate.to_le_bytes());
    buf.extend_from_slice(&byte_rate.to_le_bytes());
    buf.extend_from_slice(&block_align.to_le_bytes());
    buf.extend_from_slice(&bits_per_sample.to_le_bytes());
    buf.extend_from_slice(b"data");
    buf.extend_from_slice(&data_size.to_le_bytes());
    for i in 0..num_samples {
        buf.push((i % 256) as u8);
    }
    fs::write(path, &buf).unwrap();
}

/// Copy a file from test/general/ to a destination dir.
fn copy_test_file(name: &str, dest: &Path) {
    let src = test_general_dir().join(name);
    assert!(src.exists(), "Test data file {:?} not found", src);
    fs::copy(&src, dest.join(name)).unwrap();
}

/// Get list output as sorted Vec of names.
fn list_archive_in(bin: &Path, cwd: &Path, archive: &str) -> Vec<String> {
    let stdout = run_ok_in(bin, cwd, &["list", archive]);
    let mut names: Vec<String> = stdout
        .split('\n')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();
    names.sort();
    names
}

fn copy_dir_recursive(src: &Path, dst: &Path) {
    fs::create_dir_all(dst).unwrap();
    for entry in fs::read_dir(src).unwrap() {
        let entry = entry.unwrap();
        let ty = entry.file_type().unwrap();
        let dst_path = dst.join(entry.file_name());
        if ty.is_dir() {
            copy_dir_recursive(&entry.path(), &dst_path);
        } else {
            fs::copy(entry.path(), &dst_path).unwrap();
        }
    }
}

fn has_files_recursive(dir: &Path) -> bool {
    if !dir.exists() { return false; }
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        if entry.file_type().unwrap().is_file() { return true; }
        if entry.file_type().unwrap().is_dir() && has_files_recursive(&entry.path()) {
            return true;
        }
    }
    false
}

/// Parse `compare`'s listing into (marker, path) pairs, e.g. ("+", "added.txt")
/// or ("m", "folder/icons.lod:item085v5"), with the colour escapes stripped.
///
/// Asserting on these instead of on "the output mentions [+] somewhere" is the
/// difference between checking that compare ran and checking that it is right.
fn compare_listing(stdout: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for line in stdout.lines() {
        let mut clean = String::new();
        let mut chars = line.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\x1b' {
                // skip the CSI sequence up to its final letter
                while let Some(n) = chars.next() {
                    if n.is_ascii_alphabetic() {
                        break;
                    }
                }
            } else {
                clean.push(c);
            }
        }
        let clean = clean.trim();
        let bytes = clean.as_bytes();
        if bytes.len() > 4 && bytes[0] == b'[' && bytes[2] == b']' && bytes[3] == b' ' {
            // compare prints native separators; the assertions spell them `/`.
            out.push(((clean[1..2]).to_string(), clean[4..].replace('\\', "/")));
        }
    }
    out
}

fn assert_listed(listing: &[(String, String)], marker: &str, path: &str, label: &str) {
    assert!(listing.iter().any(|(m, p)| m == marker && p == path),
        "[{}] compare should report [{}] {:?}; got {:?}", label, marker, path, listing);
}

fn assert_not_listed(listing: &[(String, String)], path: &str, label: &str) {
    assert!(!listing.iter().any(|(_, p)| p == path),
        "[{}] compare should not mention {:?}; got {:?}", label, path, listing);
}

/// CRC-32 (the same polynomial `mmarch checksum` prints), so a test can state
/// an expected value instead of trusting whatever mmarch wrote to disk.
fn crc32_hex(data: &[u8]) -> String {
    let mut crc: u32 = 0xFFFF_FFFF;
    for &b in data {
        crc ^= b as u32;
        for _ in 0..8 {
            crc = if crc & 1 != 0 { (crc >> 1) ^ 0xEDB8_8320 } else { crc >> 1 };
        }
    }
    format!("{:08X}", crc ^ 0xFFFF_FFFF)
}

fn collect_files_recursive(dir: &Path, base: &Path) -> Vec<String> {
    let mut result = vec![];
    if !dir.exists() { return result; }
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if entry.file_type().unwrap().is_file() {
            let rel = path.strip_prefix(base).unwrap();
            result.push(rel.to_string_lossy().to_string().replace('\\', "/"));
        } else if entry.file_type().unwrap().is_dir() {
            result.extend(collect_files_recursive(&path, base));
        }
    }
    result.sort();
    result
}

// ===========================================================================
// C.1: H3 LOD full workflow
// ===========================================================================

#[test]
fn test_h3lod_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("h3lod_full_{}", label));

        write_test_file(&dir.join("test_orig.txt"), b"this is original test.txt");
        write_test_file(&dir.join("test_new.txt"), b"this is new test.txt");
        write_test_file(&dir.join("test.str"), b"this is test.str");
        copy_test_file("test.wav", &dir);
        copy_test_file("testpal.bmp", &dir);

        // Create h3lod (without extensionless "test" to avoid name collision with test.wav stem)
        run_ok_in(bin, &dir, &["create", "h3lod.lod", "h3lod", ".",
            "test.str", "test.wav", "testpal.bmp", "test_orig.txt", "test_new.txt"]);

        let names = list_archive_in(bin, &dir, "h3lod.lod");
        assert!(names.len() >= 4, "[{}] expected at least 4 files, got {:?}", label, names);

        // Delete two
        run_ok_in(bin, &dir, &["delete", "h3lod.lod", "test_orig.txt", "test_new.txt"]);

        // Rename
        run_ok_in(bin, &dir, &["rename", "h3lod.lod", "test.str", "test_ren.str"]);

        // Optimize
        run_ok_in(bin, &dir, &["optimize", "h3lod.lod"]);

        // List again
        let names = list_archive_in(bin, &dir, "h3lod.lod");
        assert!(!names.contains(&"test_orig.txt".to_string()), "[{}] test_orig.txt should be deleted", label);
        assert!(!names.contains(&"test_new.txt".to_string()), "[{}] test_new.txt should be deleted", label);
        assert!(names.contains(&"test_ren.str".to_string()), "[{}] test_ren.str should exist: {:?}", label, names);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.2: H3 SND
// ===========================================================================

#[test]
fn test_h3snd_from_bat() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("h3snd_bat_{}", label));
        copy_test_file("test.wav", &dir);

        run_ok_in(bin, &dir, &["create", "h3snd.snd", "h3snd", ".", "test.wav"]);

        let names = list_archive_in(bin, &dir, "h3snd.snd");
        assert!(names.contains(&"test".to_string()), "[{}] h3snd should contain 'test': {:?}", label, names);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.3: H3 VID
// ===========================================================================

#[test]
fn test_h3vid_from_bat() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("h3vid_bat_{}", label));
        copy_test_file("JVC.bik", &dir);

        run_ok_in(bin, &dir, &["create", "h3mm78vid.vid", "h3mm78vid", ".", "JVC.bik"]);

        let names = list_archive_in(bin, &dir, "h3mm78vid.vid");
        // VID may or may not strip extension depending on implementation
        assert!(names.contains(&"JVC".to_string()) || names.contains(&"JVC.bik".to_string()),
                "[{}] h3vid should contain JVC: {:?}", label, names);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.4: mmbitmapslod
// ===========================================================================

#[test]
fn test_mmbitmapslod_from_bat() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("bitmaps_bat_{}", label));
        copy_test_file("pal994.act", &dir);
        copy_test_file("testpal.bmp", &dir);

        run_ok_in(bin, &dir, &["create", "mmbitmapslod.bitmaps.lod", "mmbitmapslod",
            ".", "pal994.act", "testpal.bmp"]);

        let stdout = run_ok_in(bin, &dir, &["list", "mmbitmapslod.bitmaps.lod", "|"]);
        assert!(stdout.contains("|"), "[{}] pipe separator: {}", label, stdout);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.5: mmspriteslod
// ===========================================================================

#[test]
fn test_mmspriteslod_from_bat() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sprites_bat_{}", label));
        copy_test_file("testpal.bmp", &dir);

        run_ok_in(bin, &dir, &["create", "mmspriteslod.sprites.lod", "mmspriteslod", "."]);
        run_ok_in(bin, &dir, &["add", "mmspriteslod.sprites.lod", "testpal.bmp", "/p", "3"]);
        run_ok_in(bin, &dir, &["add", "mmspriteslod.sprites.lod", "testpal.bmp"]);

        let names = list_archive_in(bin, &dir, "mmspriteslod.sprites.lod");
        // Sprites LOD may strip the extension
        assert!(names.contains(&"testpal.bmp".to_string()) || names.contains(&"testpal".to_string()),
                "[{}] sprites should contain testpal: {:?}", label, names);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.6: mmiconslod full workflow
// ===========================================================================

#[test]
fn test_mmiconslod_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("icons_full_{}", label));

        copy_test_file("test.bin", &dir);
        copy_test_file("test.wav", &dir);
        copy_test_file("test.pcx", &dir);
        write_test_file(&dir.join("test_orig.txt"), b"this is original test.txt");
        write_test_file(&dir.join("test_new.txt"), b"this is new test.txt");
        write_test_file(&dir.join("test.str"), b"this is test.str");

        // Create with test.bin and test.wav
        run_ok_in(bin, &dir, &["create", "mmiconslod.icons.lod", "mmiconslod",
            ".", "test.bin", "test.wav"]);

        let names = list_archive_in(bin, &dir, "mmiconslod.icons.lod");
        assert_eq!(names.len(), 2, "[{}] should have 2 files: {:?}", label, names);

        // Add test.txt test.str test.pcx
        fs::copy(dir.join("test_orig.txt"), dir.join("test.txt")).unwrap();
        run_ok_in(bin, &dir, &["add", "mmiconslod.icons.lod", "test.txt", "test.str", "test.pcx"]);
        let names = list_archive_in(bin, &dir, "mmiconslod.icons.lod");
        assert_eq!(names.len(), 5, "[{}] should have 5 files: {:?}", label, names);

        // Replace test.txt with new content
        fs::copy(dir.join("test_new.txt"), dir.join("test.txt")).unwrap();
        run_ok_in(bin, &dir, &["add", "mmiconslod.icons.lod", "test.txt"]);

        // Rename
        run_ok_in(bin, &dir, &["rename", "mmiconslod.icons.lod", "test.txt", "test_ren.txt"]);

        // Optimize
        run_ok_in(bin, &dir, &["optimize", "mmiconslod.icons.lod"]);

        // List
        let names = list_archive_in(bin, &dir, "mmiconslod.icons.lod");
        assert!(names.contains(&"test_ren.txt".to_string()), "[{}] should have test_ren.txt: {:?}", label, names);
        assert!(!names.contains(&"test.txt".to_string()), "[{}] test.txt should be renamed: {:?}", label, names);

        // Verify replaced content
        let out = dir.join("out");
        run_ok_in(bin, &dir, &["extract", "mmiconslod.icons.lod", "out", "test_ren.txt"]);
        let content = fs::read(out.join("test_ren.txt")).unwrap();
        assert_eq!(content, b"this is new test.txt", "[{}] content should be replacement", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.7: Merge
// ===========================================================================

#[test]
fn test_merge_from_bat() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("merge_bat_{}", label));

        write_test_file(&dir.join("test.txt"), b"this is original test.txt");
        write_test_file(&dir.join("test"), b"this is test file without extension");
        copy_test_file("test.bin", &dir);

        // Create icons.lod with test.txt and test
        run_ok_in(bin, &dir, &["create", "icons.lod", "mmiconslod", ".", "test.txt", "test"]);

        // Create mmiconslod.icons.lod with test.bin and test.txt
        run_ok_in(bin, &dir, &["create", "mmiconslod.icons.lod", "mmiconslod", ".", "test.bin", "test.txt"]);

        // Create mm78save.lod with test.bin
        run_ok_in(bin, &dir, &["create", "mm78save.lod", "mm78save", ".", "test.bin"]);

        // Merge mm78save into mmiconslod
        run_ok_in(bin, &dir, &["merge", "mmiconslod.icons.lod", "mm78save.lod"]);
        let names = list_archive_in(bin, &dir, "mmiconslod.icons.lod");
        assert!(names.contains(&"test.bin".to_string()), "[{}] merge: {:?}", label, names);

        // Merge mmiconslod into icons
        run_ok_in(bin, &dir, &["merge", "icons.lod", "mmiconslod.icons.lod"]);
        let names = list_archive_in(bin, &dir, "icons.lod");
        assert!(names.contains(&"test.txt".to_string()), "[{}]: {:?}", label, names);
        assert!(names.contains(&"test.bin".to_string()), "[{}]: {:?}", label, names);
        assert!(names.contains(&"test".to_string()), "[{}]: {:?}", label, names);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.8: Delete shortcut
// ===========================================================================

#[test]
fn test_delete_shortcut_d() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("del_sc_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.txt"), b"B");
        run_ok_in(bin, &dir, &["create", "test.lod", "mmiconslod", ".", "a.txt", "b.txt"]);

        run_ok_in(bin, &dir, &["d", "test.lod", "a.txt"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(!names.contains(&"a.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"b.txt".to_string()), "[{}]", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.9: All archive types create
// ===========================================================================

#[test]
fn test_create_all_archive_types() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("all_types_{}", label));

        let types = vec![
            ("mm78gameslod.games.lod", "mm78gameslod"),
            ("mmsnd.snd", "mmsnd"),
            ("mm6vid.vid", "mm6vid"),
            ("mm8loclod.T.lod", "mm8loclod"),
            ("mm6gameslod.games.lod", "mm6gameslod"),
            ("mm78save.lod", "mm78save"),
            ("mm6save.mm6", "mm6save"),
        ];

        for (filename, archtype) in &types {
            run_ok_in(bin, &dir, &["create", filename, archtype, "."]);
            assert!(dir.join(filename).exists(), "[{}] {} should be created", label, archtype);
        }

        cleanup(&dir);
    }
}

// ===========================================================================
// C.10: Extract
// ===========================================================================

#[test]
fn test_extract_h3lod_to_folder() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ext_h3_{}", label));
        write_test_file(&dir.join("hero.txt"), b"hero data");
        write_test_file(&dir.join("map.bin"), &[0xDE, 0xAD]);

        run_ok_in(bin, &dir, &["create", "test.lod", "h3lod", ".", "hero.txt", "map.bin"]);
        run_ok_in(bin, &dir, &["extract", "test.lod", "my-h3lod-resource"]);

        assert!(dir.join("my-h3lod-resource").join("hero.txt").exists(), "[{}] hero.txt extracted", label);
        assert!(dir.join("my-h3lod-resource").join("map.bin").exists(), "[{}] map.bin extracted", label);

        cleanup(&dir);
    }
}

#[test]
fn test_extract_specific_file_from_bitmapslod() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ext_bmp_{}", label));
        copy_test_file("pal994.act", &dir);
        copy_test_file("testpal.bmp", &dir);

        run_ok_in(bin, &dir, &["create", "test.bitmaps.lod", "mmbitmapslod",
            ".", "pal994.act", "testpal.bmp"]);

        // Extract testpal.bmp specifically
        run_ok_in(bin, &dir, &["extract", "test.bitmaps.lod", "out", "testpal.bmp"]);
        assert!(has_files_recursive(&dir.join("out")), "[{}] should extract testpal.bmp", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.11: Batch archive extraction (wildcard)
// ===========================================================================

#[test]
fn test_batch_extract_wildcard() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("batch_ext_{}", label));
        fs::create_dir_all(dir.join("sub1")).unwrap();
        fs::create_dir_all(dir.join("sub2")).unwrap();

        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.txt"), b"B");

        run_ok_in(bin, &dir, &["create", "arch1.lod", "mmiconslod", "sub1", "a.txt"]);
        run_ok_in(bin, &dir, &["create", "arch2.lod", "mmiconslod", "sub2", "b.txt"]);

        // Extract */*.lod . *.txt
        run_ok_in(bin, &dir, &["extract", "*/*.lod", "resource_folder", "*.txt"]);

        cleanup(&dir);
    }
}

// ===========================================================================
// C.12: Checksum tests
// ===========================================================================

fn create_checksum_archive(bin: &Path, dir: &Path) {
    write_test_file(&dir.join("hello.txt"), b"Hello World");
    write_test_file(&dir.join("data.bin"), &[1, 2, 3, 4, 5]);
    write_test_file(&dir.join("notes.txt"), b"Some notes here");
    write_test_file(&dir.join("test.str"), b"this is test.str");

    run_ok_in(bin, dir, &["create", "test.lod", "mmiconslod",
        ".", "hello.txt", "data.bin", "notes.txt", "test.str"]);
}

#[test]
fn test_checksum_archive_file() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_arch_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod"]);
        let line = stdout.trim();
        assert!(line.contains("test.lod"), "[{}] should contain filename: {}", label, line);
        let parts: Vec<&str> = line.splitn(2, "  ").collect();
        assert_eq!(parts.len(), 2, "[{}] hash + filename", label);
        assert_eq!(parts[0].len(), 8, "[{}] 8 hex chars", label);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_all_resources() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_all_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "*"]);
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        assert_eq!(lines.len(), 4, "[{}] 4 resources: {:?}", label, lines);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_specific_resource() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_spec_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "test.str"]);
        assert!(stdout.contains("test.str"), "[{}]: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_wildcard() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_wild_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "*.str"]);
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        assert_eq!(lines.len(), 1, "[{}] 1 .str match: {:?}", label, lines);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_write_and_verify() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_wv_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "*"]);
        fs::write(dir.join("checksum_all.crc32"), &stdout).unwrap();

        // Verify --v
        let (stdout2, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--v", "checksum_all.crc32"]);
        assert!(ok, "[{}] verify should succeed: {}", label, stdout2);
        assert!(stdout2.contains("OK"), "[{}] should contain OK: {}", label, stdout2);

        // Verify --vall
        let (_, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--vall", "checksum_all.crc32"]);
        assert!(ok, "[{}] verify all should succeed", label);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_verify_flag_variants() {
    // -v, --v, -vall, --vall should all work as verify flags.
    // Bare "v" and "vall" must NOT be treated as flags (they are resource names).
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_flags_{}", label));

        // Create archive with files including one named "v"
        write_test_file(&dir.join("hello.txt"), b"Hello World");
        write_test_file(&dir.join("v"), b"I am a file named v");
        run_ok_in(bin, &dir, &["create", "test.lod", "mmiconslod",
            ".", "hello.txt", "v"]);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "*"]);
        fs::write(dir.join("all.crc32"), &stdout).unwrap();

        // Flags with leading dash should work as verify
        for flag in &["-v", "--v"] {
            let (stdout2, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", flag, "all.crc32"]);
            assert!(ok, "[{}] '{}' should work as verify: {}", label, flag, stdout2);
            assert!(stdout2.contains("OK"), "[{}] '{}' should contain OK", label, flag);
        }

        for flag in &["-vall", "--vall"] {
            let (_, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", flag, "all.crc32"]);
            assert!(ok, "[{}] '{}' should work as verify-all", label, flag);
        }

        // These must NOT be treated as verify flags — they are resource names or paths.
        // They should go through generate mode and produce checksum output (or empty output).
        for non_flag in &["v", "/v", "vall", "/vall"] {
            let (stdout3, stderr3, ok) = run_in(bin, &dir, &["checksum", "test.lod", non_flag]);
            assert!(ok, "[{}] '{}' should not error (treated as resource filter): stderr={}",
                    label, non_flag, stderr3);
            // Should NOT produce verify-style "OK" or "FAILED" output
            assert!(!stdout3.contains(": OK") && !stdout3.contains(": FAILED"),
                    "[{}] '{}' must not be treated as verify flag: {}", label, non_flag, stdout3);
        }

        // Bare "v" specifically should produce a checksum for the file named "v"
        let stdout4 = run_ok_in(bin, &dir, &["checksum", "test.lod", "v"]);
        assert!(stdout4.contains("v"), "[{}] bare 'v' should checksum the resource: {}", label, stdout4);
        // Output is "HASH  v.bmp" (extracted name has .bmp since mmiconslod)
        assert!(stdout4.trim().trim_end_matches('\r').starts_with(|c: char| c.is_ascii_hexdigit()),
                "[{}] should start with hex hash: {}", label, stdout4);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_shortcut_s() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_s_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["s", "test.lod", "*"]);
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        assert_eq!(lines.len(), 4, "[{}] s shortcut: {:?}", label, lines);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_inline_verify() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_iv_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "test.str"]);
        let line = stdout.trim().trim_end_matches('\r');
        let hash = &line[..8];

        let pair = format!("test.str:{}", hash);
        let (stdout2, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--v", &pair]);
        assert!(ok, "[{}] inline verify should succeed", label);
        assert!(stdout2.contains("OK"), "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_verify_failure() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_fail_{}", label));
        create_checksum_archive(bin, &dir);

        let (_, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--v", "test.str:00000000"]);
        assert!(!ok, "[{}] verify should fail with wrong hash", label);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_verify_all_missing() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_vm_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "hello.txt"]);
        fs::write(dir.join("partial.crc32"), &stdout).unwrap();

        let (stdout, stderr, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--vall", "partial.crc32"]);
        let combined = format!("{}{}", stdout, stderr).to_lowercase();
        assert!(!ok || combined.contains("not listed") || combined.contains("fail"),
                "[{}] verify all should fail when partial. ok={} out={} err={}", label, ok, stdout, stderr);

        cleanup(&dir);
    }
}

#[test]
fn test_checksum_inline_verify_all() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_iva_{}", label));
        create_checksum_archive(bin, &dir);

        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "*"]);
        let mut pairs: Vec<String> = Vec::new();
        for line in stdout.lines() {
            let line = line.trim_end_matches('\r');
            if line.is_empty() { continue; }
            let parts: Vec<&str> = line.splitn(2, "  ").collect();
            if parts.len() == 2 {
                pairs.push(format!("{}:{}", parts[1], parts[0]));
            }
        }

        let mut args: Vec<&str> = vec!["checksum", "test.lod", "--vall"];
        for p in &pairs { args.push(p); }
        let (_, _, ok) = run_in(bin, &dir, &args);
        assert!(ok, "[{}] inline verify all should succeed", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// D.1: Compare two folders (v1 vs v2)
// ===========================================================================

#[test]
fn test_compare_v1_v2_folders() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("cmp_v1v2_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));

        let (stdout, _, ok) = run_in(bin, &dir, &["compare", "v1", "v2"]);
        assert!(ok, "[{}] compare v1 v2 should succeed", label);
        let listing = compare_listing(&stdout);

        // Plain files
        assert_listed(&listing, "+", "added.txt", label);
        assert_listed(&listing, "-", "deleted.txt", label);
        assert_listed(&listing, "m", "modified.txt", label);
        // A file present and identical in both must not show up at all
        assert_not_listed(&listing, "unchanged.txt", label);

        // Whole folders are reported as folders...
        assert_listed(&listing, "+", "added folder文件夹/", label);
        assert_listed(&listing, "-", "deleted folder文件夹/", label);
        // ...and their contents are not listed again one by one
        assert_not_listed(&listing, "deleted folder文件夹/deleted.lod", label);

        // Resources inside an archive, addressed with `archive:resource`
        assert_listed(&listing, "m", "folder w modified files/icons.lod:item085v5", label);
        assert_listed(&listing, "+", "folder w modified files/icons.lod:item251v5a", label);
        assert_listed(&listing, "-", "folder w modified files/icons.lod:item114v5", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// D.2: Compare with nsis
// ===========================================================================

#[test]
fn test_compare_nsis_generation() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("cmp_nsis_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));

        run_ok_in(bin, &dir, &["compare", "v1", "v2", "nsis", "nsis_folder/script.nsi", "files"]);

        let script = dir.join("nsis_folder").join("script.nsi");
        assert!(script.exists(), "[{}] NSIS script created", label);
        let content = fs::read_to_string(&script).unwrap();
        assert!(content.contains("!include"), "[{}] !include", label);
        assert!(content.contains("Section"), "[{}] Section", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// D.3: Compare with batch
// ===========================================================================

#[test]
fn test_compare_batch_generation() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("cmp_batch_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));

        run_ok_in(bin, &dir, &["compare", "v1", "v2", "batch", "batch_folder/script.bat", "files"]);

        let script = dir.join("batch_folder").join("script.bat");
        assert!(script.exists(), "[{}] batch script created", label);
        let content = fs::read_to_string(&script).unwrap();
        assert!(content.contains("cd %~dp0"), "[{}] cd %%~dp0", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// D.4: Compare filesonly
// ===========================================================================

#[test]
fn test_compare_filesonly_v1_v2() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("cmp_fo_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));

        run_ok_in(bin, &dir, &["compare", "v1", "v2", "filesonly", "diff"]);
        let files = collect_files_recursive(&dir.join("diff"), &dir.join("diff"));

        assert!(files.iter().any(|f| f.contains("added")),
                "[{}] diff should contain added files: {:?}", label, files);
        assert!(files.iter().any(|f| f.contains(".todelete")),
                "[{}] diff should contain .todelete markers: {:?}", label, files);
        assert!(files.iter().any(|f| f.contains(".mmarchive")),
                "[{}] diff should contain .mmarchive: {:?}", label, files);

        cleanup(&dir);
    }
}

// ===========================================================================
// D.5: df2n
// ===========================================================================

#[test]
fn test_df2n_from_compare() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("df2n_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));

        run_ok_in(bin, &dir, &["compare", "v1", "v2", "filesonly", "diff"]);
        run_ok_in(bin, &dir, &["df2n", "diff", "nsis_out/script.nsi", "files"]);

        let script = dir.join("nsis_out").join("script.nsi");
        assert!(script.exists(), "[{}] df2n script created", label);
        let content = fs::read_to_string(&script).unwrap();
        assert!(content.contains("Section"), "[{}] Section", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// D.6: df2b
// ===========================================================================

#[test]
fn test_df2b_from_compare() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("df2b_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));

        run_ok_in(bin, &dir, &["compare", "v1", "v2", "filesonly", "diff"]);
        run_ok_in(bin, &dir, &["df2b", "diff", "batch_out/script.bat", "files"]);

        let script = dir.join("batch_out").join("script.bat");
        assert!(script.exists(), "[{}] df2b script created", label);
        let content = fs::read_to_string(&script).unwrap();
        assert!(content.contains("cd %~dp0"), "[{}] cd %%~dp0", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// D.7: Compare v2 vs v3
// ===========================================================================

#[test]
fn test_compare_v2_v3_folders() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("cmp_v2v3_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));
        copy_dir_recursive(&test_compare_dir().join("compare test v3"), &dir.join("v3"));

        let (stdout, _, ok) = run_in(bin, &dir, &["compare", "v2", "v3"]);
        assert!(ok, "[{}] compare v2 v3 succeed", label);
        let listing = compare_listing(&stdout);

        // v3 drops what v2 added and restores the folder v2 had deleted, so
        // the markers are the mirror image of the v1 -> v2 run.
        assert_listed(&listing, "-", "added.txt", label);
        assert_listed(&listing, "-", "added folder文件夹2/", label);
        assert_listed(&listing, "+", "deleted folder文件夹/", label);
        assert_listed(&listing, "-", "folder w modified files/modified.txt", label);
        assert_not_listed(&listing, "unchanged.txt", label);

        // Archive resources again, this time in a .lod inside an added folder
        assert_listed(&listing, "m", "added folder文件夹/added.lod:7item071v5a", label);
        assert_listed(&listing, "+", "added folder文件夹/added.lod:7item071v5b", label);
        assert_listed(&listing, "m", "folder w modified files/icons.lod:chn5iconv5", label);
        assert_listed(&listing, "+", "folder w modified files/icons.lod:chn5iconv5b", label);

        // NSIS
        run_ok_in(bin, &dir, &["compare", "v2", "v3", "nsis", "nsis/script.nsi", "files"]);
        assert!(dir.join("nsis").join("script.nsi").exists(), "[{}] nsis", label);

        // Batch
        run_ok_in(bin, &dir, &["compare", "v2", "v3", "batch", "batch/script.bat", "files"]);
        assert!(dir.join("batch").join("script.bat").exists(), "[{}] batch", label);

        // Filesonly + df2n + df2b
        run_ok_in(bin, &dir, &["compare", "v2", "v3", "filesonly", "diff"]);
        run_ok_in(bin, &dir, &["df2n", "diff", "nsis2/script.nsi", "files"]);
        assert!(dir.join("nsis2").join("script.nsi").exists(), "[{}] df2n", label);

        run_ok_in(bin, &dir, &["compare", "v2", "v3", "filesonly", "diff"]);
        run_ok_in(bin, &dir, &["df2b", "diff", "batch2/script.bat", "files"]);
        assert!(dir.join("batch2").join("script.bat").exists(), "[{}] df2b", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// D.8: Compare two archive files
// ===========================================================================

#[test]
fn test_compare_two_archives_from_testdata() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("cmp_arch_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));

        let (stdout, _, ok) = run_in(bin, &dir, &["compare",
            "v1/folder w modified files/icons.lod",
            "v2/folder w modified files/icons.lod"]);
        assert!(ok, "[{}] compare archives succeed", label);
        assert!(stdout.contains("[+]") || stdout.contains("[-]") || stdout.contains("[m]"),
                "[{}] archive diffs: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_compare_archive_filesonly() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("cmp_arch_fo_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));

        run_ok_in(bin, &dir, &["compare",
            "v1/folder w modified files/icons.lod",
            "v2/folder w modified files/icons.lod",
            "filesonly", "diff"]);

        run_ok_in(bin, &dir, &["df2n", "diff", "nsis/script.nsi", "files"]);
        assert!(dir.join("nsis").join("script.nsi").exists(), "[{}] df2n", label);

        run_ok_in(bin, &dir, &["compare",
            "v1/folder w modified files/icons.lod",
            "v2/folder w modified files/icons.lod",
            "filesonly", "diff"]);
        run_ok_in(bin, &dir, &["df2b", "diff", "batch/script.bat", "files"]);
        assert!(dir.join("batch").join("script.bat").exists(), "[{}] df2b", label);

        run_ok_in(bin, &dir, &["compare",
            "v1/folder w modified files/icons.lod",
            "v2/folder w modified files/icons.lod",
            "nsis", "nsis2/script.nsi", "files"]);
        assert!(dir.join("nsis2").join("script.nsi").exists(), "[{}] nsis direct", label);

        run_ok_in(bin, &dir, &["compare",
            "v1/folder w modified files/icons.lod",
            "v2/folder w modified files/icons.lod",
            "batch", "batch2/script.bat", "files"]);
        assert!(dir.join("batch2").join("script.bat").exists(), "[{}] batch direct", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// E.1: 3-version merger
// ===========================================================================

#[test]
fn test_3ver_merger_v1_v2_v3() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode in test data paths
        let dir = temp_dir(&format!("3ver_{}", label));
        copy_dir_recursive(&test_compare_dir().join("compare test v1"), &dir.join("v1"));
        copy_dir_recursive(&test_compare_dir().join("compare test v2"), &dir.join("v2"));
        copy_dir_recursive(&test_compare_dir().join("compare test v3"), &dir.join("v3"));

        // v1->v2 then v2->v3 to same diff
        run_ok_in(bin, &dir, &["compare", "v1", "v2", "filesonly", "diff_12_23"]);
        run_ok_in(bin, &dir, &["compare", "v2", "v3", "filesonly", "diff_12_23"]);

        // v1->v3 then v2->v3 to same diff
        run_ok_in(bin, &dir, &["compare", "v1", "v3", "filesonly", "diff_13_23"]);
        run_ok_in(bin, &dir, &["compare", "v2", "v3", "filesonly", "diff_13_23"]);

        // Compare both approaches - they may differ slightly, just verify both exist
        let (_stdout, _, ok) = run_in(bin, &dir, &["compare", "diff_12_23", "diff_13_23"]);
        assert!(ok, "[{}] comparing diffs succeed", label);
        assert!(dir.join("diff_12_23").exists(), "[{}] diff 12-23 should exist", label);
        assert!(dir.join("diff_13_23").exists(), "[{}] diff 13-23 should exist", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: CLI shortcuts
// ===========================================================================

#[test]
fn test_cli_shortcuts() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sc_{}", label));
        write_test_file(&dir.join("s.txt"), b"shortcut test");

        // c = create
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "s.txt"]);

        // l = list
        let stdout = run_ok_in(bin, &dir, &["l", "test.lod"]);
        assert!(stdout.contains("s.txt"), "[{}] l", label);

        // a = add
        write_test_file(&dir.join("t.txt"), b"T");
        run_ok_in(bin, &dir, &["a", "test.lod", "t.txt"]);

        // r = rename
        run_ok_in(bin, &dir, &["r", "test.lod", "t.txt", "u.txt"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"u.txt".to_string()), "[{}] r", label);

        // d = delete
        run_ok_in(bin, &dir, &["d", "test.lod", "u.txt"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(!names.contains(&"u.txt".to_string()), "[{}] d", label);

        // o = optimize
        run_ok_in(bin, &dir, &["o", "test.lod"]);

        // e = extract
        run_ok_in(bin, &dir, &["e", "test.lod", "out"]);
        assert!(dir.join("out").join("s.txt").exists(), "[{}] e", label);

        // h = help
        let (stdout, _, ok) = run_in(bin, &dir, &["h"]);
        assert!(ok, "[{}] h", label);
        assert!(stdout.contains("extract"), "[{}] h content", label);

        // k = compare
        fs::copy(dir.join("test.lod"), dir.join("test2.lod")).unwrap();
        let (stdout, _, ok) = run_in(bin, &dir, &["k", "test.lod", "test2.lod"]);
        assert!(ok, "[{}] k", label);
        assert!(stdout.contains("same"), "[{}] k same", label);

        // m = merge
        write_test_file(&dir.join("v.txt"), b"V");
        run_ok_in(bin, &dir, &["c", "test3.lod", "mmiconslod", ".", "v.txt"]);
        run_ok_in(bin, &dir, &["m", "test.lod", "test3.lod"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"v.txt".to_string()), "[{}] m", label);

        // s = checksum
        let stdout = run_ok_in(bin, &dir, &["s", "test.lod"]);
        assert!(stdout.contains("test.lod"), "[{}] s", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Leading dash stripping
// ===========================================================================

#[test]
fn test_leading_dash_stripping() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("dash_{}", label));
        write_test_file(&dir.join("f.txt"), b"dash test");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "f.txt"]);

        // -e = extract
        run_ok_in(bin, &dir, &["-e", "test.lod", "out"]);
        assert!(dir.join("out").join("f.txt").exists(), "[{}] -e should work", label);

        // --list
        let stdout = run_ok_in(bin, &dir, &["--list", "test.lod"]);
        assert!(stdout.contains("f.txt"), "[{}] --list should work", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Error cases
// ===========================================================================

#[test]
fn test_error_unknown_command() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("err_unk_{}", label));
        let (stdout, stderr, ok) = run_in(bin, &dir, &["nonexistent_command"]);
        let combined = format!("{}{}", stdout, stderr).to_lowercase();
        // Both binaries should print an error message.
        // Delphi returns exit 0; Rust returns exit 1. We only check the message.
        assert!(!ok || combined.contains("unknown") || combined.contains("error"),
                "[{}] unknown command should error. ok={} stdout={} stderr={}", label, ok, stdout, stderr);
        cleanup(&dir);
    }
}

#[test]
fn test_error_missing_params() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("err_miss_{}", label));
        let (stdout, stderr, ok) = run_in(bin, &dir, &["list"]);
        let combined = format!("{}{}", stdout, stderr).to_lowercase();
        assert!(!ok || combined.contains("error") || combined.contains("usage") || combined.contains("mmarch"),
                "[{}] missing arg should error. ok={} stdout={} stderr={}", label, ok, stdout, stderr);
        cleanup(&dir);
    }
}

#[test]
fn test_no_args_shows_help() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("noargs_{}", label));
        let (stdout, _, _) = run_in(bin, &dir, &[]);
        assert!(stdout.to_lowercase().contains("mmarch") || stdout.to_lowercase().contains("usage"),
                "[{}] no args should show help", label);
        cleanup(&dir);
    }
}

// ===========================================================================
// F: Wildcard support
// ===========================================================================

#[test]
fn test_wildcard_extract_txt() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("wild_et_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.txt"), b"B");
        write_test_file(&dir.join("c.bin"), b"C");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt", "b.txt", "c.bin"]);

        run_ok_in(bin, &dir, &["extract", "test.lod", "out", "*.txt"]);
        assert!(dir.join("out/a.txt").exists(), "[{}] a.txt", label);
        assert!(dir.join("out/b.txt").exists(), "[{}] b.txt", label);
        assert!(!dir.join("out/c.bin").exists(), "[{}] c.bin NOT", label);

        cleanup(&dir);
    }
}

#[test]
fn test_wildcard_extract_all() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("wild_ea_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.bin"), b"B");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt", "b.bin"]);

        run_ok_in(bin, &dir, &["extract", "test.lod", "out", "*.*"]);
        assert!(dir.join("out/a.txt").exists(), "[{}] a.txt", label);
        assert!(dir.join("out/b.bin").exists(), "[{}] b.bin", label);

        cleanup(&dir);
    }
}

#[test]
fn test_wildcard_delete() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("wild_del_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.txt"), b"B");
        write_test_file(&dir.join("c.bin"), b"C");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt", "b.txt", "c.bin"]);

        run_ok_in(bin, &dir, &["delete", "test.lod", "*.txt"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(!names.contains(&"a.txt".to_string()), "[{}]", label);
        assert!(!names.contains(&"b.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"c.bin".to_string()), "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_wildcard_no_ext() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("wild_ne_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b"), b"B");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt", "b"]);

        // *. should match extensionless files; just verify no crash
        let _ = run_in(bin, &dir, &["extract", "test.lod", "out", "*."]);
        cleanup(&dir);
    }
}

// ===========================================================================
// F: Empty archive operations
// ===========================================================================

#[test]
fn test_empty_archive_operations() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("empty_{}", label));

        run_ok_in(bin, &dir, &["c", "empty.lod", "mmiconslod", "."]);

        let names = list_archive_in(bin, &dir, "empty.lod");
        assert!(names.is_empty(), "[{}] empty archive: {:?}", label, names);

        run_ok_in(bin, &dir, &["e", "empty.lod", "out"]);
        run_ok_in(bin, &dir, &["o", "empty.lod"]);

        let names = list_archive_in(bin, &dir, "empty.lod");
        assert!(names.is_empty(), "[{}] still empty after optimize", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Path with spaces and unicode
// ===========================================================================

#[test]
fn test_path_with_spaces() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("spaces_{}", label));
        fs::create_dir_all(dir.join("my folder")).unwrap();

        write_test_file(&dir.join("hello world.txt"), b"spaced content");
        run_ok_in(bin, &dir, &["create", "test.lod", "mmiconslod", "my folder", "hello world.txt"]);

        let names = list_archive_in(bin, &dir, "my folder/test.lod");
        assert!(names.contains(&"hello world.txt".to_string()),
                "[{}] spaced path: {:?}", label, names);

        run_ok_in(bin, &dir, &["extract", "my folder/test.lod", "out dir"]);
        assert_eq!(fs::read(dir.join("out dir/hello world.txt")).unwrap(), b"spaced content");

        cleanup(&dir);
    }
}

#[test]
fn test_path_with_unicode() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi ANSI API can't handle unicode paths (System Error Code 123)

        let dir = temp_dir(&format!("unicode_{}", label));
        let sub = "folder_\u{4F60}\u{597D}";
        fs::create_dir_all(dir.join(sub)).unwrap();

        write_test_file(&dir.join("data.txt"), b"unicode path content");
        run_ok_in(bin, &dir, &["create", "test.lod", "mmiconslod", sub, "data.txt"]);

        let archive = format!("{}/test.lod", sub);
        let names = list_archive_in(bin, &dir, &archive);
        assert!(names.contains(&"data.txt".to_string()), "[{}] unicode: {:?}", label, names);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Replace on add
// ===========================================================================

#[test]
fn test_replace_file_on_add() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("repl_{}", label));
        write_test_file(&dir.join("f.txt"), b"version1");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "f.txt"]);

        write_test_file(&dir.join("f.txt"), b"version2");
        run_ok_in(bin, &dir, &["a", "test.lod", "f.txt"]);

        let names = list_archive_in(bin, &dir, "test.lod");
        let count = names.iter().filter(|n| n.eq_ignore_ascii_case("f.txt")).count();
        assert_eq!(count, 1, "[{}] replace not duplicate", label);

        run_ok_in(bin, &dir, &["e", "test.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/f.txt")).unwrap(), b"version2");

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Multiple add/delete cycles
// ===========================================================================

#[test]
fn test_multiple_add_delete_cycles() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cycles_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        write_test_file(&dir.join("b.txt"), b"B");
        run_ok_in(bin, &dir, &["a", "test.lod", "b.txt"]);
        write_test_file(&dir.join("c.txt"), b"C");
        run_ok_in(bin, &dir, &["a", "test.lod", "c.txt"]);
        run_ok_in(bin, &dir, &["d", "test.lod", "a.txt"]);
        write_test_file(&dir.join("d.txt"), b"D");
        run_ok_in(bin, &dir, &["a", "test.lod", "d.txt"]);
        run_ok_in(bin, &dir, &["d", "test.lod", "b.txt"]);

        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(!names.contains(&"a.txt".to_string()), "[{}]", label);
        assert!(!names.contains(&"b.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"c.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"d.txt".to_string()), "[{}]", label);

        run_ok_in(bin, &dir, &["e", "test.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/c.txt")).unwrap(), b"C");
        assert_eq!(fs::read(dir.join("out/d.txt")).unwrap(), b"D");

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Cross-format round-trips
// ===========================================================================

fn roundtrip_test(bin: &Path, label: &str, archive_type: &str, ext: &str, prefix: &str) {
    let dir = temp_dir(&format!("{}_{}", prefix, label));
    let archive_name = format!("test.{}", ext);

    let content_a = b"AAAA content for round trip testing with some repetition AAAA";
    let content_b: Vec<u8> = (0..200).map(|i| (i % 251) as u8).collect();

    if ext == "snd" {
        create_wav(&dir.join("alpha.wav"));
        create_wav(&dir.join("beta.wav"));
        let wav_a = fs::read(dir.join("alpha.wav")).unwrap();
        let wav_b = fs::read(dir.join("beta.wav")).unwrap();

        run_ok_in(bin, &dir, &["create", &archive_name, archive_type, ".", "alpha.wav"]);
        run_ok_in(bin, &dir, &["add", &archive_name, "beta.wav"]);
        run_ok_in(bin, &dir, &["optimize", &archive_name]);

        run_ok_in(bin, &dir, &["extract", &archive_name, "extracted"]);
        assert_eq!(fs::read(dir.join("extracted/alpha.wav")).unwrap(), wav_a, "[{}]", label);
        assert_eq!(fs::read(dir.join("extracted/beta.wav")).unwrap(), wav_b, "[{}]", label);
    } else if ext == "vid" {
        write_test_file(&dir.join("clip1.smk"), content_a);
        write_test_file(&dir.join("clip2.smk"), &content_b);

        run_ok_in(bin, &dir, &["create", &archive_name, archive_type, ".", "clip1.smk"]);
        run_ok_in(bin, &dir, &["add", &archive_name, "clip2.smk"]);
        run_ok_in(bin, &dir, &["optimize", &archive_name]);

        run_ok_in(bin, &dir, &["extract", &archive_name, "extracted"]);
        assert_eq!(fs::read(dir.join("extracted/clip1.smk")).unwrap(), content_a.to_vec(), "[{}]", label);
        assert_eq!(fs::read(dir.join("extracted/clip2.smk")).unwrap(), content_b, "[{}]", label);
    } else {
        write_test_file(&dir.join("alpha.txt"), content_a);
        write_test_file(&dir.join("beta.bin"), &content_b);

        run_ok_in(bin, &dir, &["create", &archive_name, archive_type, ".", "alpha.txt"]);
        run_ok_in(bin, &dir, &["add", &archive_name, "beta.bin"]);
        run_ok_in(bin, &dir, &["optimize", &archive_name]);

        run_ok_in(bin, &dir, &["extract", &archive_name, "extracted"]);
        // Some LOD types (sprites, bitmaps) may strip extensions
        let alpha_path = if dir.join("extracted/alpha.txt").exists() {
            dir.join("extracted/alpha.txt")
        } else {
            dir.join("extracted/alpha")
        };
        let beta_path = if dir.join("extracted/beta.bin").exists() {
            dir.join("extracted/beta.bin")
        } else {
            dir.join("extracted/beta")
        };
        assert_eq!(fs::read(&alpha_path).unwrap(), content_a.to_vec(), "[{}] alpha roundtrip", label);
        assert_eq!(fs::read(&beta_path).unwrap(), content_b, "[{}] beta roundtrip", label);
    }

    cleanup(&dir);
}

#[test]
fn test_roundtrip_h3lod() {
    for (label, ref bin) in get_binaries() { roundtrip_test(bin, label, "h3lod", "lod", "rt_h3lod"); }
}

#[test]
fn test_roundtrip_mmiconslod() {
    for (label, ref bin) in get_binaries() { roundtrip_test(bin, label, "mmiconslod", "lod", "rt_icons"); }
}

#[test]
fn test_roundtrip_mmbitmapslod() {
    for (label, ref bin) in get_binaries() { roundtrip_test(bin, label, "mmbitmapslod", "lod", "rt_bitmaps"); }
}

#[test]
fn test_roundtrip_mmspriteslod() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi rejects non-bitmap files in sprites LOD
        roundtrip_test(bin, label, "mmspriteslod", "lod", "rt_sprites");
    }
}

#[test]
fn test_roundtrip_mm78gameslod() {
    for (label, ref bin) in get_binaries() { roundtrip_test(bin, label, "mm78gameslod", "lod", "rt_games78"); }
}

#[test]
fn test_roundtrip_h3snd() {
    for (label, ref bin) in get_binaries() { roundtrip_test(bin, label, "h3snd", "snd", "rt_h3snd"); }
}

#[test]
fn test_roundtrip_mmsnd() {
    for (label, ref bin) in get_binaries() { roundtrip_test(bin, label, "mmsnd", "snd", "rt_mmsnd"); }
}

#[test]
fn test_roundtrip_h3mm78vid() {
    for (label, ref bin) in get_binaries() { roundtrip_test(bin, label, "h3mm78vid", "vid", "rt_h3vid"); }
}

#[test]
fn test_roundtrip_mm6vid() {
    for (label, ref bin) in get_binaries() { roundtrip_test(bin, label, "mm6vid", "vid", "rt_mm6vid"); }
}

// ===========================================================================
// F: Games LOD specific (.blv, .odm, .dlv, .ddm)
// ===========================================================================

#[test]
fn test_games_lod_blv_roundtrip() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("games_blv_{}", label));
        let blv: Vec<u8> = (0..256).map(|i| (i % 256) as u8).collect();
        write_test_file(&dir.join("dungeon.blv"), &blv);
        write_test_file(&dir.join("readme.txt"), b"plain text");

        run_ok_in(bin, &dir, &["create", "test.lod", "mm78gameslod", ".", "dungeon.blv", "readme.txt"]);
        run_ok_in(bin, &dir, &["extract", "test.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/dungeon.blv")).unwrap(), blv, "[{}]", label);
        assert_eq!(fs::read(dir.join("out/readme.txt")).unwrap(), b"plain text", "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_games_lod_dlv_ddm_roundtrip() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("games_dlv_{}", label));
        let dlv: Vec<u8> = (0..300).map(|i| ((i * 13 + 7) % 256) as u8).collect();
        let ddm: Vec<u8> = (0..400).map(|i| ((i * 17 + 3) % 256) as u8).collect();

        write_test_file(&dir.join("level.dlv"), &dlv);
        write_test_file(&dir.join("map.ddm"), &ddm);

        run_ok_in(bin, &dir, &["c", "test.lod", "mm78gameslod", ".", "level.dlv", "map.ddm"]);
        run_ok_in(bin, &dir, &["e", "test.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/level.dlv")).unwrap(), dlv, "[{}]", label);
        assert_eq!(fs::read(dir.join("out/map.ddm")).unwrap(), ddm, "[{}]", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Additional tests
// ===========================================================================

#[test]
fn test_list_custom_separator() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sep_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.txt"), b"B");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt", "b.txt"]);

        let stdout = run_ok_in(bin, &dir, &["list", "test.lod", ","]);
        assert!(stdout.contains(","), "[{}] comma sep: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_list_pipe_separator() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("pipe_{}", label));
        write_test_file(&dir.join("x.txt"), b"X");
        write_test_file(&dir.join("y.txt"), b"Y");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "x.txt", "y.txt"]);

        let stdout = run_ok_in(bin, &dir, &["list", "test.lod", "|"]);
        assert!(stdout.contains("|"), "[{}] pipe sep: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_merge_replaces_existing() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("merge_r_{}", label));
        write_test_file(&dir.join("f.txt"), b"original");
        run_ok_in(bin, &dir, &["c", "a.lod", "mmiconslod", ".", "f.txt"]);

        write_test_file(&dir.join("f.txt"), b"replacement");
        run_ok_in(bin, &dir, &["c", "b.lod", "mmiconslod", ".", "f.txt"]);

        run_ok_in(bin, &dir, &["merge", "a.lod", "b.lod"]);
        run_ok_in(bin, &dir, &["extract", "a.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/f.txt")).unwrap(), b"replacement", "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_large_file_roundtrip() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("large_{}", label));
        let content: Vec<u8> = (0..65536).map(|i| ((i * 31 + 17) % 256) as u8).collect();
        write_test_file(&dir.join("big.bin"), &content);

        run_ok_in(bin, &dir, &["c", "test.lod", "h3lod", ".", "big.bin"]);
        run_ok_in(bin, &dir, &["e", "test.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/big.bin")).unwrap(), content, "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_multiple_file_add() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("madd_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        write_test_file(&dir.join("b.txt"), b"B");
        write_test_file(&dir.join("c.txt"), b"C");
        write_test_file(&dir.join("d.txt"), b"D");
        run_ok_in(bin, &dir, &["add", "test.lod", "b.txt", "c.txt", "d.txt"]);

        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"b.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"c.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"d.txt".to_string()), "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_multiple_file_delete() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("mdel_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.txt"), b"B");
        write_test_file(&dir.join("c.txt"), b"C");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt", "b.txt", "c.txt"]);

        run_ok_in(bin, &dir, &["delete", "test.lod", "a.txt", "b.txt"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(!names.contains(&"a.txt".to_string()), "[{}]", label);
        assert!(!names.contains(&"b.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"c.txt".to_string()), "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_case_insensitive_find() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("case_{}", label));
        write_test_file(&dir.join("Hello.TXT"), b"hi");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "Hello.TXT"]);

        run_ok_in(bin, &dir, &["e", "test.lod", "out", "hello.txt"]);
        assert!(dir.join("out/Hello.TXT").exists(), "[{}] case-insensitive", label);

        cleanup(&dir);
    }
}

#[test]
fn test_optimize_shrinks() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("opt_{}", label));
        write_test_file(&dir.join("x.txt"), b"XXXX");
        write_test_file(&dir.join("y.txt"), b"YYYY");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "x.txt", "y.txt"]);

        run_ok_in(bin, &dir, &["delete", "test.lod", "x.txt"]);
        let before = fs::metadata(dir.join("test.lod")).unwrap().len();
        run_ok_in(bin, &dir, &["optimize", "test.lod"]);
        let after = fs::metadata(dir.join("test.lod")).unwrap().len();

        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"y.txt".to_string()), "[{}]", label);
        assert!(!names.contains(&"x.txt".to_string()), "[{}]", label);
        assert!(after <= before + 1, "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_delete_nonexistent_no_crash() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("dne_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        // Deleting a nonexistent file: prints per-item error but command succeeds
        // (matching Delphi behavior — per-item errors don't fail the overall command)
        let (_, stderr, ok) = run_in(bin, &dir, &["d", "test.lod", "ghost.txt"]);
        assert!(ok, "[{}] delete nonexistent should succeed (Delphi compat)", label);
        let combined = format!("{}", stderr).to_lowercase();
        assert!(combined.contains("not found"), "[{}] should print not-found warning", label);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"a.txt".to_string()), "[{}] intact", label);

        cleanup(&dir);
    }
}

#[test]
fn test_rename_nonexistent_no_crash() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("rne_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        let (_, _, ok) = run_in(bin, &dir, &["r", "test.lod", "ghost.txt", "new.txt"]);
        // Rename nonexistent should fail (both Delphi and Rust raise error)
        assert!(!ok, "[{}] rename nonexistent should fail", label);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"a.txt".to_string()), "[{}] intact", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Compare tests (basic)
// ===========================================================================

#[test]
fn test_compare_identical_archives() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_id_{}", label));
        write_test_file(&dir.join("f.txt"), b"same");
        run_ok_in(bin, &dir, &["c", "a.lod", "mmiconslod", ".", "f.txt"]);
        fs::copy(dir.join("a.lod"), dir.join("b.lod")).unwrap();

        let (stdout, _, ok) = run_in(bin, &dir, &["compare", "a.lod", "b.lod"]);
        assert!(ok, "[{}]", label);
        assert!(stdout.contains("same"), "[{}] identical: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_compare_identical_folders() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_idf_{}", label));
        write_test_file(&dir.join("f1/same.txt"), b"identical");
        write_test_file(&dir.join("f2/same.txt"), b"identical");

        let (stdout, _, ok) = run_in(bin, &dir, &["compare", "f1", "f2"]);
        assert!(ok, "[{}]", label);
        assert!(stdout.contains("same"), "[{}]: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_compare_different_archives() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_diff_{}", label));
        write_test_file(&dir.join("f.txt"), b"version1");
        run_ok_in(bin, &dir, &["c", "a.lod", "mmiconslod", ".", "f.txt"]);

        write_test_file(&dir.join("f.txt"), b"version2");
        write_test_file(&dir.join("g.txt"), b"newfile");
        run_ok_in(bin, &dir, &["c", "b.lod", "mmiconslod", ".", "f.txt", "g.txt"]);

        let (stdout, _, ok) = run_in(bin, &dir, &["compare", "a.lod", "b.lod"]);
        assert!(ok, "[{}]", label);
        assert!(stdout.contains("[+]") || stdout.contains("[m]"),
                "[{}] markers: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_compare_folders_basic() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_fb_{}", label));
        write_test_file(&dir.join("old/same.txt"), b"unchanged");
        write_test_file(&dir.join("old/deleted.txt"), b"removed");
        write_test_file(&dir.join("new/same.txt"), b"unchanged");
        write_test_file(&dir.join("new/added.txt"), b"brand new");

        let (stdout, _, ok) = run_in(bin, &dir, &["compare", "old", "new"]);
        assert!(ok, "[{}]", label);
        assert!(stdout.contains("[+]"), "[{}] added: {}", label, stdout);
        assert!(stdout.contains("[-]"), "[{}] deleted: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_compare_filesonly_basic() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_fob_{}", label));
        write_test_file(&dir.join("old/keep.txt"), b"same");
        write_test_file(&dir.join("old/remove.txt"), b"gone");
        write_test_file(&dir.join("new/keep.txt"), b"same");
        write_test_file(&dir.join("new/newfile.txt"), b"fresh");

        run_ok_in(bin, &dir, &["compare", "old", "new", "filesonly", "diff"]);

        assert!(dir.join("diff/newfile.txt").exists(), "[{}] added", label);
        assert!(dir.join("diff/remove.txt.todelete").exists(), "[{}] .todelete", label);

        cleanup(&dir);
    }
}

#[test]
fn test_compare_filesonly_with_archive() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_foa_{}", label));
        fs::create_dir_all(dir.join("old")).unwrap();
        fs::create_dir_all(dir.join("new")).unwrap();

        write_test_file(&dir.join("a.txt"), b"old content");
        run_ok_in(bin, &dir, &["create", "icons.lod", "mmiconslod", "old", "a.txt"]);

        write_test_file(&dir.join("a.txt"), b"new content");
        write_test_file(&dir.join("b.txt"), b"extra");
        run_ok_in(bin, &dir, &["create", "icons.lod", "mmiconslod", "new", "a.txt", "b.txt"]);

        run_ok_in(bin, &dir, &["compare", "old", "new", "filesonly", "diff"]);

        // Modified archive may appear as .mmarchive folder or as the file itself
        let diff_files = collect_files_recursive(&dir.join("diff"), &dir.join("diff"));
        assert!(!diff_files.is_empty(),
                "[{}] diff should have content for modified archive: {:?}", label, diff_files);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: df2n/df2b basic
// ===========================================================================

#[test]
fn test_diff_files_to_nsis_basic() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("df2nb_{}", label));
        write_test_file(&dir.join("old/keep.txt"), b"same");
        write_test_file(&dir.join("old/del.txt"), b"gone");
        write_test_file(&dir.join("new/keep.txt"), b"same");
        write_test_file(&dir.join("new/add.txt"), b"new");

        run_ok_in(bin, &dir, &["compare", "old", "new", "filesonly", "diff"]);
        run_ok_in(bin, &dir, &["df2n", "diff", "install.nsi", "files"]);

        let content = fs::read_to_string(dir.join("install.nsi")).unwrap();
        assert!(content.contains("Section"), "[{}] Section", label);

        cleanup(&dir);
    }
}

#[test]
fn test_diff_files_to_batch_basic() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("df2bb_{}", label));
        write_test_file(&dir.join("old/keep.txt"), b"same");
        write_test_file(&dir.join("old/del.txt"), b"gone");
        write_test_file(&dir.join("new/keep.txt"), b"same");
        write_test_file(&dir.join("new/add.txt"), b"new");

        run_ok_in(bin, &dir, &["compare", "old", "new", "filesonly", "diff"]);
        run_ok_in(bin, &dir, &["df2b", "diff", "install.bat", "files"]);

        let content = fs::read_to_string(dir.join("install.bat")).unwrap();
        assert!(content.contains("cd %~dp0"), "[{}] cd %%~dp0", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: diff-add-keep
// ===========================================================================

#[test]
fn test_diff_add_keep() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("dak_{}", label));
        fs::create_dir_all(dir.join("root/empty_sub")).unwrap();
        fs::create_dir_all(dir.join("root/has_file")).unwrap();
        write_test_file(&dir.join("root/has_file/data.txt"), b"content");

        run_ok_in(bin, &dir, &["dak", "root"]);
        assert!(dir.join("root/empty_sub/.mmarchkeep").exists(),
                "[{}] .mmarchkeep in empty", label);
        assert!(!dir.join("root/has_file/.mmarchkeep").exists(),
                "[{}] no .mmarchkeep in non-empty", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// F: Help output
// ===========================================================================

#[test]
fn test_help_output() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("help_{}", label));
        let (stdout, _, ok) = run_in(bin, &dir, &["help"]);
        assert!(ok, "[{}]", label);
        assert!(stdout.contains("mmarch"), "[{}] mmarch", label);
        assert!(stdout.contains("extract"), "[{}] extract", label);
        assert!(stdout.contains("list"), "[{}] list", label);
        assert!(stdout.contains("create"), "[{}] create", label);
        cleanup(&dir);
    }
}

#[test]
fn test_help_aliases() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("help_alias_{}", label));
        let (expected, _, ok) = run_in(bin, &dir, &["help"]);
        assert!(ok, "[{}] help", label);
        for alias in ["--help", "-h", "h"] {
            let (stdout, _, ok) = run_in(bin, &dir, &[alias]);
            assert!(ok, "[{}] `{}` should succeed", label, alias);
            assert_eq!(stdout, expected, "[{}] `{}` should print the same help", label, alias);
        }
        cleanup(&dir);
    }
}

/// Rust binary only: `version` is a Rust-only command, the Delphi
/// implementation does not have it.
#[test]
fn test_version_output() {
    let dir = temp_dir("version");
    let bin = PathBuf::from(env!("CARGO_BIN_EXE_mmarch"));
    let (expected, _, ok) = run_in(&bin, &dir, &["version"]);
    assert!(ok, "version should succeed");

    // bare version number only: no program name, no leading `v`, e.g. `6.0.1`
    let v = expected.trim();
    assert_eq!(v, env!("CARGO_PKG_VERSION"), "should report the crate version");
    let parts: Vec<&str> = v.split('.').collect();
    assert_eq!(parts.len(), 3, "expected MAJOR.MINOR.PATCH, got {:?}", v);
    assert!(parts.iter().all(|p| !p.is_empty() && p.chars().all(|c| c.is_ascii_digit())),
            "version parts should be numeric, got {:?}", v);

    for alias in ["--version", "-v", "v"] {
        let (stdout, _, ok) = run_in(&bin, &dir, &[alias]);
        assert!(ok, "`{}` should succeed", alias);
        assert_eq!(stdout, expected, "`{}` should print the same version", alias);
    }
    cleanup(&dir);
}

// ===========================================================================
// F: Extract specific file
// ===========================================================================

#[test]
fn test_extract_specific_file() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ext_sp_{}", label));
        write_test_file(&dir.join("a.txt"), b"AAA");
        write_test_file(&dir.join("b.txt"), b"BBB");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt", "b.txt"]);

        run_ok_in(bin, &dir, &["e", "test.lod", "out", "b.txt"]);
        assert!(!dir.join("out/a.txt").exists(), "[{}] a.txt NOT", label);
        assert!(dir.join("out/b.txt").exists(), "[{}] b.txt yes", label);

        cleanup(&dir);
    }
}

#[test]
fn test_extract_nonexistent_filter() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ext_nf_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        // Extract nonexistent prints per-item error but command succeeds (Delphi compat)
        let (_, _, ok) = run_in(bin, &dir, &["e", "test.lod", "out", "nonexistent.txt"]);
        assert!(ok, "[{}] extract nonexistent should succeed (Delphi compat)", label);
        assert!(!dir.join("out/a.txt").exists(), "[{}] a.txt not extracted", label);

        cleanup(&dir);
    }
}

// ===========================================================================
// G: Tests from comprehensive bash testing (merged, no duplicates)
// ===========================================================================

// ---------------------------------------------------------------------------
// G.1: Games LOD full workflow (create, add, extract, delete, rename, checksum)
// Tests Bug #1 from bash testing: games.lod extract/delete/rename
// ---------------------------------------------------------------------------

#[test]
fn test_mm78gameslod_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("g78_full_{}", label));
        let blv: Vec<u8> = (0..512).map(|i| ((i * 7 + 3) % 256) as u8).collect();
        let dlv: Vec<u8> = (0..300).map(|i| ((i * 13 + 7) % 256) as u8).collect();
        write_test_file(&dir.join("dungeon.blv"), &blv);
        write_test_file(&dir.join("level.dlv"), &dlv);
        write_test_file(&dir.join("readme.txt"), b"plain text data");

        // Create
        run_ok_in(bin, &dir, &["create", "test.lod", "mm78gameslod", ".",
            "dungeon.blv", "level.dlv", "readme.txt"]);

        // List
        let names = list_archive_in(bin, &dir, "test.lod");
        assert_eq!(names.len(), 3, "[{}] 3 files: {:?}", label, names);

        // Extract specific
        run_ok_in(bin, &dir, &["extract", "test.lod", "out1", "dungeon.blv"]);
        assert_eq!(fs::read(dir.join("out1/dungeon.blv")).unwrap(), blv, "[{}] blv roundtrip", label);

        // Extract all
        run_ok_in(bin, &dir, &["extract", "test.lod", "out2"]);
        assert_eq!(fs::read(dir.join("out2/readme.txt")).unwrap(), b"plain text data", "[{}]", label);

        // Rename
        run_ok_in(bin, &dir, &["rename", "test.lod", "readme.txt", "info.txt"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"info.txt".to_string()), "[{}] renamed: {:?}", label, names);
        assert!(!names.contains(&"readme.txt".to_string()), "[{}] old gone: {:?}", label, names);

        // Delete
        run_ok_in(bin, &dir, &["delete", "test.lod", "level.dlv"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(!names.contains(&"level.dlv".to_string()), "[{}] deleted: {:?}", label, names);
        assert_eq!(names.len(), 2, "[{}] 2 remaining: {:?}", label, names);

        // Checksum
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "*"]);
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        assert_eq!(lines.len(), 2, "[{}] 2 checksums: {:?}", label, lines);

        // Optimize
        run_ok_in(bin, &dir, &["optimize", "test.lod"]);
        run_ok_in(bin, &dir, &["extract", "test.lod", "out3"]);
        assert_eq!(fs::read(dir.join("out3/dungeon.blv")).unwrap(), blv, "[{}] after optimize", label);

        cleanup(&dir);
    }
}

#[test]
fn test_mm6gameslod_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("g6_full_{}", label));
        let blv: Vec<u8> = (0..400).map(|i| ((i * 11 + 5) % 256) as u8).collect();
        write_test_file(&dir.join("map.blv"), &blv);
        write_test_file(&dir.join("data.bin"), b"some data");

        run_ok_in(bin, &dir, &["create", "test.lod", "mm6gameslod", ".", "map.blv", "data.bin"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert_eq!(names.len(), 2, "[{}] 2 files: {:?}", label, names);

        run_ok_in(bin, &dir, &["extract", "test.lod", "out", "map.blv"]);
        assert_eq!(fs::read(dir.join("out/map.blv")).unwrap(), blv, "[{}]", label);

        run_ok_in(bin, &dir, &["rename", "test.lod", "data.bin", "renamed.bin"]);
        run_ok_in(bin, &dir, &["delete", "test.lod", "map.blv"]);
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"renamed.bin".to_string()), "[{}]: {:?}", label, names);
        assert!(!names.contains(&"map.blv".to_string()), "[{}]: {:?}", label, names);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.2: MM saves full workflow
// Tests Bug from bash testing: save rename/delete
// ---------------------------------------------------------------------------

#[test]
fn test_mm78save_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sv78_full_{}", label));
        write_test_file(&dir.join("clock.bin"), b"clock data here");
        write_test_file(&dir.join("save.dat"), b"save file data");
        let dlv: Vec<u8> = (0..200).map(|i| ((i * 3) % 256) as u8).collect();
        write_test_file(&dir.join("area.dlv"), &dlv);

        run_ok_in(bin, &dir, &["create", "test.dod", "mm78save", ".", "clock.bin", "save.dat", "area.dlv"]);

        let names = list_archive_in(bin, &dir, "test.dod");
        assert_eq!(names.len(), 3, "[{}] 3 files: {:?}", label, names);

        // Extract
        run_ok_in(bin, &dir, &["extract", "test.dod", "out"]);
        assert_eq!(fs::read(dir.join("out/clock.bin")).unwrap(), b"clock data here", "[{}]", label);
        assert_eq!(fs::read(dir.join("out/area.dlv")).unwrap(), dlv, "[{}]", label);

        // Rename
        run_ok_in(bin, &dir, &["rename", "test.dod", "clock.bin", "timer.bin"]);
        let names = list_archive_in(bin, &dir, "test.dod");
        assert!(names.contains(&"timer.bin".to_string()), "[{}] renamed: {:?}", label, names);

        // Delete
        run_ok_in(bin, &dir, &["delete", "test.dod", "save.dat"]);
        let names = list_archive_in(bin, &dir, "test.dod");
        assert!(!names.contains(&"save.dat".to_string()), "[{}] deleted: {:?}", label, names);
        assert_eq!(names.len(), 2, "[{}]: {:?}", label, names);

        cleanup(&dir);
    }
}

#[test]
fn test_mm6save_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sv6_full_{}", label));
        write_test_file(&dir.join("header.bin"), b"header");
        write_test_file(&dir.join("party.dat"), b"party data content");

        run_ok_in(bin, &dir, &["create", "test.mm6", "mm6save", ".", "header.bin", "party.dat"]);

        let names = list_archive_in(bin, &dir, "test.mm6");
        assert_eq!(names.len(), 2, "[{}]: {:?}", label, names);

        // Extract + verify
        run_ok_in(bin, &dir, &["extract", "test.mm6", "out"]);
        assert_eq!(fs::read(dir.join("out/header.bin")).unwrap(), b"header", "[{}]", label);

        // Rename
        run_ok_in(bin, &dir, &["rename", "test.mm6", "header.bin", "hdr.bin"]);
        let names = list_archive_in(bin, &dir, "test.mm6");
        assert!(names.contains(&"hdr.bin".to_string()), "[{}]: {:?}", label, names);

        // Delete
        run_ok_in(bin, &dir, &["delete", "test.mm6", "party.dat"]);
        let names = list_archive_in(bin, &dir, "test.mm6");
        assert_eq!(names.len(), 1, "[{}]: {:?}", label, names);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.3: MM8 localization LOD full workflow
// Tests Bug #2/#3 from bash testing: EnglishD/T.lod issues
// ---------------------------------------------------------------------------

#[test]
fn test_mm8loclod_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("mm8loc_full_{}", label));
        write_test_file(&dir.join("2DEvents.txt"), b"event table data");
        write_test_file(&dir.join("sounds.bin"), b"sound reference data");
        write_test_file(&dir.join("D07.EVT"), b"event script");

        run_ok_in(bin, &dir, &["create", "test.T.lod", "mm8loclod", ".",
            "2DEvents.txt", "sounds.bin", "D07.EVT"]);

        let names = list_archive_in(bin, &dir, "test.T.lod");
        assert_eq!(names.len(), 3, "[{}] 3 files: {:?}", label, names);

        // Extract specific
        run_ok_in(bin, &dir, &["extract", "test.T.lod", "out", "2DEvents.txt"]);
        // mm8loclod strips extension on store for non-.bmp: check what we get
        let has_txt = dir.join("out/2DEvents.txt").exists();
        let has_bmp = dir.join("out/2DEvents.bmp").exists();
        assert!(has_txt || has_bmp, "[{}] extracted 2DEvents", label);

        // Extract all
        run_ok_in(bin, &dir, &["extract", "test.T.lod", "out2"]);

        // Rename
        run_ok_in(bin, &dir, &["rename", "test.T.lod", "D07.EVT", "D08.EVT"]);
        let names = list_archive_in(bin, &dir, "test.T.lod");
        assert!(names.contains(&"D08.EVT".to_string()), "[{}] renamed: {:?}", label, names);

        // Delete
        run_ok_in(bin, &dir, &["delete", "test.T.lod", "sounds.bin"]);
        let names = list_archive_in(bin, &dir, "test.T.lod");
        assert!(!names.contains(&"sounds.bin".to_string()), "[{}] deleted: {:?}", label, names);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.4: Bitmaps LOD full workflow (including palette support)
// ---------------------------------------------------------------------------

#[test]
fn test_mmbitmapslod_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("bmp_full_{}", label));
        copy_test_file("testpal.bmp", &dir);
        copy_test_file("pal994.act", &dir);
        write_test_file(&dir.join("extra.txt"), b"extra data");

        // Create with bmp and act
        run_ok_in(bin, &dir, &["create", "test.bitmaps.lod", "mmbitmapslod", ".",
            "testpal.bmp", "pal994.act", "extra.txt"]);

        let names = list_archive_in(bin, &dir, "test.bitmaps.lod");
        assert!(names.len() >= 2, "[{}] at least 2 files: {:?}", label, names);

        // Extract
        run_ok_in(bin, &dir, &["extract", "test.bitmaps.lod", "out"]);
        assert!(has_files_recursive(&dir.join("out")), "[{}] files extracted", label);

        // Delete
        run_ok_in(bin, &dir, &["delete", "test.bitmaps.lod", "extra.txt"]);
        let names_after = list_archive_in(bin, &dir, "test.bitmaps.lod");
        assert!(names_after.len() < names.len(), "[{}] file deleted", label);

        // Add with /p palette
        run_ok_in(bin, &dir, &["add", "test.bitmaps.lod", "testpal.bmp", "/p", "0"]);

        // Optimize
        run_ok_in(bin, &dir, &["optimize", "test.bitmaps.lod"]);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.5: Sprites LOD full workflow
// ---------------------------------------------------------------------------

#[test]
fn test_mmspriteslod_full_workflow() {
    for (label, ref bin) in get_binaries() {
        if label == "delphi" { continue; } // Delphi rejects non-bitmap files in sprites LOD
        let dir = temp_dir(&format!("spr_full_{}", label));
        write_test_file(&dir.join("sprite1.bin"), b"sprite data 1");
        write_test_file(&dir.join("sprite2.bin"), b"sprite data 2");

        run_ok_in(bin, &dir, &["create", "test.sprites.lod", "mmspriteslod", ".",
            "sprite1.bin", "sprite2.bin"]);

        let names = list_archive_in(bin, &dir, "test.sprites.lod");
        assert_eq!(names.len(), 2, "[{}]: {:?}", label, names);

        run_ok_in(bin, &dir, &["extract", "test.sprites.lod", "out"]);
        run_ok_in(bin, &dir, &["rename", "test.sprites.lod", "sprite1.bin", "renamed.bin"]);
        run_ok_in(bin, &dir, &["delete", "test.sprites.lod", "sprite2.bin"]);

        let names = list_archive_in(bin, &dir, "test.sprites.lod");
        assert_eq!(names.len(), 1, "[{}]: {:?}", label, names);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.6: Checksum --vall round-trip for multiple archive types
// Tests Bug #4 from bash testing
// ---------------------------------------------------------------------------

fn checksum_vall_roundtrip(bin: &Path, label: &str, archtype: &str, ext: &str, prefix: &str) {
    let dir = temp_dir(&format!("{}_{}", prefix, label));
    let archive_name = format!("test.{}", ext);

    write_test_file(&dir.join("alpha.txt"), b"Alpha file content");
    write_test_file(&dir.join("beta.bin"), &[1, 2, 3, 4, 5, 6, 7, 8]);
    write_test_file(&dir.join("gamma.dat"), b"Gamma data here for testing");

    run_ok_in(bin, &dir, &["create", &archive_name, archtype, ".",
        "alpha.txt", "beta.bin", "gamma.dat"]);

    // Generate checksums for all files
    let stdout = run_ok_in(bin, &dir, &["checksum", &archive_name, "*"]);
    let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
    assert_eq!(lines.len(), 3, "[{}] 3 checksums for {}: {:?}", label, archtype, lines);

    // Write to file
    fs::write(dir.join("all.crc32"), &stdout).unwrap();

    // Verify with --v
    let (stdout2, _, ok) = run_in(bin, &dir, &["checksum", &archive_name, "--v", "all.crc32"]);
    assert!(ok, "[{}] --v verify {} should succeed: {}", label, archtype, stdout2);

    // Verify with --vall
    let (stdout3, stderr3, ok) = run_in(bin, &dir, &["checksum", &archive_name, "--vall", "all.crc32"]);
    assert!(ok, "[{}] --vall verify {} should succeed.\nstdout: {}\nstderr: {}", label, archtype, stdout3, stderr3);

    cleanup(&dir);
}

#[test]
fn test_checksum_vall_mmiconslod() {
    for (label, ref bin) in get_binaries() {
        checksum_vall_roundtrip(bin, label, "mmiconslod", "icons.lod", "cvall_icons");
    }
}

#[test]
fn test_checksum_vall_mm78gameslod() {
    for (label, ref bin) in get_binaries() {
        checksum_vall_roundtrip(bin, label, "mm78gameslod", "games.lod", "cvall_games78");
    }
}

#[test]
fn test_checksum_vall_mm78save() {
    for (label, ref bin) in get_binaries() {
        checksum_vall_roundtrip(bin, label, "mm78save", "dod", "cvall_sv78");
    }
}

#[test]
fn test_checksum_vall_mm6save() {
    for (label, ref bin) in get_binaries() {
        checksum_vall_roundtrip(bin, label, "mm6save", "mm6", "cvall_sv6");
    }
}

#[test]
fn test_checksum_vall_mm8loclod() {
    for (label, ref bin) in get_binaries() {
        checksum_vall_roundtrip(bin, label, "mm8loclod", "T.lod", "cvall_mm8loc");
    }
}

#[test]
fn test_checksum_vall_mmbitmapslod() {
    for (label, ref bin) in get_binaries() {
        checksum_vall_roundtrip(bin, label, "mmbitmapslod", "bitmaps.lod", "cvall_bitmaps");
    }
}

#[test]
fn test_checksum_vall_h3lod() {
    for (label, ref bin) in get_binaries() {
        checksum_vall_roundtrip(bin, label, "h3lod", "lod", "cvall_h3");
    }
}

// ---------------------------------------------------------------------------
// G.7: Error exit codes (Bug #5 from bash testing)
// ---------------------------------------------------------------------------

#[test]
fn test_delete_nonexistent_succeeds_like_delphi() {
    // Both Delphi and Rust: delete prints per-item error but command succeeds
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_del_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        let (stdout, stderr, ok) = run_in(bin, &dir, &["delete", "test.lod", "nonexistent.xyz"]);
        assert!(ok, "[{}] delete nonexistent should succeed (per-item error only)", label);
        let combined = format!("{}{}", stdout, stderr).to_lowercase();
        assert!(combined.contains("not found"), "[{}] should warn about not found", label);

        // Archive should still be intact
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"a.txt".to_string()), "[{}] archive intact", label);

        cleanup(&dir);
    }
}

#[test]
fn test_rename_nonexistent_errors() {
    // Both Delphi and Rust raise an error for rename of non-existent file.
    // Delphi prints "Error: ..." but returns exit code 0 (doesn't set ExitCode).
    // Rust prints "Error: ..." and returns exit code 1.
    // We only check the error message here, not exit code, since that differs.
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_ren_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        let (stdout, stderr, _ok) = run_in(bin, &dir, &["rename", "test.lod", "nonexistent.xyz", "new.xyz"]);
        let combined = format!("{}{}", stdout, stderr).to_lowercase();
        assert!(combined.contains("not found") || combined.contains("error"),
                "[{}] rename nonexistent should print error: {}", label, combined);

        cleanup(&dir);
    }
}

#[test]
fn test_extract_nonexistent_succeeds_like_delphi() {
    // Both Delphi and Rust: extract prints per-item error but command succeeds
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_ext_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        let (_, _, ok) = run_in(bin, &dir, &["extract", "test.lod", "out", "nonexistent.xyz"]);
        assert!(ok, "[{}] extract nonexistent should succeed (per-item error only)", label);

        cleanup(&dir);
    }
}

#[test]
fn test_error_unknown_command_prints_error() {
    // Both Delphi and Rust print an error for unknown commands.
    // Delphi returns exit code 0; Rust returns 1. We only check error message.
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_unk_{}", label));

        let (stdout, stderr, _ok) = run_in(bin, &dir, &["totally_invalid_command"]);
        let combined = format!("{}{}", stdout, stderr).to_lowercase();
        assert!(combined.contains("unknown") || combined.contains("error"),
                "[{}] unknown command should print error: {}", label, combined);

        cleanup(&dir);
    }
}

#[test]
fn test_error_missing_params_prints_error() {
    // Both Delphi and Rust print an error for missing params.
    // Delphi returns exit code 0; Rust returns 1. We only check error message.
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_miss_{}", label));

        let (stdout, stderr, _ok) = run_in(bin, &dir, &["list"]);
        let combined = format!("{}{}", stdout, stderr).to_lowercase();
        assert!(combined.contains("error") || combined.contains("must specify") || combined.contains("mmarch"),
                "[{}] missing params should print error: {}", label, combined);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.8: Games LOD with .odm and .ddm files (compressed extensions)
// ---------------------------------------------------------------------------

#[test]
fn test_games_lod_odm_roundtrip() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("g_odm_{}", label));
        let odm: Vec<u8> = (0..500).map(|i| ((i * 19 + 11) % 256) as u8).collect();
        let ddm: Vec<u8> = (0..350).map(|i| ((i * 23 + 7) % 256) as u8).collect();
        write_test_file(&dir.join("world.odm"), &odm);
        write_test_file(&dir.join("detail.ddm"), &ddm);

        run_ok_in(bin, &dir, &["create", "test.lod", "mm78gameslod", ".",
            "world.odm", "detail.ddm"]);

        run_ok_in(bin, &dir, &["extract", "test.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/world.odm")).unwrap(), odm, "[{}] odm roundtrip", label);
        assert_eq!(fs::read(dir.join("out/detail.ddm")).unwrap(), ddm, "[{}] ddm roundtrip", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.9: Cross-type merge
// ---------------------------------------------------------------------------

#[test]
fn test_merge_different_types() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("merge_cross_{}", label));

        write_test_file(&dir.join("a.txt"), b"from games");
        run_ok_in(bin, &dir, &["create", "base.lod", "mm78gameslod", ".", "a.txt"]);

        write_test_file(&dir.join("b.txt"), b"from save");
        run_ok_in(bin, &dir, &["create", "other.lod", "mm78save", ".", "b.txt"]);

        run_ok_in(bin, &dir, &["merge", "base.lod", "other.lod"]);
        let names = list_archive_in(bin, &dir, "base.lod");
        assert!(names.contains(&"a.txt".to_string()), "[{}]: {:?}", label, names);
        assert!(names.contains(&"b.txt".to_string()), "[{}]: {:?}", label, names);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.10: Compare archives of various types
// ---------------------------------------------------------------------------

#[test]
fn test_compare_games_archives() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_games_{}", label));

        write_test_file(&dir.join("a.txt"), b"version1");
        run_ok_in(bin, &dir, &["create", "v1.lod", "mm78gameslod", ".", "a.txt"]);

        write_test_file(&dir.join("a.txt"), b"version2");
        write_test_file(&dir.join("b.txt"), b"new");
        run_ok_in(bin, &dir, &["create", "v2.lod", "mm78gameslod", ".", "a.txt", "b.txt"]);

        let (stdout, _, ok) = run_in(bin, &dir, &["compare", "v1.lod", "v2.lod"]);
        assert!(ok, "[{}]", label);
        assert!(stdout.contains("[+]") || stdout.contains("[m]"),
                "[{}] diff markers: {}", label, stdout);

        cleanup(&dir);
    }
}

#[test]
fn test_compare_saves() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_saves_{}", label));

        write_test_file(&dir.join("data.bin"), b"old save");
        run_ok_in(bin, &dir, &["create", "old.dod", "mm78save", ".", "data.bin"]);

        write_test_file(&dir.join("data.bin"), b"new save");
        run_ok_in(bin, &dir, &["create", "new.dod", "mm78save", ".", "data.bin"]);

        let (stdout, _, ok) = run_in(bin, &dir, &["compare", "old.dod", "new.dod"]);
        assert!(ok, "[{}]", label);
        assert!(stdout.contains("[m]"), "[{}] modified: {}", label, stdout);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.11: Batch extract multiple archives (no crash on error)
// Tests Bug #7: batch extract should not SIGABRT
// ---------------------------------------------------------------------------

#[test]
fn test_batch_extract_multiple_lod_types() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("batch_multi_{}", label));

        // Create various archive types in a subdirectory
        let sub = dir.join("archives");
        fs::create_dir_all(&sub).unwrap();

        write_test_file(&sub.join("a.txt"), b"icons data");
        write_test_file(&sub.join("b.txt"), b"games data");

        run_ok_in(bin, &dir, &["create", "icons.lod", "mmiconslod", "archives", "a.txt"]);
        run_ok_in(bin, &dir, &["create", "games.lod", "mm78gameslod", "archives", "b.txt"]);

        // Batch extract *.lod from archives/
        run_ok_in(bin, &dir, &["extract", "archives/*.lod", "output"]);

        // Should have extracted without crashing
        assert!(dir.join("output").exists(), "[{}] output dir created", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.12: SND full workflow (mmsnd and h3snd)
// ---------------------------------------------------------------------------

#[test]
fn test_mmsnd_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("mmsnd_full_{}", label));
        create_wav(&dir.join("sound1.wav"));
        create_wav(&dir.join("sound2.wav"));

        run_ok_in(bin, &dir, &["create", "test.snd", "mmsnd", ".", "sound1.wav", "sound2.wav"]);

        let names = list_archive_in(bin, &dir, "test.snd");
        assert_eq!(names.len(), 2, "[{}]: {:?}", label, names);

        // Extract
        run_ok_in(bin, &dir, &["extract", "test.snd", "out"]);
        assert!(dir.join("out/sound1.wav").exists(), "[{}]", label);
        assert!(dir.join("out/sound2.wav").exists(), "[{}]", label);

        // Delete
        run_ok_in(bin, &dir, &["delete", "test.snd", "sound1"]);
        let names = list_archive_in(bin, &dir, "test.snd");
        assert_eq!(names.len(), 1, "[{}] after delete: {:?}", label, names);

        // Add back
        create_wav(&dir.join("sound3.wav"));
        run_ok_in(bin, &dir, &["add", "test.snd", "sound3.wav"]);
        let names = list_archive_in(bin, &dir, "test.snd");
        assert_eq!(names.len(), 2, "[{}] after add: {:?}", label, names);

        // Checksum
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.snd", "*"]);
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        assert_eq!(lines.len(), 2, "[{}] checksums: {:?}", label, lines);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.13: VID full workflow
// ---------------------------------------------------------------------------

#[test]
fn test_vid_full_workflow() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("vid_full_{}", label));
        write_test_file(&dir.join("clip1.smk"), b"video data 1");
        write_test_file(&dir.join("clip2.smk"), b"video data 2");

        run_ok_in(bin, &dir, &["create", "test.vid", "h3mm78vid", ".", "clip1.smk", "clip2.smk"]);

        let names = list_archive_in(bin, &dir, "test.vid");
        assert_eq!(names.len(), 2, "[{}]: {:?}", label, names);

        run_ok_in(bin, &dir, &["extract", "test.vid", "out"]);
        assert_eq!(fs::read(dir.join("out/clip1.smk")).unwrap(), b"video data 1", "[{}]", label);

        // Delete
        run_ok_in(bin, &dir, &["delete", "test.vid", "clip2"]);
        let names = list_archive_in(bin, &dir, "test.vid");
        assert_eq!(names.len(), 1, "[{}] after delete: {:?}", label, names);

        // Checksum
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.vid"]);
        assert!(stdout.contains("test.vid"), "[{}]: {}", label, stdout);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.14: Checksum extension-filtered
// ---------------------------------------------------------------------------

#[test]
fn test_checksum_extension_filter() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_extf_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.txt"), b"B");
        write_test_file(&dir.join("c.bin"), b"C");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt", "b.txt", "c.bin"]);

        // *.txt should match 2 files
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "*.txt"]);
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        assert_eq!(lines.len(), 2, "[{}] 2 .txt files: {:?}", label, lines);

        // *.bin should match 1
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "*.bin"]);
        let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
        assert_eq!(lines.len(), 1, "[{}] 1 .bin file: {:?}", label, lines);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.15: Checksum wrong hash detection
// ---------------------------------------------------------------------------

#[test]
fn test_checksum_wrong_hash_detection() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_wrong_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        run_ok_in(bin, &dir, &["c", "test.lod", "mmiconslod", ".", "a.txt"]);

        // Verify with intentionally wrong hash
        let (_, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--v", "a.txt:00000000"]);
        assert!(!ok, "[{}] wrong hash should fail", label);

        // Verify with correct hash
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "a.txt"]);
        let hash = &stdout.trim().trim_end_matches('\r')[..8];
        let pair = format!("a.txt:{}", hash);
        let (_, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--v", &pair]);
        assert!(ok, "[{}] correct hash should pass", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.16: Archive-level checksum (whole file)
// ---------------------------------------------------------------------------

#[test]
fn test_checksum_archive_level_various_types() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_al_{}", label));
        write_test_file(&dir.join("data.txt"), b"content");

        let types = vec![
            ("icons.lod", "mmiconslod"),
            ("games.lod", "mm78gameslod"),
            ("save.dod", "mm78save"),
        ];

        for (fname, atype) in &types {
            run_ok_in(bin, &dir, &["create", fname, atype, ".", "data.txt"]);
            let stdout = run_ok_in(bin, &dir, &["checksum", fname]);
            let line = stdout.trim().trim_end_matches('\r');
            assert!(line.contains(fname), "[{}] {} checksum: {}", label, atype, line);
            let parts: Vec<&str> = line.splitn(2, "  ").collect();
            assert_eq!(parts.len(), 2, "[{}] {} format: {}", label, atype, line);
            assert_eq!(parts[0].len(), 8, "[{}] {} 8-char hex: {}", label, atype, parts[0]);
        }

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.17: Add after create with content for all types
// ---------------------------------------------------------------------------

#[test]
fn test_add_to_all_types() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("add_all_{}", label));

        let types: Vec<(&str, &str)> = vec![
            ("icons.lod", "mmiconslod"),
            ("games.lod", "mm78gameslod"),
            ("g6.lod", "mm6gameslod"),
            ("save.dod", "mm78save"),
            ("save6.mm6", "mm6save"),
            ("loc.T.lod", "mm8loclod"),
        ];

        write_test_file(&dir.join("init.txt"), b"initial");
        write_test_file(&dir.join("added.txt"), b"added later");

        for (fname, atype) in &types {
            run_ok_in(bin, &dir, &["create", fname, atype, ".", "init.txt"]);
            let before = list_archive_in(bin, &dir, fname);
            assert_eq!(before.len(), 1, "[{}] {} initial: {:?}", label, atype, before);

            run_ok_in(bin, &dir, &["add", fname, "added.txt"]);
            let after = list_archive_in(bin, &dir, fname);
            assert_eq!(after.len(), 2, "[{}] {} after add: {:?}", label, atype, after);
        }

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.18: Delete and verify content integrity for remaining files
// ---------------------------------------------------------------------------

#[test]
fn test_delete_preserves_remaining_content() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("del_int_{}", label));

        let content_b = b"BBB content that should survive deletion";
        let content_c = b"CCC content that should also survive";

        write_test_file(&dir.join("a.txt"), b"AAA to be deleted");
        write_test_file(&dir.join("b.txt"), content_b);
        write_test_file(&dir.join("c.txt"), content_c);

        run_ok_in(bin, &dir, &["create", "test.lod", "mm78gameslod", ".",
            "a.txt", "b.txt", "c.txt"]);

        run_ok_in(bin, &dir, &["delete", "test.lod", "a.txt"]);
        run_ok_in(bin, &dir, &["extract", "test.lod", "out"]);

        assert!(!dir.join("out/a.txt").exists(), "[{}] deleted", label);
        assert_eq!(fs::read(dir.join("out/b.txt")).unwrap(), content_b, "[{}] b intact", label);
        assert_eq!(fs::read(dir.join("out/c.txt")).unwrap(), content_c, "[{}] c intact", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.18b: Create with many files — verify all extracted content (RSLod Add regression)
// Tests that file data is not corrupted when directory grows across data regions.
// ---------------------------------------------------------------------------

#[test]
fn test_create_many_files_content_integrity() {
    let file_data: Vec<(&str, &[u8])> = vec![
        ("f1.txt", b"content of file one"),
        ("f2.txt", b"second file here"),
        ("f3.txt", b"third"),
        ("f4.txt", b"file number four with more data"),
        ("f5.txt", b"five"),
    ];

    // Test across archive types with different ItemSize values
    let types: Vec<(&str, &str)> = vec![
        ("test.lod", "mmiconslod"),      // ItemSize=0x20
        ("test.dod", "mm78save"),         // ItemSize=0x20, chapter LOD
        ("test.mm6", "mm6save"),          // ItemSize=0x20, chapter LOD
        ("games.lod", "mm78gameslod"),    // ItemSize=0x20, games LOD
    ];

    for (label, ref bin) in get_binaries() {
        for (arch_name, arch_type) in &types {
            let dir = temp_dir(&format!("many_{}_{}", arch_type, label));

            let mut create_args: Vec<&str> = vec!["create", arch_name, arch_type, "."];
            for (name, data) in &file_data {
                write_test_file(&dir.join(name), data);
                create_args.push(name);
            }

            run_ok_in(bin, &dir, &create_args);

            let names = list_archive_in(bin, &dir, arch_name);
            assert_eq!(names.len(), file_data.len(),
                "[{}] {} file count: {:?}", label, arch_type, names);

            run_ok_in(bin, &dir, &["extract", arch_name, "out"]);

            for (name, expected) in &file_data {
                let actual = fs::read(dir.join("out").join(name)).unwrap();
                assert_eq!(actual, *expected,
                    "[{}] {} file '{}' content mismatch", label, arch_type, name);
            }

            cleanup(&dir);
        }
    }
}

// ---------------------------------------------------------------------------
// G.19: Merge detailed verification
// ---------------------------------------------------------------------------

#[test]
fn test_merge_detailed_verification() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("merge_det_{}", label));

        write_test_file(&dir.join("shared.txt"), b"base version");
        write_test_file(&dir.join("only_base.txt"), b"only in base");
        run_ok_in(bin, &dir, &["create", "base.lod", "mmiconslod", ".",
            "shared.txt", "only_base.txt"]);

        write_test_file(&dir.join("shared.txt"), b"other version");
        write_test_file(&dir.join("only_other.txt"), b"only in other");
        run_ok_in(bin, &dir, &["create", "other.lod", "mmiconslod", ".",
            "shared.txt", "only_other.txt"]);

        run_ok_in(bin, &dir, &["merge", "base.lod", "other.lod"]);

        let names = list_archive_in(bin, &dir, "base.lod");
        assert!(names.contains(&"shared.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"only_base.txt".to_string()), "[{}]", label);
        assert!(names.contains(&"only_other.txt".to_string()), "[{}]", label);

        // shared.txt should have the other version (replacement)
        run_ok_in(bin, &dir, &["extract", "base.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/shared.txt")).unwrap(), b"other version",
                   "[{}] merge replaces shared", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// G.20: Round-trip for mm6gameslod and mm6save
// ---------------------------------------------------------------------------

#[test]
fn test_roundtrip_mm6gameslod() {
    for (label, ref bin) in get_binaries() {
        roundtrip_test(bin, label, "mm6gameslod", "lod", "rt_games6");
    }
}

#[test]
fn test_roundtrip_mm6save() {
    for (label, ref bin) in get_binaries() {
        roundtrip_test(bin, label, "mm6save", "mm6", "rt_mm6save");
    }
}

#[test]
fn test_roundtrip_mm78save() {
    for (label, ref bin) in get_binaries() {
        roundtrip_test(bin, label, "mm78save", "dod", "rt_mm78save");
    }
}

#[test]
fn test_roundtrip_mm8loclod() {
    for (label, ref bin) in get_binaries() {
        roundtrip_test(bin, label, "mm8loclod", "lod", "rt_mm8loc");
    }
}

// ---------------------------------------------------------------------------
// G.21: Compare filesonly for various archive types
// ---------------------------------------------------------------------------

#[test]
fn test_compare_filesonly_games_lod() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_fo_games_{}", label));

        write_test_file(&dir.join("old.txt"), b"old");
        run_ok_in(bin, &dir, &["create", "v1.lod", "mm78gameslod", ".", "old.txt"]);

        write_test_file(&dir.join("old.txt"), b"modified");
        write_test_file(&dir.join("new.txt"), b"added");
        run_ok_in(bin, &dir, &["create", "v2.lod", "mm78gameslod", ".", "old.txt", "new.txt"]);

        run_ok_in(bin, &dir, &["compare", "v1.lod", "v2.lod", "filesonly", "diff"]);
        let files = collect_files_recursive(&dir.join("diff"), &dir.join("diff"));
        assert!(!files.is_empty(), "[{}] diff should have content: {:?}", label, files);

        cleanup(&dir);
    }
}

// ===========================================================================
// H: Tests for codex-reported Delphi compatibility issues
// ===========================================================================

// ---------------------------------------------------------------------------
// H.1: SND/VID find_entry with extracted extension (Claim 5)
// ---------------------------------------------------------------------------

#[test]
fn test_snd_find_by_wav_extension() {
    // SND stores files without extension. extract/delete/rename with .wav should work.
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("snd_wav_{}", label));
        create_wav(&dir.join("alert.wav"));
        create_wav(&dir.join("beep.wav"));

        run_ok_in(bin, &dir, &["create", "test.snd", "mmsnd", ".", "alert.wav", "beep.wav"]);

        // Extract by .wav name (in-archive name is "alert", not "alert.wav")
        run_ok_in(bin, &dir, &["extract", "test.snd", "out", "alert.wav"]);
        assert!(dir.join("out/alert.wav").exists(), "[{}] extract by .wav name", label);

        // Delete by .wav name
        run_ok_in(bin, &dir, &["delete", "test.snd", "beep.wav"]);
        let names = list_archive_in(bin, &dir, "test.snd");
        assert_eq!(names.len(), 1, "[{}] after delete beep.wav: {:?}", label, names);

        cleanup(&dir);
    }
}

#[test]
fn test_vid_find_by_smk_extension() {
    // VID stores files without extension. Operations with .smk should work.
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("vid_smk_{}", label));
        write_test_file(&dir.join("intro.smk"), b"video data");
        write_test_file(&dir.join("outro.smk"), b"outro data");

        run_ok_in(bin, &dir, &["create", "test.vid", "h3mm78vid", ".", "intro.smk", "outro.smk"]);

        // Extract by .smk name
        run_ok_in(bin, &dir, &["extract", "test.vid", "out", "intro.smk"]);
        assert!(dir.join("out/intro.smk").exists(), "[{}] extract by .smk name", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.2: Checksum outputs extracted names, not in-archive names (Claim 8)
// ---------------------------------------------------------------------------

#[test]
fn test_checksum_uses_extracted_names() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_extname_{}", label));
        create_wav(&dir.join("sound.wav"));

        run_ok_in(bin, &dir, &["create", "test.snd", "mmsnd", ".", "sound.wav"]);

        // Checksum output should use extracted name "sound.wav", not in-archive "sound"
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.snd", "*"]);
        assert!(stdout.contains("sound.wav"),
                "[{}] checksum should output extracted name 'sound.wav': {}", label, stdout);
        assert!(!stdout.contains("  sound\r\n") && !stdout.contains("  sound\n"),
                "[{}] should not output bare 'sound' without .wav: {}", label, stdout);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.3: Checksum file format accepts name:HASH (Claim 9)
// ---------------------------------------------------------------------------

#[test]
fn test_checksum_file_format_name_colon_hash() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_fmt_{}", label));
        write_test_file(&dir.join("data.txt"), b"test content");
        run_ok_in(bin, &dir, &["create", "test.lod", "mmiconslod", ".", "data.txt"]);

        // Get the correct hash
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "data.txt"]);
        let hash = &stdout.trim().trim_end_matches('\r')[..8];

        // Write checksum file in name:HASH format (instead of standard HASH  name)
        let crc_content = format!("data.txt:{}\r\n", hash);
        fs::write(dir.join("check.crc32"), &crc_content).unwrap();

        // Verify should work with this format
        let (stdout2, _, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--v", "check.crc32"]);
        assert!(ok, "[{}] name:HASH format should be accepted: {}", label, stdout2);
        assert!(stdout2.contains("OK"), "[{}] should contain OK: {}", label, stdout2);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.4: --vall does not count unlisted files as failures (Claim 11)
// ---------------------------------------------------------------------------

#[test]
fn test_checksum_vall_unlisted_is_warning_not_failure() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_vallw_{}", label));
        write_test_file(&dir.join("a.txt"), b"A");
        write_test_file(&dir.join("b.txt"), b"B");
        write_test_file(&dir.join("c.txt"), b"C");
        run_ok_in(bin, &dir, &["create", "test.lod", "mmiconslod", ".", "a.txt", "b.txt", "c.txt"]);

        // Get checksum for only a.txt
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "a.txt"]);
        fs::write(dir.join("partial.crc32"), &stdout).unwrap();

        // --vall with partial file: b.txt and c.txt are unlisted
        // Should succeed (with warning), not fail (matching Delphi behavior)
        let (stdout2, stderr2, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--vall", "partial.crc32"]);
        assert!(ok, "[{}] --vall with partial should succeed (Delphi compat).\nstdout: {}\nstderr: {}",
                label, stdout2, stderr2);
        // Should still warn about unlisted files
        assert!(stderr2.contains("not listed") || stderr2.contains("WARNING"),
                "[{}] should warn about unlisted", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.5: Empty mmsnd preserves MM kind (Claim 16)
// ---------------------------------------------------------------------------

#[test]
fn test_empty_mmsnd_preserves_kind() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("empty_mmsnd_{}", label));

        // Create empty mmsnd, then add a file in a separate operation
        run_ok_in(bin, &dir, &["create", "test.snd", "mmsnd", "."]);
        create_wav(&dir.join("sound.wav"));
        run_ok_in(bin, &dir, &["add", "test.snd", "sound.wav"]);

        // The archive should still be readable and extractable
        let names = list_archive_in(bin, &dir, "test.snd");
        assert_eq!(names.len(), 1, "[{}] should have 1 file: {:?}", label, names);

        run_ok_in(bin, &dir, &["extract", "test.snd", "out"]);
        assert!(dir.join("out/sound.wav").exists(), "[{}] should extract sound.wav", label);

        // Verify round-trip: the extracted wav should match the original
        let orig = fs::read(dir.join("sound.wav")).unwrap();
        let extracted = fs::read(dir.join("out/sound.wav")).unwrap();
        assert_eq!(orig, extracted, "[{}] round-trip should preserve content", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.6: mm78gameslod/mm78save version strings (Claim 17)
// ---------------------------------------------------------------------------

#[test]
fn test_mm78gameslod_version_string_roundtrip() {
    // After creating mm78gameslod and reopening, the kind should still be Games7
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ver78g_{}", label));
        let blv: Vec<u8> = (0..256).map(|i| (i % 256) as u8).collect();
        write_test_file(&dir.join("test.blv"), &blv);
        write_test_file(&dir.join("data.txt"), b"plain");

        // Create, then reopen and add more files
        run_ok_in(bin, &dir, &["create", "test.lod", "mm78gameslod", ".", "test.blv"]);
        write_test_file(&dir.join("more.txt"), b"added later");
        run_ok_in(bin, &dir, &["add", "test.lod", "more.txt"]);

        // Extract and verify — if kind is wrong, compressed files will fail
        run_ok_in(bin, &dir, &["extract", "test.lod", "out"]);
        assert_eq!(fs::read(dir.join("out/test.blv")).unwrap(), blv,
                   "[{}] blv should roundtrip after reopen+add", label);
        assert_eq!(fs::read(dir.join("out/more.txt")).unwrap(), b"added later", "[{}]", label);

        cleanup(&dir);
    }
}

#[test]
fn test_mm78save_version_string_roundtrip() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ver78s_{}", label));
        let dlv: Vec<u8> = (0..200).map(|i| ((i * 3) % 256) as u8).collect();
        write_test_file(&dir.join("area.dlv"), &dlv);
        write_test_file(&dir.join("header.bin"), b"hdr");

        run_ok_in(bin, &dir, &["create", "test.dod", "mm78save", ".", "header.bin"]);
        // Add compressed file in a separate step (simulates reopen)
        run_ok_in(bin, &dir, &["add", "test.dod", "area.dlv"]);

        run_ok_in(bin, &dir, &["extract", "test.dod", "out"]);
        assert_eq!(fs::read(dir.join("out/area.dlv")).unwrap(), dlv,
                   "[{}] dlv should roundtrip after reopen+add", label);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.7: Merge preserves TMMLodFile wrapper (Claim 18)
// ---------------------------------------------------------------------------

#[test]
fn test_merge_bitmapslod_preserves_data() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("merge_bmp_{}", label));
        copy_test_file("pal994.act", &dir);
        write_test_file(&dir.join("data.txt"), b"some data");

        // Create two bitmaps archives
        run_ok_in(bin, &dir, &["create", "a.bitmaps.lod", "mmbitmapslod", ".", "pal994.act"]);
        run_ok_in(bin, &dir, &["create", "b.bitmaps.lod", "mmbitmapslod", ".", "data.txt"]);

        // Merge b into a
        run_ok_in(bin, &dir, &["merge", "a.bitmaps.lod", "b.bitmaps.lod"]);

        // Extract all and verify content is intact
        run_ok_in(bin, &dir, &["extract", "a.bitmaps.lod", "out"]);

        // pal994.act should extract as .act (768 bytes palette)
        let act_path = dir.join("out/pal994.act");
        assert!(act_path.exists(), "[{}] pal994.act should exist after merge+extract", label);
        let act_data = fs::read(&act_path).unwrap();
        assert_eq!(act_data.len(), 768, "[{}] palette should be 768 bytes: got {}", label, act_data.len());

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.8: /p with non-numeric value should error (Claim 4)
// ---------------------------------------------------------------------------

#[test]
fn test_palette_index_non_numeric_errors() {
    // Both Delphi (StrToInt) and Rust return error for non-numeric /p argument.
    // Delphi exit code is 0; Rust is 1. Check error message only.
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("p_nan_{}", label));
        copy_test_file("testpal.bmp", &dir);
        run_ok_in(bin, &dir, &["create", "test.lod", "mmbitmapslod", "."]);

        let (stdout, stderr, _ok) = run_in(bin, &dir, &["add", "test.lod", "testpal.bmp", "/p", "notanumber"]);
        let combined = format!("{}{}", stdout, stderr).to_lowercase();
        assert!(combined.contains("error") || combined.contains("invalid"),
                "[{}] /p with non-numeric should print error: {}", label, combined);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.9: Unknown command has non-zero exit and no duplicate output (Claim 1)
// ---------------------------------------------------------------------------

#[test]
fn test_unknown_command_no_duplicate_output() {
    // Both binaries should print "Error:" exactly once for unknown commands.
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("unk_dup_{}", label));

        let (stdout, stderr, _ok) = run_in(bin, &dir, &["foobar_invalid"]);

        // Count how many times "Error:" appears — should be exactly once
        let combined = format!("{}{}", stdout, stderr);
        let error_count = combined.matches("Error:").count();
        assert_eq!(error_count, 1,
                   "[{}] 'Error:' should appear exactly once, got {}:\n{}", label, error_count, combined);

        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// H.10: Checksum --v verify also accepts extracted names (Claim 11)
// ---------------------------------------------------------------------------

#[test]
fn test_checksum_verify_with_extracted_names() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_vext_{}", label));
        create_wav(&dir.join("music.wav"));

        run_ok_in(bin, &dir, &["create", "test.snd", "mmsnd", ".", "music.wav"]);

        // Get checksum — should output extracted name "music.wav"
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.snd", "*"]);
        fs::write(dir.join("all.crc32"), &stdout).unwrap();

        // Verify with --v should work since names match
        let (stdout2, _, ok) = run_in(bin, &dir, &["checksum", "test.snd", "--v", "all.crc32"]);
        assert!(ok, "[{}] --v verify with extracted names should work: {}", label, stdout2);

        // --vall should also work
        let (stdout3, stderr3, ok) = run_in(bin, &dir, &["checksum", "test.snd", "--vall", "all.crc32"]);
        assert!(ok, "[{}] --vall verify should succeed.\nstdout: {}\nstderr: {}",
                label, stdout3, stderr3);

        cleanup(&dir);
    }
}

// ===========================================================================
// I: --ec flag tests (exit code mode: strict / normal / loose)
// ===========================================================================

/// Helper: create a test archive with a.txt, b.txt
fn setup_ec_archive(bin: &Path, dir: &Path) {
    write_test_file(&dir.join("a.txt"), b"A");
    write_test_file(&dir.join("b.txt"), b"B");
    run_ok_in(bin, dir, &["create", "test.lod", "mmiconslod", ".", "a.txt", "b.txt"]);
}

// ---------------------------------------------------------------------------
// I.1: Unknown command
// ---------------------------------------------------------------------------

#[test]
fn test_ec_unknown_command_strict() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_unk_s_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "strict", "nonexistent_cmd"]);
        assert!(!ok, "[{}] strict: unknown command should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_unknown_command_normal() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_unk_n_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "normal", "nonexistent_cmd"]);
        assert!(!ok, "[{}] normal: unknown command should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_unknown_command_loose() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_unk_l_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "loose", "nonexistent_cmd"]);
        assert!(ok, "[{}] loose: unknown command should exit 0", label);
        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// I.2: Missing params
// ---------------------------------------------------------------------------

#[test]
fn test_ec_missing_params_strict() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_miss_s_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "strict", "list"]);
        assert!(!ok, "[{}] strict: missing params should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_missing_params_normal() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_miss_n_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "normal", "list"]);
        assert!(!ok, "[{}] normal: missing params should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_missing_params_loose() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_miss_l_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "loose", "list"]);
        assert!(ok, "[{}] loose: missing params should exit 0", label);
        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// I.3: Rename not found
// ---------------------------------------------------------------------------

#[test]
fn test_ec_rename_notfound_strict() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_ren_s_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "strict", "rename", "test.lod", "ghost.txt", "new.txt"]);
        assert!(!ok, "[{}] strict: rename not found should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_rename_notfound_normal() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_ren_n_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "normal", "rename", "test.lod", "ghost.txt", "new.txt"]);
        assert!(!ok, "[{}] normal: rename not found should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_rename_notfound_loose() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_ren_l_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "loose", "rename", "test.lod", "ghost.txt", "new.txt"]);
        assert!(ok, "[{}] loose: rename not found should exit 0", label);
        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// I.4: Open non-existent archive
// ---------------------------------------------------------------------------

#[test]
fn test_ec_open_nonexistent_strict() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_open_s_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "strict", "list", "nonexistent.lod"]);
        assert!(!ok, "[{}] strict: open nonexistent should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_open_nonexistent_normal() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_open_n_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "normal", "list", "nonexistent.lod"]);
        assert!(!ok, "[{}] normal: open nonexistent should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_open_nonexistent_loose() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_open_l_{}", label));
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "loose", "list", "nonexistent.lod"]);
        assert!(ok, "[{}] loose: open nonexistent should exit 0", label);
        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// I.5: Delete per-item not found
// ---------------------------------------------------------------------------

#[test]
fn test_ec_delete_notfound_strict() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_del_s_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "strict", "delete", "test.lod", "ghost.txt"]);
        assert!(!ok, "[{}] strict: delete not found should exit 1", label);
        // Archive should still be rebuilt (existing files intact)
        let names = list_archive_in(bin, &dir, "test.lod");
        assert!(names.contains(&"a.txt".to_string()), "[{}] intact", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_delete_notfound_normal() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_del_n_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "normal", "delete", "test.lod", "ghost.txt"]);
        assert!(ok, "[{}] normal: delete not found should exit 0", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_delete_notfound_loose() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_del_l_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "loose", "delete", "test.lod", "ghost.txt"]);
        assert!(ok, "[{}] loose: delete not found should exit 0", label);
        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// I.6: Extract per-item not found
// ---------------------------------------------------------------------------

#[test]
fn test_ec_extract_notfound_strict() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_ext_s_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "strict", "extract", "test.lod", "out", "ghost.txt"]);
        assert!(!ok, "[{}] strict: extract not found should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_extract_notfound_normal() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_ext_n_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "normal", "extract", "test.lod", "out", "ghost.txt"]);
        assert!(ok, "[{}] normal: extract not found should exit 0", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_extract_notfound_loose() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_ext_l_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "loose", "extract", "test.lod", "out", "ghost.txt"]);
        assert!(ok, "[{}] loose: extract not found should exit 0", label);
        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// I.7: Checksum verify failure
// ---------------------------------------------------------------------------

#[test]
fn test_ec_checksum_fail_strict() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_cs_s_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "strict", "checksum", "test.lod", "--v", "a.txt:00000000"]);
        assert!(!ok, "[{}] strict: checksum fail should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_checksum_fail_normal() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_cs_n_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "normal", "checksum", "test.lod", "--v", "a.txt:00000000"]);
        assert!(!ok, "[{}] normal: checksum fail should exit 1", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_checksum_fail_loose() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_cs_l_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["--ec", "loose", "checksum", "test.lod", "--v", "a.txt:00000000"]);
        assert!(!ok, "[{}] loose: checksum fail should STILL exit 1", label);
        cleanup(&dir);
    }
}

// ---------------------------------------------------------------------------
// I.8: --ec flag position and default
// ---------------------------------------------------------------------------

#[test]
fn test_ec_default_is_normal() {
    // Without --ec flag, behavior should be normal (delete not-found = exit 0)
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_def_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["delete", "test.lod", "ghost.txt"]);
        assert!(ok, "[{}] default (no --ec): delete not found should exit 0", label);
        cleanup(&dir);
    }
}

#[test]
fn test_ec_flag_at_end() {
    // --ec can appear anywhere in args
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("ec_end_{}", label));
        setup_ec_archive(bin, &dir);
        let (_, _, ok) = run_in(bin, &dir, &["delete", "test.lod", "ghost.txt", "--ec", "strict"]);
        assert!(!ok, "[{}] --ec at end should work", label);
        cleanup(&dir);
    }
}

// ===========================================================================
// J: Real game archive file tests
// ===========================================================================

/// Helper to get a real test file path
fn real_file(name: &str) -> PathBuf {
    test_general_dir().join(name)
}

/// Helper: copy real file to temp dir, return archive path in temp dir
fn copy_real_to_temp(name: &str, dir: &Path) -> PathBuf {
    let src = real_file(name);
    assert!(src.exists(), "Real test file {:?} not found", src);
    let dst = dir.join(name);
    fs::copy(&src, &dst).unwrap();
    dst
}

/// Generic test: list, extract all, checksum, extract specific, delete, rename
fn real_archive_test(
    bin: &Path,
    label: &str,
    file_name: &str,
    expected_names: &[&str],      // in-archive names
    expected_extracted: &[&str],   // extracted names (with .bmp/.wav/.smk etc)
    expected_crcs: &[(&str, &str)], // (extracted_name, hex CRC32)
) {
    let dir = temp_dir(&format!("real_{}_{}", file_name.replace('.', "_"), label));

    // Copy to temp
    let archive = copy_real_to_temp(file_name, &dir);
    let archive_rel = file_name;

    // --- List ---
    let names = list_archive_in(bin, &dir, archive_rel);
    assert_eq!(names.len(), expected_names.len(),
        "[{}] {} list count: {:?}", label, file_name, names);
    for en in expected_names {
        assert!(names.iter().any(|n| n.eq_ignore_ascii_case(en)),
            "[{}] {} should contain '{}': {:?}", label, file_name, en, names);
    }

    // --- Extract all ---
    run_ok_in(bin, &dir, &["extract", archive_rel, "out_all"]);
    for ef in expected_extracted {
        assert!(dir.join("out_all").join(ef).exists(),
            "[{}] {} should extract '{}'. Dir contents: {:?}",
            label, file_name, ef,
            fs::read_dir(dir.join("out_all")).ok().map(|d|
                d.filter_map(|e| e.ok()).map(|e| e.file_name().to_string_lossy().to_string()).collect::<Vec<_>>()
            ));
    }

    // --- Checksum all ---
    // The CRC is of the extracted file: `checksum` hashes exactly what
    // `extract` writes.
    let stdout = run_ok_in(bin, &dir, &["checksum", archive_rel, "*"]);
    for (ext_name, hex_crc) in expected_crcs {
        assert!(stdout.contains(ext_name) && stdout.contains(hex_crc),
            "[{}] {} checksum should contain '{}' with CRC '{}': {}",
            label, file_name, ext_name, hex_crc, stdout);
    }

    // --- Extract specific file (by extracted name) ---
    if !expected_extracted.is_empty() {
        let first_ext = expected_extracted[0];
        run_ok_in(bin, &dir, &["extract", archive_rel, "out_single", first_ext]);
        assert!(dir.join("out_single").join(first_ext).exists(),
            "[{}] {} extract specific '{}'", label, file_name, first_ext);
    }

    // --- Delete + Rename (on a copy) ---
    let copy_name = format!("copy_{}", file_name);
    fs::copy(&archive, dir.join(&copy_name)).unwrap();

    if expected_names.len() >= 2 {
        // Delete last entry
        let last = expected_names.last().unwrap();
        run_ok_in(bin, &dir, &["delete", &copy_name, last]);
        let names_after = list_archive_in(bin, &dir, &copy_name);
        assert_eq!(names_after.len(), expected_names.len() - 1,
            "[{}] {} after delete: {:?}", label, file_name, names_after);

        // Rename first entry
        let first = expected_names[0];
        run_ok_in(bin, &dir, &["rename", &copy_name, first, "RENAMED_TEST"]);
        let names_ren = list_archive_in(bin, &dir, &copy_name);
        assert!(names_ren.iter().any(|n| n == "RENAMED_TEST"),
            "[{}] {} after rename: {:?}", label, file_name, names_ren);
    }

    cleanup(&dir);
}

// --- Individual tests per real archive type ---

#[test]
fn test_real_mmiconslod() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_icons.lod",
            &["GF-ASpell", "GF-ASpellD", "GF-Swrd"],
            &["GF-ASpell.bmp", "GF-ASpellD.bmp", "GF-Swrd.bmp"],
            &[("GF-ASpell.bmp", "2205C82B"), ("GF-ASpellD.bmp", "857A0994"), ("GF-Swrd.bmp", "2D3BE4FD")],
        );
    }
}

#[test]
fn test_real_mmbitmapslod() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_bitmaps.lod",
            &["HDLAV007", "HDWTR006", "pal453"],
            &["HDLAV007.bmp", "HDWTR006.bmp", "pal453.act"],
            &[("HDLAV007.bmp", "B3FE7A28"), ("HDWTR006.bmp", "09800FAF"), ("pal453.act", "C35ACC9F")],
        );
    }
}

#[test]
fn test_real_mmspriteslod() {
    // These three name palette 23391, which is in no bitmaps.lod. RSPak raises
    // and the Delphi CLI falls back to writing the stored bytes; so does
    // mmarch. test_real_sprite_palette below covers the decoding path.
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_sprites.lod",
            &["Editor i1", "Editor i2", "Editor i3"],
            &["Editor i1.bmp", "Editor i2.bmp", "Editor i3.bmp"],
            &[("Editor i1.bmp", "ACDC9E8D"), ("Editor i2.bmp", "B21F7CBE"), ("Editor i3.bmp", "5B86FB98")],
        );
    }
}

#[test]
fn test_real_mm8loclod() {
    // CRCs of the zlib payload behind each entry's 64-byte name + TMMLodFile
    // header, i.e. what RSPak's TRSLod.DoExtract produces. The values this test
    // shipped with until v6.0.1 (382A0F77 / 2DEED09D / BAF06A87) were the CRCs
    // of 768 raw archive bytes each, because extraction read the header at
    // offset $10 instead of Options.NameSize = $40.
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_mm8loc.T.lod",
            &["D07.EVT", "d16.EVT", "d17.EVT"],
            &["D07.EVT", "d16.EVT", "d17.EVT"],
            &[("D07.EVT", "BC815070"), ("d16.EVT", "6E5105FE"), ("d17.EVT", "4BA639C8")],
        );
    }
}

#[test]
fn test_real_mm78gameslod() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_games78.lod",
            &["d46.blv", "eleme.blv"],
            &["d46.blv", "eleme.blv"],
            &[("d46.blv", "8E501F86"), ("eleme.blv", "7B30E419")],
        );
    }
}

#[test]
fn test_real_mm6gameslod() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_games6.lod",
            &["cd1.blv", "cd2.blv", "cd3.blv"],
            &["cd1.blv", "cd2.blv", "cd3.blv"],
            &[("cd1.blv", "4615EFE5"), ("cd2.blv", "5E914E34"), ("cd3.blv", "D69782A3")],
        );
    }
}

#[test]
fn test_real_mm78save() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_save78.mm7",
            &["clock.bin", "d01.dlv", "d02.dlv"],
            &["clock.bin", "d01.dlv", "d02.dlv"],
            &[("clock.bin", "46EECD98"), ("d01.dlv", "55399557"), ("d02.dlv", "64DAD008")],
        );
    }
}

#[test]
fn test_real_mm6save() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_save6.mm6",
            &["cd1.dlv", "cd2.dlv", "cd3.dlv"],
            &["cd1.dlv", "cd2.dlv", "cd3.dlv"],
            &[("cd1.dlv", "D4AD9237"), ("cd2.dlv", "9282B222"), ("cd3.dlv", "733613DC")],
        );
    }
}

#[test]
fn test_real_mmsnd() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_audio.snd",
            &["01archerA_attack", "01archerA_charge", "01archerA_die"],
            &["01archerA_attack.wav", "01archerA_charge.wav", "01archerA_die.wav"],
            &[("01archerA_attack.wav", "A3C75AA5"), ("01archerA_charge.wav", "ABC10D20"), ("01archerA_die.wav", "3767B11E")],
        );
    }
}

#[test]
fn test_real_h3mm78vid() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_video.vid",
            &["Apthcmid", "Apthcrch"],
            &["Apthcmid.smk", "Apthcrch.smk"],
            &[("Apthcmid.smk", "F8D57F66"), ("Apthcrch.smk", "24C36BEB")],
        );
    }
}

// --- Heroes 3 specific formats ---

#[test]
fn test_real_h3lod() {
    // h3lod: .pcx in-archive → .bmp extracted; .h3c stays as-is
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_h3.lod",
            &["ab.h3c", "Ar_Bg.pcx", "ArA_CoBl.pcx"],
            &["ab.h3c", "Ar_Bg.bmp", "ArA_CoBl.bmp"],
            &[("ab.h3c", "E03F056B"), ("Ar_Bg.bmp", "69A2B374"), ("ArA_CoBl.bmp", "543EA27E")],
        );
    }
}

#[test]
fn test_real_h3snd() {
    // h3snd: no extension in-archive → .wav extracted
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_h3.snd",
            &["Azurattk", "Azurdfnd", "Azurkill"],
            &["Azurattk.wav", "Azurdfnd.wav", "Azurkill.wav"],
            &[("Azurattk.wav", "035C29FF"), ("Azurdfnd.wav", "929FE477"), ("Azurkill.wav", "1DC26581")],
        );
    }
}

#[test]
fn test_real_h3vid() {
    // h3vid: keeps original extension (.bik)
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_h3.vid",
            &["C1ab7.bik", "C1db2.bik"],
            &["C1ab7.bik", "C1db2.bik"],
            &[("C1ab7.bik", "C7248045"), ("C1db2.bik", "B5D8F584")],
        );
    }
}

/// Read a BMP's header: (width, height, bits per pixel, colour table entries).
fn bmp_header(data: &[u8]) -> (i32, i32, u16, u32) {
    assert!(data.len() > 54 && &data[..2] == b"BM", "not a BMP");
    let i32_at = |o: usize| i32::from_le_bytes(data[o..o + 4].try_into().unwrap());
    let u32_at = |o: usize| u32::from_le_bytes(data[o..o + 4].try_into().unwrap());
    (i32_at(18), i32_at(22), u16::from_le_bytes(data[28..30].try_into().unwrap()), u32_at(46))
}

/// Sprites become .bmp files, coloured with a palette that lives in a
/// neighbouring bitmaps.lod, and two of the games name the wrong one.
///
/// `real_sprite_pal.lod` puts one sprite from each game side by side:
///
/// * `BATATA0` (MM6) names palette 422, which exists but belongs to something
///   else — the right one is 156.
/// * `Swptree1` (MM7) names 940, which is in no bitmaps.lod at all — 120.
/// * `ARROWA0` (MM8) is ordinary, and has 38 fully transparent rows.
/// * `C3_HASTE` (MM6) is ordinary and tiny, with none.
///
/// The corrections come from the table MMArchive carries (`PalFix` in
/// RSLodEdt.pas). The two corrected CRCs below are of the .bmp files
/// **MMArchive itself produced**, kept in `tests/reference/mmarchive/`, so
/// this asserts byte-for-byte agreement with the reference tool.
///
/// The palettes in the fixture are spelled `pal156`, `Pal002`, `PAL120`,
/// `pal338` and `PaL422` — all three spellings the games actually use, because
/// RSPak looks them up with a case-insensitive FindFile and MM8's own
/// bitmaps.lod mixes them.
#[test]
fn test_real_sprite_palette() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sprite_pal_{}", label));
        copy_real_to_temp("real_sprite_pal.lod", &dir);
        copy_real_to_temp("real_pictures.bitmaps.lod", &dir);

        run_ok_in(bin, &dir, &["extract", "real_sprite_pal.lod", "out"]);
        for (name, len, crc, w, h) in [
            ("BATATA0.bmp", 52606usize, "2822756C", 226, 226),  // == MMArchive's own file
            ("Swptree1.bmp", 24710, "CBBB5B29", 112, 211),      // == MMArchive's own file
            ("ARROWA0.bmp", 8278, "149AFB37", 60, 120),
            ("C3_HASTE.bmp", 1218, "DB768106", 20, 7),
        ] {
            let p = dir.join("out").join(name);
            assert!(p.exists(), "[{}] {} should be extracted", label, name);
            let data = fs::read(&p).unwrap();
            assert_eq!(data.len(), len, "[{}] {} size", label, name);
            assert_eq!(crc32_hex(&data), crc, "[{}] {} content", label, name);
            assert_eq!(bmp_header(&data), (w, h, 8, 256), "[{}] {} header", label, name);
        }
        cleanup(&dir);
    }
}

/// The palette table is keyed on the sprite's 20 header bytes as well as its
/// name, so a sprite someone replaced with their own keeps the palette it
/// names. `real_sprite_nofix.lod` holds a resource called `BATATA0` that is not
/// the game's bat: the correction must not fire, and it must come out coloured
/// with palette 2, which is what its own header asks for.
#[test]
fn test_sprite_palette_fixup_is_keyed_on_the_header() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sprite_nofix_{}", label));
        copy_real_to_temp("real_sprite_nofix.lod", &dir);
        copy_real_to_temp("real_pictures.bitmaps.lod", &dir);

        run_ok_in(bin, &dir, &["extract", "real_sprite_nofix.lod", "out"]);
        let data = fs::read(dir.join("out").join("BATATA0.bmp")).unwrap();
        assert_eq!(bmp_header(&data), (20, 7, 8, 256), "[{}] not the game's bat", label);
        // Same bytes as C3_HASTE, which is what it is a copy of.
        assert_eq!(crc32_hex(&data), "DB768106",
            "[{}] the correction must not have fired", label);
        cleanup(&dir);
    }
}

/// With no bitmaps.lod to take a palette from there is nothing to colour a
/// sprite with, so the stored bytes are written instead — which is what the
/// Delphi CLI ends up doing too, its extract loop catching the failure and
/// re-extracting raw. `real_sprites.lod` names palette 23391, which no
/// bitmaps.lod has.
#[test]
fn test_sprite_without_a_palette_falls_back_to_stored_bytes() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sprite_nopal_{}", label));
        copy_real_to_temp("real_sprites.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_sprites.lod", "out"]);
        let data = fs::read(dir.join("out").join("Editor i1.bmp")).unwrap();
        assert_ne!(&data[..2], b"BM", "[{}] there was no palette to build a BMP with", label);
        assert_eq!(crc32_hex(&data), "ACDC9E8D", "[{}] stored bytes", label);
        cleanup(&dir);
    }
}

/// Textures and icons carry their own palette, so they always decode. What
/// differs is the mipmaps: a bitmaps.lod texture decompresses to more than
/// BmpWidth*BmpHeight because three half-size levels follow the picture, and
/// those are not part of it. An icons.lod resource has none.
#[test]
fn test_texture_decoding_with_and_without_mipmaps() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("texture_{}", label));
        copy_real_to_temp("real_bitmaps.lod", &dir);
        copy_real_to_temp("real_icons.lod", &dir);

        // bitmaps.lod: 128x128 with mipmaps (UnpSize 21760 vs BmpSize 16384)
        run_ok_in(bin, &dir, &["extract", "real_bitmaps.lod", "tex"]);
        for (name, crc) in [("HDLAV007.bmp", "B3FE7A28"), ("HDWTR006.bmp", "09800FAF")] {
            let data = fs::read(dir.join("tex").join(name)).unwrap();
            assert_eq!(bmp_header(&data), (128, 128, 8, 256), "[{}] {}", label, name);
            // 14 + 40 + 1024 + 128*128, i.e. the picture only
            assert_eq!(data.len(), 17462, "[{}] {} must not carry the mip levels", label, name);
            assert_eq!(crc32_hex(&data), crc, "[{}] {}", label, name);
        }
        // the palette resource is not a picture
        let act = fs::read(dir.join("tex").join("pal453.act")).unwrap();
        assert_eq!(act.len(), 768, "[{}] palette", label);

        // icons.lod: no mipmaps, and sizes that are not powers of two
        run_ok_in(bin, &dir, &["extract", "real_icons.lod", "ico"]);
        for (name, crc, w, h) in [
            ("GF-ASpell.bmp", "2205C82B", 36, 46),
            ("GF-ASpellD.bmp", "857A0994", 36, 46),
            ("GF-Swrd.bmp", "2D3BE4FD", 42, 43),
        ] {
            let data = fs::read(dir.join("ico").join(name)).unwrap();
            assert_eq!(bmp_header(&data), (w, h, 8, 256), "[{}] {}", label, name);
            assert_eq!(crc32_hex(&data), crc, "[{}] {}", label, name);
        }
        cleanup(&dir);
    }
}

/// Heroes stores pictures in something it calls PCX that is not PCX at all: a
/// 12-byte header, raw pixels, and a palette only when they are 8-bit.
/// `real_h3.lod` has one of each, plus a resource that is not a picture.
#[test]
fn test_h3_pcx_decoding() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("h3pcx_{}", label));
        copy_real_to_temp("real_h3.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_h3.lod", "out"]);

        // 24-bit: no colour table
        let big = fs::read(dir.join("out").join("Ar_Bg.bmp")).unwrap();
        assert_eq!(bmp_header(&big), (800, 600, 24, 0), "[{}] 24-bit PCX", label);
        assert_eq!(crc32_hex(&big), "69A2B374", "[{}] Ar_Bg", label);

        // 8-bit: 256 colours
        let small = fs::read(dir.join("out").join("ArA_CoBl.bmp")).unwrap();
        assert_eq!(bmp_header(&small), (48, 88, 8, 256), "[{}] 8-bit PCX", label);
        assert_eq!(crc32_hex(&small), "543EA27E", "[{}] ArA_CoBl", label);

        // not a picture, so not touched
        let other = fs::read(dir.join("out").join("ab.h3c")).unwrap();
        assert_eq!(crc32_hex(&other), "E03F056B", "[{}] ab.h3c", label);
        cleanup(&dir);
    }
}

/// Pull one resource's stored bytes out of an MM LOD, for tests that need to
/// look at what was written rather than at what comes back out.
/// Returns (blob, name field size).
fn lod_stored_entry(path: &Path, want: &str) -> (Vec<u8>, usize) {
    let raw = fs::read(path).unwrap();
    assert_eq!(&raw[..4], b"LOD\0", "{:?} is not a LOD", path);
    let u32_at = |o: usize| u32::from_le_bytes(raw[o..o + 4].try_into().unwrap());
    let archive_start = u32_at(272) as usize;
    let count = u16::from_le_bytes(raw[284..286].try_into().unwrap()) as usize;
    let version = String::from_utf8_lossy(&raw[4..84]);
    let version = version.split('\0').next().unwrap_or("");
    let (name_size, item) = if version == "MMVIII" { (0x40, 0x4C) } else { (0x10, 0x20) };
    for i in 0..count {
        let at = archive_start + i * item;
        let end = raw[at..at + name_size].iter().position(|&b| b == 0).unwrap_or(name_size);
        let name = String::from_utf8_lossy(&raw[at..at + end]).to_string();
        if name.eq_ignore_ascii_case(want) {
            let addr = archive_start + u32_at(at + name_size) as usize;
            let size = u32_at(at + name_size + 4) as usize;
            return (raw[addr..addr + size].to_vec(), name_size);
        }
    }
    panic!("{:?} has no resource called {}", path, want);
}

/// A picture added to an archive must come back out as the same picture.
///
/// This is the half the extraction tests cannot cover: `add` has to turn a
/// .bmp back into the game's own format (RSLod.pas PackBitmap / PackSprite /
/// PackPcx), and getting the header wrong there produces an archive that
/// round-trips through mmarch and is unreadable everywhere else.
#[test]
fn test_picture_add_roundtrip() {
    for (label, ref bin) in get_binaries() {
        for (fixture, archive_type, new_name, resource) in [
            ("real_bitmaps.lod", "mmbitmapslod", "made.bitmaps.lod", "HDLAV007.bmp"),
            ("real_icons.lod", "mmiconslod", "made.icons.lod", "GF-Swrd.bmp"),
            ("real_sprite_pal.lod", "mmspriteslod", "made.sprites.lod", "C3_HASTE.bmp"),
            ("real_h3.lod", "h3lod", "made.lod", "ArA_CoBl.bmp"),
            ("real_h3.lod", "h3lod", "made24.lod", "Ar_Bg.bmp"),
        ] {
            let dir = temp_dir(&format!("pic_rt_{}_{}", archive_type, label));
            copy_real_to_temp(fixture, &dir);
            // the palettes sprites and textures are matched against
            copy_real_to_temp("real_pictures.bitmaps.lod", &dir);

            run_ok_in(bin, &dir, &["extract", fixture, "out"]);
            let original = fs::read(dir.join("out").join(resource)).unwrap();
            assert_eq!(&original[..2], b"BM", "[{}] {} should be a BMP", label, resource);

            let from = format!("out/{}", resource);
            run_ok_in(bin, &dir, &["create", new_name, archive_type, ".", &from]);
            run_ok_in(bin, &dir, &["extract", new_name, "back"]);

            let again = fs::read(dir.join("back").join(resource)).unwrap();
            assert_eq!(again, original,
                "[{}] {} must survive a trip through {}", label, resource, archive_type);
            cleanup(&dir);
        }
    }
}

/// Adding a texture to a bitmaps.lod has to find the palette index it belongs
/// to (RSLod.pas FindBitmapPalette -> RSMMArchivesFindSamePalette), and
/// replacing one has to keep the flags the original carried. HDLAV007 uses
/// palette 862 and Bits 18, i.e. mipmapped.
#[test]
fn test_bitmaps_add_finds_the_palette_and_keeps_the_flags() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("bmp_pal_{}", label));
        copy_real_to_temp("real_bitmaps.lod", &dir);
        copy_real_to_temp("real_pictures.bitmaps.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_bitmaps.lod", "out"]);

        // a fresh archive: the index has to come from matching the palette
        run_ok_in(bin, &dir, &["create", "fresh.bitmaps.lod", "mmbitmapslod", ".",
                               "out/HDLAV007.bmp"]);
        let (blob, ns) = lod_stored_entry(&dir.join("fresh.bitmaps.lod"), "HDLAV007");
        let i16_at = |o: usize| i16::from_le_bytes(blob[o..o + 2].try_into().unwrap());
        let i32_at = |o: usize| i32::from_le_bytes(blob[o..o + 4].try_into().unwrap());
        assert_eq!(i32_at(ns), 128 * 128, "[{}] BmpSize", label);
        assert_eq!((i16_at(ns + 8), i16_at(ns + 10)), (128, 128), "[{}] dimensions", label);
        assert_eq!(i16_at(ns + 20), 862, "[{}] palette index, matched by content", label);
        assert_eq!(i32_at(ns + 28), 0x12, "[{}] a new texture gets Bits $12", label);
        assert_eq!((i16_at(ns + 12), i16_at(ns + 14)), (7, 7), "[{}] log2 of the size", label);
        assert_eq!((i16_at(ns + 16), i16_at(ns + 18)), (127, 127), "[{}] size - 1", label);
        // mipmapped: the pixel buffer is the picture plus three half-size levels
        assert_eq!(i32_at(ns + 24), 16384 + 4096 + 1024 + 256, "[{}] UnpSize", label);

        // replacing the resource in the archive it came from keeps its flags
        run_ok_in(bin, &dir, &["add", "real_bitmaps.lod", "out/HDLAV007.bmp"]);
        let (again, ns2) = lod_stored_entry(&dir.join("real_bitmaps.lod"), "HDLAV007");
        let j16 = |o: usize| i16::from_le_bytes(again[o..o + 2].try_into().unwrap());
        let j32 = |o: usize| i32::from_le_bytes(again[o..o + 4].try_into().unwrap());
        assert_eq!(j16(ns2 + 20), 862, "[{}] kept the palette index", label);
        assert_eq!(j32(ns2 + 28), 18, "[{}] kept Bits", label);
        cleanup(&dir);
    }
}

/// A texture needs a palette index too, and there is no sensible default: a
/// texture stored under palette 0 is a picture that renders in the wrong
/// colours. MMArchMain.pas `getPalette` raises rather than guess, and so does
/// mmarch — but `/p` overrides the search, and replacing an existing texture
/// inherits the index it already had (covered above).
#[test]
fn test_bitmaps_add_without_a_palette_is_refused() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("bmp_nopal_{}", label));
        copy_real_to_temp("real_bitmaps.lod", &dir);
        copy_real_to_temp("real_pictures.bitmaps.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_bitmaps.lod", "out"]);

        // Repaint the colour table so it matches no palette anywhere.
        let mut orphan = fs::read(dir.join("out").join("HDLAV007.bmp")).unwrap();
        for i in 0..256 {
            orphan[54 + i * 4] = (i as u8).wrapping_mul(7);
            orphan[54 + i * 4 + 1] = (i as u8).wrapping_mul(11);
            orphan[54 + i * 4 + 2] = (i as u8).wrapping_mul(13);
        }
        fs::write(dir.join("orphan.bmp"), &orphan).unwrap();

        let (_out, err, ok) = run_in(bin, &dir,
            &["create", "nopal.bitmaps.lod", "mmbitmapslod", ".", "orphan.bmp"]);
        assert!(ok, "[{}] the command itself should not blow up: {}", label, err);
        assert!(err.contains("palette"), "[{}] it should say what was missing: {}", label, err);
        assert!(list_archive_in(bin, &dir, "nopal.bitmaps.lod").is_empty(),
            "[{}] and nothing should have been stored", label);

        // /p says which one to use, and the header carries it.
        run_ok_in(bin, &dir, &["create", "given.bitmaps.lod", "mmbitmapslod", ".",
                               "orphan.bmp", "/p", "42"]);
        let (blob, ns) = lod_stored_entry(&dir.join("given.bitmaps.lod"), "orphan");
        assert_eq!(i16::from_le_bytes(blob[ns + 20..ns + 22].try_into().unwrap()), 42,
            "[{}] /p should pick the palette index", label);
        cleanup(&dir);
    }
}

/// A sprite needs a palette, and an add that cannot find one must fail without
/// taking the resource it was replacing with it.
#[test]
fn test_sprite_add_without_a_palette_keeps_the_old_resource() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("sprite_add_{}", label));
        copy_real_to_temp("real_sprite_pal.lod", &dir);
        copy_real_to_temp("real_pictures.bitmaps.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_sprite_pal.lod", "out"]);
        let before = fs::read(dir.join("out").join("C3_HASTE.bmp")).unwrap();

        // Repaint it with a palette that is in no bitmaps.lod, so nothing
        // matches and no resource of that name exists to inherit one from.
        let mut orphan = before.clone();
        for i in 0..256 {
            orphan[54 + i * 4] = (i as u8).wrapping_mul(7);
            orphan[54 + i * 4 + 1] = (i as u8).wrapping_mul(11);
            orphan[54 + i * 4 + 2] = (i as u8).wrapping_mul(13);
        }
        fs::write(dir.join("orphan.bmp"), &orphan).unwrap();

        let (_out, err, ok) = run_in(bin, &dir, &["add", "real_sprite_pal.lod", "orphan.bmp"]);
        assert!(ok, "[{}] the command itself should not blow up: {}", label, err);
        assert!(err.contains("palette"), "[{}] it should say what was missing: {}", label, err);

        // everything that was in the archive is still in it
        let names = list_archive_in(bin, &dir, "real_sprite_pal.lod");
        for want in ["BATATA0", "C3_HASTE", "Swptree1", "ARROWA0"] {
            assert!(names.iter().any(|n| n.eq_ignore_ascii_case(want)),
                "[{}] {} should still be there: {:?}", label, want, names);
        }
        run_ok_in(bin, &dir, &["extract", "real_sprite_pal.lod", "after"]);
        assert_eq!(fs::read(dir.join("after").join("C3_HASTE.bmp")).unwrap(), before,
            "[{}] and unchanged", label);
        cleanup(&dir);
    }
}

/// A mipmapped texture is halved three times, so both sides have to be a power
/// of two of at least 4 (RSLod.pas raises SRSLodMustPowerOf2). Icons have no
/// mipmaps and take any size — `GF-Swrd` is 42x43.
#[test]
fn test_mipmapped_texture_must_be_a_power_of_two() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("pow2_{}", label));
        copy_real_to_temp("real_icons.lod", &dir);
        copy_real_to_temp("real_pictures.bitmaps.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_icons.lod", "out"]);

        // /p gets past the palette lookup, which RSPak does first, so the size
        // check is what this has to fail on.
        let (_out, err, ok) = run_in(bin, &dir,
            &["create", "odd.bitmaps.lod", "mmbitmapslod", ".", "out/GF-Swrd.bmp", "/p", "1"]);
        assert!(ok, "[{}] the command itself should not blow up: {}", label, err);
        assert!(err.contains("power of 2"), "[{}] it should say why: {}", label, err);

        // the same picture is fine in an icons archive
        run_ok_in(bin, &dir, &["create", "fine.icons.lod", "mmiconslod", ".", "out/GF-Swrd.bmp"]);
        run_ok_in(bin, &dir, &["extract", "fine.icons.lod", "back"]);
        assert_eq!(fs::read(dir.join("back").join("GF-Swrd.bmp")).unwrap(),
                   fs::read(dir.join("out").join("GF-Swrd.bmp")).unwrap(),
                   "[{}] icons take any size", label);
        cleanup(&dir);
    }
}

/// A Heroes LOD says a resource is compressed by setting its packed-size
/// field, not by the two size fields disagreeing — and that distinction is
/// load-bearing, because Heroes archives do contain resources that compress to
/// exactly their own length. `Lcdesc.txt` in Heroes 3's `H3ab_bmp.lod` is 54
/// bytes either way, and `AVLXsu12.def` in `H3ab_spr.lod` is 1683; treating
/// "the sizes match" as "not compressed" handed both of them out still zipped.
///
/// The fixture also carries the other two shapes — an ordinary compressed
/// resource and one stored as it is — and one picture of each colour depth.
#[test]
fn test_h3_packed_flag_and_picture_depths() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("h3_sizes_{}", label));
        copy_real_to_temp("real_h3_sizes.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_h3_sizes.lod", "out"]);

        // compressed to exactly its own length: 54 stored, 54 unpacked
        let same = fs::read(dir.join("out").join("Lcdesc.txt")).unwrap();
        assert_eq!(same.len(), 54, "[{}] Lcdesc.txt length", label);
        assert_ne!(&same[..2], b"\x78\x9c",
            "[{}] Lcdesc.txt came out still compressed", label);
        assert_eq!(crc32_hex(&same), "97A0721A", "[{}] Lcdesc.txt", label);

        // compressed the ordinary way: 25 stored, 36 unpacked
        let packed = fs::read(dir.join("out").join("SkillLev.txt")).unwrap();
        assert_eq!(packed.len(), 36, "[{}] SkillLev.txt length", label);
        assert_eq!(crc32_hex(&packed), "B1D03872", "[{}] SkillLev.txt", label);

        // stored as it is: the packed-size field is 0
        let stored = fs::read(dir.join("out").join("AH16_.msk")).unwrap();
        assert_eq!(stored.len(), 14, "[{}] AH16_.msk length", label);
        assert_eq!(crc32_hex(&stored), "EDFC8859", "[{}] AH16_.msk", label);

        // pictures, one of each depth
        let p8 = fs::read(dir.join("out").join("HPSyyy.bmp")).unwrap();
        assert_eq!(bmp_header(&p8), (48, 32, 8, 256), "[{}] 8-bit picture", label);
        assert_eq!(crc32_hex(&p8), "626C1906", "[{}] HPSyyy", label);
        let p24 = fs::read(dir.join("out").join("Camp1DB2.bmp")).unwrap();
        assert_eq!(bmp_header(&p24), (200, 116, 24, 0), "[{}] 24-bit picture", label);
        assert_eq!(crc32_hex(&p24), "C41A9854", "[{}] Camp1DB2", label);

        // and all five survive a rebuild
        run_ok_in(bin, &dir, &["optimize", "real_h3_sizes.lod"]);
        run_ok_in(bin, &dir, &["extract", "real_h3_sizes.lod", "again"]);
        for name in ["Lcdesc.txt", "SkillLev.txt", "AH16_.msk", "HPSyyy.bmp", "Camp1DB2.bmp"] {
            assert_eq!(fs::read(dir.join("again").join(name)).unwrap(),
                       fs::read(dir.join("out").join(name)).unwrap(),
                       "[{}] {} after optimize", label, name);
        }
        cleanup(&dir);
    }
}

// ===========================================================================
// Regression fixtures: archives whose entries are NOT stored in table order
// ===========================================================================
//
// Real MM6/7/8 archives do not lay their files out in the order the entry table
// lists them — bitmaps.lod, icons.lod, sprites.lod and MM8's English*.lod all
// have unsorted addresses. Up to v6.0.1 the Rust port derived an entry's stored
// size from the gap to the *next table entry's* address, which is only right for
// a sorted archive. Every fixture below is a handful of entries lifted verbatim
// out of a real game archive (header copied byte for byte, blobs copied byte for
// byte) and re-laid-out so the addresses run out of order, which is enough to
// reproduce the whole family of bugs in a couple of KB.

/// MM8 localisation LOD (`mm8loclod`): 64-byte names in front of each
/// TMMLodFile header, unsorted addresses, and `.str` entries whose NUL
/// separators RSPak turns into CRLF.
///
/// v6.0.1 wrote 3 of these 4 files and two of them were 768 bytes of raw
/// archive data; `Out15.STR` failed outright with "failed to fill whole buffer"
/// because its size came out as a ~4 GB underflow.
#[test]
fn test_real_mm8loclod_unsorted() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_mm8loc_unsorted.T.lod",
            &["Awards.txt", "Out15.STR", "fontpal.pcx", "D07.STR"],
            &["Awards.txt", "Out15.STR", "fontpal.pcx", "D07.STR"],
            &[("Awards.txt", "F864563F"), ("Out15.STR", "2C683AE2"),
              ("fontpal.pcx", "AE969F9E"), ("D07.STR", "680B20DD")],
        );
    }
}

/// MM6/7 icons LOD (`mmiconslod`): 16-byte names, unsorted addresses.
/// v6.0.1 wrote 2 of these 4 files.
#[test]
fn test_real_mmiconslod_unsorted() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_icons_unsorted.lod",
            &["dchest.bin", "NWC.STR", "D28.EVT", "D13.STR"],
            &["dchest.bin", "NWC.STR", "D28.EVT", "D13.STR"],
            &[("dchest.bin", "7F51337F"), ("NWC.STR", "3B6D3EAD"),
              ("D28.EVT", "18C73750"), ("D13.STR", "293DEB46")],
        );
    }
}

/// MM6/7 bitmaps LOD (`mmbitmapslod`): unsorted addresses, and every shape a
/// picture archive holds. v6.0.1 mis-sized 10 of MM7 `BITMAPS.LOD`'s entries
/// outright and silently handed back the wrong bytes for the rest.
#[test]
fn test_real_mmbitmapslod_unsorted() {
    for (label, ref bin) in get_binaries() {
        real_archive_test(bin, label, "real_bitmaps_unsorted.lod",
            &["sgSTARS", "solid01", "Trim9_16a", "Trim9_16",
              "PAL007", "pal008", "errorlog.txt"],
            &["sgSTARS.bmp", "solid01.bmp", "Trim9_16a.bmp", "Trim9_16.bmp",
              "PAL007.act", "pal008.act", "errorlog.txt"],
            &[("sgSTARS.bmp", "2FE05744"), ("solid01.bmp", "9AB54B11"),
              ("Trim9_16a.bmp", "937680A4"), ("Trim9_16.bmp", "74823250"),
              ("PAL007.act", "56FD8F25"), ("pal008.act", "7F387331"),
              ("errorlog.txt", "08E7484A")],
        );
    }
}

/// The same fixture from the other side: not "does the CRC still match" but
/// "is each of these the picture the header describes". Every shipped texture
/// is mipmapped, so the two kinds that exist are Bits $12 (pixels stored as
/// they are) and Bits $13 (zlib); `Trim9_16a` and `Trim9_16` are 64x16 and
/// 16x64 with the same palette, so swapping width for height anywhere in the
/// decoder turns one into the other instead of going unnoticed.
#[test]
fn test_bitmaps_lod_covers_every_stored_shape() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("bmp_shapes_{}", label));
        copy_real_to_temp("real_bitmaps_unsorted.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_bitmaps_unsorted.lod", "out"]);
        let read = |n: &str| fs::read(dir.join("out").join(n)).unwrap();

        // Bits $13, compressed: 211 stored bytes become 5440, of which the
        // first 4096 are the picture and the rest are the three mipmap levels.
        assert_eq!(bmp_header(&read("sgSTARS.bmp")), (64, 64, 8, 256),
            "[{}] compressed texture", label);
        // Bits $12, stored: 16x16 with no compression at all.
        assert_eq!(bmp_header(&read("solid01.bmp")), (16, 16, 8, 256),
            "[{}] uncompressed texture", label);

        // the transposed pair
        let wide = read("Trim9_16a.bmp");
        let tall = read("Trim9_16.bmp");
        assert_eq!(bmp_header(&wide), (64, 16, 8, 256), "[{}] wide", label);
        assert_eq!(bmp_header(&tall), (16, 64, 8, 256), "[{}] tall", label);
        assert_eq!(wide.len(), tall.len(),
            "[{}] 64x16 and 16x64 are the same number of bytes", label);
        assert_ne!(wide, tall,
            "[{}] width and height must not be interchangeable", label);

        // A palette is 768 bytes with no BMP wrapper, and the name it is
        // spelt with varies inside a single archive.
        for n in ["PAL007.act", "pal008.act"] {
            assert_eq!(read(n).len(), 768, "[{}] {} is a bare palette", label, n);
        }
        // ...and a picture LOD can hold plain data as well.
        assert_eq!(read("errorlog.txt").len(), 57, "[{}] plain entry", label);
        assert!(read("errorlog.txt").starts_with(b"CSpriteFrameTable"),
            "[{}] plain entry content", label);
        cleanup(&dir);
    }
}

/// Writing the fixture back: a decoded texture has to survive being added to
/// the archive it came out of, and the palette index has to be found again by
/// matching the 768 bytes against the archive's own palettes — which are
/// spelt `PAL007` and `pal008` here, so a case-sensitive comparison loses
/// them. (In MM8's `bitmaps.lod` 93 of the 292 palettes are spelt `Pal`.)
#[test]
fn test_bitmaps_lod_roundtrip_finds_its_palette_by_content() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("bmp_roundtrip_{}", label));
        copy_real_to_temp("real_bitmaps_unsorted.lod", &dir);
        run_ok_in(bin, &dir, &["extract", "real_bitmaps_unsorted.lod", "out"]);

        for (name, palette, w, h) in [("solid01", 8i16, 16i16, 16i16),
                                      ("Trim9_16a", 8, 64, 16),
                                      ("sgSTARS", 7, 64, 64)] {
            run_ok_in(bin, &dir, &["add", "real_bitmaps_unsorted.lod",
                                   &format!("out/{}.bmp", name)]);
            let (blob, ns) = lod_stored_entry(&dir.join("real_bitmaps_unsorted.lod"), name);
            let i16_at = |o: usize| i16::from_le_bytes(blob[o..o + 2].try_into().unwrap());
            let i32_at = |o: usize| i32::from_le_bytes(blob[o..o + 4].try_into().unwrap());
            assert_eq!(i16_at(ns + 20), palette,
                "[{}] {}: palette found by content, whatever its name's case", label, name);
            assert_eq!((i16_at(ns + 8), i16_at(ns + 10)), (w, h),
                "[{}] {}: dimensions", label, name);
            let px = (w as i32) * (h as i32);
            assert_eq!(i32_at(ns), px, "[{}] {}: BmpSize", label, name);
            assert_eq!(i32_at(ns + 24), px + px / 4 + px / 16 + px / 64,
                "[{}] {}: UnpSize is the picture plus three mipmap levels", label, name);
        }

        // and the pictures still decode to the same bytes
        run_ok_in(bin, &dir, &["extract", "real_bitmaps_unsorted.lod", "again"]);
        for n in ["solid01.bmp", "Trim9_16a.bmp", "sgSTARS.bmp", "Trim9_16.bmp",
                  "PAL007.act", "pal008.act", "errorlog.txt"] {
            assert_eq!(fs::read(dir.join("again").join(n)).unwrap(),
                       fs::read(dir.join("out").join(n)).unwrap(),
                       "[{}] {} changed on the way back in", label, n);
        }
        cleanup(&dir);
    }
}

/// MM6 chapter LOD (`mm6save`): unsorted addresses plus `OutB3.ddm`, whose
/// TMM6GamesFile header counts its own 8 bytes in DataSize. RSPak never reads
/// that field — it passes `Size[i] - headerSize` to Unzip — so trusting it
/// overruns the entry and the file is refused.
#[test]
fn test_real_chapter_ddm_datasize() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("chapter_ddm_{}", label));
        copy_real_to_temp("real_chapter_ddm.lod", &dir);

        run_ok_in(bin, &dir, &["extract", "real_chapter_ddm.lod", "out"]);
        for (name, len) in [("clock.bin", 40usize), ("outa1.ddm", 86292),
                            ("OutB3.ddm", 194996), ("header.bin", 100)] {
            let p = dir.join("out").join(name);
            assert!(p.exists(), "[{}] {} should be extracted", label, name);
            assert_eq!(fs::metadata(&p).unwrap().len() as usize, len,
                "[{}] {} unpacked size", label, name);
        }

        let stdout = run_ok_in(bin, &dir, &["checksum", "real_chapter_ddm.lod", "*"]);
        for crc in ["A17FA155", "434032F2", "581F20E3"] {
            assert!(stdout.contains(crc), "[{}] checksum missing {}: {}", label, crc, stdout);
        }
        cleanup(&dir);
    }
}

/// The same chapter LOD carries `header.bin` twice. Delphi extracts entries in
/// index order, so the later one lands on disk; the Rust port extracts on all
/// cores, so without a tie-break the winner depended on which worker finished
/// last. Extracting repeatedly has to give the same bytes every time.
#[test]
fn test_duplicate_entry_name_extraction_is_deterministic() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("dup_name_{}", label));
        copy_real_to_temp("real_chapter_ddm.lod", &dir);

        // CRC D5FB4143 is the second header.bin, the one Delphi's loop leaves
        // behind; EE6D648E is the first.
        for round in 0..8 {
            let out = dir.join(format!("out{}", round));
            run_ok_in(bin, &dir, &["extract", "real_chapter_ddm.lod",
                                   out.file_name().unwrap().to_str().unwrap()]);
            let got = fs::read(out.join("header.bin")).unwrap();
            assert_eq!(crc32_hex(&got), "D5FB4143",
                "[{}] round {}: duplicate name must resolve to the last entry", label, round);
        }
        cleanup(&dir);
    }
}

/// VID archives have no size field at all: an entry runs until the nearest
/// following address of ANY other entry (RSLod.pas TRSVid.GetFileSize), not
/// until the next one in the table. v6.0.1 used the table order, so `Charlie`
/// came out 711 bytes instead of 274 and `Bravo` failed outright.
#[test]
fn test_real_vid_unsorted() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("vid_unsorted_{}", label));
        copy_real_to_temp("real_vid_unsorted.vid", &dir);

        run_ok_in(bin, &dir, &["extract", "real_vid_unsorted.vid", "out"]);
        for (name, len, crc) in [("Alpha.smk", 200usize, "A7A654F7"),
                                 ("Bravo.smk", 237, "3B82F353"),
                                 ("Charlie.smk", 274, "84A4D962")] {
            let p = dir.join("out").join(name);
            assert!(p.exists(), "[{}] {} should be extracted", label, name);
            let data = fs::read(&p).unwrap();
            assert_eq!(data.len(), len, "[{}] {} size", label, name);
            assert_eq!(crc32_hex(&data), crc, "[{}] {} content", label, name);
        }
        cleanup(&dir);
    }
}

/// A `mm8loclod` written by mmarch must put the name in a 64-byte field, the
/// way the game and MMArchive read it. v6.0.1 wrote 16, which put the
/// TMMLodFile header inside the name field: the archive round-tripped through
/// mmarch itself and was unreadable everywhere else (the same failure mode as
/// issue #2).
#[test]
fn test_mm8loclod_written_layout() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("mm8_layout_{}", label));
        // > 256 bytes so the add path actually compresses (TRSLod.Zip's cutoff)
        write_test_file(&dir.join("Hello.txt"), &b"MM8 localisation payload. ".repeat(40));
        run_ok_in(bin, &dir, &["create", "out.T.lod", "mm8loclod", ".", "Hello.txt"]);

        let raw = fs::read(dir.join("out.T.lod")).unwrap();
        let archive_start = u32::from_le_bytes(raw[272..276].try_into().unwrap()) as usize;
        let count = u16::from_le_bytes(raw[284..286].try_into().unwrap()) as usize;
        assert_eq!(count, 1, "[{}] entry count", label);

        // 76-byte entry: name[64], addr, size, unused
        let e = archive_start;
        let addr = u32::from_le_bytes(raw[e + 64..e + 68].try_into().unwrap()) as usize;
        let size = u32::from_le_bytes(raw[e + 68..e + 72].try_into().unwrap()) as usize;
        assert_eq!(archive_start + addr + size, raw.len(),
            "[{}] the entry's size field must cover the rest of the file", label);

        // The blob is name[64] + TMMLodFile[32] + payload; BmpSize is 0 and
        // DataSize is the payload length for a plain file.
        let blob = &raw[archive_start + addr..archive_start + addr + size];
        assert_eq!(&blob[..9], b"Hello.txt", "[{}] blob starts with the name", label);
        let bmp_size = i32::from_le_bytes(blob[64..68].try_into().unwrap());
        let data_size = i32::from_le_bytes(blob[68..72].try_into().unwrap()) as usize;
        let unp_size = i32::from_le_bytes(blob[88..92].try_into().unwrap()) as usize;
        assert_eq!(bmp_size, 0, "[{}] BmpSize at offset 64", label);
        assert_eq!(64 + 32 + data_size, size, "[{}] DataSize at offset 68", label);
        assert_eq!(unp_size, 26 * 40, "[{}] UnpSize at offset 88", label);
        cleanup(&dir);
    }
}

/// A malformed entry must cost you that entry, not the rest of the archive.
///
/// `hasincorrectresource.icons.lod` carries a TMMLodFile header with
/// BmpSize = UnpSize = -16777216. Those fields are signed Int32 in RSPak, and
/// casting a negative one to a length asks for ~18 exabytes — which, with
/// panic = "abort" in the release profile, takes the whole process down and
/// loses the files that would have extracted fine.
#[test]
fn test_malformed_header_does_not_abort_extraction() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("failsafe_{}", label));
        let src = test_general_dir().join("failsafe_test").join("hasincorrectresource.icons.lod");
        assert!(src.exists(), "fixture {:?} missing", src);
        fs::copy(&src, dir.join("bad.icons.lod")).unwrap();

        let (_out, err, ok) = run_in(bin, &dir, &["extract", "bad.icons.lod", "out"]);
        assert!(ok, "[{}] extraction must finish, not abort: {}", label, err);
        assert!(err.contains("test.bin"),
            "[{}] the bad entry should be named on stderr: {}", label, err);

        // The three sound entries are intact and must still be there.
        for good in ["test.pcx", "test.str", "test.txt"] {
            assert!(dir.join("out").join(good).exists(),
                "[{}] {} should still extract. Got: {:?}", label, good,
                fs::read_dir(dir.join("out")).ok().map(|d| d
                    .filter_map(|e| e.ok())
                    .map(|e| e.file_name().to_string_lossy().to_string())
                    .collect::<Vec<_>>()));
        }
        cleanup(&dir);
    }
}

// ===========================================================================
// Delphi alignment fixes
// ===========================================================================

// Fix #1: checksum --v a:HASH — single-char resource name inline verify
// Rust used to treat "a:HASH" as a file path (colon at pos 1 = drive letter).
// Delphi treats any ':' as inline mode. Now Rust matches.
#[test]
fn test_checksum_inline_single_char_name() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cs_sc_{}", label));
        // Create an archive with a single-char-name file
        write_test_file(&dir.join("a"), b"data for a");
        run_ok_in(bin, &dir, &["create", "test.lod", "mmiconslod", ".", "a"]);

        // Get the checksum of resource "a"
        let stdout = run_ok_in(bin, &dir, &["checksum", "test.lod", "a"]);
        let line = stdout.trim().trim_end_matches('\r');
        let hash = &line[..8];

        // Inline verify with single-char name: "a:HASH"
        let pair = format!("a:{}", hash);
        let (stdout2, stderr, ok) = run_in(bin, &dir, &["checksum", "test.lod", "--v", &pair]);
        assert!(ok, "[{}] inline verify of single-char name should succeed.\nstdout: {}\nstderr: {}",
                label, stdout2, stderr);
        assert!(stdout2.contains("OK"), "[{}] should contain OK: {}", label, stdout2);

        cleanup(&dir);
    }
}

// Fix #4: wildcard .act/.bmp matching — extract *.act should NOT include .bmp files
// In a bitmaps LOD, extensionless entries are either .act (768-byte palette) or .bmp.
// extract *.act must only get palette files, not bitmaps.
#[test]
fn test_wildcard_act_bmp_disambiguation() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("wild_actbmp_{}", label));
        copy_test_file("pal994.act", &dir);
        copy_test_file("testpal.bmp", &dir);
        run_ok_in(bin, &dir, &["create", "test.bitmaps.lod", "mmbitmapslod",
            ".", "pal994.act", "testpal.bmp"]);

        // Extract only *.act — should get pal994.act but NOT testpal.bmp
        let out_act = dir.join("out_act");
        run_ok_in(bin, &dir, &["extract", "test.bitmaps.lod", &out_act.to_string_lossy(), "*.act"]);
        assert!(out_act.join("pal994.act").exists(),
                "[{}] pal994.act should be extracted by *.act", label);
        assert!(!out_act.join("testpal.bmp").exists(),
                "[{}] testpal.bmp should NOT be extracted by *.act", label);

        // Extract only *.bmp — should get testpal.bmp but NOT pal994.act
        let out_bmp = dir.join("out_bmp");
        run_ok_in(bin, &dir, &["extract", "test.bitmaps.lod", &out_bmp.to_string_lossy(), "*.bmp"]);
        assert!(out_bmp.join("testpal.bmp").exists(),
                "[{}] testpal.bmp should be extracted by *.bmp", label);
        assert!(!out_bmp.join("pal994.act").exists(),
                "[{}] pal994.act should NOT be extracted by *.bmp", label);

        cleanup(&dir);
    }
}

// Fix #4: delete *.act should NOT delete .bmp files (and vice versa)
#[test]
fn test_wildcard_delete_act_bmp_disambiguation() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("wild_del_actbmp_{}", label));
        copy_test_file("pal994.act", &dir);
        copy_test_file("testpal.bmp", &dir);
        run_ok_in(bin, &dir, &["create", "test.bitmaps.lod", "mmbitmapslod",
            ".", "pal994.act", "testpal.bmp"]);

        // Delete *.act — should remove pal994 but keep testpal
        run_ok_in(bin, &dir, &["delete", "test.bitmaps.lod", "*.act"]);
        let names = list_archive_in(bin, &dir, "test.bitmaps.lod");
        assert!(!names.iter().any(|n| n.eq_ignore_ascii_case("pal994")),
                "[{}] pal994 should be deleted by *.act: {:?}", label, names);
        assert!(names.iter().any(|n| n.eq_ignore_ascii_case("testpal")),
                "[{}] testpal should survive *.act delete: {:?}", label, names);

        cleanup(&dir);
    }
}

// Fix #5: compare with non-folder/non-archive should error (exit 1), not silently succeed
#[test]
fn test_compare_bad_input_errors() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_bad_{}", label));
        write_test_file(&dir.join("notarchive.txt"), b"hello");
        write_test_file(&dir.join("other.txt"), b"world");

        let (_, stderr, ok) = run_in(bin, &dir, &["compare", "notarchive.txt", "other.txt"]);
        assert!(!ok, "[{}] compare of non-archive files should fail.\nstderr: {}", label, stderr);

        cleanup(&dir);
    }
}

// Fix #5: compare with bad archive should say "Incorrect MM Archive files" (no details)
#[test]
fn test_compare_bad_archive_error_message() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_badarch_{}", label));
        write_test_file(&dir.join("a.lod"), b"not a real lod");
        write_test_file(&dir.join("b.lod"), b"also not real");

        let (stdout, stderr, ok) = run_in(bin, &dir, &["compare", "a.lod", "b.lod"]);
        let combined = format!("{}{}", stdout, stderr);
        assert!(!ok, "[{}] compare of bad archives should fail", label);
        assert!(combined.contains("Incorrect MM Archive files"),
                "[{}] should mention 'Incorrect MM Archive files': {}", label, combined);

        cleanup(&dir);
    }
}

// Fix #8: compare_in_archive_data — entries with different unpacked sizes should be
// considered different without needing to decompress.
// This is tested indirectly: if two archives have an entry with same name but
// different unpacked_size, compare should show it as modified.
// (The existing compare tests already cover this, but we add an explicit one.)
#[test]
fn test_compare_detects_modified_entries() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("cmp_mod_{}", label));

        // Create two archives with same-named but different-content file
        write_test_file(&dir.join("data.txt"), b"version1");
        run_ok_in(bin, &dir, &["create", "old.lod", "mmiconslod", ".", "data.txt"]);

        write_test_file(&dir.join("data.txt"), b"version2-longer");
        run_ok_in(bin, &dir, &["create", "new.lod", "mmiconslod", ".", "data.txt"]);

        let (stdout, _, _) = run_in(bin, &dir, &["compare", "old.lod", "new.lod"]);
        assert!(stdout.contains("[m]") || stdout.contains("data.txt"),
                "[{}] compare should detect modified entry: {}", label, stdout);

        cleanup(&dir);
    }
}

// Fix #9: add with non-existent folder should error
#[test]
fn test_add_nonexistent_folder_errors() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("add_nodir_{}", label));
        write_test_file(&dir.join("dummy.txt"), b"x");
        run_ok_in(bin, &dir, &["create", "test.lod", "mmiconslod", ".", "dummy.txt"]);

        let (_, stderr, ok) = run_in(bin, &dir, &["add", "test.lod", "nonexistent_dir/*.txt"]);
        assert!(!ok, "[{}] add from non-existent dir should fail.\nstderr: {}", label, stderr);

        cleanup(&dir);
    }
}

// Fix #9: diff-add-keep with non-existent folder should error
#[test]
fn test_diff_add_keep_nonexistent_folder_errors() {
    for (label, ref bin) in get_binaries() {
        let dir = temp_dir(&format!("dak_nodir_{}", label));

        let (_, stderr, ok) = run_in(bin, &dir, &["diff-add-keep", "nonexistent_dir"]);
        assert!(!ok, "[{}] diff-add-keep on non-existent dir should fail.\nstderr: {}", label, stderr);

        cleanup(&dir);
    }
}
