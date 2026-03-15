use crate::path_utils::*;

pub fn generate_script(
    deleted_folder_list: &[String],
    deleted_non_res_file_list: &[String],
    deleted_res_file_list: &[String],
    modified_archive_list: &[String],
    script_file_path: &str,
    diff_folder_name: &str,
    is_nsis: bool,
) {
    let mut deleted_folders = deleted_folder_list.to_vec();
    let mut deleted_non_res = deleted_non_res_file_list.to_vec();
    let mut deleted_res = deleted_res_file_list.to_vec();
    let mut modified_archives = modified_archive_list.to_vec();
    deleted_folders.sort();
    deleted_non_res.sort();
    deleted_res.sort();
    modified_archives.sort();

    let no_res = deleted_res.is_empty() && modified_archives.is_empty();

    let script = if is_nsis {
        generate_nsis(
            &deleted_folders,
            &deleted_non_res,
            &deleted_res,
            &modified_archives,
            diff_folder_name,
            no_res,
        )
    } else {
        generate_batch(
            &deleted_folders,
            &deleted_non_res,
            &deleted_res,
            &modified_archives,
            diff_folder_name,
        )
    };

    let _ = str_to_file(script_file_path, &script);
}

fn generate_nsis(
    deleted_folders: &[String],
    deleted_non_res: &[String],
    deleted_res: &[String],
    modified_archives: &[String],
    diff_folder_name: &str,
    no_res: bool,
) -> String {
    let mut s = String::new();
    s.push_str("\r\n");
    s.push_str(";--------------------------------\r\n");
    s.push_str(";Include Modern UI\r\n");
    s.push_str("!include \"MUI2.nsh\"\r\n");
    s.push_str("\r\n");
    s.push_str(";--------------------------------\r\n");
    s.push_str(";General\r\n");
    s.push_str("\r\n");
    s.push_str(";Name and file\r\n");
    s.push_str("Name \"Might and Magic Patch\"\r\n");
    s.push_str("OutFile \"patch.exe\"\r\n");
    s.push_str("Unicode True\r\n");
    s.push_str("; AutoCloseWindow true\r\n");
    s.push_str("\r\n");
    s.push_str("BrandingText \"NWC/3DO; Ubisoft\"\r\n");
    s.push_str("\r\n");
    s.push_str("; !define MUI_ICON \"icon.ico\"\r\n");
    s.push_str("\r\n");
    s.push_str(";--------------------------------\r\n");
    s.push_str(";Default installation folder\r\n");
    s.push_str("InstallDir $EXEDIR\r\n");
    s.push_str("\r\n");
    s.push_str(";Request application privileges for Windows Vista\r\n");
    s.push_str("RequestExecutionLevel user\r\n");
    s.push_str("\r\n");
    s.push_str(";--------------------------------\r\n");
    s.push_str(";Pages\r\n");
    s.push_str("\r\n");
    s.push_str("!insertmacro MUI_PAGE_INSTFILES\r\n");
    s.push_str("\r\n");
    s.push_str(";--------------------------------\r\n");
    s.push_str(";Languages\r\n");
    s.push_str("\r\n");
    s.push_str("!insertmacro MUI_LANGUAGE \"English\"\r\n");
    s.push_str("\r\n");
    s.push_str(";--------------------------------\r\n");
    s.push_str(";Installer Sections\r\n");
    s.push_str("\r\n");
    s.push_str("Section\r\n");
    s.push_str("\r\n");
    s.push_str(";-----FILE COPYING (MODIFYING, DELETING) STARTS HERE-----\r\n");
    s.push_str("\r\n");
    s.push_str("\tSetOutPath $INSTDIR\r\n");

    if !no_res {
        s.push_str("\tFile mmarch.exe\r\n");
    }
    s.push_str("\r\n");

    if !deleted_folders.is_empty() {
        for f in deleted_folders {
            s.push_str(&format!("\tRMDir /r /REBOOTOK \"$INSTDIR\\{}\"\r\n", f));
        }
        s.push_str("\r\n");
    }

    let diff_path = with_trailing_slash(&beautify_path(diff_folder_name));
    s.push_str(&format!(
        "\tFile /r /x *{} /x *{} {}*.*\r\n",
        TODELETE_EXT, EMPTY_FOLDER_KEEP, diff_path
    ));
    s.push_str("\r\n");

    if !deleted_non_res.is_empty() {
        for f in deleted_non_res {
            s.push_str(&format!("\tDelete \"{}\"\r\n", f));
        }
        s.push_str("\r\n");
    }

    if !deleted_res.is_empty() {
        for r in deleted_res {
            if let Some(pos) = r.find(ARCH_RES_SEPARATOR) {
                let archive = &r[..pos];
                let resource = &r[pos + 1..];
                s.push_str(&format!(
                    "\tnsExec::Exec 'mmarch delete \"{}\" \"{}\"'\r\n",
                    archive, resource
                ));
            }
        }
        s.push_str("\r\n");
    }

    if !modified_archives.is_empty() {
        for a in modified_archives {
            let stripped = a.trim_end_matches(ARCH_RES_SEPARATOR);
            s.push_str(&format!(
                "\tnsExec::Exec 'mmarch add \"{}\" \"{}{}\\*.*\"'\r\n",
                stripped, stripped, MMARCHIVE_EXT
            ));
            s.push_str(&format!(
                "\tRMDir /r /REBOOTOK \"$INSTDIR\\{}{}\"\r\n",
                stripped, MMARCHIVE_EXT
            ));
        }
        s.push_str("\r\n");
    }

    if !no_res {
        s.push_str("\tDelete \"mmarch.exe\"\r\n");
    }

    s.push_str("\r\n");
    s.push_str(";-----FILE COPYING (MODIFYING, DELETING) ENDS HERE-----\r\n");
    s.push_str("\r\n");
    s.push_str("SectionEnd\r\n");

    s
}

fn generate_batch(
    deleted_folders: &[String],
    deleted_non_res: &[String],
    deleted_res: &[String],
    modified_archives: &[String],
    _diff_folder_name: &str,
) -> String {
    let mut s = String::new();
    s.push_str("cd %~dp0\r\n");
    s.push_str("\r\n");

    for f in deleted_folders {
        s.push_str(&format!("rmdir /s /q \"{}\"\r\n", f));
    }
    s.push_str("\r\n");

    s.push_str(&format!(
        "(echo {} && echo {})>excludelist.txt\r\n",
        TODELETE_EXT, EMPTY_FOLDER_KEEP
    ));
    s.push_str("Xcopy files . /s /e /y /EXCLUDE:excludelist.txt\r\n");
    s.push_str("del excludelist.txt\r\n");
    s.push_str("\r\n");

    for f in deleted_non_res {
        s.push_str(&format!("del \"{}\"\r\n", f));
    }
    s.push_str("\r\n");

    for r in deleted_res {
        if let Some(pos) = r.find(ARCH_RES_SEPARATOR) {
            let archive = &r[..pos];
            let resource = &r[pos + 1..];
            s.push_str(&format!(
                "mmarch delete \"{}\" \"{}\"\r\n",
                archive, resource
            ));
        }
    }
    s.push_str("\r\n");

    for a in modified_archives {
        let stripped = a.trim_end_matches(ARCH_RES_SEPARATOR);
        s.push_str(&format!(
            "mmarch add \"{}\" \"{}{}\\*.*\"\r\n",
            stripped, stripped, MMARCHIVE_EXT
        ));
        s.push_str(&format!(
            "rmdir /s /q \"{}{}\"\r\n",
            stripped, MMARCHIVE_EXT
        ));
    }

    s
}
