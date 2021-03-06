# [mmarch](https://github.com/might-and-magic/mmarch)

Command line tool to handle (extract, replace, compare resources and more) Heroes of Might and Magic 3 and Might and Magic 6, 7, 8 resource archive files (e.g. lod files) for Windows.

[Download mmarch v3.2](https://github.com/might-and-magic/mmarch/releases/download/v3.2/mmarch.7z)

Based on [GrayFace's MMArchive](https://grayface.github.io/mm/#MMArchive) ([repo](https://github.com/GrayFace/Misc/)). If you need a graphical user interface tool, use MMArchive.

## Summary & Table of Contents

<pre>
mmarch <a href="#extract"><strong>e</strong>xtract</a> &lt;ARCHIVE_FILE&gt; &lt;FOLDER&gt; [FILE_TO_EXTRACT_1] [FILE_TO_EXTRACT_2] [...]
mmarch <a href="#list"><strong>l</strong>ist</a> &lt;ARCHIVE_FILE&gt; [SEPARATOR]
mmarch <a href="#add"><strong>a</strong>dd</a> &lt;ARCHIVE_FILE&gt; &lt;FILE_TO_ADD_1&gt; [FILE_TO_ADD_2] [...]
mmarch <a href="#delete"><strong>d</strong>elete</a> &lt;ARCHIVE_FILE&gt; &lt;FILE_TO_DELETE_1&gt; [FILE_TO_DELETE_2] [...]
mmarch <a href="#rename"><strong>r</strong>ename</a> &lt;ARCHIVE_FILE&gt; &lt;OLD_FILE_NAME&gt; &lt;NEW_FILE_NAME&gt;
mmarch <a href="#create"><strong>c</strong>reate</a> &lt;ARCHIVE_FILE&gt; &lt;ARCHIVE_FILE_TYPE&gt; &lt;FOLDER&gt; [FILE_TO_ADD_1] [FILE_TO_ADD_2] [...]
mmarch <a href="#merge"><strong>m</strong>erge</a> &lt;ARCHIVE_FILE&gt; &lt;ARCHIVE_FILE_2&gt;
mmarch <a href="#compare">compare</a> &lt;ARCHIVE_FILE_OR_FOLDER&gt; &lt;ARCHIVE_FILE_OR_FOLDER_2&gt;
mmarch compare &lt;ARCHIVE_FILE_OR_FOLDER&gt; &lt;ARCHIVE_FILE_OR_FOLDER_2&gt; <a href="#nsis">{nsis|batch}</a> &lt;SCRIPT_FILE&gt; &lt;DIFF_FOLDER_NAME&gt;
mmarch compare &lt;ARCHIVE_FILE_OR_FOLDER&gt; &lt;ARCHIVE_FILE_OR_FOLDER_2&gt; <a href="#filesonly">filesonly</a> &lt;DIFF_FOLDER&gt;
mmarch <a href="#diff-files-to-nsis-batch">diff-files-to-{nsis|batch}</a> &lt;OLD_DIFF_FOLDER&gt; &lt;SCRIPT_FILE&gt; &lt;DIFF_FOLDER_NAME&gt;
mmarch <a href="#diff-add-keep"><strong>d</strong>iff-<strong>a</strong>dd-<strong>k</strong>eep</a> &lt;DIFF_FOLDER&gt;
mmarch <a href="#optimize"><strong>o</strong>ptimize</a> &lt;ARCHIVE_FILE&gt;
mmarch <a href="#help"><strong>h</strong>elp</a>
</pre>

`<>`: required; `[]`: optional; `{a|b|c}`: required, choose one of them

For the first argument, use <code><strong>k</strong></code> for <code>compare</code>, <code><strong>df2n</strong></code> for <code>diff-files-to-nsis</code>, <code><strong>df2b</strong></code> for <code>diff-files-to-batch</code>, <code><strong>dak</strong></code> for <code>diff-add-keep</code>, otherwise, use the initial letter as a shortcut.

* Usage Notes: | [`FOLDER`](#notes-on-folder) | [`FILE_TO_XXXX_?`](#notes-on-file_to_xxxx_) | [Batch archive extraction](#batch-archive-extraction) | [Palette](#add-file-with-palette) arguments | [Other notes](#other-tips-and-notes)
* For developer: | [Work with batch, NSIS scripts](#work-with-batch-nsis-and-other-scripts) (to produce game patch or MOD installation files) | [Compilation](#compilation) | [Change Log](#change-log)

[Real-world example and tutorial: How to make a .exe MMMerge Update Patch](tutorial)

## `extract`

```
mmarch extract <ARCHIVE_FILE> <FOLDER> [FILE_TO_EXTRACT_1] [FILE_TO_EXTRACT_2] [...]
```

Extract (i.e. unpack) file(s) from the archive file (i.e. resource package file, typically .lod files).

Read "[§ Batch archive extraction](#batch-archive-extraction)" section to learn how to use wildcard for `<ARCHIVE_FILE>` in `mmarch extract` command to extract all archives in specified folder(s) with just one command.

If no `[FILE_TO_EXTRACT_?]` is specified, it will extract all files in the archive file.

Read "[§ Notes on `FILE_TO_XXXX_?`](#notes-on-file_to_xxxx_)" section for more important notes about `[FILE_TO_EXTRACT_?]`.

`<FOLDER>` is the path of the folder where the extracted file(s) will be placed.

Read "[§ Notes on `FOLDER`](#notes-on-folder)" section for more important notes about `<FOLDER>`.

**Examples:**

```
mmarch extract events.lod .
mmarch extract events.lod myfolder
mmarch extract events.lod "my folder/my subfolder"
mmarch extract events.lod myfolder items.txt OUT04.EVT
```

Batch archive extraction examples see: "[§ Batch archive extraction](#batch-archive-extraction)" section.

## `list`

```
mmarch list <ARCHIVE_FILE> [SEPARATOR]
```

List all file names in the archive file.

`[SEPARATOR]` is a string that separates the file names, use double quotes (`""`) to enclose the separator. By default (when `[SEPARATOR]` is not specified), windows newline (CRLF) will be used as the separator, which means it will output one file name per line.

**Examples:**

```
mmarch list events.lod
mmarch list events.lod "|"
```

## `add`

```
mmarch add <ARCHIVE_FILE> <FILE_TO_ADD_1> [FILE_TO_ADD_2] [...]
```

Add file(s) into the archive file.

If a file with the same name (case-insensitive) exists in the archive file, it will be replaced.

Read "[§ Notes on `FILE_TO_XXXX_?`](#notes-on-file_to_xxxx_)" section for more important notes about `FILE_TO_ADD_?`.

If you need to force using a palette for a `.bmp` file in a `mmspriteslod` or `mmbitmapslod` archive, read "[§ Add file with palette](#add-file-with-palette)" section for details.

**Example:**

```
mmarch add events.lod items.txt OUT04.EVT new.txt
```

## `delete`

```
mmarch delete <ARCHIVE_FILE> <FILE_TO_DELETE_1> [FILE_TO_DELETE_2] [...]
```

Delete file(s) from the archive file.

Read "[§ Notes on `FILE_TO_XXXX_?`](#notes-on-file_to_xxxx_)" section for more important notes about `FILE_TO_DELETE_?`.

**Example:**

```
mmarch delete events.lod items.txt OUT04.EVT
```

## `rename`

```
mmarch rename <ARCHIVE_FILE> <OLD_FILE_NAME> <NEW_FILE_NAME>
```

Rename a file in the archive file.

**Example:**

```
mmarch rename events.lod items.txt items_new.txt
```

## `create`

```
mmarch create <ARCHIVE_FILE> <ARCHIVE_FILE_TYPE> <FOLDER> [FILE_TO_ADD_1] [FILE_TO_ADD_2] [...]
```

Create a new archive file from scratch. It will be empty if no `[FILE_TO_ADD_?]` is specified.

Read "[§ Notes on `FILE_TO_XXXX_?`](#notes-on-file_to_xxxx_)" section for more important notes about `[FILE_TO_ADD_?]`.

`<FOLDER>` is the path of the folder where the new archive file will be placed.

Read "[§ Notes on `FOLDER`](#notes-on-folder)" section for more important notes about `<FOLDER>`.

`ARCHIVE_FILE_TYPE` can be one of the following:
* `h3lod`: Heroes 3 LOD archive (*.lod; *.pac)
* `h3snd`: Heroes 3 sound archive (*.snd)
* `mmsnd`: MM sound archive (*.snd)
* `h3mm78vid`: Heroes 3 or MM 7-8 video archive (*.vid)
* `mm6vid`: MM 6 video archive (*.vid)
* `mmbitmapslod`: MM bitmaps archive ([*.]bitmaps.lod; *.lod; *.lwd)
* `mmiconslod`: MM icons archive ([*.]icons.lod; *.lod)
* `mmspriteslod`: MM sprites archive ([*.]sprites.lod; *.lod)
* `mm8loclod`: MM 8 localization archive (*.T.lod; *.lod)
* `mm78gameslod`: MM 7-8 games archive ([*.]games.lod; *.lod)
* `mm6gameslod`: MM 6 games archive ([*.]games.lod; *.lod)
* `mm78save`: MM 7-8 saved game archive (*.lod; *.mm7; *.dod)
* `mm6save`: MM 6 saved game archive (*.mm6; *.lod)

Use correct file extension for your `<ARCHIVE_FILE>`, wrong extension will cause wrong file format even if `<ARCHIVE_FILE_TYPE>` is correct.

If you need to force using a palette for a `.bmp` file in a `mmspriteslod` or `mmbitmapslod` archive, read "[§ Add file with palette](#add-file-with-palette)" section for details.

**Example:**

```
mmarch create events_new.lod mmiconslod . items.txt OUT04.EVT new.txt
```

## `merge`

```
mmarch merge <ARCHIVE_FILE> <ARCHIVE_FILE_2>
```

Merge two archive files.

The first archive will change and the second will not. Resource files in the second archive, will be added into the first archive if they do not exist in the first archive, and will replace those in the first archive if files with same names exist in the first archive.

**Example:**

```
mmarch merge events.lod events2.lod
```

## `compare`

Compare two archive files, or two folders containing archive files and/or files of any other type.

`mmarch compare` and all related commands and features (incl. NSIS/batch script generation) work totally even if your folders do not contain any MM archive files at all. Therefore, you can use **mmarch** as a general file comparison and diff generation tool.

(`k` is short for `compare`)

The fourth parameter, `[OPTION]`, can be:

### not specified

```
mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2>
```

Print a comparison report. Legend:

![Comparison report legend](https://raw.githubusercontent.com/might-and-magic/mmarch/master/compare_legend.png "Comparison report legend")

**Example:**

```
mmarch compare events.lod new.events.lod
mmarch compare game_folder_old game_folder_new
```

### `nsis`

```
mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2> nsis <SCRIPT_FILE> <DIFF_FOLDER_NAME>
```

Generate a .nsi script file `SCRIPT_FILE` which can be compiled to a .exe patch installation file using [NSIS](https://nsis.sourceforge.io/). Diff files (same as with `filesonly` option) will be copied to a subfolder of `SCRIPT_FILE`'s folder, and the subfolder will be named with `DIFF_FOLDER_NAME` (it's a name, not a path). Read [§ NSIS-compiled patch installer](#nsis-compiled-patch-installer) for the following steps.

**Example:**

```
mmarch compare game_folder_old game_folder_new nsis nsis_folder/script.nsi files
```

### `batch`

```
mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2> batch <SCRIPT_FILE> <DIFF_FOLDER_NAME>
```

Generate a .bat (Window Batch) file `SCRIPT_FILE` which can work along with your DIFF_FOLDER and mmarch.exe. Diff files (same as with `filesonly` option) will be copied to a subfolder of `SCRIPT_FILE`'s folder, and the subfolder will be named with `DIFF_FOLDER_NAME` (it's a name, not a path). Read [§ Batch patch installer](#batch-patch-installer) for the following steps.

### `filesonly`

```
mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2> filesonly <DIFF_FOLDER>
```

Copy all diff files (i.e. non-resource file and extract in-archive resource files that are different, including added, modified or deleted. read [§ Details](#details-of-diff_folder-of-compare) if needed) in the two `ARCHIVE_FILE_OR_FOLDER`s, to `DIFF_FOLDER`.

Note that if `DIFF_FOLDER` exsits, it will perform a merger of old diff files and new diff files by cleaning up old diff files. Therefore, you can do: `mmarch compare VERSION_1 VERSION_2 filesonly diff_folder` and then `mmarch compare VERSION_2 VERSION_3 filesonly diff_folder`. It's OK to do VER1 -> VER2 then VER2 -> VER3, or VER1 -> VER3 then VER2 -> VER3. But VER1 -> VER2 then VER1 -> VER3 will cause problem ([image demo](tutorial/img/multi_version.png)). This merger (cleanup) is only performed in `filesonly` command, and not in `nsis` or `batch`.

## `diff-*`

There are also 3 special arguments related to `compare`, all predeced by `diff-*`:

### `diff-files-to-nsis`/`-batch`

```
mmarch diff-files-to-nsis <OLD_DIFF_FOLDER> <SCRIPT_FILE> <DIFF_FOLDER_NAME>
OR
mmarch diff-files-to-batch <OLD_DIFF_FOLDER> <SCRIPT_FILE> <DIFF_FOLDER_NAME>
```

(`df2n` is short for `diff-files-to-nsis`; `df2b` is short for `diff-files-to-batch`)

The former command generates a .nsi script file, while the later command generates a .bat (Window Batch) file `SCRIPT_FILE`, according to the files in `[OLD_DIFF_FOLDER]` that you get using `filesonly` option of `mmarch compare`. `[OLD_DIFF_FOLDER]` will then be moved to `SCRIPT_FILE`'s folder (becoming its subfolder) and renamed with `DIFF_FOLDER_NAME`.

**Examples:**

If `diff_folder_temp/` is empty, the aforementioned

```
mmarch compare game_folder_old game_folder_new nsis nsis_folder/script.nsi files
```

has the same effect as the two following commands combined

```
mmarch compare game_folder_old game_folder_new filesonly diff_folder_temp
mmarch diff-files-to-nsis diff_folder_temp nsis_folder/script.nsi files
```

### `diff-add-keep`

```
mmarch diff-add-keep <DIFF_FOLDER>
```

(`dak` is short for `diff-add-keep`)

This is for developers using Git. It will add an empty file `.mmarchkeep` to every empty folder in `DIFF_FOLDER`, so that Git can keep the empty folders tracked. `diff-files-to-nsis`/`-batch` ignore `.mmarchkeep` files.

## `optimize`

```
mmarch optimize <ARCHIVE_FILE>
```

Optimize an archive file.

Note that when **mmarch** outputs an archive file (with `mmarch {add|delete|rename|create|merge}` commands), it has already been optimized and you don't need to do it again.

**Example:**

```
mmarch optimize events.lod
```

## `help`

```
mmarch help
```

Display help information.

## Notes on `FOLDER`

The "Notes on `FOLDER`" applys to the argument representing a **folder path**  in <code>mmarch <a href="#extract">extract</a></code> and <code>mmarch <a href="#create">create</a></code>.

* Folder path cannot be empty when it is required
* If folder path contains space (` `), use double quotes (`""`) to enclose the folder path
* Path without a leading slash, or with a leading `./`: **relative path**
  * `.`: **current directory**
  * `..`: parent directory of the current directory (use with **CAUTION!**, not expected to work for `compare`)
* A leading slash `/`: **absolute path** (the root being `C:\` or `D:\` or ...) (use with **CAUTION!**)
* A trailing slash is optional. Same effect with or without it.
* Slash (`/`) and backslash (`\`) have the same effect.

## Notes on `FILE_TO_XXXX_?`

The "Notes on `FILE_TO_XXXX_?`" applys to the argument representing a **file path** in <code>mmarch <a href="#extract">extract</a></code>, <code>mmarch <a href="#add">add</a></code>, <code>mmarch <a href="#delete">delete</a></code> and <code>mmarch <a href="#create">create</a></code>.

* `*` or `*.*`: **all the files**
* `*.EXT` (e.g. `*.txt`): all the files with the specified **extension**
* `*.`: all the files **without extension**
* You can add directory path before the aforementioned wildcard character. Similar rules for folder path apply to the file path (incl. the double quotes, relative and absolute path, slash usage)

## Batch archive extraction

You can use wildcard for `<ARCHIVE_FILE>` in `mmarch extract` command to extract all archives in specified folder(s) with just one command.

* File path in `<ARCHIVE_FILE>`:
  * `**`:  zero or more directories (i.e. the current directory and all its non-hidden subdirectories, **recursively**)
  * `*`: any **ONE** directory
  * Read the section "[§ Notes on `FOLDER`](#notes-on-folder)" above for **relative path** and **absolute path** (absolute path is **NOT recommended at all!**)
* File name at the end of `<ARCHIVE_FILE>`:
  * `*` or `*.*`: all the **supported** archive files (`.lod`, `.pac`, `.snd`, `.vid`, `.lwd`, `.mm7`, `.dod` & `.mm6`)
  * `*.EXT` (e.g. `*.lod`): all the **supported** archive files with the specified **extension** (`a.bitmaps.lod`'s extension is `.lod`, not `.bitmaps.lod`)
  * `"*.EXT1|EXT2|EXT3..."` (e.g. `"*.lod|lwd|vid"`): all the supported archive files with any of the specified extensions, note that it has to be enclosed by double quotes

You might need to wait a few minutes if you are trying to extract all the archive files from a whole game.

**Examples:**

```
mmarch extract **/* resource_folder
```

The command above can extract all the resource files in all the supported archive files in current directory and its subdirectories. The resources will be placed in an auto-named (e.g. `icons.lod.mmarchive` for `icons.lod`) subdirectory (or sub-subdirectory depending on whether the archive is in a subdirectory) in `resource_folder/`.

```
mmarch extract data/*.lod . *.txt
```

The command above can extract all `.txt` resource files in all `.lod` archive files in `data/` directory. The resources will be placed in an auto-named subdirectory in current directory (current directory = `.`).

```
mmarch extract "*/*.lod|lwd" ../resource_folder
```

The command above can extract all the resource files in all `.lod` and `.lwd` archive files in any first level subdirectories of the current directory. The resources will be placed in an auto-named subdirectory in `resource_folder/` that belongs to the parent directory of the current directory.

## Add file with palette

```
mmarch add <ARCHIVE_FILE> <FILE_TO_ADD_1> [/p PALETTE_INDEX_1] [FILE_TO_ADD_2] [/p PALETTE_INDEX_2] [...]
```

```
mmarch create <ARCHIVE_FILE> <ARCHIVE_FILE_TYPE> <FOLDER> [FILE_TO_ADD_1] [/p PALETTE_INDEX_1] [FILE_TO_ADD_2] [/p PALETTE_INDEX_2] [...]
```

If you are adding a `.bmp` file into a MM sprites archive (`mmspriteslod`) or MM bitmaps archive (`mmbitmapslod`) file, then you can **optionally** add `/p PALETTE_INDEX` right after the `.bmp` file in your command, in order to specify a palette stored in any `[*.]bitmaps.lod` archive file (including your target archive file itself if it is a `[*.]bitmaps.lod`) in the same directory. A palette can be extracted as a `pal<three_digit_palette_index>.act` file (e.g. `pal023.act`).

If there is no `/p PALETTE_INDEX` after the `.bmp` file in your command, then the program will try to, from every `[*.]bitmaps.lod` archive file, automatically find a palette that matches your `.bmp` file's color table (palette). If you do specify a palette index, the program will not check if your palette exists in the `[*.]bitmaps.lod`.

Do not pad 0 to palette index. `pal023.act`'s palette index is 23.

**Examples:**

```
mmarch add sprites.lod mymonster01.bmp /p 23 mymonster02.bmp /p 558
mmarch create myfiles.sprites.lod mmspriteslod . mymonster01.bmp /p 23 mymonster02.bmp /p 558
```

## Other tips and notes

Less important tips and notes include:

### Use initial letter for the first argument

For the first argument, the initial letter of <code><strong>e</strong>xtract</code>, <code><strong>l</strong>ist</code>, <code><strong>a</strong>dd</code>, <code><strong>d</strong>elete</code>, <code><strong>r</strong>ename</code>, <code><strong>c</strong>reate</code>, <code><strong>m</strong>erge</code>, <code><strong>o</strong>ptimize</code>, <code><strong>h</strong>elp</code> can be used instead (e.g. <code>mmarch <strong>e</strong> events.lod myfolder</code>); use <code><strong>k</strong></code> for <code>compare</code>, <code><strong>df2n</strong></code> for <code>diff-files-to-nsis</code>, <code><strong>df2b</strong></code> for <code>diff-files-to-batch</code>, <code><strong>dak</strong></code> for <code>diff-add-keep</code>; they can be optionally preceded by a leading `-` or `--` which will do the same job.

### Paths are case-insensitive

File names and paths are case-insensitive.

### Backup please

The tool changes or overrides original archive or unpacked resource files permanently, you should consider copying them to other directory or with other names to make backups (e.g. `copy a.lod a.backup.lod`).

### File will be skipped if it fails

If the program encounters an error when extracting, adding or deleting a resource file from archive file(s), this resource file will be skipped and the rest will still be processed.

### In-archive and extracted extension difference

For some archive format, some files have different file extensions in the archive and as extracted files out of the archive. Don't wrong, you can use either extension to refer to the file. Below is the list (same extension is used if not listed):

| Archive Format | In-Archive Ext | Extracted Ext |
|----------------|----------------|---------------|
| `h3lod`        | `pcx`          | `bmp`         |
| `h3snd`        | No Extension   | `wav`         |
| `mmsnd`        | No Extension   | `wav`         |
| `mm6vid`       | No Extension   | `smk`         |
| `mmbitmapslod` | No Extension   | `bmp`         |
| `mmbitmapslod` | No Extension   | `act`         |
| `mmiconslod`   | No Extension   | `bmp`         |
| `mmiconslod`   | No Extension   | `act`         |
| `mmspriteslod` | No Extension   | `bmp`         |
| `mm8loclod`    | No Extension   | `bmp`         |

### Sprites with incorrect palette

Official Might and Magic VI and VII has some sprites with incorrect palette:

* MM6's bat (yes, the monster that allegedly caused the coronavirus pandemic) images, stored in data/SPRITES.LOD as `BAT****` files, have incorrect palette: their palette should be 156 instead of 422 (pal422 exists in BITMAPS.LOD but is unrelated). However, it seems neither 422 nor 156 is correct for some of the bat bitmaps, both mmarch and MMArchive can't retrieve their correct palette.
* MM7's "swptree" images, stored in data/SPRITES.LOD as `swptree*` files, have incorrect palette: their palette should be 120 instead of 940 (pal940 doesn't exist in BITMAPS.LOD at all).

**mmarch** will not fix their problem and will skip these sprite bitmaps (though GrayFace's MMArchive can fix them). However, you may find these sprites bitmap files well extracted with correct palette in [`fixedsprites.zip` in the repo](https://github.com/might-and-magic/mmarch/blob/master/fixedsprites.zip).

### Details of `DIFF_FOLDER` of `compare`

* `DIFF_FOLDER` of `mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2> {filesonly|nsis|batch}`:
  * if a file is not present in old ARCHIVE_FILE_OR_FOLDER and is added in the new ARCHIVE_FILE_OR_FOLDER, then it will be copied to `DIFF_FOLDER`
  * if a file is present in old ARCHIVE_FILE_OR_FOLDER and is deleted in the new ARCHIVE_FILE_OR_FOLDER, then an **empty file named `FILENAME.todelete`** will be put into `DIFF_FOLDER`
  * if a non-MM Archive file is present in old ARCHIVE_FILE_OR_FOLDER and is modified in the new ARCHIVE_FILE_OR_FOLDER, then it will be copied to `DIFF_FOLDER`
  * if an MM Archive file is present in old ARCHIVE_FILE_OR_FOLDER and is modified in the new ARCHIVE_FILE_OR_FOLDER, then a **folder named `FILENAME.mmarchive`** will be created and:
    * if a resource file is not present in old ARCHIVE_FILE and is added in the new ARCHIVE_FILE, then it will be copied into `FILENAME.mmarchive` folder
    * if a resource file is present in old ARCHIVE_FILE and is deleted in the new ARCHIVE_FILE, then an empty file named `FILENAME.todelete` will be put into `FILENAME.mmarchive` folder
    * if a resource file is present in old ARCHIVE_FILE and is modified in the new ARCHIVE_FILE, then it will be copied to `FILENAME.mmarchive` folder

## Work with batch, NSIS and other scripts

**mmarch** can be used with [batch file](https://en.wikibooks.org/wiki/Windows_Batch_Scripting) (.bat) or [NSIS script](https://nsis.sourceforge.io/Main_Page) to produce game patch or MOD installation files. Also, with [Python](https://www.python.org/), [JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript), batch files, [PowerShell](https://docs.microsoft.com/en-us/powershell/) script, etc., **mmarch** can automatize the workflow of the development of Heroes of Might and Magic 3 and Might and Magic 6, 7, 8 MODs and patches.

In case you need it, Smacker (.smk) and Bink (.bik) video file formats' original developer's official [The RAD Video Tools](http://www.radgametools.com/bnkdown.htm) can be used to extract and replace sound from .bik and .smk videos. This is essential for game video localization.

No matter you have a Might and Magic project (with MM archive files) or non MM projects (without MM archive files), you can read the following sections and learn how to use **mmarch** to easily compare old and new folders, in order to make a patch.

### NSIS-compiled patch installer

[`mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2> nsis <SCRIPT_FILE> <DIFF_FOLDER_NAME>`](#nsis) will compare the two folders, copy all different files (including in-archive resource files) to `DIFF_FOLDER_NAME` (which is a subfolder of `SCRIPT_FILE`'s folder) and generate a .nsi script file `SCRIPT_FILE`. You may then:

* Modify the .nsi file if needed
* Put mmarch.exe in the folder of the .nsi file
* Compile the .nsi file to a .exe patch setup file with the latest NSIS 3

### Batch patch installer

[`mmarch compare <ARCHIVE_FILE_OR_FOLDER> <ARCHIVE_FILE_OR_FOLDER_2> batch <SCRIPT_FILE> <DIFF_FOLDER_NAME>`](#batch) will compare the two folders, copy all different files (including in-archive resource files) to `DIFF_FOLDER_NAME` (which is a subfolder of `SCRIPT_FILE`'s folder) and generate a .bat (Window Batch) file `SCRIPT_FILE`. You may then:

* Modify the .bat file if needed
* Put mmarch.exe in the folder of the .bat file
* Compress them into a .zip
* Distribute the .zip to users

A user may:

* Unzip it
* Put everything in the game folder
* Double click .bat file to patch the game

The batch file will not perform a self-deletion, users have to delete .bat, mmarch.exe and `DIFF_FOLDER` manually.

## Compilation

How to compile the source of **mmarch**:

* `git clone` or download **mmarch**'s source
* `git clone` or download [GrayFace/Misc](https://github.com/GrayFace/Misc)
* Copy or move RSPak/ folder from GrayFace/Misc project into mmarch's source folder
* Open "mmarch.bdsproj" file with Borland Developer Studio 2006 or Delphi 10 (it may or may not work with newer version Borland, see [GrayFace's note](https://github.com/GrayFace/Misc))
* Compile

## Change Log
* [2020-03-11] v1.0: initial release
* [2020-03-18] v2.0: support palette; support `*.EXT` and batch archive extraction; deal with in-archive & extracted file extension differences and the "cannot find the path specified" problem caused by it
* [2020-03-31] v3.0: add `compare` method that can compare two dir and generate NSIS/Batch installer; tutorial
* [2020-04-02] v3.1: diff-files-to-* instead of compare-files-to-*; diff-add-keep; fix problem moving to subfolder
* [2020-04-22] v3.2: minor fix: diff-files-to-* do not work when old and new diff folders are the same

## License

[MIT](https://github.com/might-and-magic/mmarch/blob/master/LICENSE)
