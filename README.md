# [mmarch](https://github.com/might-and-magic/mmarch)

Command line tool to handle (extract, replace resources and more) Heroes of Might and Magic 3 and Might and Magic 6, 7, 8 resource archive files (e.g. lod files).

[Download mmarch v2.0](https://github.com/might-and-magic/mmarch/releases/download/v2.0/mmarch.zip)

Based on [GrayFace's MMArchive](https://grayface.github.io/mm/#MMArchive) ([source](https://github.com/GrayFace/Misc/)) (mmarch is actually kind of a wrapper of MMArchive). If you need a graphical user interface tool, use MMArchive.

## Usage Summary & Table of Contents

<pre>
mmarch {<a href="#extract">extract</a>|<a href="#list">list</a>|<a href="#add">add</a>|<a href="#delete">delete</a>|<a href="#rename">rename</a>|<a href="#create">create</a>|<a href="#merge">merge</a>|<a href="#optimize">optimize</a>|<a href="#help">help</a>} &lt;ARCHIVE_FILE&gt; [OTHER_ARGUMENTS]
</pre>

`<>`: required; `[]`: optional; `{a|b|c}`: required, choose one of them

* Usage Notes: [`FOLDER`](#notes-on-folder), [`FILE_TO_XXXX_?`](#notes-on-file_to_xxxx_) and [Palette](#add-file-with-palette) arguments; [Other notes](#other-tips-and-notes)
* For developer: [Work with batch, NSIS scripts](#work-with-batch-nsis-and-other-scripts) (to produce game patch or MOD installation files); [Compilation](#compilation); [Change Log](#change-log)

## extract

```
mmarch extract <ARCHIVE_FILE> <FOLDER> [FILE_TO_EXTRACT_1] [FILE_TO_EXTRACT_2] [...]
```

Extract (i.e. unpack) file(s) from the archive file (i.e. resource package file, typically .lod files).

If no `[FILE_TO_EXTRACT_?]` is specified, it will extract all files in the archive file.

Read "[§ Notes on `FILE_TO_XXXX_?`](#notes-on-file_to_xxxx_)" section for more important notes about `[FILE_TO_EXTRACT_?]`.

`<FOLDER>` is the path of the folder where the extracted file(s) will be placed.

Read "[§ Notes on `FOLDER`](#notes-on-folder)" section for more important notes about `<FOLDER>`.

**Examples:**

```
mmarch extract events.lod .
```

```
mmarch extract events.lod myfolder
```

```
mmarch extract events.lod "my folder/my subfolder"
```

```
mmarch extract events.lod myfolder items.txt OUT04.EVT
```

## list

```
mmarch list <ARCHIVE_FILE> [SEPARATOR]
```

List all file names in the archive file.

`[SEPARATOR]` is a string that separates the file names, use double quote (`""`) to enclose the separator. By default (when `[SEPARATOR]` is not specified), windows newline (CRLF) will be used as the separator, which means it will output one file name per line.

**Examples:**

```
mmarch list events.lod
```

```
mmarch list events.lod "|"
```

## add

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

## delete

```
mmarch delete <ARCHIVE_FILE> <FILE_TO_DELETE_1> [FILE_TO_DELETE_2] [...]
```

Delete file(s) from the archive file.

Read "[§ Notes on `FILE_TO_XXXX_?`](#notes-on-file_to_xxxx_)" section for more important notes about `FILE_TO_DELETE_?`.

**Example:**

```
mmarch delete events.lod items.txt OUT04.EVT
```

## rename

```
mmarch rename <ARCHIVE_FILE> <OLD_FILE_NAME> <NEW_FILE_NAME>
```

Rename a file in the archive file.

**Example:**

```
mmarch rename events.lod items.txt items_new.txt
```

## create

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

## merge

```
mmarch merge <ARCHIVE_FILE> <ARCHIVE_FILE_2>
```

Merge two archive files.

The first archive will change and the second will not. Resource files in the second archive, will be added into the first archive if they do not exist in the first archive, and will replace those in the first archive if files with same names exist in the first archive.

**Example:**

```
mmarch merge events.lod events2.lod
```

## optimize

```
mmarch optimize <ARCHIVE_FILE>
```

Optimize an archive file.

Note that when **mmarch** outputs an archive file (with `mmarch {add|delete|rename|create|merge}` commands), it has already been optimized and you don't need to do it again.

**Example:**

```
mmarch optimize events.lod
```

## help

```
mmarch help
```

Display help information.

## Notes on `FOLDER`

The "Notes on `FOLDER`" applys to the argument representing a **folder path**  in <code>mmarch <a href="#extract">extract</a></code> and <code>mmarch <a href="#create">create</a></code>.

* If folder path contains space (` `), use double quote (`""`) to enclose the folder path
* Path without a leading slash, or with a leading `./`: **relative path**
  * `.`: **current directory**
* A leading slash `/`: **absolute path** (the root being `C:\` or `D:\` or ...)
  * Empty string `""`: the root (`C:\` or `D:\` or ...)
* A trailing slash is optional. Same effect with or without it.
* Slash (`/`) and backslash (`\`) have the same effect.

## Notes on `FILE_TO_XXXX_?`

The "Notes on `FILE_TO_XXXX_?`" applys to the argument representing a **file path** in <code>mmarch <a href="#extract">extract</a></code>, <code>mmarch <a href="#add">add</a></code>, <code>mmarch <a href="#delete">delete</a></code> and <code>mmarch <a href="#create">create</a></code>.

* `*` and `*.*`: **all the files**
* `*.EXT` (e.g. `*.txt`): all the files with the specified **extension**
* `*.`: all the files **without extension**
* You can add directory path before the aforementioned wildcard character. Similar rules for folder path apply to the file path (incl. the double quote, relative and absolute path, slash usage)

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
```

```
mmarch create myfiles.sprites.lod mmspriteslod . mymonster01.bmp /p 23 mymonster02.bmp /p 558
```

## Other tips and notes

For the first argument, the initial letter of <code><strong>e</strong>xtract</code>, <code><strong>l</strong>ist</code>, <code><strong>a</strong>dd</code>, <code><strong>d</strong>elete</code>, <code><strong>r</strong>ename</code>, <code><strong>c</strong>reate</code>, <code><strong>m</strong>erge</code>, <code><strong>o</strong>ptimize</code>, <code><strong>h</strong>elp</code> can be used instead of them; they can be optionally preceded by a leading `-` or `--` which will do the same job.

File names and paths are case-insensitive.

The tool changes or overrides original archive or unpacked resource files permanently, you should consider copying them to other directory or with other names to make backups (e.g. `copy a.lod a.backup.lod`).

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

## Work with batch, NSIS and other scripts

**mmarch** can be used with [batch file](https://en.wikibooks.org/wiki/Windows_Batch_Scripting) (.bat) or [NSIS script](https://nsis.sourceforge.io/Main_Page) to produce game patch or MOD installation files. Also, with [Python](https://www.python.org/), [JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript), batch files, [PowerShell](https://docs.microsoft.com/en-us/powershell/) script, etc., **mmarch** can automatize the workflow of the development of Heroes of Might and Magic 3 and Might and Magic 6, 7, 8 MODs and patches.

Other useful tools that can be used by MM MOD/patch developers include Smacker (.smk) and Bink (.bik) video file formats' original developer's official [The RAD Video Tools](http://www.radgametools.com/bnkdown.htm) (to extract and replace sound from .bik and .smk videos. Essential tool for game video localization), hash tools ([certutil](https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/certutil), [NSIS Crypto plug-in](https://nsis.sourceforge.io/Crypto_plug-in), ...), etc.

### Batch file

Simple demostration of some non-straightforward, advanced usages of batch file:

Save the resource list (one file name per line) in an archive as a txt file:

```
mmarch list events.lod> events_temp_list.txt
```

Save the resource list in an archive as a txt file, with `|` as leading, trailing character and separators. Then search to see if `D17.STR` file exists in the archive:

```
@echo|set /p="|"> events_temp_list.txt
mmarch list events.loD "|">> events_temp_list.txt
@echo|set /p="|">> events_temp_list.txt

findstr "|D17.STR|" events_temp_list.txt
IF %errorlevel% == 0 (
	echo Found!
) ELSE (
	echo Not found!
)
```

Extract all resource files in all archive files in current folder, resource file will be placed in a subfolder with the same name of its archive file without extension:

```
for %i in (*) do ( mmarch extract "%~nxi" "%~ni" )
```

## Compilation

How to compile the source of **mmarch**:

* `git clone` or download **mmarch**'s source
* `git clone` or download [GrayFace/Misc](https://github.com/GrayFace/Misc)
* Copy or move RSPak/ folder from GrayFace/Misc project into mmarch/ source folder
* Open "mmarch.bdsproj" file with Borland Developer Studio 2006 or Delphi 10 (it may or may not work with newer version Borland, see [GrayFace's note](https://github.com/GrayFace/Misc))
* Compile

## Change Log
* [2020-03-11] v1.0: initial release
* [2020-03-15] v2.0: support palette; support `*.EXT`; deal with in-archive & extracted file extension differences and the "cannot find the path specified" problem caused by it

## License

[MIT](https://github.com/might-and-magic/mmarch/blob/master/LICENSE.md)
