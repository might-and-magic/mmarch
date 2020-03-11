# mmarch

Command line tool to handle (extract, replace resources and more) Heroes of Might and Magic 3 and Might and Magic 6, 7, 8 resource archive files, typically lod files.

Based on [GrayFace's MMArchive](https://grayface.github.io/mm/#MMArchive) ([source](https://github.com/GrayFace/Misc/)) (mmarch is actually kind of a wrapper of MMArchive). If you need a graphical user interface tool, use MMArchive.

## Usage

<pre>
mmarch {<a href="#extract">extract</a>|<a href="#list">list</a>|<a href="#add">add</a>|<a href="#delete">delete</a>|<a href="#rename">rename</a>|<a href="#create">create</a>|<a href="#merge">merge</a>|<a href="#optimize">optimize</a>|<a href="#help">help</a>} &lt;archive_file&gt; [other_arguments]
</pre>

`<>`: required; `[]`: optional; `{a|b|c}`: required, choose one of them

### extract

```
mmarch extract <archive_file> <folder> [file_to_extract_1] [file_to_extract_2] [...]
```

Extract (i.e. unpack) file(s) from the archive file (i.e. resource package file, typically .lod files).

If no `[file_to_extract_?]` is specified, it will extract all files in the archive file.

`<folder>` is the path of the folder where the extracted file(s) will be placed.

**Notes for `<folder>`**:
* If file name contains space (` `), use double quote (`""`) to enclose the file name
* Path without a leading slash, or with a leading `./`: **relative path**
  * `.`: **current directory**
* A leading slash `/`: **absolute path** (the root being `C:\` or `D:\` or ...)
  * Empty string `""`: the root (`C:\` or `D:\` or ...)
* A trailing slash is optional. Same effect with or without it.
* Slash (`/`) and backslash (`\`) have the same effect.

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

### list

```
mmarch list <archive_file> [separator]
```

List all file names in the archive file.

`[separator]` is a string that separates the file names, use double quote (`""`) to enclose the separator. By default (when `[separator]` is not specified), windows newline (CRLF) will be used as the separator, which means it will output one file name per line.

**Example:**

```
mmarch list events.lod
```

```
mmarch list events.lod "|"
```

### add

```
mmarch add <archive_file> <file_to_add_1> [file_to_add_2] [...]
```

Add file(s) into the archive file.

If a file with the same name (case-insensitive) exists in the archive file, it will be replaced.

**Example:**

```
mmarch add events.lod items.txt OUT04.EVT new.txt
```

### delete

```
mmarch delete <archive_file> <file_to_delete_1> [file_to_delete_2] [...]
```

Delete file(s) from the archive file.

**Example:**

```
mmarch delete events.lod items.txt OUT04.EVT
```

### rename

```
mmarch rename <archive_file> <old_file_name> <new_file_name>
```

Rename a file in the archive file.

**Example:**

```
mmarch rename events.lod items.txt items_new.txt
```

### create

```
mmarch create <archive_file> <archive_file_type> <folder> [file_to_add_1] [file_to_add_2] [...]
```

Create new archive file. It will be empty if no file to add is specified.

`<folder>` is the path of the folder where the new archive file will be placed.

`archive_file_type` can be one of the following:
* `h3lod`: Heroes 3 LOD archive (*.lod; *.pac)
* `h3snd`: Heroes 3 sound archive (*.snd)
* `mmsnd`: MM sound archive (*.snd)
* `h3mm78vid`: Heroes 3 or MM 7-8 video archive (*.vid)
* `mm6vid`: MM 6 video archive (*.vid)
* `mmbitmapslod`: MM bitmaps archive (*.lod; *.lwd)
* `mmiconslod`: MM icons archive (*.lod)
* `mmspriteslod`: MM sprites archive (*.lod)
* `mm8loclod`: MM 8 localization archive (*.lod)
* `mm78gameslod`: MM 7-8 games archive (*.lod)
* `mm6gameslod`: MM 6 games archive (*.lod)
* `mm78save`: MM 7-8 saved game archive (*.lod; *.mm7; *.dod)
* `mm6save`: MM 6 saved game archive (*.lod; *.mm6)

**Example:**

```
mmarch create events_new.lod mmiconslod . items.txt OUT04.EVT new.txt
```

### merge

```
mmarch merge <archive_file> <archive_file_2>
```

Merge two archive files.

The first archive will change and the second will not. Resource files in the second archive, will be added into the first archive if they do not exist in the first archive, and will replace those in the first archive if files with same names exist in the first archive.

**Example:**

```
mmarch merge events.lod events2.lod
```

### optimize

```
mmarch optimize <archive_file>
```

Optimize an archive file.

Note that when mmarch outputs an archive file (in `mmarch {add|delete|rename|create|merge}` commands), it has already been optimized and you don't need to do it again.

**Example:**

```
mmarch optimize events.lod
```

### help

```
mmarch help
```

Display help information.

### Other tips and notes

For the first argument, the initial letter of <code><strong>e</strong>xtract</code>, <code><strong>l</strong>ist</code>, <code><strong>a</strong>dd</code>, <code><strong>d</strong>elete</code>, <code><strong>r</strong>ename</code>, <code><strong>c</strong>reate</code>, <code><strong>m</strong>erge</code>, <code><strong>o</strong>ptimize</code>, <code><strong>h</strong>elp</code> can be used instead of them; they can be optionally preceded by a leading `-` or `--` which will do the same job.

File names and paths are case-insensitive.

The tool changes or replaces original archive or unpacked resource files permanently, you should consider copying them to other directory or with other names to make backups (e.g. `copy a.lod a.backup.lod`).

## Work with batch, NSIS and other script

`mmarch` can be used with [batch file](https://en.wikibooks.org/wiki/Windows_Batch_Scripting) (.bat) or [NSIS script](https://nsis.sourceforge.io/Main_Page) to produce game patch or MOD installation files. Also, with [Python](https://www.python.org/), [JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript), batch files, [PowerShell](https://docs.microsoft.com/en-us/powershell/) script, etc., `mmarch` can automatize the workflow of the development of Heroes of Might and Magic 3 and Might and Magic 6, 7, 8 MODs and patches.

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

## Limits

`mmarch` can't handle palette.

Actually among all those formats supported by MMArchive, I have only tested `.lod`.

## Compilation

How to compile the source of `mmarch`:

* `git clone` or download mmarch's source
* `git clone` or download [GrayFace/Misc](https://github.com/GrayFace/Misc)
* Copy or move RSPak/ folder from GrayFace/Misc project into mmarch/ source folder
* Open "mmarch.bdsproj" file with Borland Developer Studio 2006 or Delphi 10 (it may or may not work with newer version Borland, see [GrayFace's note](https://github.com/GrayFace/Misc))
* Compile

## License

[MIT](https://github.com/might-and-magic/mmarch/blob/master/LICENSE.md)




## To do
* extract  : done. to be retested
* list     : done. to be retested
* add      : done. to be retested
* delete   : done. to be retested
* rename   : done. to be retested
* create   : 
* merge    : done. to be retested
* optimize : done. to be retested
* help     : done. to be retested






	// to do: check if method = `fvsrb`, methodNumber = ?
	再试一下merge的顺序