# mmarc

Command line tool to handle (extract, replace, pack, etc.) Heroes of Might and Magic 3 and Might and Magic 6, 7, 8 ressources from the games' files, typically lod files.

Based on [GrayFace's MMArchive](https://grayface.github.io/mm/#MMArchive) ([source](https://github.com/GrayFace/Misc/)) (mmarc is actually kind of a wrapper of MMArchive). If you need a graphical user interface tool, use MMArchive.

## Usage

<pre>
mmarc {<a href="#extract">extract</a>|<a href="#list">list</a>|<a href="#add">add</a>|<a href="#delete">delete</a>|<a href="#rename">rename</a>|<a href="#create">create</a>|<a href="#merge">merge</a>|<a href="#optimize">optimize</a>|<a href="#help">help</a>} &lt;archive_file&gt; [other_arguments]
</pre>

`<>`: required; `[]`: optional; `{a|b|c}`: required, choose one of them

### `extract`

```
mmarc extract <archive_file> <folder> [file_to_extract_1] [file_to_extract_2] [...]
```

Extract file(s) from the archive file.

If no file to extract is specified, it will extract all files in the archive file.

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
mmarc extract events.lod .
```

```
mmarc extract events.lod myfolder
```

```
mmarc extract events.lod "my folder/my subfolder"
```

```
mmarc extract events.lod myfolder items.txt OUT04.EVT
```

### `list`

```
mmarc list <archive_file>
```

List all file names in the archive file.

**Example:**

```
mmarc list events.lod
```

### `add`

```
mmarc add <archive_file> <file_to_add_1> [file_to_add_2] [...]
```

Add file(s) into the archive file.

If a file with the same name (case-insensitive) exists in the archive file, it will be replaced.

**Example:**

```
mmarc add events.lod items.txt OUT04.EVT new.txt
```

### `delete`

```
mmarc delete <archive_file> <file_to_delete_1> [file_to_delete_2] [...]
```

Delete file(s) from the archive file.

**Example:**

```
mmarc delete events.lod items.txt OUT04.EVT
```

### `rename`

```
mmarc rename <archive_file> <old_file_name> <new_file_name>
```

Rename a file in the archive file.

**Example:**

```
mmarc rename events.lod items.txt items_new.txt
```

### `create`

```
mmarc create <archive_file> <archive_file_type> <folder> [file_to_add_1] [file_to_add_2] [...]
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
mmarc create events_new.lod mmiconslod . items.txt OUT04.EVT new.txt
```

### `merge`

```
mmarc merge <archive_file> <archive_file_2>
```

Merge two archive files.

**Example:**

```
mmarc merge events.lod events2.lod
```

### `optimize`

```
mmarc optimize <archive_file>
```

Optimize an archive file.

Note that when mmarc outputs an archive file (in `mmarc {add|delete|rename|create|merge}` commands), it has already been optimized and you don't need to do it again.

**Example:**

```
mmarc optimize events.lod
```

### `help`

```
mmarc help
```

Display help information.

### Other tips and notes

For the first argument, the initial letter of <code><strong>e</strong>xtract</code>, <code><strong>l</strong>ist</code>, <code><strong>a</strong>dd</code>, <code><strong>d</strong>elete</code>, <code><strong>r</strong>ename</code>, <code><strong>c</strong>reate</code>, <code><strong>m</strong>erge</code>, <code><strong>o</strong>ptimize</code>, <code><strong>h</strong>elp</code> can be used instead of them; they can be optionally preceded by a leading `-` or `--` which will do the same job.

File names and paths are case-insensitive.

## License

[MIT](https://github.com/might-and-magic/mmarc/blob/master/LICENSE.md)

## How to compile the source

For developers:

* `git clone` or download mmarc's source
* `git clone` or download [GrayFace/Misc](https://github.com/GrayFace/Misc)
* Copy or move RSPak/ folder from GrayFace/Misc project into mmarc/ source folder
* Open "mmarc.bdsproj" file with Borland Developer Studio 2006 or Delphi 10 (it may or may not work with newer version Borland, see [GrayFace's note](https://github.com/GrayFace/Misc))
* Compile
