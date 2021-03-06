# Tutorial of mmarch: How to make a .exe MMMerge Update Patch

This tutorial shows how to make a .exe Update Patch for MMMerge (Might and Magic 878 Merge Mod) with **mmarch** and NSIS.

(You can go to [MMMerge Update Patch Project](https://github.com/might-and-magic/mmmerge-update-patch) to download the made patch)

## Two-version diff patch

### Step 1: Preparation

Download and install [the latest (Version 3) NSIS](https://nsis.sourceforge.io/Download).

You have MMMerge Version 2019-09-22 and Version 2020-03-29 in `mmmerge-2019-09-22/` and `mmmerge-2020-03-29/` folders respectively, you want to make a .exe patch setup file that can update the former version to the later version.

Copy `mmarch.exe` to the folder.

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/1-1.png "")

`Win + R` to open "Run", then type in `cmd` and tap `Enter` to open "Windows Command Prompt".

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/1-2.png "")

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/1-3.png "")

`cd <DIRECTORY>` (e.g. `cd C:\Users\Chen\Desktop\mm`) and tap `Enter`, to go to your working directory (folder).

### Step 2: Script file and patch generation

#### Step 2.1.

Use command

<pre>
mmarch <a href="https://github.com/might-and-magic/mmarch#compare">compare</a> mmmerge-2019-09-22 mmmerge-2020-03-29 nsis nsis_folder/script.nsi files
</pre>

to generate diff and NSIS script files in `nsis_folder/`.

#### Step 2.2.

Use Windows File Explorer GUI, **OR** use command

```
copy mmarch.exe nsis_folder
```

to copy `mmarch.exe` to `nsis_folder/` folder.

#### Step 2.3.

Use MakeNSISW GUI as the screenshot shows

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/1-2-3.png "")

**OR** use command

```
"C:\Program Files (x86)\NSIS\makensis" nsis_folder/script.nsi
```

to compile `nsis_folder/script.nsi` to `patch.exe` with NSIS.

Full command line screenshot:

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/cl1.png "")

## Three-or-more-version diff patch

### Step 1: Preparation

You have MMMerge Version 2019-09-22, 2019-10-08, 2020-03-17 and 2020-03-29 in their respective folders, you want to make a .exe patch setup file that can update any of the three old versions to the latest version.

(Same as in [Step 1 of "Two-version diff patch"](#step-1-preparation)) You have installed NSIS, copied `mmarch.exe` to the folder, opened "Windows Command Prompt" and now you are on your working directory.

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/2-1.png "")

### Step 2: Script file and patch generation

#### Step 2.1.

<pre>
mmarch compare mmmerge-2019-09-22 mmmerge-2019-10-08 <a href="https://github.com/might-and-magic/mmarch#filesonly">filesonly</a> diff_folder_temp
mmarch compare mmmerge-2019-10-08 mmmerge-2020-03-17 filesonly diff_folder_temp
mmarch compare mmmerge-2020-03-17 mmmerge-2020-03-29 filesonly diff_folder_temp
</pre>

These commands will:
* generate diff files "from mmmerge-2019-09-22 to mmmerge-2019-10-08" in the folder `diff_folder_temp/`
* generate diff files "from mmmerge-2019-10-08 to mmmerge-2020-03-17" again in `diff_folder_temp/`
* generate diff files "from mmmerge-2020-03-17 to mmmerge-2020-03-29" again in `diff_folder_temp/`

If `DIFF_FOLDER` contains previous diff files (such as `diff_folder_temp/` above), `mmarch compare OLD NEW filesonly DIFF_FOLDER` will perform a merger of old diff files and new diff files by cleaning up old diff files. It's OK to do VER1 -> VER2 then VER2 -> VER3, or VER1 -> VER3 then VER2 -> VER3. But VER1 -> VER2 then VER1 -> VER3 will cause problem.

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/multi_version.png "")

#### Step 2.2.

Use command

<pre>
mmarch <a href="https://github.com/might-and-magic/mmarch#diff-files-to-nsis-batch-diff-add-keep">diff-files-to-nsis</a> diff_folder_temp nsis_folder/script.nsi files
</pre>

to generate NSIS script `script.nsi` in `nsis_folder/` folder, from diff files in `diff_folder_temp/` folder.

#### Step 2.3.

(Same as in [Step 2.2 of "Two-version diff patch"](#step-22)) Copy `mmarch.exe` to `nsis_folder/` folder.

#### Step 2.4.

(Same as in [Step 2.3 of "Two-version diff patch"](#step-23)) Compile `nsis_folder/script.nsi` to `patch.exe` with NSIS.

Full command line screenshot:

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/cl2.png "")

## Patch Usage and More

Put `patch.exe` installation file into your game folder, then double click to run it. Your game will be patched automatically.

![](https://raw.githubusercontent.com/might-and-magic/mmarch/master/tutorial/img/execute.png "")

In order to make more complex installation file, you can modify the `.nsi` script file before compiling it to `.exe`.

You may also generate a Windows Batch file patch instead of an NSIS `.exe` installation file.

Developers using Git may need [`diff-add-keep`](https://github.com/might-and-magic/mmarch#diff-files-to-nsis-batch-diff-add-keep) command.

`mmarch compare` and all related commands and features (incl. NSIS/batch script generation) work totally even if your folders do not contain any MM archive files at all. Therefore, you can use **mmarch** as a general file comparison and diff generation tool.

Visit [**mmarch**'s Home Page](https://github.com/might-and-magic/mmarch) for the full documentation.
