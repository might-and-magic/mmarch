// MMArchMain unit and MMArchSimple class
// Part of mmarch
// Command line tool to handle Heroes 3 and Might and Magic 6, 7, 8
// resource archive files (e.g. lod files). Based on GrayFace's MMArchive.
// By Tom CHEN <tomchen.org@gmail.com> (tomchen.org)
// MIT License
// https://github.com/might-and-magic/mmarch



unit MMArchMain;

interface

uses
	Windows, SysUtils, StrUtils, Classes, RSLod, RSSysUtils, Graphics, RSGraphics, RSDefLod, MMArchPath, RSQ;

type

	MMArchSimple = class
	private
		arch: TRSMMArchive;
		function getIndexByExactFileName(fileName: string): integer;
		function verifyExtractedExt(fileIndex: integer; extractedExtToVerify: string): boolean;
		function getInArchiveExt(extractedExt: string): string;

	public

		// constructor create;
		constructor load(archiveFile: string); overload;
		constructor load(archParam: TRSMMArchive); overload;

		// public utilities
		function getIndexByFileName(fileName: string): integer;
		function getPalette(Bitmap: TBitmap): integer;
		function getTRSMMArchive: TRSMMArchive;

		// ===== common procedures and functions =====

		function list(separator: string = #13#10) : string;

		// `ext` parameter in extractAll(), deleteAll() and addAll()
		// differs from https://github.com/might-and-magic/mmarch#notes-on-file_to_xxxx_
		// `ext` normally has dot `.` (`.txt`); `ext` of file without extension is ``; default to `*` meaning all files
		// you should not use any other format here

		// `folder` parameter in extractAll(), extract() and addAll() generally uses
		// the same rule here https://github.com/might-and-magic/mmarch#notes-on-folder

		procedure extractAll(folder: string; ext: string = '*');
		procedure extract(folder: string; fileToExtract: string);

		procedure deleteAll(ext: string = '*');
		procedure delete(fileToDelete: string);

		procedure addAll(folder: string; ext: string = '*');
		procedure add(bmpFileToAdd: string; paletteIndex: integer; needOptimize: boolean = true); overload;
		procedure add(fileToAdd: string; needOptimize: boolean = true); overload;

		procedure new(archiveFile: string; archiveFileType: string; folder: string);

		procedure rename(oldFileName: string; newFileName: string);

		procedure merge(archiveFile2: string);

		procedure optimize;
	end;


resourcestring
	         FileNotFound = 'File `%s` is not found in the archive';
	        FileNameEmpty = 'File name is empty';
	   SEPaletteMustExist = 'Image must be in 256 colors mode and palette must be added to bitmaps.lod';
	    SEPaletteNotFound = 'Failed to find matching palette in [*.]bitmaps.lod';
	             ErrorStr = 'Error: %s';
	         FileErrorStr = 'File `%s` error: %s';
	FileInArchiveErrorStr = 'File `%s` in archive `%s` error:';
	  ArchiveFileErrorStr = 'Archive file `%s` (or perhaps not an MM Archive file) error:';


implementation


constructor MMArchSimple.load(archiveFile: string);
begin
	// inherited Create() is called implicitly
	arch := RSLoadMMArchive(archiveFile);
end;


constructor MMArchSimple.load(archParam: TRSMMArchive);
begin
	// inherited Create() is called implicitly
	arch := archParam;
end;


function MMArchSimple.getIndexByExactFileName(fileName: string): integer;
var
	fFiles: TRSMMFiles;
	fileCountMinusOne: integer;
	i: integer;
begin
	fFiles := arch.RawFiles;
	Result := -1; // if file is not found, returns -1
	fileCountMinusOne := fFiles.Count - 1;
	for i := 0 to fileCountMinusOne do
	begin
		if SameText(fileName, fFiles.Name[i]) then
		begin
			Result := i;
			exit;
		end;
	end;
end;


function MMArchSimple.verifyExtractedExt(fileIndex: integer; extractedExtToVerify: string): boolean;
begin
	Result := SameText( ExtractFileExt(arch.GetExtractName(fileIndex)), extractedExtToVerify );
end;


function MMArchSimple.getInArchiveExt(extractedExt: string): string;
var
	ver: TRSLodVersion;
begin
	ver := TRSLod(arch).Version;

	// | Type         | Archive Format | In-Archive Ext | Extracted Ext |
	// |--------------|----------------|----------------|---------------|
	// | RSLodHeroes  | `h3lod`        | `pcx`          | `bmp`         |
	// | RSLodHeroes  | `h3snd`        | No Extension   | `wav`         |
	// | RSLodGames   | `mmsnd`        | No Extension   | `wav`         |
	// | RSLodGames   | `mm6vid`       | No Extension   | `smk`         |
	// | RSLodBitmaps | `mmbitmapslod` | No Extension   | `bmp`         |
	// | RSLodBitmaps | `mmbitmapslod` | No Extension   | `act`         |
	// | RSLodIcons   | `mmiconslod`   | No Extension   | `bmp`         |
	// | RSLodIcons   | `mmiconslod`   | No Extension   | `act`         |
	// | RSLodSprites | `mmspriteslod` | No Extension   | `bmp`         |
	// | RSLodMM8     | `mm8loclod`    | No Extension   | `bmp`         |

	if
	( (ver = RSLodHeroes ) and SameText(extractedExt, '.wav') ) or
	( (ver = RSLodGames  ) and SameText(extractedExt, '.wav') ) or
	( (ver = RSLodGames  ) and SameText(extractedExt, '.smk') ) or
	( (ver = RSLodBitmaps) and SameText(extractedExt, '.bmp') ) or
	( (ver = RSLodBitmaps) and SameText(extractedExt, '.act') ) or
	( (ver = RSLodIcons  ) and SameText(extractedExt, '.bmp') ) or
	( (ver = RSLodIcons  ) and SameText(extractedExt, '.act') ) or
	( (ver = RSLodSprites) and SameText(extractedExt, '.bmp') ) or
	( (ver = RSLodMM8    ) and SameText(extractedExt, '.bmp') )
	then
		Result := ''
	else
	begin
		if (ver = RSLodHeroes) and SameText(extractedExt, '.bmp') then
		begin
			Result := '.pcx';
		end;
	end;

	// you may want to use verifyExtractedExt() after using getInArchiveExt()
end;


function MMArchSimple.getIndexByFileName(fileName: string): integer;
var
	fileName2, ext: string;
	indexTemp: integer;
begin
	if fileName = '' then
		raise Exception.Create(FileNameEmpty);

	fileName2 := fileName;

	// we don't use fFiles.FindFile(fileName, indexTemp);
	// it's actually a fuzzy match and unreliable
	// (icons.lod '2HSword1.bmp' can get 2HSword2.bmp's index)
	Result := getIndexByExactFileName(fileName2);
	
	if Result = -1 then // it may be caused by the difference between in-archive and extracted file extensions
	begin
		ext := ExtractFileExt(fileName2);

		SetLength( fileName2, (length(fileName2) - length(ext)) );
		fileName2 := fileName2 + getInArchiveExt(ext);

		indexTemp := getIndexByExactFileName(fileName2);

		if verifyExtractedExt(indexTemp, ext) then
			Result := indexTemp;
	end;

end;


function MMArchSimple.getPalette(Bitmap: TBitmap): integer; // similar to TRSLodEdit.NeedPalette()
var
	tLod: TRSLod;
	PalData: array[0..767] of Byte;
	pal: integer;
begin
	tLod := TRSLod(arch);
	if (Bitmap.PixelFormat <> pf8bit) or (tLod.BitmapsLods = nil) then
		raise Exception.Create(SEPaletteMustExist);

	RSWritePalette(@PalData, Bitmap.Palette); // convert to the same palette

	pal := RSMMArchivesFindSamePalette(tLod.BitmapsLods, PalData);
	if pal <> 0 then
		Result := pal
	else
		raise Exception.Create(SEPaletteNotFound);
end;


function MMArchSimple.getTRSMMArchive: TRSMMArchive;
begin
	Result := arch;
end;


function MMArchSimple.list(separator: string = #13#10) : string;
var
	fFiles: TRSMMFiles;
	fileNameListStr: string;
	fileCountMinusOne, i: integer;
begin
	fFiles := arch.RawFiles;
	fileCountMinusOne := fFiles.Count - 1;
	fileNameListStr := '';

	for i := 0 to fileCountMinusOne do
		begin
			if i = fileCountMinusOne then
				separator := '';
			fileNameListStr := fileNameListStr + fFiles.Name[i] + separator;
		end;
	Result := fileNameListStr;
end;


procedure MMArchSimple.extractAll(folder: string; ext: string = '*');
var
	fFiles: TRSMMFiles;
	fileCountMinusOne, i: integer;
begin
	fFiles := arch.RawFiles;
	fileCountMinusOne := fFiles.Count - 1;
	for i := 0 to fileCountMinusOne do
	begin
		if (ext = '*')
		or SameText(ext, ExtractFileExt(fFiles.Name[i]))
		or (
			SameText(getInArchiveExt(ext), ExtractFileExt(fFiles.Name[i])) and
			verifyExtractedExt(i, ext)
		) then
		begin
			RSCreateDir(folder); // this function checks DirectoryExists()

			try // the individual resource file will be skipped if it gets an exception
				arch.Extract(i, folder);
			except
				on E: Exception do
				begin
					WriteLn(format(FileInArchiveErrorStr, [beautifyPath(fFiles.Name[i]), beautifyPath(fFiles.FileName)]));
					WriteLn(E.Message);
				end;
			end;
		end;
	end;
end;


procedure MMArchSimple.extract(folder: string; fileToExtract: string);
var
	fileIndex: integer;
begin
	fileIndex := getIndexByFileName(fileToExtract);

	if fileIndex = -1 then
		raise Exception.CreateFmt(FileNotFound, [fileToExtract])
	else
	begin
		RSCreateDir(folder);
		arch.Extract(fileIndex, folder);
	end;
end;


procedure MMArchSimple.deleteAll(ext: string = '*');
var
	fFiles: TRSMMFiles;
	fileCountMinusOne, i: integer;
	currentFileExt: string;
begin
	fFiles := arch.RawFiles;
	fileCountMinusOne := fFiles.Count - 1;
	for i := fileCountMinusOne downto 0 do
	begin
		currentFileExt := ExtractFileExt(fFiles.Name[i]);
		if (ext = '*')
		or SameText(ext, currentFileExt)
		or (
			SameText(getInArchiveExt(ext), currentFileExt) and
			verifyExtractedExt(i, ext)
		) then
		begin
			try // the individual resource file will be skipped if it gets an exception
				fFiles.Delete(i);
			except
				on E: Exception do
					WriteLn(format(FileErrorStr, [beautifyPath(fFiles.Name[i]), E.Message]));
			end;
		end;
	end;
	optimize;
end;


procedure MMArchSimple.delete(fileToDelete: string);
var
	fileIndex: integer;
begin
	fileIndex := getIndexByFileName(fileToDelete);
	if fileIndex = -1 then
		raise Exception.CreateFmt(FileNotFound, [fileToDelete])
	else
		arch.RawFiles.Delete(fileIndex);
	optimize;
end;


procedure MMArchSimple.addAll(folder: string; ext: string = '*');
var
	fileNames: TStringList;
	fileName: string;
begin
	fileNames := getAllFilesInFolder(folder, ext);
	for fileName in fileNames do
	begin
		try // the individual resource file will be skipped if it gets an exception
			add(withTrailingSlash(folder) + fileName, false);
		except
			on E: Exception do
				WriteLn(format(FileErrorStr, [beautifyPath(fileName), E.Message]));
		end;
	end;
	fileNames.Free;
	optimize;
end;


procedure MMArchSimple.add(bmpFileToAdd: string; paletteIndex: integer; needOptimize: boolean = true);
begin
	arch.Add(bmpFileToAdd, paletteIndex);
	if needOptimize then
		optimize;
end;


procedure MMArchSimple.add(fileToAdd: string; needOptimize: boolean = true);
var
	tLod: TRSLod;
	ver: TRSLodVersion;
	ext: string;
	paletteIndex: integer;
begin
	tLod := TRSLod(arch);
	ver := tLod.Version;
	ext := ExtractFileExt(fileToAdd);
	if SameText(ext, '.bmp') and ((ver = RSLodBitmaps) or (ver = RSLodSprites)) then // need palette
	begin
		tLod.LoadBitmapsLods(ExtractFilePath(arch.RawFiles.FileName));
		paletteIndex := getPalette(RSLoadBitmap(fileToAdd));
		add(fileToAdd, paletteIndex);
	end
	else
		arch.Add(fileToAdd);
		if needOptimize then
			optimize;
end;


procedure MMArchSimple.new(archiveFile: string; archiveFileType: string; folder: string);
var
	ext: string;
	ver: TRSLodVersion;

const
	vers: array[0..12] of TRSLodVersion = (RSLodHeroes, RSLodHeroes,
		RSLodGames, RSLodHeroes, RSLodGames, RSLodBitmaps, RSLodIcons, RSLodSprites,
		RSLodMM8, RSLodGames7, RSLodGames, RSLodChapter7, RSLodChapter);

	versStrs: array[0..12] of string = ('h3lod', 'h3snd',
		'mmsnd', 'h3mm78vid', 'mm6vid', 'mmbitmapslod', 'mmiconslod', 'mmspriteslod',
		'mm8loclod', 'mm78gameslod', 'mm6gameslod', 'mm78save', 'mm6save');

begin
	ver := vers[AnsiIndexStr(AnsiLowerCase(archiveFileType), versStrs)];
	ext := ExtractFileExt(archiveFile);

	if SameText(ext, '.snd') then
	begin
		arch := TRSSnd.Create;
		TRSSnd(arch).New(withTrailingSlash(folder) + archiveFile, ver <> RSLodHeroes);
	end else
	if SameText(ext, '.vid') then
	begin
		arch := TRSVid.Create;
		TRSVid(arch).New(withTrailingSlash(folder) + archiveFile, ver <> RSLodHeroes);
	end else
	begin
		arch := TRSLod.Create;
		TRSLod(arch).New(withTrailingSlash(folder) + archiveFile, ver);
	end;

	optimize;
end;


procedure MMArchSimple.rename(oldFileName: string; newFileName: string);
var
	fFiles: TRSMMFiles;
	fileIndex: integer;
begin
	fileIndex := getIndexByFileName(oldFileName);
	if fileIndex = -1 then
		raise Exception.CreateFmt(FileNotFound, [oldFileName]);
	fFiles := arch.RawFiles;
	fFiles.Rename(fileIndex, newFileName);
	optimize;
end;


procedure MMArchSimple.merge(archiveFile2: string);
var
	arch2: TRSMMArchive;
	fFiles, fFiles2: TRSMMFiles;
begin
	fFiles := arch.RawFiles;

	arch2 := RSLoadMMArchive(archiveFile2);
	fFiles2 := arch2.RawFiles;

	fFiles2.MergeTo(fFiles);

	optimize;
end;


procedure MMArchSimple.optimize;
begin
	arch.RawFiles.Rebuild;
end;


end.
