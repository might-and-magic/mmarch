unit MMArchUnit;

interface

uses
	Windows, SysUtils, StrUtils, Classes, RSLod, RSSysUtils, Graphics, RSGraphics, RSDefLod;

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

		// `ext` normally has dot `.` (`.txt`); `ext` of file without extension is ``; default to `*` meaning all files
		// you should not use any other format here. Same applys to extractAll(), deleteAll() and addAll()
		procedure extractAll(folder: string; ext: string = '*');
		procedure extract(folder: string; fileToExtract: string);

		procedure deleteAll(ext: string = '*');
		procedure delete(fileToDelete: string);

		procedure addAll(folder: string; ext: string = '*');
		procedure add(bmpFileToAdd: string; paletteIndex: integer); overload;
		procedure add(fileToAdd: string); overload;

		procedure new(archiveFile: string; archiveFileType: string; folder: string);

		procedure rename(oldFileName: string; newFileName: string);

		procedure merge(archiveFile2: string);

		procedure optimize;
	end;

	OtherMmarchException = class(Exception);

	function getAllFilesInFolder(path: string; ext: string = '*'): TStringList;

resourcestring
	      FileNotFound = 'File %s is not found in the archive';
	     FileNameEmpty = 'File name is empty';
	       DirNotFound = 'Directory %s is not found';
	SEPaletteMustExist = 'Image must be in 256 colors mode and palette must be added to bitmaps.lod';
	 SEPaletteNotFound = 'Failed to find matching palette in [*.]bitmaps.lod';


implementation


function getAllFilesInFolder(path: string; ext: string = '*'): TStringList;
var
	searchResult: TSearchRec;
	fileMask: string;
	currentDir: array[0 .. MAX_PATH] of char;
begin
	Result := TStringList.Create;

	if ext = '*' then // all files
		fileMask := '*'
	else
	begin
		if (ext = '') then // without extension
			fileMask := '*.'
			// '*.' will match `.gitignore` (no stem) and `LICENSE` (no ext)
			// because they are seen as `.gitignore.` and `LICENSE.`
			// so we'll have to check against its real extension it later
		else
		begin
			fileMask := '*' + ext;
		end;
	end;

	GetCurrentDirectory(MAX_PATH, currentDir);
	if not DirectoryExists(path) then
		raise OtherMmarchException.CreateFmt(DirNotFound, [path]);

	SetCurrentDirectory(PChar(path));
	if findfirst(fileMask, faAnyFile, searchResult) = 0 then
	begin
		repeat
			if FileExists(searchResult.Name) and (
				(fileMask <> '*.') or
				( (fileMask = '*.') and (ExtractFileExt(searchResult.Name) = '') )
			) then
				Result.Add(searchResult.Name);
		until FindNext(searchResult) <> 0;
		SysUtils.FindClose(searchResult);
	end;
	SetCurrentDirectory(currentDir);
end;


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
		raise OtherMmarchException.Create(FileNameEmpty);

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
			arch.Extract(i, folder);
		end;
	end;
end;


procedure MMArchSimple.extract(folder: string; fileToExtract: string);
var
	fileIndex: integer;
begin
	fileIndex := getIndexByFileName(fileToExtract);

	if fileIndex = -1 then
		raise OtherMmarchException.CreateFmt(FileNotFound, [fileToExtract])
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
			fFiles.Delete(i);
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
		raise OtherMmarchException.CreateFmt(FileNotFound, [fileToDelete])
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
		add(IncludeTrailingPathDelimiter(folder) + fileName);
	end;
end;


procedure MMArchSimple.add(bmpFileToAdd: string; paletteIndex: integer);
begin
	arch.Add(bmpFileToAdd, paletteIndex);
	optimize;
end;


procedure MMArchSimple.add(fileToAdd: string);
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
		TRSSnd(arch).New(IncludeTrailingPathDelimiter(folder) + archiveFile, ver <> RSLodHeroes);
	end else
	if SameText(ext, '.vid') then
	begin
		arch := TRSVid.Create;
		TRSVid(arch).New(IncludeTrailingPathDelimiter(folder) + archiveFile, ver <> RSLodHeroes);
	end else
	begin
		arch := TRSLod.Create;
		TRSLod(arch).New(IncludeTrailingPathDelimiter(folder) + archiveFile, ver);
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
		raise OtherMmarchException.CreateFmt(FileNotFound, [oldFileName]);
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
