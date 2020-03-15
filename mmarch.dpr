program mmarch;

{$R *.res}
{$APPTYPE CONSOLE}

uses
	SysUtils, StrUtils, RSLod, RSLodEdt, RSSysUtils, Graphics, RSGraphics, RSDefLod, RSQ, MMArchUnit;

const
	VERSION: string = '2.0';

var
	method, archiveFile: string;
	methodNumber: integer;

	mmarchs: MMArchSimple;

type
	MissingParamException = class(Exception);
	OtherMmarchException = class(Exception);

procedure help;
begin
	WriteLn('mmarch Version ' + VERSION + ' Usage:');
	WriteLn;
	WriteLn('mmarch extract <ARCHIVE_FILE> <FOLDER> [FILE_TO_EXTRACT_1] [FILE_TO_EXTRACT_2] [...]');
	WriteLn('mmarch list <ARCHIVE_FILE> [SEPARATOR]');
	WriteLn('mmarch add <ARCHIVE_FILE> <FILE_TO_ADD_1> [FILE_TO_ADD_2] [...]');
	WriteLn('mmarch delete <ARCHIVE_FILE> <FILE_TO_DELETE_1> [FILE_TO_DELETE_2] [...]');
	WriteLn('mmarch rename <ARCHIVE_FILE> <OLD_FILE_NAME> <NEW_FILE_NAME>');
	WriteLn('mmarch create <ARCHIVE_FILE> <ARCHIVE_FILE_TYPE> <FOLDER> [FILE_TO_ADD_1] [FILE_TO_ADD_2] [...]');
	WriteLn('mmarch merge <ARCHIVE_FILE> <ARCHIVE_FILE_2>');
	WriteLn('mmarch optimize <ARCHIVE_FILE>');
	WriteLn('mmarch help');
	WriteLn;
	WriteLn('(`<>`: required; `[]`: optional):');
	WriteLn;
	WriteLn('- Initial letter of the first argument can be used (e.g. `e` for `extract`)');
	WriteLn('- File names are case-insensitive');
	WriteLn('-             . : current folder');
	WriteLn('-      * or *.* : all files');
	WriteLn('- *.EXT / *.txt : all files with specified extension');
	WriteLn('-            *. : all files without extension');
	WriteLn;
	WriteLn('Read https://github.com/might-and-magic/mmarch for more details.');
end;

function getIndexByExactFileName(const fileName: string; const fFiles: TRSMMFiles): integer;
var
	fileCountMinusOne: integer;
	i: integer;
begin
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

function getIndexByFileName(const fileName: string; const arch: TRSMMArchive): integer;
var
	fFiles: TRSMMFiles;
	fileName2, ext: string;
	ver: TRSLodVersion;
begin
	fFiles := arch.RawFiles;
	fileName2 := fileName;

	// we don't use fFiles.FindFile(fileName, indexTemp);
	// it's actually a fuzzy match and unreliable
	// (icons.lod '2HSword1.bmp' can get 2HSword2.bmp's index)
	Result := getIndexByExactFileName(fileName2, fFiles);
	
	if Result = -1 then
	begin
		ver := TRSLod(arch).Version;
		ext := AnsiRightStr(fileName2, 4);

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
		( (ver = RSLodHeroes ) and SameText(ext, '.wav') ) or
		( (ver = RSLodGames  ) and SameText(ext, '.wav') ) or
		( (ver = RSLodGames  ) and SameText(ext, '.smk') ) or
		( (ver = RSLodBitmaps) and SameText(ext, '.bmp') ) or
		( (ver = RSLodBitmaps) and SameText(ext, '.act') ) or
		( (ver = RSLodIcons  ) and SameText(ext, '.bmp') ) or
		( (ver = RSLodIcons  ) and SameText(ext, '.act') ) or
		( (ver = RSLodSprites) and SameText(ext, '.bmp') ) or
		( (ver = RSLodMM8    ) and SameText(ext, '.bmp') )
		then
			SetLength(fileName2, length(fileName2) - 4);

		if (ver = RSLodHeroes) and SameText(ext, '.bmp') then
		begin
			SetLength(fileName2, length(fileName2) - 3);
			fileName2 := fileName2 + 'pcx';
		end;

		Result := getIndexByExactFileName(fileName2, fFiles);
	end;

end;

procedure extractAll(const arch: TRSMMArchive; const folder: string);
var
	fileCountMinusOne, i: integer;
begin
	fileCountMinusOne := arch.RawFiles.Count - 1;
	if fileCountMinusOne <> -1 then
		RSCreateDir(folder);
	for i := 0 to fileCountMinusOne do
		arch.Extract(i, folder);
end;

procedure extract;
var
	folder, fileName: string;
	fileIndex, i: integer;
	arch: TRSMMArchive;
begin
	folder := ParamStr(3);
	if folder = '' then
		raise MissingParamException.Create('You must specify a folder (use `.` for current folder)');

	arch := RSLoadMMArchive(archiveFile);

	fileName := ParamStr(4);
	if fileName = '' then // file_to_extract_1 is empty, extract all
		extractAll(arch, folder);

	for i := 4 to ParamCount do
	begin
		fileName := ParamStr(i);
		if fileName <> '' then
		begin

// fileName contains '*'



			fileIndex := getIndexByFileName(fileName, arch);

			if fileIndex = -1 then
				raise OtherMmarchException.CreateFmt('File %s is not found in the archive and therefore skipped', [fileName])
			else
			begin
				RSCreateDir(folder);
				arch.Extract(fileIndex, folder);
			end;
		end;
	end;
end;

procedure list;
var
	separator, fileNameListStr: string;
	arch: TRSMMArchive;
	fFiles: TRSMMFiles;
	fileCountMinusOne, i: integer;
begin
	separator := ParamStr(3);
	if separator = '' then
		separator := #13#10;

	arch := RSLoadMMArchive(archiveFile);
	fFiles := arch.RawFiles;
	fileCountMinusOne := fFiles.Count - 1;
	fileNameListStr := '';

	for i := 0 to fileCountMinusOne do
		begin
			if i = fileCountMinusOne then
				separator := '';
			fileNameListStr := fileNameListStr + fFiles.Name[i] + separator;
		end;
	Write(fileNameListStr);
end;

function getPalette(Sender: TRSLod; Bitmap: TBitmap): integer; // similar to TRSLodEdit.NeedPalette()
var
	PalData: array[0..767] of Byte;
	pal: integer;
const
	SEPaletteMustExist: string = 'Image must be in 256 colors mode and palette must be added to bitmaps.lod';
	SEPaletteNotFound: string = 'Failed to find matching palette in [*.]bitmaps.lod';
begin
	if (Bitmap.PixelFormat <> pf8bit) or (Sender.BitmapsLods = nil) then
		raise Exception.Create(SEPaletteMustExist);

	RSWritePalette(@PalData, Bitmap.Palette); // convert to the same palette

	pal := RSMMArchivesFindSamePalette(Sender.BitmapsLods, PalData);
	if pal <> 0 then
		Result := pal
	else
		raise Exception.Create(SEPaletteNotFound);
end;

procedure addProc(arch: TRSMMArchive; paramIndexFrom: integer);
var
	fileName, ext: string;
	i, pal: integer;
	ver: TRSLodVersion;
	Sender: TRSLod;
begin
	Sender := TRSLod(arch);
	ver := Sender.Version;
	i := paramIndexFrom;
	while i <= ParamCount do
	begin
		fileName := ParamStr(i);
		if fileName <> '' then // has file to add
		begin

// fileName contains '*'



			ext := AnsiRightStr(fileName, 4);
			if SameText(ext, '.bmp') and ((ver = RSLodBitmaps) or (ver = RSLodSprites)) then // need pal
			begin
				if (i <= ParamCount - 2) and (SameText(ParamStr(i + 1), '/p')) then // pal specified
				begin
					pal := strtoint(ParamStr(i + 2));
					i := i + 2;
				end
				else
				begin
					Sender.LoadBitmapsLods(ExtractFilePath(arch.RawFiles.FileName));
					pal := getPalette(Sender, RSLoadBitmap(fileName));
				end;
				arch.Add(fileName, pal);
			end
			else
				arch.Add(fileName);
		end;
		i := i + 1; // increment no matter what
	end;
	// this procedure does not do arch.RawFiles.Rebuild();
end;

procedure add;
var
	fileName: string;
	arch: TRSMMArchive;
begin
	fileName := ParamStr(3);
	if fileName = '' then
		raise MissingParamException.Create('You must specify at least one file to add');
	arch := RSLoadMMArchive(archiveFile);
	addProc(arch, 3);
	arch.RawFiles.Rebuild();
end;

procedure delete;
var
	fileName: string;
	arch: TRSMMArchive;
	fileIndex, i: integer;
begin
	fileName := ParamStr(3);
	if fileName = '' then
		raise MissingParamException.Create('You must specify at least one file to delete');
	arch := RSLoadMMArchive(archiveFile);

	for i := 3 to ParamCount do
	begin
		fileName := ParamStr(i);
		if fileName <> '' then
		begin

// fileName contains '*'


			fileIndex := getIndexByFileName(fileName, arch);
			if fileIndex = -1 then
				raise OtherMmarchException.CreateFmt('File %s is not found in the archive and therefore skipped', [fileName])
			else
				arch.RawFiles.Delete(fileIndex);
		end;
	end;
	arch.RawFiles.Rebuild();
end;

procedure rename;
var
	fileName, newFileName: string;
	arch: TRSMMArchive;
	fFiles: TRSMMFiles;
	fileIndex: integer;
begin
	fileName := ParamStr(3);
	newFileName := ParamStr(4);
	if (fileName = '') or (newFileName = '') then
		raise MissingParamException.Create('You must specify a file to rename, and a file name that the file will be renamed to');
	arch := RSLoadMMArchive(archiveFile);
	fileIndex := getIndexByFileName(fileName, arch);
	if fileIndex = -1 then
		raise OtherMmarchException.CreateFmt('File %s is not found in the archive', [fileName]);
	fFiles := arch.RawFiles;
	fFiles.Rename(fileIndex, newFileName);
	fFiles.Rebuild();
end;

procedure create;
var
	arcFileType, folder, ext: string;
	arch: TRSMMArchive;
	ver: TRSLodVersion;

const
	vers: array[0..12] of TRSLodVersion = (RSLodHeroes, RSLodHeroes,
		RSLodGames, RSLodHeroes, RSLodGames, RSLodBitmaps, RSLodIcons, RSLodSprites,
		RSLodMM8, RSLodGames7, RSLodGames, RSLodChapter7, RSLodChapter);

	versStrs: array[0..12] of string = ('h3lod', 'h3snd',
		'mmsnd', 'h3mm78vid', 'mm6vid', 'mmbitmapslod', 'mmiconslod', 'mmspriteslod',
		'mm8loclod', 'mm78gameslod', 'mm6gameslod', 'mm78save', 'mm6save');

begin
	arcFileType := AnsiLowerCase(ParamStr(3)); 
	folder := ParamStr(4);
	if (arcFileType = '') or (folder = '') then
		raise MissingParamException.Create('You must specify a type of your archive file that will be created and a folder (use `.` for current folder)');
	ver := vers[AnsiIndexStr(arcFileType, versStrs)];
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

	addProc(arch, 5);

	arch.RawFiles.Rebuild();
end;

procedure merge;
var
	arch, archB: TRSMMArchive;
	fFiles, fFilesB: TRSMMFiles;
	archiveFileB: string;
begin
	archiveFileB := ParamStr(3);
	if archiveFileB = '' then
		raise MissingParamException.Create('You must specify two archive files to be merged together');

	arch := RSLoadMMArchive(archiveFile);
	fFiles := arch.RawFiles;

	archB := RSLoadMMArchive(archiveFileB);
	fFilesB := archB.RawFiles;

	fFilesB.MergeTo(fFiles);
	fFiles.Rebuild();
end;

procedure optimize;
var
	arch: TRSMMArchive;
begin
	arch := RSLoadMMArchive(archiveFile);
	arch.RawFiles.Rebuild();
end;

function trimChar(const str: string): string;
var
	n: integer;
begin
	n := 1;
	while (n <= Length(str)) and (str[n] = '-') do
		Inc(n);
	SetString(Result, PChar(@str[n]), Length(str) - n + 1);
end;

begin

	try

	// mmarchs := MMArchSimple.create;
	// mmarchs.load('mmspriteslod.sprites.lod');

	mmarchs := MMArchSimple.load('icons.lod');

	// mmarchs.deleteAll('.bmp');

	mmarchs.deleteAll('');

	// MMArchSimple.load('mmbitmapslod.bitmaps.lod').optimize;


	// 	method := trimChar(ParamStr(1));
	// 	methodNumber := AnsiIndexStr(method, ['extract', 'e', 'list', 'l',
	// 		'add', 'a', 'delete', 'd', 'rename', 'r', 'create', 'c', 'merge', 'm',
	// 		'optimize', 'o', 'help', 'h', '']);
	// 	archiveFile := ParamStr(2);

	// 	if (archiveFile = '') And (methodNumber < 16) And (methodNumber >= 0) then // < 16: method is not `help`
	// 		raise MissingParamException.Create('You must specify an archive file');

	// 	Case methodNumber of
	// 		 0,  1: extract;
	// 		 2,  3: list;
	// 		 4,  5: add;
	// 		 6,  7: delete;
	// 		 8,  9: rename;
	// 		10, 11: create;
	// 		12, 13: merge;
	// 		14, 15: optimize;
	// 		16, 17, 18: help;
	// 	else // -1: not found in the array
	// 		raise MissingParamException.CreateFmt('Unknown method: %s', [method]);
	// 	end;

	except
		on E: MissingParamException do
		begin
			WriteLn('Error: ' + E.Message);
			WriteLn;
			WriteLn;
			help;
		end;
		on E: OtherMmarchException do
			WriteLn('Error: ' + E.Message);
		on E: Exception do
			WriteLn(E.Message);
	end;

end.
