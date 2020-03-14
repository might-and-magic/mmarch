program mmarch;

{$R *.res}
{$APPTYPE CONSOLE}

uses
	SysUtils, StrUtils, RSLod, RSLodEdt, RSSysUtils, Graphics, RSGraphics, RSDefLod;

const
	VERSION : string = '1.1';

var
	method, archiveFile : string;
	methodNumber : integer;

procedure help;
begin
	WriteLn('mmarch Version ' + VERSION + ' Usage:');
	WriteLn;
	WriteLn('mmarch extract <ARCHIVE_FILE> <FOLDER> [FILE_TO_EXTRACT_1] [FILE_TO_EXTRACT_2] [...]');
	WriteLn('mmarch list <ARCHIVE_FILE> [SEPARATOR]');
	WriteLn('mmarch add <ARCHIVE_FILE> <FILE_TO_ADD_1> [FILE_TO_ADD_2] [...]');
	WriteLn('mmarch add <ARCHIVE_FILE> <FILE_TO_ADD_1> [/p PALETTE_INDEX_1] [FILE_TO_ADD_2] [/p PALETTE_INDEX_2] [...]');
	WriteLn('mmarch delete <ARCHIVE_FILE> <FILE_TO_DELETE_1> [FILE_TO_DELETE_2] [...]');
	WriteLn('mmarch rename <ARCHIVE_FILE> <OLD_FILE_NAME> <NEW_FILE_NAME>');
	WriteLn('mmarch create <ARCHIVE_FILE> <ARCHIVE_FILE_TYPE> <FOLDER> [FILE_TO_ADD_1] [FILE_TO_ADD_2] [...]');
	WriteLn('mmarch create <ARCHIVE_FILE> <ARCHIVE_FILE_TYPE> <FOLDER> [FILE_TO_ADD_1] [/p PALETTE_INDEX_1] [FILE_TO_ADD_2] [/p PALETTE_INDEX_2] [...]');
	WriteLn('mmarch merge <ARCHIVE_FILE> <ARCHIVE_FILE_2>');
	WriteLn('mmarch optimize <ARCHIVE_FILE>');
	WriteLn('mmarch help');
	WriteLn;
	WriteLn('(`<>`: required; `[]`: optional):');
	WriteLn;
	WriteLn('- Initial letter of the first argument can be used (e.g. `e` for `extract`)');
	WriteLn('- Use `.` for current folder');
	WriteLn('- File names are case-insensitive');
	WriteLn;
	WriteLn('Read https://github.com/might-and-magic/mmarch for more details.');
end;

procedure showError(const str : string);
begin
	WriteLn('Error: ' + str);
	WriteLn;
	help;
end;

function getIndexByExactFileName(const fileName : string; const fFiles : TRSMMFiles) : integer;
var
	fileCountMinusOne : integer;
	i : integer;
begin
	Result := -1; // if file is not found, returns -1
	try
		fileCountMinusOne := fFiles.Count - 1;
		for i := 0 to fileCountMinusOne do
		begin
			if SameText(fileName, fFiles.Name[i]) then
			begin
				Result := i;
				exit;
			end;
		end;
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

function getIndexByFileName(const fileName : string; const arch : TRSMMArchive) : integer;
var
	fFiles : TRSMMFiles;
	fileName2, dotExt : string;
	ver : TRSLodVersion;
begin
	Result := -1;
	try

		fFiles := arch.RawFiles;
		fileName2 := fileName;

		// we don't use fFiles.FindFile(fileName, indexTemp);
		// it's actually a fuzzy match and unreliable
		// (icons.lod '2HSword1.bmp' can get 2HSword2.bmp's index)
		Result := getIndexByExactFileName(fileName2, fFiles);
		
		if Result = -1 then
		begin
			ver := TRSLod(arch).Version;
			dotExt := AnsiRightStr(fileName2, 4);

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
			( (ver = RSLodHeroes ) and SameText(dotExt, '.wav') ) or
			( (ver = RSLodGames  ) and SameText(dotExt, '.wav') ) or
			( (ver = RSLodGames  ) and SameText(dotExt, '.smk') ) or
			( (ver = RSLodBitmaps) and SameText(dotExt, '.bmp') ) or
			( (ver = RSLodBitmaps) and SameText(dotExt, '.act') ) or
			( (ver = RSLodIcons  ) and SameText(dotExt, '.bmp') ) or
			( (ver = RSLodIcons  ) and SameText(dotExt, '.act') ) or
			( (ver = RSLodSprites) and SameText(dotExt, '.bmp') ) or
			( (ver = RSLodMM8    ) and SameText(dotExt, '.bmp') )
			then
				SetLength(fileName2, length(fileName2) - 4);

			if (ver = RSLodHeroes) and SameText(dotExt, '.bmp') then
			begin
				SetLength(fileName2, length(fileName2) - 3);
				fileName2 := fileName2 + 'pcx';
			end;

			Result := getIndexByExactFileName(fileName2, fFiles);
		end;

	except
		on E : Exception do
			WriteLn(E.Message);
	end;

end;

procedure extractAll(const arch : TRSMMArchive; const folder : string);
var
	fileCountMinusOne, i : integer;
begin
	try
		fileCountMinusOne := arch.RawFiles.Count - 1;
		if fileCountMinusOne <> -1 then
			RSCreateDir(folder);
		for i := 0 to fileCountMinusOne do
			arch.Extract(i, folder);
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

procedure extract;
var
	folder, fileName : string;
	fileIndex, i : integer;
	arch : TRSMMArchive;
begin
	try
		folder := ParamStr(3);
		if folder = '' then
			showError('You must specify a folder (use `.` for current folder)')
		else
		begin
			arch := RSLoadMMArchive(archiveFile);

			fileName := ParamStr(4);
			if fileName = '' then // file_to_extract_1 is empty, extract all
				extractAll(arch, folder);

			for i := 4 to ParamCount do
			begin
				fileName := ParamStr(i);
				if fileName <> '' then
				begin
					fileIndex := getIndexByFileName(fileName, arch);

					if fileIndex = -1 then
						WriteLn('Warning: file ' + fileName + ' is not found in the archive and therefore skipped')
					else
					begin
						RSCreateDir(folder);
						arch.Extract(fileIndex, folder);
					end;
				end;
			end;
		end;
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

procedure list;
var
	separator, fileNameListStr : string;
	arch : TRSMMArchive;
	fFiles : TRSMMFiles;
	fileCountMinusOne, i : integer;
begin
	try
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
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

function getPalette(Sender: TRSLod; Bitmap: TBitmap) : integer; // similar to TRSLodEdit.NeedPalette()
var
	PalData : array[0..767] of Byte;
	pal : integer;
const
	SEPaletteMustExist: string = 'Image must be in 256 colors mode and palette must be added to bitmaps.lod';
	SEPaletteNotFound: string = 'Failed to find matching palette in [*.]bitmaps.lod';
begin
	Sender.LoadBitmapsLods('./');//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	if (Bitmap.PixelFormat <> pf8bit) or (Sender.BitmapsLods = nil) then
		raise Exception.Create(SEPaletteMustExist);

	RSWritePalette(@PalData, Bitmap.Palette); // convert to the same palette

	pal := RSMMArchivesFindSamePalette(Sender.BitmapsLods, PalData);
	if pal <> 0 then
		Result := pal
	else
		raise Exception.Create(SEPaletteNotFound);
end;

procedure add;
var
	fileName, dotExt : string;
	arch : TRSMMArchive;
	i, pal : integer;
	ver : TRSLodVersion;
begin
	try
		fileName := ParamStr(3);
		if fileName = '' then
			showError('You must specify at least one file to add')
		else
		begin
			arch := RSLoadMMArchive(archiveFile);
			ver := TRSLod(arch).Version;

			i := 3;
			while i <= ParamCount do
			begin
				fileName := ParamStr(i);
				if fileName <> '' then // has file to add
				begin
					dotExt := AnsiRightStr(fileName, 4);
					if SameText(dotExt, '.bmp') and ((ver = RSLodBitmaps) or (ver = RSLodSprites)) then // need pal
					begin
						if (i <= ParamCount - 2) and (SameText(ParamStr(i + 1), '/p')) then // pal specified
						begin
							pal := strtoint(ParamStr(i + 2));
							i := i + 2;
						end
						else
							pal := getPalette(TRSLod(arch), RSLoadBitmap(fileName));
						WriteLn(IntToStr(pal));
						arch.Add(fileName, pal);
					end
					else
						arch.Add(fileName);
				end;
				i := i + 1; // increment no matter what
			end;
			arch.RawFiles.Rebuild();
		end;
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

procedure delete;
var
	fileName : string;
	arch : TRSMMArchive;
	fileIndex, i : integer;
begin
	try
		fileName := ParamStr(3);
		if fileName = '' then
			showError('You must specify at least one file to delete')
		else
		begin
			arch := RSLoadMMArchive(archiveFile);

			for i := 3 to ParamCount do
			begin
				fileName := ParamStr(i);
				if fileName <> '' then
				begin
					fileIndex := getIndexByFileName(fileName, arch);
					if fileIndex = -1 then
						WriteLn('Warning: file ' + fileName + ' is not found in the archive and therefore skipped')
					else
						arch.RawFiles.Delete(fileIndex);
				end;
			end;
			arch.RawFiles.Rebuild();
		end;
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

procedure rename;
var
	fileName, newFileName : string;
	arch : TRSMMArchive;
	fFiles : TRSMMFiles;
	fileIndex : integer;
begin
	try
		fileName := ParamStr(3);
		newFileName := ParamStr(4);
		if (fileName = '') or (newFileName = '') then
			showError('You must specify a file to rename, and a file name that the file will be renamed to')
		else
		begin
			arch := RSLoadMMArchive(archiveFile);
			fileIndex := getIndexByFileName(fileName, arch);
			if fileIndex = -1 then
				WriteLn('Error: file ' + fileName + ' is not found in the archive')
			else
			begin
				fFiles := arch.RawFiles;
				fFiles.Rename(fileIndex, newFileName);
				fFiles.Rebuild();
			end;
		end;
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

procedure create;
var
	arcFileType, folder, ext, fileName : string;
	arch : TRSMMArchive;
	i : integer;
	ver: TRSLodVersion;

const
	vers: array[0..12] of TRSLodVersion = (RSLodHeroes, RSLodHeroes,
		RSLodGames, RSLodHeroes, RSLodGames, RSLodBitmaps, RSLodIcons, RSLodSprites,
		RSLodMM8, RSLodGames7, RSLodGames, RSLodChapter7, RSLodChapter);

	versStrs: array[0..12] of string = ('h3lod', 'h3snd',
		'mmsnd', 'h3mm78vid', 'mm6vid', 'mmbitmapslod', 'mmiconslod', 'mmspriteslod',
		'mm8loclod', 'mm78gameslod', 'mm6gameslod', 'mm78save', 'mm6save');

begin
	try
		arcFileType := AnsiLowerCase(ParamStr(3)); 
		folder := ParamStr(4);
		if (arcFileType = '') or (folder = '') then
			showError('You must specify a type of your archive file that will be created and a folder (use `.` for current folder)')
		else
		begin
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

			for i := 5 to ParamCount do
			begin
				fileName := ParamStr(i);
				if fileName <> '' then
				begin
					arch.Add(fileName);
				end;
			end;

			arch.RawFiles.Rebuild();
		end;
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

procedure merge;
var
	arch, archB : TRSMMArchive;
	fFiles, fFilesB : TRSMMFiles;
	archiveFileB : string;
begin
	try
		archiveFileB := ParamStr(3);
		if archiveFileB = '' then
			showError('You must specify two archive files to be merged together')
		else
		begin
			arch := RSLoadMMArchive(archiveFile);
			fFiles := arch.RawFiles;

			archB := RSLoadMMArchive(archiveFileB);
			fFilesB := archB.RawFiles;

			fFilesB.MergeTo(fFiles);
			fFiles.Rebuild();
		end;
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

procedure optimize;
var
	arch : TRSMMArchive;
begin
	try
		arch := RSLoadMMArchive(archiveFile);
		arch.RawFiles.Rebuild();
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
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
	method := trimChar(ParamStr(1));
	methodNumber := AnsiIndexStr(method, ['extract', 'e', 'list', 'l',
		'add', 'a', 'delete', 'd', 'rename', 'r', 'create', 'c', 'merge', 'm',
		'optimize', 'o', 'help', 'h', '']);
	archiveFile := ParamStr(2);
	if (archiveFile = '') And (methodNumber < 16) then // < 16 : method is not `help`
		showError('You must specify an archive file')
	else
	begin
		Case methodNumber of
			0: extract;
			1: extract;
			2: list;
			3: list;
			4: add;
			5: add;
			6: delete;
			7: delete;
			8: rename;
			9: rename;
			10: create;
			11: create;
			12: merge;
			13: merge;
			14: optimize;
			15: optimize;
			16: help;
			17: help;
			18: help;
		else // -1 : not found in array
			showError('Unknown method: ' + method);
		end;
	end;
end.
