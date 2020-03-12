program mmarch;

{$R *.res}
{$APPTYPE CONSOLE}

uses
	SysUtils, StrUtils, RSLod, RSLodEdt;

var
	method : string;
	methodNumber : integer;
	archiveFile : string;

procedure help;
begin
	WriteLn('mmarch Version 1.0 Usage:');
	WriteLn;
	WriteLn('mmarch extract <archive_file> <folder> [file_to_extract_1] [file_to_extract_2] [...]');
	WriteLn('mmarch list <archive_file> [separator]');
	WriteLn('mmarch add <archive_file> <file_to_add_1> [file_to_add_2] [...]');
	WriteLn('mmarch delete <archive_file> <file_to_delete_1> [file_to_delete_2] [...]');
	WriteLn('mmarch rename <archive_file> <old_file_name> <new_file_name>');
	WriteLn('mmarch create <archive_file> <archive_file_type> <folder> [file_to_add_1] [file_to_add_2] [...]');
	WriteLn('mmarch merge <archive_file> <archive_file_2>');
	WriteLn('mmarch optimize <archive_file>');
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

function getIndexByFileName(const fileName : string; const arch : TRSMMArchive) : integer;
var
	fFiles : TRSMMFiles;
	fileCountMinusOne : integer;
	i : integer;
begin
	Result := -1; // if file is not found, returns -1
	try
		fFiles := arch.RawFiles;
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

procedure extractAll(const arch : TRSMMArchive; const folder : string);
var
	fileCountMinusOne : integer;
	i : integer;
begin
	try
		fileCountMinusOne := arch.RawFiles.Count - 1;
		for i := 0 to fileCountMinusOne do
			arch.Extract(i, folder);
	except
		on E : Exception do
			WriteLn(E.Message);
	end;
end;

procedure extract;
var
	folder : string;
	i : integer;
	fileName : string;
	arch : TRSMMArchive;
	fileIndex : integer;
begin
	try
		folder := ParamStr(3);
		if folder = '' then
			showError('You must specify a folder')
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
						arch.Extract(fileIndex, folder);

					if fileIndex <> -1 then
						arch.Extract(fileIndex, folder);
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
	separator : string;
	arch : TRSMMArchive;
	fFiles : TRSMMFiles;
	fileCountMinusOne : integer;
	i : integer;
	fileNameListStr : string;
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

procedure add;
var
	fileName : string;
	arch : TRSMMArchive;
	i : integer;
begin
	try
		fileName := ParamStr(3);
		if fileName = '' then
			showError('You must specify at least one file to add')
		else
		begin
			arch := RSLoadMMArchive(archiveFile);

			for i := 3 to ParamCount do
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

procedure delete;
var
	fileName : string;
	arch : TRSMMArchive;
	i : integer;
	fileIndex : integer;
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
	fileName : string;
	newFileName : string;
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
	arcFileType : string;
	folder : string;
	arch : TRSMMArchive;
	i : integer;
	ext: string;
	ver: TRSLodVersion;
	fileName : string;

const
	vers: array[0..12] of TRSLodVersion = (RSLodHeroes, RSLodHeroes,
		RSLodGames, RSLodHeroes, RSLodGames, RSLodBitmaps, RSLodIcons, RSLodSprites,
		RSLodMM8, RSLodGames7, RSLodGames, RSLodChapter7, RSLodChapter);

	versStrs: array[0..12] of string = ('h3lod', 'h3snd',
		'mmsnd', 'h3mm78vid', 'mm6vid', 'mmbitmapslod', 'mmiconslod', 'mmspriteslod',
		'mm8loclod', 'mm78gameslod', 'mm6gameslod', 'mm78save', 'mm6save');

begin
	try
		arcFileType := ParamStr(3);
		folder := ParamStr(4);
		if (arcFileType = '') or (folder = '') then
			showError('You must specify a type of your archive file that will be created and a folder')
		else
		begin
			ver := vers[AnsiIndexStr(arcFileType, versStrs)];
			ext := ExtractFileExt(archiveFile);

			if SameText(ext, '.snd') then
			begin
				arch := TRSSnd.Create;
				TRSSnd(arch).New(archiveFile, ver <> RSLodHeroes);
			end else
			if SameText(ext, '.vid') then
			begin
				arch := TRSVid.Create;
				TRSVid(arch).New(archiveFile, ver <> RSLodHeroes);
			end else
			begin
				arch := TRSLod.Create;
				TRSLod(arch).New(archiveFile, ver);
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
	arch : TRSMMArchive;
	fFiles : TRSMMFiles;
	archiveFileB : string;
	archB : TRSMMArchive;
	fFilesB : TRSMMFiles;
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
		else
			showError('Unknown method: ' + method);
		end;
	end;
end.
