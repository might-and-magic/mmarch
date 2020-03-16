program mmarch;

{$R *.res}
{$APPTYPE CONSOLE}

uses
	SysUtils, StrUtils, MMArchUnit;

type
	MissingParamException = class(Exception);
	OtherMmarchException = class(Exception);

const
	MMARCHVERSION: string = '2.0';
	MMARCHURL: string = 'https://github.com/might-and-magic/mmarch';

var
	method, archiveFile: string;
	methodNumber: integer;

resourcestring
	NeedFolder                   = 'You must specify a folder (use `.` for current folder)';
	NeedFileToAdd                = 'You must specify at least one file to add';
	NeedFileToDelete             = 'You must specify at least one file to delete';
	NeedFilesToRename            = 'You must specify a file to rename, and a file name that the file will be renamed to';
	NeedArchiveFileTypeAndFolder = 'You must specify a type of your archive file that will be created and a folder (use `.` for current folder)';
	NeedArchiveFilesToMerge      = 'You must specify two archive files to be merged together';
	NeedArchiveFile              = 'You must specify an archive file';
	UnknownMethod                = 'Unknown method: %s';

	HELPSTR_FirstLine       = 'mmarch Version %s Usage:';
	HELPSTR_ReqOpt          = '(`%s`: required; `%s`: optional):';
	HELPSTR_Initial         = 'Initial letter of the first argument can be used (e.g. `%s` for `%s`)';
	HELPSTR_CaseInsensitive = 'File names are case-insensitive';
	HELPSTR_CurrentFolder   = 'current folder';
	HELPSTR_AllFiles        = 'all files';
	HELPSTR_AllFilesWExt    = 'all files with specified extension';
	HELPSTR_AllFilesWOExt   = 'all files without extension';
	HELPSTR_ReadDetails     = 'Read %s for more details.';

	HELPPUNCTSTR_Colon      = ': ';

	HELPPARAMSTR_ARCHIVE_FILE      = 'ARCHIVE_FILE';
	HELPPARAMSTR_FOLDER            = 'FOLDER';
	HELPPARAMSTR_FILE_TO_EXTRACT_1 = 'FILE_TO_EXTRACT_1';
	HELPPARAMSTR_FILE_TO_EXTRACT_2 = 'FILE_TO_EXTRACT_2';
	HELPPARAMSTR_SEPARATOR         = 'SEPARATOR';
	HELPPARAMSTR_FILE_TO_ADD_1     = 'FILE_TO_ADD_1';
	HELPPARAMSTR_FILE_TO_ADD_2     = 'FILE_TO_ADD_2';
	HELPPARAMSTR_FILE_TO_DELETE_1  = 'FILE_TO_DELETE_1';
	HELPPARAMSTR_FILE_TO_DELETE_2  = 'FILE_TO_DELETE_2';
	HELPPARAMSTR_OLD_FILE_NAME     = 'OLD_FILE_NAME';
	HELPPARAMSTR_NEW_FILE_NAME     = 'NEW_FILE_NAME';
	HELPPARAMSTR_ARCHIVE_FILE_TYPE = 'ARCHIVE_FILE_TYPE';
	HELPPARAMSTR_ARCHIVE_FILE_2    = 'ARCHIVE_FILE_2';

	HELPPARAMSTR_FILE_TO_XX_X      = 'FILE_TO_XX_?';


procedure help;
begin
	WriteLn(format(HELPSTR_FirstLine, [MMARCHVERSION]));
	WriteLn;
	WriteLn('mmarch extract <' + HELPPARAMSTR_ARCHIVE_FILE + '> <' + HELPPARAMSTR_FOLDER + '> [' + HELPPARAMSTR_FILE_TO_EXTRACT_1 + '] [' + HELPPARAMSTR_FILE_TO_EXTRACT_2 + '] [...]');
	WriteLn('mmarch list <' + HELPPARAMSTR_ARCHIVE_FILE + '> [' + HELPPARAMSTR_SEPARATOR + ']');
	WriteLn('mmarch add <' + HELPPARAMSTR_ARCHIVE_FILE + '> <' + HELPPARAMSTR_FILE_TO_ADD_1 + '> [' + HELPPARAMSTR_FILE_TO_ADD_2 + '] [...]');
	WriteLn('mmarch delete <' + HELPPARAMSTR_ARCHIVE_FILE + '> <' + HELPPARAMSTR_FILE_TO_DELETE_1 + '> [' + HELPPARAMSTR_FILE_TO_DELETE_2 + '] [...]');
	WriteLn('mmarch rename <' + HELPPARAMSTR_ARCHIVE_FILE + '> <' + HELPPARAMSTR_OLD_FILE_NAME + '> <' + HELPPARAMSTR_NEW_FILE_NAME + '>');
	WriteLn('mmarch create <' + HELPPARAMSTR_ARCHIVE_FILE + '> <' + HELPPARAMSTR_ARCHIVE_FILE_TYPE + '> <' + HELPPARAMSTR_FOLDER + '> [' + HELPPARAMSTR_FILE_TO_ADD_1 + '] [' + HELPPARAMSTR_FILE_TO_ADD_2 + '] [...]');
	WriteLn('mmarch merge <' + HELPPARAMSTR_ARCHIVE_FILE + '> <' + HELPPARAMSTR_ARCHIVE_FILE_2 + '>');
	WriteLn('mmarch optimize <' + HELPPARAMSTR_ARCHIVE_FILE + '>');
	WriteLn('mmarch help');
	WriteLn;
	WriteLn(format(HELPSTR_ReqOpt, ['<>', '[]']));
	WriteLn;
	WriteLn('- ' + format(HELPSTR_Initial, ['e', 'extract']));
	WriteLn('- ' + HELPSTR_CaseInsensitive);
	WriteLn;
	WriteLn(HELPPARAMSTR_FOLDER + HELPPUNCTSTR_Colon);
	WriteLn('            .    ' + HELPSTR_CurrentFolder);
	WriteLn;
	WriteLn(HELPPARAMSTR_FILE_TO_XX_X + HELPPUNCTSTR_Colon);
	WriteLn('            *    ' + HELPSTR_AllFiles);
	WriteLn('          *.*    ' + HELPSTR_AllFiles);
	WriteLn('        *.txt    ' + HELPSTR_AllFilesWExt);
	WriteLn('           *.    ' + HELPSTR_AllFilesWOExt);
	WriteLn;
	WriteLn(format(HELPSTR_ReadDetails, [MMARCHURL]));
end;


function trimCharLeft(str: string; charToTrim: string): string;
var
	n: integer;
begin
	n := 1;
	while (n <= Length(str)) and (str[n] = charToTrim) do
		Inc(n);
	SetString(Result, PChar(@str[n]), Length(str) - n + 1);
end;


function wildCardFileNameToExt(fileName: string): string;
begin
	if (fileName = '*') or (fileName = '*.*') then // all files
		Result := '*'
	else
	begin
		if fileName = '*.' then // all files without extension
			Result := ''
		else // all files with specified extension
			Result := trimCharLeft(fileName, '*');
	end;
end;


procedure extract;
var
	archSimp: MMArchSimple;
	folder, fileName: string;
	i: integer;
begin

	folder := ParamStr(3);
	if folder = '' then
		raise MissingParamException.Create(NeedFolder);

	archSimp := MMArchSimple.load(archiveFile);

	fileName := ParamStr(4);
	if fileName = '' then // FILE_TO_EXTRACT_1 is empty, extract all
		archSimp.extractAll(folder);

	for i := 4 to ParamCount do
	begin
		fileName := ParamStr(i);
		if fileName <> '' then
		begin
			if Pos('*', fileName) > 0 then
				archSimp.extractAll(folder, wildCardFileNameToExt(fileName))
			else
				archSimp.extract(folder, fileName);
		end;
	end;
end;


procedure list;
var
	archSimp: MMArchSimple;
	separator: string;
begin
	separator := ParamStr(3);
	if separator = '' then
		separator := #13#10;

	archSimp := MMArchSimple.load(archiveFile);

	Write(archSimp.list(separator));
end;


procedure addProc(archSimp: MMArchSimple; paramIndexFrom: integer);
var
	filePath, ext, folder, fileName: string;
	i, paletteIndex: integer;
begin
	i := paramIndexFrom;
	while i <= ParamCount do
	begin
		filePath := ParamStr(i);
		if filePath <> '' then // has file to add
		begin

			folder := ExtractFilePath(filePath);
			fileName := ExtractFileName(filePath);
			ext := ExtractFileName(fileName);

			if Pos('*', fileName) > 0 then
				archSimp.addAll(folder, wildCardFileNameToExt(fileName))
			else
			begin
				if (i <= ParamCount - 2) and (SameText(ParamStr(i + 1), '/p')) then // pal specified
				begin
					paletteIndex := strtoint(ParamStr(i + 2));
					archSimp.add(filePath, paletteIndex);
					i := i + 2;
				end
				else
					archSimp.add(filePath);
			end;

		end;
		i := i + 1; // increment no matter what
	end;
end;


procedure add;
var
	filePath: string;
	archSimp: MMArchSimple;
begin
	filePath := ParamStr(3);
	if filePath = '' then
		raise MissingParamException.Create(NeedFileToAdd);
	archSimp := MMArchSimple.load(archiveFile);
	addProc(archSimp, 3);
end;

procedure delete;
var
	fileName: string;
	archSimp: MMArchSimple;
	i: integer;
begin
	fileName := ParamStr(3);
	if fileName = '' then
		raise MissingParamException.Create(NeedFileToDelete);
	archSimp := MMArchSimple.load(archiveFile);

	for i := 3 to ParamCount do
	begin
		fileName := ParamStr(i);
		if fileName <> '' then
		begin
			if Pos('*', fileName) > 0 then
				archSimp.deleteAll(wildCardFileNameToExt(fileName))
			else
				archSimp.delete(fileName);
		end;
	end;
end;


procedure rename;
var
	oldFileName, newFileName: string;
	archSimp: MMArchSimple;
begin
	oldFileName := ParamStr(3);
	newFileName := ParamStr(4);
	if (oldFileName = '') or (newFileName = '') then
		raise MissingParamException.Create(NeedFilesToRename);
	archSimp := MMArchSimple.load(archiveFile);
	archSimp.rename(oldFileName, newFileName);
end;


procedure create;
var
	archiveFileType, folder: string;
	archSimp: MMArchSimple;
begin
	archiveFileType := ParamStr(3);
	folder := ParamStr(4);
	if (archiveFileType = '') or (folder = '') then
		raise MissingParamException.Create(NeedArchiveFileTypeAndFolder);

	archSimp := MMArchSimple.create;
	archSimp.new(archiveFile, archiveFileType, folder);
	addProc(archSimp, 5);
end;


procedure merge;
var
	archSimp: MMArchSimple;
	archiveFile2: string;
begin
	archiveFile2 := ParamStr(3);
	if archiveFile2 = '' then
		raise MissingParamException.Create(NeedArchiveFilesToMerge);

	archSimp := MMArchSimple.load(archiveFile);
	archSimp.merge(archiveFile2);
end;


procedure optimize;
var
	archSimp: MMArchSimple;
begin
	archSimp := MMArchSimple.load(archiveFile);
	archSimp.optimize;
end;


begin

	try

		method := trimCharLeft(ParamStr(1), '-');
		methodNumber := AnsiIndexStr(method, ['extract', 'e', 'list', 'l',
			'add', 'a', 'delete', 'd', 'rename', 'r', 'create', 'c', 'merge', 'm',
			'optimize', 'o', 'help', 'h', '']);
		archiveFile := ParamStr(2);

		if (archiveFile = '') And (methodNumber < 16) And (methodNumber >= 0) then // < 16: method is not `help`
			raise MissingParamException.Create(NeedArchiveFile);

		Case methodNumber of
			 0,  1: extract;
			 2,  3: list;
			 4,  5: add;
			 6,  7: delete;
			 8,  9: rename;
			10, 11: create;
			12, 13: merge;
			14, 15: optimize;
			16, 17, 18: help;
		else // -1: not found in the array
			raise MissingParamException.CreateFmt(UnknownMethod, [method]);
		end;

	except
		on E: MissingParamException do
		begin
			WriteLn('Error: ' + E.Message);
			WriteLn;
			WriteLn('----------------------------------------');
			WriteLn;
			help;
		end;
		on E: OtherMmarchException do
			WriteLn('Error: ' + E.Message);
		on E: Exception do
			WriteLn(E.Message);
	end;

end.
