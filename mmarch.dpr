// mmarch
// Command line tool to handle Heroes 3 and Might and Magic 6, 7, 8
// resource archive files (e.g. lod files). Based on GrayFace's MMArchive.
// By Tom CHEN <tomchen.org@gmail.com> (tomchen.org)
// MIT License
// https://github.com/might-and-magic/mmarch



program mmarch;

{$R *.res}
{$APPTYPE CONSOLE}

uses
	SysUtils, StrUtils, Classes, MMArchUnit, RSQ;

type
	MissingParamException = class(Exception);
	OtherMmarchException = class(Exception);

const
	MMARCHVERSION: string = '2.0';
	MMARCHURL: string = 'https://github.com/might-and-magic/mmarch';
	supportedExts: array[0..7] of string = ('.lod', '.pac', '.snd', '.vid', '.lwd', '.mm7', '.dod', '.mm6');
	nameValSeparator: char = ':';

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
	HELPSTR_ReqOpt          = '(`%s`: required; `%s`: optional; `%s`: or):';
	HELPSTR_Initial         = 'Initial letter of the first argument can be used (e.g. `%s` for `%s`)';
	HELPSTR_CaseInsensitive = 'File names are case-insensitive';
	HELPSTR_UseNotations    = 'You can use the following notations';
	HELPSTR_CurrentFolder   = 'current folder';
	HELPSTR_AllFiles        = 'all files';
	HELPSTR_AllFilesWExt    = 'all files with specified extension';
	HELPSTR_AllFilesWOExt   = 'all files without extension';
	HELPSTR_ReadDetails     = 'Read README.md file or go to the following page for more details and examples:';
	HELPSTR_Colon           = ': ';

	HELPSTR_BatchArchive      = '%s in `%s`';
	HELPSTR_FilePath          = 'File path:';
	HELPSTR_AllDirRecur       = 'all directories recursively';
	HELPSTR_AnyOneDir         = 'any ONE directory';

	HELPSTR_FileName          = 'File name at the end:';
	HELPSTR_AllArchive        = 'all supported archive files';
	HELPSTR_AllArchiveWExt    = 'all supported archive files with specified extension';
	HELPSTR_AllArchiveWAnyExt = 'all supported archive files with any of specified extensions';

	HELPPRM_ARCHIVE_FILE      = 'ARCHIVE_FILE';
	HELPPRM_FOLDER            = 'FOLDER';
	HELPPRM_FILE_TO_EXTRACT_1 = 'FILE_TO_EXTRACT_1';
	HELPPRM_FILE_TO_EXTRACT_2 = 'FILE_TO_EXTRACT_2';
	HELPPRM_SEPARATOR         = 'SEPARATOR';
	HELPPRM_FILE_TO_ADD_1     = 'FILE_TO_ADD_1';
	HELPPRM_FILE_TO_ADD_2     = 'FILE_TO_ADD_2';
	HELPPRM_FILE_TO_DELETE_1  = 'FILE_TO_DELETE_1';
	HELPPRM_FILE_TO_DELETE_2  = 'FILE_TO_DELETE_2';
	HELPPRM_OLD_FILE_NAME     = 'OLD_FILE_NAME';
	HELPPRM_NEW_FILE_NAME     = 'NEW_FILE_NAME';
	HELPPRM_ARCHIVE_FILE_TYPE = 'ARCHIVE_FILE_TYPE';
	HELPPRM_ARCHIVE_FILE_2    = 'ARCHIVE_FILE_2';

	HELPPRM_FILE_TO_XX_X      = 'FILE_TO_XX_?';


procedure help(short: boolean = false);
begin
	WriteLn(format(HELPSTR_FirstLine, [MMARCHVERSION]));
	WriteLn;
	WriteLn('mmarch extract <' + HELPPRM_ARCHIVE_FILE + '> <' + HELPPRM_FOLDER + '> [' + HELPPRM_FILE_TO_EXTRACT_1 + '] [' + HELPPRM_FILE_TO_EXTRACT_2 + '] [...]');
	WriteLn('mmarch list <' + HELPPRM_ARCHIVE_FILE + '> [' + HELPPRM_SEPARATOR + ']');
	WriteLn('mmarch add <' + HELPPRM_ARCHIVE_FILE + '> <' + HELPPRM_FILE_TO_ADD_1 + '> [' + HELPPRM_FILE_TO_ADD_2 + '] [...]');
	WriteLn('mmarch delete <' + HELPPRM_ARCHIVE_FILE + '> <' + HELPPRM_FILE_TO_DELETE_1 + '> [' + HELPPRM_FILE_TO_DELETE_2 + '] [...]');
	WriteLn('mmarch rename <' + HELPPRM_ARCHIVE_FILE + '> <' + HELPPRM_OLD_FILE_NAME + '> <' + HELPPRM_NEW_FILE_NAME + '>');
	WriteLn('mmarch create <' + HELPPRM_ARCHIVE_FILE + '> <' + HELPPRM_ARCHIVE_FILE_TYPE + '> <' + HELPPRM_FOLDER + '> [' + HELPPRM_FILE_TO_ADD_1 + '] [' + HELPPRM_FILE_TO_ADD_2 + '] [...]');
	WriteLn('mmarch merge <' + HELPPRM_ARCHIVE_FILE + '> <' + HELPPRM_ARCHIVE_FILE_2 + '>');
	WriteLn('mmarch optimize <' + HELPPRM_ARCHIVE_FILE + '>');
	WriteLn('mmarch help');

	if not short then
	begin
		WriteLn;
		WriteLn(format(HELPSTR_ReqOpt, ['<>', '[]', '|']));
		WriteLn;
		WriteLn('- ' + format(HELPSTR_Initial, ['e', 'extract']));
		WriteLn('- ' + HELPSTR_CaseInsensitive);
		WriteLn('- ' + HELPSTR_UseNotations);
		WriteLn;
		WriteLn(HELPPRM_FOLDER + HELPSTR_Colon);
		WriteLn('            .    ' + HELPSTR_CurrentFolder);
		WriteLn;
		WriteLn(HELPPRM_FILE_TO_XX_X + HELPSTR_Colon);
		WriteLn('*.*   |     *    ' + HELPSTR_AllFiles);
		WriteLn('        *.txt    ' + HELPSTR_AllFilesWExt);
		WriteLn('           *.    ' + HELPSTR_AllFilesWOExt);
		WriteLn;
		WriteLn(format(HELPSTR_BatchArchive, [HELPPRM_ARCHIVE_FILE, 'mmarch extract']) + HELPSTR_Colon);
		WriteLn(HELPSTR_FilePath);
		WriteLn('           **    ' + HELPSTR_AllDirRecur);
		WriteLn('            *    ' + HELPSTR_AnyOneDir);
		WriteLn(HELPSTR_FileName);
		WriteLn('*.*   |     *    ' + HELPSTR_AllArchive);
		WriteLn('        *.lod    ' + HELPSTR_AllArchiveWExt);
		WriteLn('*.lod|lwd|vid    ' + HELPSTR_AllArchiveWAnyExt);
		WriteLn;
		WriteLn(HELPSTR_ReadDetails);
		WriteLn(MMARCHURL);
	end;
end;


procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
	ListOfStrings.Clear;
	ListOfStrings.Delimiter       := Delimiter;
	ListOfStrings.StrictDelimiter := True;
	ListOfStrings.DelimitedText   := Str;
end;


function trimCharLeft(str: string; charToTrim: char): string;
var
	n: integer;
begin
	n := 1;
	while (n <= Length(str)) and (str[n] = charToTrim) do
		Inc(n);
	SetString(Result, PChar(@str[n]), Length(str) - n + 1);
end;


function trimCharsRight(str: string; charToTrim: char; char2ToTrim: char): string;
var
	n: integer;
begin
	n := Length(str);
	while (n >= 1) and ((str[n] = charToTrim) or (str[n] = char2ToTrim)) do
		Dec(n);
	SetString(Result, PChar(@str[1]), n);
end;


function stringsToStringList(const Strings: array of string): TStringList;
var
	i: Integer;
begin
	Result := TStringList.Create;
	for i := low(Strings) to high(Strings) do
		Result.Add(Strings[i]);
end;


function wildcardFileNameToExt(fileName: string): string;
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


function fileNameToExtList(fileName: string): TStringList;
var
	ext, extTemp: string;
	ResultTemp: TStringList;
begin
	ext := ExtractFileExt(fileName);
	if (fileName = '*') or (fileName = '*.*') then
		Result := stringsToStringList(supportedExts)
	else
	begin
		Result := TStringList.Create;
		if Pos('|', archiveFile) > 0 then
		begin
			ResultTemp := TStringList.Create;
			Split('|', trimCharLeft(ext, '.'), ResultTemp);
			for extTemp in ResultTemp do
				if MatchStr('.' + AnsiLowerCase(extTemp), supportedExts) then
					Result.Add('.' + AnsiLowerCase(extTemp));
			ResultTemp.Free;
		end
		else
			Result.Add(ext);
	end;
end;


procedure addAllExtFilesToFileList(path: string; extList: TStringList; var fileList: TStringList);
var
	ext, fileName: string;
begin
	for ext in extList do
		for fileName in getAllFilesInFolder(path, ext) do
			fileList.Add(fileName + nameValSeparator + path);
end;


procedure addFilesInAllDirsToFileList(path: string; extList: TStringList; recursive: boolean; var fileList: TStringList);
var
	dirListTemp: TStringList;
	dir: string;
begin
	if recursive = true then
		addAllExtFilesToFileList(path, extList, fileList);

	dirListTemp := getAllFilesInFolder(path, '<DIR>');

	for dir in dirListTemp do
		if recursive = true then
		begin
			addFilesInAllDirsToFileList(withTrailingSlash(path) + dir, extList, true, fileList);
		end
		else
		begin
			addAllExtFilesToFileList(dir, extList, fileList);
		end;

	dirListTemp.Free;
end;


function wildcardArchiveNameToArchiveList(archiveName: string): TStringList;
var
	path, fileName, pathRightAst, pathTemp: string;
	extList: TStringList;
begin

	path := trimCharsRight(ExtractFilePath(archiveName), '\', '/');
	fileName := ExtractFileName(archiveName);

	extList := fileNameToExtList(fileName);

	Result := TStringList.Create;
	Result.Clear;
	Result.NameValueSeparator := nameValSeparator;

	pathRightAst := copy(path, length(path)-1, 2);
	if pathRightAst = '**' then
	begin
		pathTemp := copy(path, 1, length(path)-2); // part of path where pathRightAst is substracted
		addFilesInAllDirsToFileList(pathTemp, extList, true, Result);
	end
	else
	begin
		pathRightAst := copy(path, length(path), 1);
		if pathRightAst = '*' then
		begin
			pathTemp := copy(path, 1, length(path)-1);
			addFilesInAllDirsToFileList(pathTemp, extList, false, Result);
		end
		else
			addAllExtFilesToFileList(path, extList, Result);
	end;
	extList.Free;
end;


procedure extract;
var
	archSimp: MMArchSimple;
	extractToBaseFolder, extractToFolder, archiveFileFolder, fileName: string;
	i, j: integer;
	archiveFileList: TStringList; // archive file name - archive file path pair
begin

	extractToBaseFolder := ParamStr(3);
	if extractToBaseFolder = '' then
		raise MissingParamException.Create(NeedFolder);
	extractToFolder := extractToBaseFolder;

	if Pos('*', archiveFile) > 0 then
		archiveFileList := wildcardArchiveNameToArchiveList(archiveFile)
	else
	begin
		archiveFileList := TStringList.Create;
		archiveFileList.Clear;
		archiveFileList.NameValueSeparator := nameValSeparator;
		archiveFileList.Add(archiveFile + nameValSeparator + '.');
	end;

	for j := 0 to archiveFileList.Count - 1 do
	begin
		archiveFileFolder := archiveFileList.ValueFromIndex[j];
		archSimp := MMArchSimple.load(withTrailingSlash(archiveFileFolder) + archiveFileList.Names[j]);
		fileName := ParamStr(4);

		if Pos('*', archiveFile) > 0 then
			extractToFolder := withTrailingSlash(extractToBaseFolder)
			+ withTrailingSlash(archiveFileFolder)
			+ AnsiReplaceStr(archiveFileList.Names[j], '.', '_');

		if fileName = '' then // FILE_TO_EXTRACT_1 is empty, extract all
			archSimp.extractAll(extractToFolder);

		for i := 4 to ParamCount do
		begin
			fileName := ParamStr(i);
			if fileName <> '' then
			begin
				if Pos('*', fileName) > 0 then
					archSimp.extractAll(extractToFolder, wildcardFileNameToExt(fileName))
				else
				begin
					try // the individual file will be skipped if it gets an exception
						archSimp.extract(extractToFolder, fileName);
					except
						on E: OtherMmarchException do
						begin
							WriteLn(format(FileInArchiveErrorStr, [fileName, archiveFileList.Names[j]]));
							WriteLn(E.Message);
						end;
						on E: Exception do
						begin
							WriteLn(format(FileInArchiveErrorStr, [fileName, archiveFileList.Names[j]]));
							WriteLn(E.Message);
						end;
					end;
				end;
			end;
		end;
	end;

	archiveFileList.Free;

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
				archSimp.addAll(folder, wildcardFileNameToExt(fileName))
			else
			begin
				if (i <= ParamCount - 2) and (SameText(ParamStr(i + 1), '/p')) then // pal specified
				begin
					paletteIndex := strtoint(ParamStr(i + 2));

					try // the individual file will be skipped if it gets an exception
						archSimp.add(filePath, paletteIndex);
					except
						on E: OtherMmarchException do
							WriteLn(format(FileErrorStr, [filePath, E.Message]));
						on E: Exception do
							WriteLn(format(FileErrorStr, [filePath, E.Message]));
					end;

					i := i + 2;
				end
				else
				begin
					try // the individual file will be skipped if it gets an exception
						archSimp.add(filePath);
					except
						on E: OtherMmarchException do
							WriteLn(format(FileErrorStr, [filePath, E.Message]));
						on E: Exception do
							WriteLn(format(FileErrorStr, [filePath, E.Message]));
					end;
				end;
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
				archSimp.deleteAll(wildcardFileNameToExt(fileName))
			else
			begin
				try // the individual file will be skipped if it gets an exception
					archSimp.delete(fileName);
				except
					on E: OtherMmarchException do
						WriteLn(format(FileErrorStr, [fileName, E.Message]));
					on E: Exception do
						WriteLn(format(FileErrorStr, [fileName, E.Message]));
				end;
			end;
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
			WriteLn(format(ErrorStr, [E.Message]));
			WriteLn;
			help(true);
		end;
		on E: OtherMmarchException do
			WriteLn(format(ErrorStr, [E.Message]));
		on E: Exception do
			WriteLn(E.Message);
	end;

end.
