// MMArchPath unit and MMArchSimple class
// Part of mmarch
// Command line tool to handle Heroes 3 and Might and Magic 6, 7, 8
// resource archive files (e.g. lod files). Based on GrayFace's MMArchive.
// By Tom CHEN <tomchen.org@gmail.com> (tomchen.org)
// MIT License
// https://github.com/might-and-magic/mmarch



unit MMArchPath;

interface

uses
	Windows, SysUtils, StrUtils, Classes, RSQ, ShellApi;

type
	OtherMmarchException = class(Exception);

procedure Split(Delimiter: Char; Str: string; TStr: TStrings);
function trimCharLeft(str: string; charToTrim: char): string;
function trimCharsRight(str: string; charToTrim, char2ToTrim: char): string;
function wildcardFileNameToExt(fileName: string): string;
function wildcardArchiveNameToArchiveList(archiveName: string): TStringList;

function getAllFilesInFolder(path: string; ext: string = '*'; isDir: boolean = false): TStringList;

procedure addAllFilesToFileList(var fileList: TStringList; path: string; recursive: integer; isDir: boolean; usePathFilenamePair: boolean; extList: TStringList); overload;
procedure addAllFilesToFileList(var fileList: TStringList; path: string; recursive: integer; isDir: boolean; usePathFilenamePair: boolean); overload;

function beautifyPath(oldStr: String): string;
function withTrailingSlash(path: string): string;

procedure createDirRecur(dir: string);
procedure copyFile0(oldFile, newFile: string);
function createEmptyFile(filePath: string): boolean;
procedure StrToFile(filePath, SourceString: string);
function moveDir(folderFrom, folderTo: string): Boolean;

const
	nameValSeparator: char = ':';
	archResSeparator: char = ':';

	supportedExts: array[0..7] of string = ('.lod', '.pac', '.snd', '.vid', '.lwd', '.mm7', '.dod', '.mm6');

	{$IFDEF MSWINDOWS}
		slash: char = '\';
	{$ELSE}
		slash: char = '/';
	{$ENDIF}

resourcestring
	DirNotFound = 'Directory %s is not found';


implementation


procedure Split(Delimiter: Char; Str: string; TStr: TStrings);
begin
	TStr.Clear;
	TStr.Delimiter       := Delimiter;
	TStr.StrictDelimiter := True;
	TStr.DelimitedText   := Str;
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


function trimCharsRight(str: string; charToTrim, char2ToTrim: char): string;
var
	n: integer;
begin
	n := Length(str);
	while (n >= 1) and ((str[n] = charToTrim) or (str[n] = char2ToTrim)) do
		Dec(n);
	SetString(Result, PChar(@str[1]), n);
end;


// private
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


// private
function fileNameToExtList(fileName: string): TStringList; // Ext in Result has dot
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
		if Pos('|', fileName) > 0 then
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
		addAllFilesToFileList(Result, pathTemp, 1, false, true, extList);
	end
	else
	begin
		pathRightAst := copy(path, length(path), 1);
		if pathRightAst = '*' then
		begin
			pathTemp := copy(path, 1, length(path)-1);
			addAllFilesToFileList(Result, pathTemp, 2, false, true, extList);
		end
		else
			addAllFilesToFileList(Result, path, 3, false, true, extList);
	end;
	extList.Free;
end;


function getAllFilesInFolder(path: string; ext: string = '*'; isDir: boolean = false): TStringList;
// `ext` :
// '*': all files or all directory
// '': without extension, works no matter isDir or not
// (other string with dot): extension filter, works no matter isDir or not
var
	searchResult: TSearchRec;
	fileMask: string;
	attr: integer;
	currentDir: array[0 .. MAX_PATH] of char;
begin
	Result := TStringList.Create;

	if ext = '*' then // all files or all directory
		fileMask := '*'
	else
	begin
		if (ext = '') then // without extension, works no matter isDir or not
			fileMask := '*.'
			// '*.' will match `.gitignore` (no stem) and `LICENSE` (no ext)
			// because they are seen as `.gitignore.` and `LICENSE.`
			// so we'll have to check against its real extension it later
		else
		begin
			fileMask := '*' + ext;
		end;
	end;

	if isDir then
		attr := faDirectory
	else
		attr := faAnyFile;

	if path = '' then // DirectoryExists('') = false, so we need this
		path := '.';

	GetCurrentDirectory(MAX_PATH, currentDir);
	if not DirectoryExists(path) then
		raise OtherMmarchException.CreateFmt(DirNotFound, [path]);

	SetCurrentDirectory(PChar(path));

	if findfirst(fileMask, attr, searchResult) = 0 then
	begin
		repeat
			if ( (
				(not isDir) and
				FileExists(searchResult.Name)
			) or (
				isDir and
				( (searchResult.attr and faDirectory) = faDirectory ) and
				(searchResult.Name <> '.') and
				(searchResult.Name <> '..')
			) ) and (
				(fileMask <> '*.') or
				( (fileMask = '*.') and (ExtractFileExt(searchResult.Name) = '') )
			)
			then
				Result.Add(searchResult.Name);
		until FindNext(searchResult) <> 0;
		SysUtils.FindClose(searchResult);
	end;
	SetCurrentDirectory(currentDir);
end;


// private
procedure addAllFilesToFileListNonRecur(var fileList: TStringList; path: string; isDir: boolean; usePathFilenamePair: boolean; extList: TStringList); overload;
var
	ext, fileName: string;
begin
	for ext in extList do
		for fileName in getAllFilesInFolder(path, ext, isDir) do
		if usePathFilenamePair then
			fileList.Add(fileName + nameValSeparator + beautifyPath(path))
		else
			fileList.Add(withTrailingSlash(beautifyPath(path)) + fileName);
end;


// private
procedure addAllFilesToFileListNonRecur(var fileList: TStringList; path: string; isDir: boolean; usePathFilenamePair: boolean); overload;
var
	extList: TStringList;
begin
	extList := TStringList.Create;
	extList.Add('*');
	addAllFilesToFileListNonRecur(fileList, path, isDir, usePathFilenamePair, extList);
end;


procedure addAllFilesToFileList(var fileList: TStringList; path: string; recursive: integer; isDir: boolean; usePathFilenamePair: boolean; extList: TStringList); overload;
// `recursive`:
// `1`: recursive
// `2`: in level 1 folders only
// `3`: current folder only
// `isDir`: directory or file
// `extList`: file/dir extension list, ext has dot
var
	dirListTemp: TStringList;
	dir: string;
begin

	if (recursive = 1) or (recursive = 3) then
		addAllFilesToFileListNonRecur(fileList, path, isDir, usePathFilenamePair, extList);

	if recursive <> 3 then
	begin

		dirListTemp := getAllFilesInFolder(path, '*', true);

		for dir in dirListTemp do
			if recursive = 1 then
				addAllFilesToFileList(fileList, withTrailingSlash(path) + dir, 1, isDir, usePathFilenamePair, extList)
			else
				if recursive = 2 then
					addAllFilesToFileListNonRecur(fileList, withTrailingSlash(path) + dir, isDir, usePathFilenamePair, extList);

		dirListTemp.Free;

	end;

end;


procedure addAllFilesToFileList(var fileList: TStringList; path: string; recursive: integer; isDir: boolean; usePathFilenamePair: boolean); overload;
var
	extList: TStringList;
begin
	extList := TStringList.Create;
	extList.Add('*');

	addAllFilesToFileList(fileList, path, recursive, isDir, usePathFilenamePair, extList);
end;


function beautifyPath(oldStr: String): string;
var
	i, start: integer;
	currentChar: char;
begin
	if length(oldStr) > 0 then
	begin
		if oldStr[1] = '.' then
		begin
			Result := '.';
			start := 2;
		end
		else
		begin
			Result := '';
			start := 1;
		end;

		for i := start to length(oldStr) do
		begin
			currentChar := oldStr[i];
			if (currentChar = '\') or (currentChar = '/') then
				begin
					if (Result = '') or not (Result[length(Result)] = slash) then
						Result := Result + slash;
				end
			else
			begin
				Result := Result + currentChar;
			end;
		end;
		if System.Copy(Result, 0, 2) = ('.' + slash) then
		System.Delete(Result, 1, 2);
	end
	else
		Result := '';
end;


function withTrailingSlash(path: string): string;
begin
	if path <> '' then
		Result := IncludeTrailingPathDelimiter(path)
	else
		Result := path;
end;


procedure createDirRecur(dir: string);
var
	currentDir, absDir: string;
begin
	currentDir := GetCurrentDir;
	absDir := withTrailingSlash(currentDir) + beautifyPath(dir);
	if not DirectoryExists(absDir) then
		ForceDirectories(absDir);
end;


procedure copyFile0(oldFile, newFile: string);
var
	currentDir: string;
begin
	currentDir := GetCurrentDir;
	createDirRecur(ExtractFilePath(newFile));
	CopyFile(
		PAnsiChar(withTrailingSlash(currentDir) + beautifyPath(oldFile)),
		PAnsiChar(withTrailingSlash(currentDir) + beautifyPath(newFile)),
		false
	);
end;


function createEmptyFile(filePath: string): boolean;
var
	currentDir: string;
	f: textfile;
begin
	currentDir := GetCurrentDir;
	createDirRecur(ExtractFilePath(filePath));
	AssignFile(f, withTrailingSlash(currentDir) + beautifyPath(filePath));
	{$I-}
	Rewrite(f);
	{$I+}
	Result := IOResult = 0;
	CloseFile(f);
end;


procedure StrToFile(filePath, SourceString: string);
var
	currentDir: string;
	Stream: TFileStream;
begin
	currentDir := GetCurrentDir;
	createDirRecur(ExtractFilePath(filePath));
	Stream := TFileStream.Create(withTrailingSlash(currentDir) + beautifyPath(filePath), fmCreate);
	try
		Stream.WriteBuffer(Pointer(SourceString)^, Length(SourceString));
	finally
		Stream.Free;
	end;
end;


function moveDir(folderFrom, folderTo: string): Boolean;
// folderFrom, folderTo cannot be current folder or ancestor folder
var
	currentDir: string;
	fos: TSHFileOpStruct;
begin
	currentDir := GetCurrentDir;
	folderFrom := trimCharsRight(beautifyPath(folderFrom), '\', '/');
	folderTo := trimCharsRight(beautifyPath(folderTo), '\', '/');

	if folderFrom <> folderTo then
	begin
		createDirRecur(ExtractFilePath(folderTo));
		ZeroMemory(@fos, SizeOf(fos));
		with fos do
		begin
			wFunc  := FO_MOVE;
			fFlags := FOF_FILESONLY;
			pFrom  := PChar(withTrailingSlash(currentDir) + folderFrom + #0);
			pTo    := PChar(withTrailingSlash(currentDir) + folderTo);
		end;
		Result := (0 = ShFileOperation(fos));
	end
	else
	begin
		Result := false;
	end;

end;


end.