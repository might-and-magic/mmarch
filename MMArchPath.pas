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
	Windows, SysUtils, Classes, RSQ;

type
	OtherMmarchException = class(Exception);


function getAllFilesInFolder(path: string; ext: string = '*'; isDir: boolean = false): TStringList;

procedure addAllFilesToFileList(var fileList: TStringList; path: string; recursive: integer; isDir: boolean; usePathFilenamePair: boolean; extList: TStringList); overload;
procedure addAllFilesToFileList(var fileList: TStringList; path: string; recursive: integer; isDir: boolean; usePathFilenamePair: boolean); overload;

function beautifyPath(oldStr: String): string;
function withTrailingSlash(path: string): string;

procedure createDirRecur(dir: string);
procedure copyFile0(oldFile, newFile: string);
function createEmptyFile(filePath: string): boolean;

const
	nameValSeparator: char = ':';

	{$IFDEF MSWINDOWS}
		slash: char = '\';
	{$ELSE}
		slash: char = '/';
	{$ENDIF}

resourcestring
	DirNotFound = 'Directory %s is not found';


implementation


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
// `extList`: file/dir extension list
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
	AssignFile(f, withTrailingSlash(currentDir) + filePath);
	{$I-}
	Rewrite(f);
	{$I+}
	Result := IOResult = 0;
	CloseFile(f);
end;


end.