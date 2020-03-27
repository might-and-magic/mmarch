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
	Windows, SysUtils, StrUtils, Classes, RSLod, MMArchMain, MMArchPath, RSQ;

type
	MissingParamException = class(Exception);
	OtherMmarchException = class(Exception);

const
	MMARCHVERSION: string = '3.0';
	MMARCHURL: string = 'https://github.com/might-and-magic/mmarch';
	supportedExts: array[0..7] of string = ('.lod', '.pac', '.snd', '.vid', '.lwd', '.mm7', '.dod', '.mm6');

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
	InsufficientParameters       = 'Insufficient parameters';
	NeedArchiveFile              = 'You must specify an archive file';
	UnknownMethod                = 'Unknown method: %s';
	UnknownCompareOption         = 'Unknown compare option: %s';

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

	HELPSTR_BatchArchive             = '%s in `%s`';
	HELPSTR_FilePath                 = 'File path:';
	HELPSTR_AllDirRecur              = 'all directories recursively';
	HELPSTR_AnyOneDir                = 'any ONE directory';

	HELPSTR_FileName                 = 'File name at the end:';
	HELPSTR_AllArchive               = 'all supported archive files';
	HELPSTR_AllArchiveWExt           = 'all supported archive files with specified extension';
	HELPSTR_AllArchiveWAnyExt        = 'all supported archive files with any of specified extensions';

	HELPPRM_ARCHIVE_FILE             = 'ARCHIVE_FILE';
	HELPPRM_FOLDER                   = 'FOLDER';
	HELPPRM_FILE_TO_EXTRACT_1        = 'FILE_TO_EXTRACT_1';
	HELPPRM_FILE_TO_EXTRACT_2        = 'FILE_TO_EXTRACT_2';
	HELPPRM_SEPARATOR                = 'SEPARATOR';
	HELPPRM_FILE_TO_ADD_1            = 'FILE_TO_ADD_1';
	HELPPRM_FILE_TO_ADD_2            = 'FILE_TO_ADD_2';
	HELPPRM_FILE_TO_DELETE_1         = 'FILE_TO_DELETE_1';
	HELPPRM_FILE_TO_DELETE_2         = 'FILE_TO_DELETE_2';
	HELPPRM_OLD_FILE_NAME            = 'OLD_FILE_NAME';
	HELPPRM_NEW_FILE_NAME            = 'NEW_FILE_NAME';
	HELPPRM_ARCHIVE_FILE_TYPE        = 'ARCHIVE_FILE_TYPE';
	HELPPRM_ARCHIVE_FILE_2           = 'ARCHIVE_FILE_2';
	HELPPRM_ARCHIVE_FILE_OR_FOLDER   = 'ARCHIVE_FILE_OR_FOLDER';
	HELPPRM_ARCHIVE_FILE_OR_FOLDER_2 = 'ARCHIVE_FILE_OR_FOLDER_2';
	HELPPRM_DIFF_FILE_FOLDER         = 'DIFF_FILE_FOLDER';
	HELPPRM_DIFF_FILE_FOLDER_NAME    = 'DIFF_FILE_FOLDER_NAME';
	HELPPRM_OLD_DIFF_FILE_FOLDER     = 'OLD_DIFF_FILE_FOLDER';
	HELPPRM_SCRIPT_FOLDER            = 'SCRIPT_FOLDER';
	HELPPRM_FILE_TO_XX_X             = 'FILE_TO_XX_?';


procedure colorWriteLn(str: string; color: word);
var
	ConOut: THandle;
	BufInfo: TConsoleScreenBufferInfo;
begin
	ConOut := TTextRec(Output).Handle;
	GetConsoleScreenBufferInfo(ConOut, BufInfo);
	SetConsoleTextAttribute(TTextRec(Output).Handle, color);
	WriteLn(str);
	SetConsoleTextAttribute(ConOut, BufInfo.wAttributes);
end;


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
	WriteLn('mmarch compare <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER + '> <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER_2 + '>');
	WriteLn('mmarch compare <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER + '> <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER_2 + '> {nsis|batch} <' + HELPPRM_SCRIPT_FOLDER + '> <' + HELPPRM_DIFF_FILE_FOLDER_NAME + '>');
	WriteLn('mmarch compare <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER + '> <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER_2 + '> filesonly <' + HELPPRM_DIFF_FILE_FOLDER + '>');
	WriteLn('mmarch compare-files-to-{nsis|batch} <' + HELPPRM_OLD_DIFF_FILE_FOLDER + '> <' + HELPPRM_SCRIPT_FOLDER + '> <' + HELPPRM_DIFF_FILE_FOLDER_NAME + '>');
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


procedure Split(Delimiter: Char; Str: string; TStr: TStrings) ;
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

		try // the individual archive file will be skipped if it gets an exception

			archiveFileFolder := archiveFileList.ValueFromIndex[j];
			archSimp := MMArchSimple.load(withTrailingSlash(archiveFileFolder) + archiveFileList.Names[j]);
			fileName := ParamStr(4);

			if Pos('*', archiveFile) > 0 then
				extractToFolder := withTrailingSlash(extractToBaseFolder)
				+ withTrailingSlash(archiveFileFolder)
				+ archiveFileList.Names[j] + '.' + 'mmarchive';

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
						try // the individual resource file will be skipped if it gets an exception
							archSimp.extract(extractToFolder, fileName);
						except
							on E: OtherMmarchException do
							begin
								WriteLn(format(FileInArchiveErrorStr, [beautifyPath(fileName), beautifyPath(archiveFileList.Names[j])]));
								WriteLn(E.Message);
							end;
							on E: Exception do
							begin
								WriteLn(format(FileInArchiveErrorStr, [beautifyPath(fileName), beautifyPath(archiveFileList.Names[j])]));
								WriteLn(E.Message);
							end;
						end;
					end;
				end;
			end;

		except
			on E: OtherMmarchException do
			begin
				WriteLn(format(ArchiveFileErrorStr, [beautifyPath(archiveFileList.Names[j])]));
				WriteLn(E.Message);
			end;
			on E: Exception do
			begin
				WriteLn(format(ArchiveFileErrorStr, [beautifyPath(archiveFileList.Names[j])]));
				WriteLn(E.Message);
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

					try // the individual resource file will be skipped if it gets an exception
						archSimp.add(filePath, paletteIndex);
					except
						on E: OtherMmarchException do
							WriteLn(format(FileErrorStr, [beautifyPath(filePath), E.Message]));
						on E: Exception do
							WriteLn(format(FileErrorStr, [beautifyPath(filePath), E.Message]));
					end;

					i := i + 2;
				end
				else
				begin
					try // the individual resource file will be skipped if it gets an exception
						archSimp.add(filePath);
					except
						on E: OtherMmarchException do
							WriteLn(format(FileErrorStr, [beautifyPath(filePath), E.Message]));
						on E: Exception do
							WriteLn(format(FileErrorStr, [beautifyPath(filePath), E.Message]));
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
				try // the individual resource file will be skipped if it gets an exception
					archSimp.delete(fileName);
				except
					on E: OtherMmarchException do
						WriteLn(format(FileErrorStr, [beautifyPath(fileName), E.Message]));
					on E: Exception do
						WriteLn(format(FileErrorStr, [beautifyPath(fileName), E.Message]));
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


function compareFile(oldFile, newFile: string): boolean;
var
	memOld, memNew: TMemoryStream;
begin
	memOld := TMemoryStream.Create;
	memNew := TMemoryStream.Create;

	memOld.LoadFromFile(oldFile);
	memNew.LoadFromFile(newFile);

	Result := (memOld.Size = memNew.Size) and CompareMem(memOld.Memory, memNew.Memory, memOld.Size);
end;


function compareInArchiveFile(oldRaw, newRaw: TRSMMFiles; oldIndex, newIndex: integer): boolean;
// mostly from TForm1.DoCompare in Unit1.pas in GrayFace's LodCompare's source files
var
	r: TStream;
	mem1, mem2: TMemoryStream;

begin

	mem1 := TMemoryStream.Create;
	mem2 := TMemoryStream.Create;

	Result := (newRaw.UnpackedSize[newIndex] = oldRaw.UnpackedSize[oldIndex]);

	if Result then
	begin
		// raw compare
		Result := newRaw.Size[newIndex] = oldRaw.Size[oldIndex];
		if Result then
		begin
			mem1.SetSize(newRaw.Size[newIndex]);
			r := newRaw.GetAsIsFileStream(newIndex);
			try
				r.ReadBuffer(mem1.Memory^, mem1.Size);
			finally
				newRaw.FreeAsIsFileStream(newIndex, r);
			end;

			mem2.SetSize(newRaw.Size[newIndex]);
			r := oldRaw.GetAsIsFileStream(oldIndex);
			try
				r.ReadBuffer(mem2.Memory^, mem2.Size);
			finally
				oldRaw.FreeAsIsFileStream(oldIndex, r);
			end;

			Result := CompareMem(mem1.Memory, mem2.Memory, mem1.Size);
		end;
		// compare unpacked
		if not Result and (newRaw.IsPacked[newIndex] or oldRaw.IsPacked[oldIndex]) then
			try
				if newRaw.IsPacked[newIndex] then
				begin
					mem1.SetSize(newRaw.UnpackedSize[newIndex]);
					mem1.Position := 0;
					newRaw.RawExtract(newIndex, mem1);
				end;
				if oldRaw.IsPacked[oldIndex] then
				begin
					mem2.SetSize(oldRaw.UnpackedSize[oldIndex]);
					mem2.Position := 0;
					oldRaw.RawExtract(oldIndex, mem2);
				end;
				Result := (mem1.Size = mem2.Size) and CompareMem(mem1.Memory, mem2.Memory, mem1.Size);
			except
				Result := false;
			end;
	end;

end;


procedure compareArchive(oldArchive, newArchive: string; var addedFileList, modifiedFileList, deletedFileList: TStringList);
var
	oldArchi, newArchi: TRSMMArchive;
	oldFFiles, newFFiles: TRSMMFiles;
	oldFileNameList, oldFileNameListTemp: TStringList;
	i, nTemp: integer;
	elTemp: string;

begin

	oldArchi := RSLoadMMArchive(oldArchive);
	oldFFiles := oldArchi.RawFiles;
	oldFileNameList := TStringList.Create;

	for i := 0 to oldFFiles.Count - 1 do
		oldFileNameList.Add(oldFFiles.Name[i]);

	newArchi := RSLoadMMArchive(newArchive);
	newFFiles := newArchi.RawFiles;

	oldFileNameListTemp := TStringList.Create;
	oldFileNameListTemp.Assign(oldFileNameList);

	for i := 0 to newFFiles.Count - 1 do
	begin
		elTemp := newFFiles.Name[i];
		nTemp := oldFileNameList.IndexOf(elTemp);
		if nTemp = -1 then
			addedFileList.Add(elTemp)
		else
		begin
			oldFileNameList.Delete(nTemp);
			if not compareInArchiveFile(oldArchi.RawFiles, newArchi.RawFiles, oldFileNameListTemp.IndexOf(elTemp), i) then
				modifiedFileList.Add(elTemp);
		end;
	end;

	for elTemp in oldFileNameList do
		deletedFileList.Add(elTemp);

	oldFileNameList.Free;
	oldFileNameListTemp.Free;

end;


procedure generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList; isNsis: boolean);
// isNsis is true : NSIS
// isNsis is false: Batch
var
	elTemp: string;
begin

	writeln('deletedFolderList');
	for elTemp in deletedFolderList do
	begin
		writeln(elTemp);
	end;

	writeln('deletedNonResFileList');
	for elTemp in deletedNonResFileList do
	begin
		writeln(elTemp);
	end;

	writeln('deletedResFileList');
	for elTemp in deletedResFileList do
	begin
		writeln(elTemp);
	end;

	writeln('modifiedArchiveList');
	for elTemp in modifiedArchiveList do
	begin
		writeln(elTemp);
	end;

end;


procedure compareBase(oldArchiveOrFolder, newArchiveOrFolder, copyToFolder: string;
			var deletedFolderList0, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList); overload;
var
	oldFolderList,
	newFolderList,

	oldFileList,
	newFileList,

	addedFolderList,
	deletedFolderList,

	addedFileList,
	modifiedFileList,
	deletedFileList,

	addedFileListTemp,
	modifiedFileListTemp,
	deletedFileListTemp,
	
	allList: TStringList;

	elTemp, elTemp2, archResSeparator,
	oldArchivePath, newArchivePath, pathTemp: string;

	oldArchiveOrFolderLen, newArchiveOrFolderLen,

	i, nTemp: integer;

	color: word;

	archSimpNew: MMArchSimple;

begin
	archResSeparator := ':';

	oldArchiveOrFolderLen := length(withTrailingSlash(beautifyPath(oldArchiveOrFolder)));
	newArchiveOrFolderLen := length(withTrailingSlash(beautifyPath(newArchiveOrFolder)));

	oldFolderList := TStringList.Create;
	newFolderList := TStringList.Create;

	addAllFilesToFileList(oldFolderList, oldArchiveOrFolder, 1, true, false);
	addAllFilesToFileList(newFolderList, newArchiveOrFolder, 1, true, false);

	for i := 0 to oldFolderList.Count - 1 do // remove root from path
	begin
		elTemp := oldFolderList[i];
		System.Delete(elTemp, 1, oldArchiveOrFolderLen);
		oldFolderList[i] := elTemp;
	end;

	for i := 0 to newFolderList.Count - 1 do // remove root from path
	begin
		elTemp := newFolderList[i];
		System.Delete(elTemp, 1, newArchiveOrFolderLen);
		newFolderList[i] := elTemp;
	end;

	oldFileList := TStringList.Create;
	newFileList := TStringList.Create;

	addAllFilesToFileList(oldFileList, oldArchiveOrFolder, 1, false, false);
	addAllFilesToFileList(newFileList, newArchiveOrFolder, 1, false, false);

	for i := 0 to oldFileList.Count - 1 do // remove root from path
	begin
		elTemp := oldFileList[i];
		System.Delete(elTemp, 1, oldArchiveOrFolderLen);
		oldFileList[i] := elTemp;
	end;

	for i := 0 to newFileList.Count - 1 do // remove root from path
	begin
		elTemp := newFileList[i];
		System.Delete(elTemp, 1, newArchiveOrFolderLen);
		newFileList[i] := elTemp;
	end;

	addedFolderList := TStringList.Create;
	deletedFolderList := TStringList.Create;

	for elTemp in newFolderList do // add to added & modified list
	begin
		nTemp := oldFolderList.IndexOf(elTemp);
		if nTemp = -1 then
		begin
			addedFolderList.Add(elTemp);

			if copyToFolder <> '' then // if needs file copy
			begin
				createDirRecur(withTrailingSlash(copyToFolder) + elTemp);
			end;
		end
		else
			oldFolderList.Delete(nTemp);
	end;

	for elTemp in oldFolderList do // add to deleted list
	begin
		deletedFolderList.Add(elTemp);

		if copyToFolder <> '' then // if needs file copy
		begin
			createDirRecur(withTrailingSlash(copyToFolder) + elTemp + '.todelete');
		end;
	end;


	addedFileList := TStringList.Create;
	modifiedFileList := TStringList.Create;
	deletedFileList := TStringList.Create;

	for elTemp in newFileList do // add to added & modified list
	begin
		nTemp := oldFileList.IndexOf(elTemp);
		if nTemp = -1 then
		begin
			addedFileList.Add(elTemp);

			if copyToFolder <> '' then // if needs file copy
			begin
				copyFile0(withTrailingSlash(newArchiveOrFolder) + elTemp, withTrailingSlash(copyToFolder) + elTemp);
			end;
		end
		else
		begin
			oldFileList.Delete(nTemp);
			oldArchivePath := withTrailingSlash(beautifyPath(oldArchiveOrFolder)) + elTemp;
			newArchivePath := withTrailingSlash(beautifyPath(newArchiveOrFolder)) + elTemp;

			if not compareFile(oldArchivePath, newArchivePath) then
				if MatchStr(AnsiLowerCase(ExtractFileExt(elTemp)), supportedExts) then // has MM Archive extension and likely MM Archive file
				begin
					addedFileListTemp := TStringList.Create;
					modifiedFileListTemp := TStringList.Create;
					deletedFileListTemp := TStringList.Create;

					try // try it as if it's MM Archive file
						compareArchive(oldArchivePath, newArchivePath,
										addedFileListTemp, modifiedFileListTemp, deletedFileListTemp);
						archSimpNew := MMArchSimple.load(newArchivePath);

						for elTemp2 in addedFileListTemp do
						begin
							addedFileList.Add(elTemp + archResSeparator + elTemp2);
							
							if copyToFolder <> '' then // if needs file copy
							begin
								archSimpNew.extract(withTrailingSlash(copyToFolder) + elTemp + '.mmarchive', elTemp2);
							end;
						end;

						for elTemp2 in modifiedFileListTemp do
						begin
							modifiedFileList.Add(elTemp + archResSeparator + elTemp2);
							
							if copyToFolder <> '' then // if needs file copy
							begin
								archSimpNew.extract(withTrailingSlash(copyToFolder) + elTemp + '.mmarchive', elTemp2);
							end;
						end;

						for elTemp2 in deletedFileListTemp do
						begin
							deletedFileList.Add(elTemp + archResSeparator + elTemp2);

							if copyToFolder <> '' then // if needs file copy
							begin
								createEmptyFile(withTrailingSlash(withTrailingSlash(copyToFolder) + elTemp + '.mmarchive') + elTemp2 + '.todelete');
							end;
						end;

						modifiedFileList.Add(elTemp + archResSeparator);

					except // very unlikely to be MM Archive file
						modifiedFileList.Add(elTemp);

						if copyToFolder <> '' then // if needs file copy
						begin
							copyFile0(withTrailingSlash(newArchiveOrFolder) + elTemp, withTrailingSlash(copyToFolder) + elTemp);
						end;
					end;

					addedFileListTemp.Free;
					modifiedFileListTemp.Free;
					deletedFileListTemp.Free;
				end
				else // doesn't have MM Archive extension therefore not MM Archive file
				begin
					modifiedFileList.Add(elTemp);

					if copyToFolder <> '' then // if needs file copy
					begin
						copyFile0(withTrailingSlash(newArchiveOrFolder) + elTemp, withTrailingSlash(copyToFolder) + elTemp);
					end;
				end;

		end;
	end;

	for elTemp in oldFileList do // add to deleted list
	begin
		deletedFileList.Add(elTemp);

		if copyToFolder <> '' then // if needs file copy
		begin
			if deletedFolderList.IndexOf(trimCharsRight(ExtractFilePath(elTemp), '\', '/')) = -1 then
				createEmptyFile(withTrailingSlash(copyToFolder) + elTemp + '.todelete');
		end;
	end;


	allList := TStringList.Create;

	for elTemp in addedFolderList do
		allList.Add(elTemp + slash + ' +');

	for elTemp in deletedFolderList do
		allList.Add(elTemp + slash + ' -');

	for elTemp in addedFileList do
		allList.Add(elTemp + ' +');

	for elTemp in modifiedFileList do
		allList.Add(elTemp + ' m');

	for elTemp in deletedFileList do
		allList.Add(elTemp + ' -');

	allList.Sort;

	for i := 0 to allList.Count - 1 do
	begin
		elTemp := allList[i];
		allList[i] := '[' + elTemp[length(elTemp)] + '] ' +
					System.Copy(elTemp, 1, length(elTemp) - 2);
	end;

	WriteLn;
	for elTemp in allList do
	begin

		if System.Copy(elTemp, 1, 3) = '[+]' then
			color := FOREGROUND_GREEN or FOREGROUND_INTENSITY
		else
			if System.Copy(elTemp, 1, 3) = '[-]' then
				color := FOREGROUND_RED or FOREGROUND_INTENSITY
			else // '[m]'
				color := FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;

		if AnsiContainsStr(elTemp, ':') then
			color := color or BACKGROUND_BLUE;
		colorWriteLn(elTemp, color);

	end;

	if deletedFolderList0 <> nil then
	begin
		// deletedFolderList0.Assign(deletedFolderList);
		// deletedFolderList0, deletedNonResFileList, deletedResFileList, modifiedArchiveList
	end;



// mmarch k .///\/test/compare_test_from ./\///\test/compare_test_to filesonly res


	oldFolderList.Free;
	newFolderList.Free;
	oldFileList.Free;
	newFileList.Free;
	addedFolderList.Free;
	deletedFolderList.Free;
	addedFileList.Free;
	modifiedFileList.Free;
	deletedFileList.Free;

end;


procedure compareBase(oldArchiveOrFolder, newArchiveOrFolder: string; copyToFolder: string = ''); overload;
var
	deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;

begin

	deletedFolderList := nil;
	deletedNonResFileList := nil;
	deletedResFileList := nil;
	modifiedArchiveList := nil;

	compareBase(oldArchiveOrFolder, newArchiveOrFolder, copyToFolder,
				deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList);

end;


procedure compareReport(oldArchiveOrFolder, newArchiveOrFolder: string);
begin
	compareBase(oldArchiveOrFolder, newArchiveOrFolder);
end;

procedure compareFilesonly(oldArchiveOrFolder, newArchiveOrFolder, diffFileFolderName: string);
begin
	compareBase(oldArchiveOrFolder, newArchiveOrFolder, diffFileFolderName);
end;

procedure compareFilesToAny(isNsis: boolean);
var
	oldDiffFileFolder, scriptFolder, diffFileFolderName: string;
	deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
begin

	oldDiffFileFolder := ParamStr(2);
	scriptFolder := ParamStr(3);
	diffFileFolderName := ParamStr(4);

	if (oldDiffFileFolder = '') or (scriptFolder = '') or (diffFileFolderName = '') then
		raise MissingParamException.Create(InsufficientParameters);

	deletedFolderList     := TStringList.Create;
	deletedNonResFileList := TStringList.Create;
	deletedResFileList    := TStringList.Create;
	modifiedArchiveList   := TStringList.Create;

	// ...

	generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList, isNsis);

end;

procedure compareFilesToNsis;
begin
	compareFilesToAny(true);
end;

procedure compareFilesToBatch;
begin
	compareFilesToAny(false);
end;

procedure compareAny(oldArchiveOrFolder, newArchiveOrFolder, scriptFolder, diffFileFolderName: string; isNsis: boolean);
var
	deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
begin

	deletedFolderList     := TStringList.Create;
	deletedNonResFileList := TStringList.Create;
	deletedResFileList    := TStringList.Create;
	modifiedArchiveList   := TStringList.Create;

	compareBase(oldArchiveOrFolder, newArchiveOrFolder, withTrailingSlash(scriptFolder) + diffFileFolderName,
				deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList);
	generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList, isNsis);

end;

procedure compareNsis(oldArchiveOrFolder, newArchiveOrFolder, scriptFolder, diffFileFolderName: string);
begin
	compareAny(oldArchiveOrFolder, newArchiveOrFolder, scriptFolder, diffFileFolderName, true);
end;

procedure compareBatch(oldArchiveOrFolder, newArchiveOrFolder, scriptFolder, diffFileFolderName: string);
begin
	compareAny(oldArchiveOrFolder, newArchiveOrFolder, scriptFolder, diffFileFolderName, true);
end;


procedure compare;
var
	archiveFile2, option, p5, p6: string;
	optionNumber: integer;

begin

	archiveFile2 := ParamStr(3);
	if archiveFile2 = '' then
		raise MissingParamException.Create(NeedArchiveFilesToMerge);

	option := ParamStr(4);

	optionNumber := AnsiIndexStr(option,
	['nsis',
	'batch',
	'filesonly',
	'']);

	p5 := ParamStr(5);
	p6 := ParamStr(6);

	if (p5 = '') and (optionNumber < 3) then
		raise MissingParamException.Create(InsufficientParameters);

	if (p6 = '') and (optionNumber < 2) then
		raise MissingParamException.Create(InsufficientParameters);

	Case optionNumber of
		0: compareNsis(archiveFile, archiveFile2, p5, p6);
		1: compareBatch(archiveFile, archiveFile2, p5, p6);
		2: compareFilesonly(archiveFile, archiveFile2, p5);
		3: compareReport(archiveFile, archiveFile2);
	else // -1: not found in the array
		raise MissingParamException.CreateFmt(UnknownCompareOption, [option]);
	end;

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
			'compare', 'k', 'optimize', 'o',
			'compare-files-to-nsis', 'cf2n', 'compare-files-to-batch', 'cf2b',
			'help', 'h', '']);
		archiveFile := ParamStr(2);

		if (archiveFile = '') And (methodNumber < 22) And (methodNumber >= 0) then // < 18: method is not `help`, `compareFilesToNsis` or `compareFilesToBatch`
			raise MissingParamException.Create(NeedArchiveFile);

		Case methodNumber of
			 0,  1: extract;
			 2,  3: list;
			 4,  5: add;
			 6,  7: delete;
			 8,  9: rename;
			10, 11: create;
			12, 13: merge;
			14, 15: compare;
			16, 17: optimize;
			18, 19: compareFilesToNsis;
			20, 21: compareFilesToBatch;
			22, 23, 24: help;
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
