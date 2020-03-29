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
	Windows, SysUtils, StrUtils, Classes, RSLod, MMArchMain, MMArchPath, MMArchCompare, RSQ;

type
	MissingParamException = class(Exception);

const
	MMARCHVERSION: string = '3.0';
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
	HELPPRM_DIFF_FOLDER              = 'DIFF_FOLDER';
	HELPPRM_DIFF_FOLDER_NAME         = 'DIFF_FOLDER_NAME';
	HELPPRM_OLD_DIFF_FOLDER          = 'OLD_DIFF_FOLDER';
	HELPPRM_SCRIPT_FOLDER            = 'SCRIPT_FOLDER';
	HELPPRM_FILE_TO_XX_X             = 'FILE_TO_XX_?';


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
	WriteLn('mmarch compare <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER + '> <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER_2 + '> {nsis|batch} <' + HELPPRM_SCRIPT_FOLDER + '> <' + HELPPRM_DIFF_FOLDER_NAME + '>');
	WriteLn('mmarch compare <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER + '> <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER_2 + '> fileonly <' + HELPPRM_DIFF_FOLDER + '>');
	WriteLn('mmarch compare-files-to-{nsis|batch} <' + HELPPRM_OLD_DIFF_FOLDER + '> <' + HELPPRM_SCRIPT_FOLDER + '> <' + HELPPRM_DIFF_FOLDER_NAME + '>');
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


procedure compareReport(oldArchiveOrFolder, newArchiveOrFolder: string);
begin
	compareBase(oldArchiveOrFolder, newArchiveOrFolder);
end;


procedure compareFileonly(oldArchiveOrFolder, newArchiveOrFolder, diffFileFolderName: string);
begin
	compareBase(oldArchiveOrFolder, newArchiveOrFolder, diffFileFolderName);
end;


procedure compareFilesToAny(isNsis: boolean);
var
	oldDiffFileFolder, scriptFilePath, diffFileFolderName, scriptFileFolder: string;
	deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
begin

	oldDiffFileFolder := ParamStr(2);
	scriptFilePath := ParamStr(3);
	diffFileFolderName := ParamStr(4);

	if (oldDiffFileFolder = '') or (scriptFilePath = '') or (diffFileFolderName = '') then
		raise MissingParamException.Create(InsufficientParameters);

	scriptFilePath := beautifyPath(scriptFilePath);
	scriptFileFolder := ExtractFilePath(scriptFilePath);

	deletedFolderList     := TStringList.Create;
	deletedNonResFileList := TStringList.Create;
	deletedResFileList    := TStringList.Create;
	modifiedArchiveList   := TStringList.Create;

	getListFromDiffFiles(oldDiffFileFolder, deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList);

	generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList, scriptFilePath, diffFileFolderName, isNsis);

	moveDir(oldDiffFileFolder, withTrailingSlash(scriptFileFolder) + beautifyPath(diffFileFolderName));

	deletedFolderList.Free;
	deletedNonResFileList.Free;
	deletedResFileList.Free;
	modifiedArchiveList.Free;

end;


procedure compareFilesToNsis;
begin
	compareFilesToAny(true);
end;


procedure compareFilesToBatch;
begin
	compareFilesToAny(false);
end;


procedure compareAny(oldArchiveOrFolder, newArchiveOrFolder, scriptFilePath, diffFileFolderName: string; isNsis: boolean);
var
	deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
begin

	deletedFolderList     := TStringList.Create;
	deletedNonResFileList := TStringList.Create;
	deletedResFileList    := TStringList.Create;
	modifiedArchiveList   := TStringList.Create;

	compareBase(oldArchiveOrFolder, newArchiveOrFolder, withTrailingSlash(ExtractFilePath(scriptFilePath)) + diffFileFolderName,
				deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList);
	generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList, scriptFilePath, diffFileFolderName, isNsis);

	deletedFolderList.Free;
	deletedNonResFileList.Free;
	deletedResFileList.Free;
	modifiedArchiveList.Free;

end;


procedure compareNsis(oldArchiveOrFolder, newArchiveOrFolder, scriptFilePath, diffFileFolderName: string);
begin
	compareAny(oldArchiveOrFolder, newArchiveOrFolder, scriptFilePath, diffFileFolderName, true);
end;


procedure compareBatch(oldArchiveOrFolder, newArchiveOrFolder, scriptFilePath, diffFileFolderName: string);
begin
	compareAny(oldArchiveOrFolder, newArchiveOrFolder, scriptFilePath, diffFileFolderName, false);
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
	'fileonly',
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
		2: compareFileonly(archiveFile, archiveFile2, p5);
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
		on E: Exception do
			WriteLn(format(ErrorStr, [E.Message]));
	end;

end.
