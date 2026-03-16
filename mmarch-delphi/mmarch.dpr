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
	Windows, SysUtils, StrUtils, Classes, RSLod, MMArchMain, MMArchPath, MMArchCompare, MMArchChecksum, RSQ;

type
	MissingParamException = class(Exception);
	ItemNotFoundException = class(Exception); // per-item not-found in delete/extract

const
	MMARCHVERSION: string = '5.0.0';
	MMARCHURL: string = 'https://github.com/might-and-magic/mmarch';

type
	TEcMode = (ecStrict, ecNormal, ecLoose);

var
	method, archiveFile: string;
	methodNumber: integer;
	ecMode: TEcMode = ecNormal;


resourcestring
	NeedFolder                   = 'You must specify a folder (use `.` for current folder)';
	NeedFileToAdd                = 'You must specify at least one file to add';
	NeedFileToDelete             = 'You must specify at least one file to delete';
	NeedFilesToRename            = 'You must specify a file to rename, and a file name that the file will be renamed to';
	NeedArchiveFileTypeAndFolder = 'You must specify a type of your archive file that will be created and a folder (use `.` for current folder)';
	NeedArchiveFilesToMerge      = 'You must specify two archive files to be merged together';
	InsufficientParameters       = 'Insufficient parameters';
	NeedArchiveFile              = 'You must specify an archive file';
	UnknownMethod                = 'Unknown method: `%s`';
	UnknownCompareOption         = 'Unknown compare option: `%s`';
	OldDiffFolderEmpty           = 'Folder `%s` is empty';

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
	HELPPRM_SCRIPT_FILE              = 'SCRIPT_FILE';
	HELPPRM_FILE_TO_XX_X             = 'FILE_TO_XX_?';


var
	// Cleaned args (with --ec stripped). Index 0 = program name.
	cleanArgs: array of string;
	cleanArgCount: integer;

function CleanParamStr(index: integer): string;
begin
	if (index >= 0) and (index < cleanArgCount) then
		Result := cleanArgs[index]
	else
		Result := '';
end;

function CleanParamCount: integer;
begin
	Result := cleanArgCount - 1; // exclude program name
end;

procedure ParseEcFlag;
var
	i: integer;
	normalized, modeStr: string;
begin
	SetLength(cleanArgs, ParamCount + 1);
	cleanArgCount := 0;
	i := 0;
	while i <= ParamCount do
	begin
		if i > 0 then
		begin
			normalized := AnsiLowerCase(trimCharLeft(ParamStr(i), '-'));
			if (normalized = 'ec') and (i + 1 <= ParamCount) then
			begin
				modeStr := AnsiLowerCase(ParamStr(i + 1));
				if modeStr = 'strict' then
					ecMode := ecStrict
				else if modeStr = 'normal' then
					ecMode := ecNormal
				else if modeStr = 'loose' then
					ecMode := ecLoose
				else
					WriteLn('Warning: unknown --ec mode ''' + ParamStr(i + 1) + ''', using ''normal''');
				i := i + 2;
				Continue;
			end;
		end;
		cleanArgs[cleanArgCount] := ParamStr(i);
		Inc(cleanArgCount);
		Inc(i);
	end;
	SetLength(cleanArgs, cleanArgCount);
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
	WriteLn('mmarch compare <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER + '> <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER_2 + '> {nsis|batch} <' + HELPPRM_SCRIPT_FILE + '> <' + HELPPRM_DIFF_FOLDER_NAME + '>');
	WriteLn('mmarch compare <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER + '> <' + HELPPRM_ARCHIVE_FILE_OR_FOLDER_2 + '> filesonly <' + HELPPRM_DIFF_FOLDER + '>');
	WriteLn('mmarch diff-files-to-{nsis|batch} <' + HELPPRM_OLD_DIFF_FOLDER + '> <' + HELPPRM_SCRIPT_FILE + '> <' + HELPPRM_DIFF_FOLDER_NAME + '>');
	WriteLn('mmarch diff-add-keep <' + HELPPRM_DIFF_FOLDER + '>');
    WriteLn('mmarch checksum <' + HELPPRM_ARCHIVE_FILE + '>');
    WriteLn('mmarch checksum <' + HELPPRM_ARCHIVE_FILE + '> [FILE_1] [FILE_2] [...]');
    WriteLn('mmarch checksum <' + HELPPRM_ARCHIVE_FILE + '> --v[all] <CRC32_FILE>');
    WriteLn('mmarch checksum <' + HELPPRM_ARCHIVE_FILE + '> --v[all] <name1:HASH1> [name2:HASH2] [...]');
	WriteLn('mmarch optimize <' + HELPPRM_ARCHIVE_FILE + '>');
	WriteLn('mmarch help');
	WriteLn;
	WriteLn('Global option: --ec {strict|normal|loose}  (default: normal)');

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
	i, j, notFoundCount: integer;
	archiveFileList: TStringList; // archive file name - archive file path pair
begin

	extractToBaseFolder := CleanParamStr(3);
	if extractToBaseFolder = '' then
		raise MissingParamException.Create(NeedFolder);
	extractToFolder := extractToBaseFolder;
	notFoundCount := 0;

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
			fileName := CleanParamStr(4);

			if Pos('*', archiveFile) > 0 then
				extractToFolder := withTrailingSlash(extractToBaseFolder)
				+ withTrailingSlash(archiveFileFolder)
				+ archiveFileList.Names[j] + MMArchiveExt;

			if fileName = '' then // FILE_TO_EXTRACT_1 is empty, extract all
				archSimp.extractAll(extractToFolder);

			for i := 4 to CleanParamCount do
			begin
				fileName := CleanParamStr(i);
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
								WriteLn(format(FileInArchiveErrorStr, [beautifyPath(fileName),
										beautifyPath(withTrailingSlash(archiveFileFolder) + archiveFileList.Names[j])]));
								WriteLn(E.Message);
								Inc(notFoundCount);
							end;
						end;
					end;
				end;
			end;

		except
			on E: Exception do
			begin
				WriteLn(format(ArchiveFileErrorStr,
						[beautifyPath(withTrailingSlash(archiveFileFolder) + archiveFileList.Names[j])]));
				WriteLn(E.Message);
			end;
		end;

	end;

	archiveFileList.Free;
	if (notFoundCount > 0) and (ecMode = ecStrict) then
		raise ItemNotFoundException.Create(IntToStr(notFoundCount) + ' file(s) not found in the archive');

end;


procedure list;
var
	archSimp: MMArchSimple;
	separator: string;
begin
	separator := CleanParamStr(3);
	if separator = '' then
		separator := #13#10;

	archSimp := MMArchSimple.load(archiveFile);

	Write(archSimp.list(separator));
end;


procedure addProc(archSimp: MMArchSimple; paramIndexFrom: integer; baseFolder: string = '');
var
	filePath, ext, folder, fileName: string;
	i, paletteIndex: integer;
begin
	i := paramIndexFrom;
	while i <= CleanParamCount do
	begin
		filePath := CleanParamStr(i);
		// If baseFolder is given (from create) and file doesn't exist at raw path, prepend baseFolder
		if (baseFolder <> '') and (filePath <> '') and (Pos('*', filePath) = 0) and
		   not FileExists(filePath) and FileExists(withTrailingSlash(baseFolder) + filePath) then
			filePath := withTrailingSlash(baseFolder) + filePath;
		if filePath <> '' then // has file to add
		begin
			folder := ExtractFilePath(filePath);
			fileName := ExtractFileName(filePath);
			ext := ExtractFileName(fileName);

			if Pos('*', fileName) > 0 then
				archSimp.addAll(folder, wildcardFileNameToExt(fileName))
			else
			begin
				if (i <= CleanParamCount - 2) and (SameText(CleanParamStr(i + 1), '/p')) then // pal specified
				begin
					paletteIndex := strtoint(CleanParamStr(i + 2));

					try // the individual resource file will be skipped if it gets an exception
						archSimp.add(filePath, paletteIndex, false);
					except
						on E: Exception do
							WriteLn(format(FileErrorStr, [beautifyPath(filePath), E.Message]));
					end;

					i := i + 2;
				end
				else
				begin
					try // the individual resource file will be skipped if it gets an exception
						archSimp.add(filePath, false);
					except
						on E: Exception do
							WriteLn(format(FileErrorStr, [beautifyPath(filePath), E.Message]));
					end;
				end;
			end;
		end;
		i := i + 1; // increment no matter what
	end;
	archSimp.optimize;
end;


procedure add;
var
	filePath: string;
	archSimp: MMArchSimple;
begin
	filePath := CleanParamStr(3);
	if filePath = '' then
		raise MissingParamException.Create(NeedFileToAdd);
	archSimp := MMArchSimple.load(archiveFile);
	addProc(archSimp, 3);
end;


procedure delete;
var
	fileName: string;
	archSimp: MMArchSimple;
	i, notFoundCount: integer;
begin
	fileName := CleanParamStr(3);
	if fileName = '' then
		raise MissingParamException.Create(NeedFileToDelete);
	archSimp := MMArchSimple.load(archiveFile);
	notFoundCount := 0;

	for i := 3 to CleanParamCount do
	begin
		fileName := CleanParamStr(i);
		if fileName <> '' then
		begin
			if Pos('*', fileName) > 0 then
				archSimp.deleteAll(wildcardFileNameToExt(fileName))
			else
			begin
				try // the individual resource file will be skipped if it gets an exception
					archSimp.delete(fileName, false);
				except
					on E: Exception do
					begin
						WriteLn(ErrOutput, format(FileErrorStr, [beautifyPath(fileName), E.Message]));
						Inc(notFoundCount);
					end;
				end;
			end;
		end;
	end;
	archSimp.optimize;
	if (notFoundCount > 0) and (ecMode = ecStrict) then
		raise ItemNotFoundException.Create(IntToStr(notFoundCount) + ' file(s) not found in the archive');
end;


procedure rename;
var
	oldFileName, newFileName: string;
	archSimp: MMArchSimple;
begin
	oldFileName := CleanParamStr(3);
	newFileName := CleanParamStr(4);
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
	archiveFileType := CleanParamStr(3);
	folder := CleanParamStr(4);
	if (archiveFileType = '') or (folder = '') then
		raise MissingParamException.Create(NeedArchiveFileTypeAndFolder);

	archSimp := MMArchSimple.create;
	archSimp.new(archiveFile, archiveFileType, folder);
	addProc(archSimp, 5, folder);
end;


procedure merge;
var
	archSimp: MMArchSimple;
	archiveFile2: string;
begin
	archiveFile2 := CleanParamStr(3);
	if archiveFile2 = '' then
		raise MissingParamException.Create(NeedArchiveFilesToMerge);

	archSimp := MMArchSimple.load(archiveFile);
	archSimp.merge(archiveFile2);
end;


procedure compareReport(oldArchiveOrFolder, newArchiveOrFolder: string);
begin
	compareBase(oldArchiveOrFolder, newArchiveOrFolder);
end;


procedure compareFilesonly(oldArchiveOrFolder, newArchiveOrFolder, diffFileFolderName: string);
begin
	compareBase(oldArchiveOrFolder, newArchiveOrFolder, diffFileFolderName);
end;


procedure diffFilesToAny(isNsis: boolean);
var
	oldDiffFileFolder, scriptFilePath, diffFileFolderName, scriptFileFolder, newDiffFileFolder: string;
	deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
	shouldGenerateScript: boolean;
begin

	oldDiffFileFolder := CleanParamStr(2);
	scriptFilePath := CleanParamStr(3);
	diffFileFolderName := CleanParamStr(4);

	if (oldDiffFileFolder = '') or (scriptFilePath = '') or (diffFileFolderName = '') then
		raise MissingParamException.Create(InsufficientParameters);

	oldDiffFileFolder := beautifyPath(oldDiffFileFolder);
	if not DirectoryExists(oldDiffFileFolder) or (
		(getAllFilesInFolder(oldDiffFileFolder, '*', false).Count = 0) and
		(getAllFilesInFolder(oldDiffFileFolder, '*', true).Count = 0)
	) then
		WriteLn(format(OldDiffFolderEmpty, [oldDiffFileFolder]))
	else
	begin

		scriptFilePath := beautifyPath(scriptFilePath);
		scriptFileFolder := ExtractFilePath(scriptFilePath);

		deletedFolderList     := TStringList.Create;
		deletedNonResFileList := TStringList.Create;
		deletedResFileList    := TStringList.Create;
		modifiedArchiveList   := TStringList.Create;
			
		getListFromDiffFiles(oldDiffFileFolder, deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList);

		newDiffFileFolder := withTrailingSlash(scriptFileFolder) + beautifyPath(diffFileFolderName);
		shouldGenerateScript := true;
		if not SameText(oldDiffFileFolder, newDiffFileFolder) then
			shouldGenerateScript := moveDir(oldDiffFileFolder, newDiffFileFolder);
		if shouldGenerateScript then
			generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList, scriptFilePath, diffFileFolderName, isNsis);

		deletedFolderList.Free;
		deletedNonResFileList.Free;
		deletedResFileList.Free;
		modifiedArchiveList.Free;

	end;

end;


procedure diffFilesToNsis;
begin
	diffFilesToAny(true);
end;


procedure diffFilesToBatch;
begin
	diffFilesToAny(false);
end;


procedure diffAddKeep;
begin
	addKeepToAllEmptyFoldersRecur(archiveFile); // archiveFile here is a folder path
end;


procedure compareAny(oldArchiveOrFolder, newArchiveOrFolder, scriptFilePath, diffFileFolderName: string; isNsis: boolean);
var
	deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
	same: boolean;
begin

	deletedFolderList     := TStringList.Create;
	deletedNonResFileList := TStringList.Create;
	deletedResFileList    := TStringList.Create;
	modifiedArchiveList   := TStringList.Create;

	same := compareBase(oldArchiveOrFolder, newArchiveOrFolder, withTrailingSlash(ExtractFilePath(scriptFilePath)) + diffFileFolderName,
				deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList);
	if not same then
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

	archiveFile2 := CleanParamStr(3);
	if archiveFile2 = '' then
		raise MissingParamException.Create(NeedArchiveFilesToMerge);

	option := CleanParamStr(4);

	optionNumber := AnsiIndexStr(option,
	['nsis',
	'batch',
	'filesonly',
	'']);

	p5 := CleanParamStr(5);
	p6 := CleanParamStr(6);

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


procedure checksumCmd;
var
	archSimp: MMArchSimple;
	param3, param4, fileName, line, resName, expectedHash, actualHash, ext: string;
	i, failCount, missingCount, colonPos: integer;
	isVerify, isVerifyAll, isInline: boolean;
	checksumLines, verifyPairs, archFileNames: TStringList;
	fFiles: TRSMMFiles;
begin
	param3 := CleanParamStr(3);

	// Accept -v, --v, -vall, --vall as verify flags.
	// Must start with '-' (bare "v" or "vall" could be a resource filename).
	isVerify := (Length(param3) > 0) and (param3[1] = '-') and SameText(trimCharLeft(param3, '-'), 'v');
	isVerifyAll := (Length(param3) > 0) and (param3[1] = '-') and SameText(trimCharLeft(param3, '-'), 'vall');

	if isVerify or isVerifyAll then
	begin
		// === Verify mode ===
		param4 := CleanParamStr(4);
		if param4 = '' then
			raise MissingParamException.Create(InsufficientParameters);

		archSimp := MMArchSimple.load(archiveFile);
		fFiles := archSimp.getTRSMMArchive.RawFiles;

		// Determine if inline (first arg after --v contains ':')
		isInline := (Pos(':', param4) > 0);

		verifyPairs := TStringList.Create;
		try
			if isInline then
			begin
				// Parse inline name:HASH pairs
				for i := 4 to CleanParamCount do
				begin
					line := CleanParamStr(i);
					if line <> '' then
						verifyPairs.Add(line);
				end;
			end
			else
			begin
				// Read from file
				checksumLines := TStringList.Create;
				try
					checksumLines.LoadFromFile(param4);
					for i := 0 to checksumLines.Count - 1 do
					begin
						line := Trim(checksumLines[i]);
						if line <> '' then
						begin
							// Format: "HASH  filename" -> convert to "filename:HASH"
							// Hash is 8 chars, then two spaces, then filename
							if (Length(line) > 10) and (line[9] = ' ') and (line[10] = ' ') then
								verifyPairs.Add(Copy(line, 11, MaxInt) + ':' + Copy(line, 1, 8))
							else
								verifyPairs.Add(line); // try as-is (name:HASH)
						end;
					end;
				finally
					checksumLines.Free;
				end;
			end;

			failCount := 0;

			for i := 0 to verifyPairs.Count - 1 do
			begin
				line := verifyPairs[i];
				colonPos := Pos(':', line);
				if colonPos > 0 then
				begin
					resName := Copy(line, 1, colonPos - 1);
					expectedHash := UpperCase(Copy(line, colonPos + 1, MaxInt));

					try
						actualHash := Copy(ChecksumResourceFile(archSimp,resName), 1, 8);
						if SameText(actualHash, expectedHash) then
							WriteLn(resName + ': OK')
						else
						begin
							WriteLn(resName + ': FAILED');
							Inc(failCount);
						end;
					except
						on E: Exception do
						begin
							WriteLn(resName + ': FAILED');
							Inc(failCount);
						end;
					end;
				end;
			end;

			// --vall: check for files in archive not listed
			if isVerifyAll then
			begin
				archFileNames := TStringList.Create;
				try
					// Build list of verified names (lowercased for comparison)
					for i := 0 to verifyPairs.Count - 1 do
					begin
						line := verifyPairs[i];
						colonPos := Pos(':', line);
						if colonPos > 0 then
							archFileNames.Add(AnsiLowerCase(Copy(line, 1, colonPos - 1)));
					end;

					missingCount := 0;
					for i := 0 to fFiles.Count - 1 do
					begin
						resName := fFiles.Name[i];
						if archFileNames.IndexOf(AnsiLowerCase(resName)) < 0 then
						begin
							// Also check extracted name (may differ from in-archive name)
							fileName := archSimp.getTRSMMArchive.GetExtractName(i);
							if archFileNames.IndexOf(AnsiLowerCase(fileName)) < 0 then
								Inc(missingCount);
						end;
					end;

					if missingCount > 0 then
						WriteLn(ErrOutput, 'WARNING: ' + IntToStr(missingCount) + ' files in archive not listed in checksum');
				finally
					archFileNames.Free;
				end;
			end;

			if failCount > 0 then
			begin
				WriteLn(ErrOutput, 'WARNING: ' + IntToStr(failCount) + ' computed checksums did NOT match');
				ExitCode := 1;
			end;

		finally
			verifyPairs.Free;
		end;
	end
	else
	begin
		// === Generate mode ===
		if param3 = '' then
		begin
			// No extra params: CRC32 of the file itself (no archive parsing needed)
			Write(CRC32ToHex(CRC32File(archiveFile)) + '  ' + ExtractFileName(archiveFile) + #13#10);
		end
		else
		begin
			archSimp := MMArchSimple.load(archiveFile);

			if (param3 = '*') or (param3 = '*.*') then
			begin
				// All resource files
				Write(ChecksumAllResources(archSimp));
			end
			else if Pos('*', param3) > 0 then
			begin
				// Wildcard match
				ext := wildcardFileNameToExt(param3);
				Write(ChecksumAllResources(archSimp, ext));
			end
			else
			begin
				// Specific files
				for i := 3 to CleanParamCount do
				begin
					fileName := CleanParamStr(i);
					if fileName <> '' then
					begin
						try
							Write(ChecksumResourceFile(archSimp,fileName) + #13#10);
						except
							on E: Exception do
							begin
								WriteLn(format(FileErrorStr, [beautifyPath(fileName), E.Message]));
							end;
						end;
					end;
				end;
			end;
		end;
	end;
end;


begin

	ParseEcFlag;

	try

		method := trimCharLeft(CleanParamStr(1), '-');
		methodNumber := AnsiIndexStr(method, ['extract', 'e', 'list', 'l',
			'add', 'a', 'delete', 'd', 'rename', 'r', 'create', 'c', 'merge', 'm',
			'compare', 'k', 'optimize', 'o',
			'diff-files-to-nsis', 'df2n', 'diff-files-to-batch', 'df2b', 'diff-add-keep', 'dak',
			'checksum', 's',
			'help', 'h', '']);
		archiveFile := CleanParamStr(2);

		// < 26: method is not `help`
		if (archiveFile = '') And (methodNumber < 26) And (methodNumber >= 0) then
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
			18, 19: diffFilesToNsis;
			20, 21: diffFilesToBatch;
			22, 23: diffAddKeep;
			24, 25: checksumCmd;
			26, 27, 28: help;
		else // -1: not found in the array
			raise MissingParamException.CreateFmt(UnknownMethod, [method]);
		end;

	except
		on E: ItemNotFoundException do
		begin
			// Per-item not-found in delete/extract: only exit 1 in strict mode
			WriteLn(ErrOutput, format(ErrorStr, [E.Message]));
			if ecMode = ecStrict then
				ExitCode := 1;
		end;
		on E: MissingParamException do
		begin
			WriteLn(ErrOutput, format(ErrorStr, [E.Message]));
			WriteLn(ErrOutput);
			help(true);
			// normal/strict: exit 1; loose: exit 0
			if ecMode <> ecLoose then
				ExitCode := 1;
		end;
		on E: Exception do
		begin
			WriteLn(ErrOutput, format(ErrorStr, [E.Message]));
			// normal/strict: exit 1; loose: exit 0
			if ecMode <> ecLoose then
				ExitCode := 1;
		end;
	end;

end.
