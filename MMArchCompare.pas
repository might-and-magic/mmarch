// MMArchCompare unit and MMArchSimple class
// Part of mmarch
// Command line tool to handle Heroes 3 and Might and Magic 6, 7, 8
// resource archive files (e.g. lod files). Based on GrayFace's MMArchive.
// By Tom CHEN <tomchen.org@gmail.com> (tomchen.org)
// MIT License
// https://github.com/might-and-magic/mmarch



unit MMArchCompare;

interface

uses
	Windows, SysUtils, StrUtils, Classes, RSLod, MMArchMain, MMArchPath, RSQ;

procedure colorWriteLn(str: string; color: word);
function compareFile(oldFile, newFile: string): boolean;
function compareInArchiveFile(oldRaw, newRaw: TRSMMFiles; oldIndex, newIndex: integer): boolean;
procedure compareArchive(oldArchive, newArchive: string; var addedFileList, modifiedFileList, deletedFileList: TStringList);
procedure generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
			scriptFilePath, diffFileFolderName: string; isNsis: boolean);
function compareBase(oldArchiveOrFolder, newArchiveOrFolder, copyToFolder: string;
			var deletedFolderList0, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList): boolean; overload;
function compareBase(oldArchiveOrFolder, newArchiveOrFolder: string; copyToFolder: string = ''): boolean; overload;
procedure getListFromDiffFiles(oldDiffFileFolder: string; var deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList);

const
	ToDeleteExt:  string = '.todelete';
	MMArchiveExt: string = '.mmarchive';

resourcestring
	IncorrectMMArchive           = 'Incorrect MM Archive files';
	FilesAreSame                 = 'Files are exactly the same';
	FoldersAreSame               = 'Folders are exactly the same';
	IncorrectFoldersOrMMArchives = 'Please specify two folders, or two MM Archive files';

implementation


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


// private
procedure colorPrintFileList(addedFileList, modifiedFileList, deletedFileList, addedFolderList, deletedFolderList: TStringList); overload;
var
	allList: TStringList;
	elTemp: string;
	i: integer;
	color: word;

begin

	allList := TStringList.Create;

	if addedFolderList <> nil then // addedFolderList, deletedFolderList are not nil
	begin
	
		for elTemp in addedFolderList do
			allList.Add(elTemp + slash + ' +');

		for elTemp in deletedFolderList do
			allList.Add(elTemp + slash + ' -');

	end;

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

		if AnsiContainsStr(elTemp, archResSeparator) then
			color := color or BACKGROUND_BLUE;
		colorWriteLn(elTemp, color);

	end;

	allList.Free;

end;


// private
procedure colorPrintFileList(addedFileList, modifiedFileList, deletedFileList: TStringList); overload;
var
	addedFolderList, deletedFolderList: TStringList;
begin
	addedFolderList := nil;
	deletedFolderList := nil;
	colorPrintFileList(addedFileList, modifiedFileList, deletedFileList, addedFolderList, deletedFolderList);
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

	memOld.Free;
	memNew.Free;
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

	mem1.Free;
	mem2.Free;

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


procedure generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
			scriptFilePath, diffFileFolderName: string; isNsis: boolean);
// isNsis is true : NSIS
// isNsis is false: Batch
var
	scriptString, elTemp, strTemp: string;
	tslTemp: TStringList;
	noRes: boolean;

begin

	deletedFolderList.Sort;
	deletedNonResFileList.Sort;
	deletedResFileList.Sort;
	modifiedArchiveList.Sort;

	noRes := (deletedResFileList.Count = 0) and (modifiedArchiveList.Count = 0);

	if isNsis then // NSIS
	begin

		scriptString := #13#10 +
			';--------------------------------'#13#10 +
			';Include Modern UI'#13#10 +
			'!include "MUI2.nsh"'#13#10 +
			#13#10 +
			';--------------------------------'#13#10 +
			';General'#13#10 +
			#13#10 +
			';Name and file'#13#10 +
			'Name "Might and Magic Patch"'#13#10 +
			'OutFile "patch.exe"'#13#10 +
			'Unicode True'#13#10 +
			'; AutoCloseWindow true'#13#10 +
			#13#10 +
			'BrandingText "NWC/3DO; Ubisoft"'#13#10 +
			#13#10 +
			'; !define MUI_ICON "icon.ico"'#13#10 +
			#13#10 +
			';--------------------------------'#13#10 +
			';Default installation folder'#13#10 +
			'InstallDir $EXEDIR'#13#10 +
			#13#10 +
			';Request application privileges for Windows Vista'#13#10 +
			'RequestExecutionLevel user'#13#10 +
			#13#10 +
			';--------------------------------'#13#10 +
			';Pages'#13#10 +
			#13#10 +
			'!insertmacro MUI_PAGE_INSTFILES'#13#10 +
			#13#10 +
			';--------------------------------'#13#10 +
			';Languages'#13#10 +
			#13#10 +
			'!insertmacro MUI_LANGUAGE "English"'#13#10 +
			#13#10 +
			';--------------------------------'#13#10 +
			';Installer Sections'#13#10 +
			#13#10 +
			'Section'#13#10 +
			#13#10 +
			';-----FILE COPYING (MODIFYING, DELETING) STARTS HERE-----'#13#10 +
			#13#10 +
			'	SetOutPath $INSTDIR'#13#10;

		if not noRes then
			scriptString := scriptString + '	File mmarch.exe'#13#10;

		scriptString := scriptString + #13#10;

		if deletedFolderList.Count > 0 then
		begin
			for elTemp in deletedFolderList do
			begin
				scriptString := scriptString + '	RMDir /r /REBOOTOK "$INSTDIR\' + elTemp + '"'#13#10;
			end;
			scriptString := scriptString + #13#10;
		end;
			
		scriptString := scriptString +'	File /r /x *' + ToDeleteExt + ' /x *' + EmptyFolderKeep + ' ' + withTrailingSlash(beautifyPath(diffFileFolderName)) + '*.*'#13#10 +
			#13#10;

		if deletedNonResFileList.Count > 0 then
		begin
			for elTemp in deletedNonResFileList do
			begin
				scriptString := scriptString + '	Delete "' + elTemp + '"'#13#10;
			end;
			scriptString := scriptString + #13#10;
		end;
		
		if deletedResFileList.Count > 0 then
		begin
			for elTemp in deletedResFileList do
			begin
				tslTemp := TStringList.Create;
				Split(archResSeparator, elTemp, tslTemp);
				scriptString := scriptString +
					'	nsExec::Exec ''mmarch delete "' + tslTemp[0] + '" "' + tslTemp[1] + '"'''#13#10;
			end;
			scriptString := scriptString + #13#10;

		end;
		
		if modifiedArchiveList.Count > 0 then
		begin
			for elTemp in modifiedArchiveList do
			begin
				strTemp := System.Copy(elTemp, 1, length(elTemp) - 1);
				scriptString := scriptString +
					'	nsExec::Exec ''mmarch add "' + strTemp + '" "' + strTemp + MMArchiveExt + '\*.*"'''#13#10 +
					'	RMDir /r /REBOOTOK "$INSTDIR\' + strTemp + MMArchiveExt + '"'#13#10;
			end;
			scriptString := scriptString + #13#10;
		end;

		if not noRes then
			scriptString := scriptString + '	Delete "mmarch.exe"'#13#10;

		scriptString := scriptString + 
			#13#10 +
			';-----FILE COPYING (MODIFYING, DELETING) ENDS HERE-----'#13#10 +
			#13#10 +
			'SectionEnd'#13#10;

	end
	else // Batch
	begin

		scriptString := 'cd %~dp0'#13#10 +
			#13#10;

		for elTemp in deletedFolderList do
		begin
			scriptString := scriptString + 'rmdir /s /q "' + elTemp + '"'#13#10;
		end;
		scriptString := scriptString + #13#10;

		scriptString := scriptString +
			'(echo ' + ToDeleteExt + ' && echo ' + EmptyFolderKeep + ')>excludelist.txt'#13#10 +
			'Xcopy files . /s /e /y /EXCLUDE:excludelist.txt'#13#10 +
			'del excludelist.txt'#13#10 +
			#13#10;

		for elTemp in deletedNonResFileList do
		begin
			scriptString := scriptString + 'del "' + elTemp + '"'#13#10;
		end;
		scriptString := scriptString + #13#10;

		for elTemp in deletedResFileList do
		begin
			tslTemp := TStringList.Create;
			Split(archResSeparator, elTemp, tslTemp);
			scriptString := scriptString +
				'mmarch delete "' + tslTemp[0] + '" "' + tslTemp[1] + '"'#13#10;
		end;
		scriptString := scriptString + #13#10;

		for elTemp in modifiedArchiveList do
		begin
			strTemp := System.Copy(elTemp, 1, length(elTemp) - 1);
			scriptString := scriptString +
				'mmarch add "' + strTemp + '" "' + strTemp + MMArchiveExt + '\*.*"'#13#10 +
				'rmdir /s /q "' + strTemp + MMArchiveExt + '"'#13#10;
		end;

	end;

	StrToFile(scriptFilePath, scriptString);

end;


function hasToDeletedParentFolder(copyToFolder, fileOrFolderPath: string; deletedFolderList: TStringList): boolean;
var
	parentFolder: string;
begin
	parentFolder := fileOrFolderPath;
	repeat
		parentFolder := trimCharsRight(ExtractFilePath(parentFolder), '\', '/');
		Result := (deletedFolderList.IndexOf(parentFolder) <> -1);
	until (parentFolder = '') or Result;
end;


// procedure cleanUpAllParentFolders(copyToFolder, innermostFolderPath: string); // including current folder
// var
// 	elTemp: string;
// begin

// 	innermostFolderPath := beautifyPath(innermostFolderPath);
// 	elTemp := trimCharsRight(innermostFolderPath, '\', '/');

// 	while elTemp <> '' do
// 	begin
// 		if DirectoryExists(copyToFolder + elTemp + ToDeleteExt) then
// 		begin
// 			delDir(copyToFolder + elTemp + ToDeleteExt);
// 		end;
// 		elTemp := trimCharsRight(ExtractFilePath(elTemp), '\', '/');
// 	end;

// end;


procedure cleanUpOldFolder(copyToFolder, fileOrFolderPath: string; fileType: integer);
// type:
// 1: [+/m] folder
// 2: [-]   folder
// 3: [+/m] file
// 4: [-]   file
// 5: [+/m] resourcefile
// 6: [-]   resourcefile
var
	strTemp: string;
begin

	copyToFolder := withTrailingSlash(copyToFolder);

	Case fileType of
		//1: // folderPath
			// cleanUpAllParentFolders(copyToFolder, fileOrFolderPath);
		2: // folderPath (without .todelete)
			delDir(copyToFolder + fileOrFolderPath);
		3: // filePath (or archiveFilePath without .mmarchive)
		begin
			DeleteFile(copyToFolder + fileOrFolderPath + ToDeleteExt);

			// cleanUpAllParentFolders(copyToFolder, fileOrFolderPath);

			if MatchStr(AnsiLowerCase(ExtractFileExt(fileOrFolderPath)), supportedExts) then
				delDir(copyToFolder + fileOrFolderPath + MMArchiveExt);
		end;
		4: // filePath (or archiveFilePath without .mmarchive)
		begin
			DeleteFile(copyToFolder + fileOrFolderPath);
			
			if MatchStr(AnsiLowerCase(ExtractFileExt(fileOrFolderPath)), supportedExts) then
				delDir(copyToFolder + fileOrFolderPath + MMArchiveExt);
		end;
		5: // resourceFilePath (archiveFilePath.mmarchive\resourceFileName)
		begin
			DeleteFile(copyToFolder + fileOrFolderPath + ToDeleteExt);

			strTemp := trimCharsRight(ExtractFilePath(fileOrFolderPath), '\', '/'); // with '.mmarchive'
			System.Delete(strTemp, length(strTemp) - 9, 10); // remove '.mmarchive'
			DeleteFile(copyToFolder + strTemp + ToDeleteExt);

			// cleanUpAllParentFolders(copyToFolder, ExtractFilePath(strTemp));
		end;
		6: // resourceFilePath (archiveFilePath.mmarchive\resourceFileName without .todelete)
			DeleteFile(copyToFolder + fileOrFolderPath);
	end;

end;


function compareBase(oldArchiveOrFolder, newArchiveOrFolder, copyToFolder: string;
			var deletedFolderList0, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList): boolean; overload;
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
	deletedFileListTemp: TStringList;

	elTemp, elTemp2, nameTemp,
	oldArchivePath, newArchivePath: string;

	oldArchiveOrFolderLen, newArchiveOrFolderLen,

	i, nTemp: integer;

	archSimpNew: MMArchSimple;
	
	copyToFolderExists : boolean;

begin

	oldArchiveOrFolder := beautifyPath(oldArchiveOrFolder);
	newArchiveOrFolder := beautifyPath(newArchiveOrFolder);

	copyToFolderExists := DirectoryExists(copyToFolder);

	if DirectoryExists(oldArchiveOrFolder) and DirectoryExists(newArchiveOrFolder) then // oldArchiveOrFolder and newArchiveOrFolder are folders
	begin

		oldArchiveOrFolderLen := length(withTrailingSlash(oldArchiveOrFolder));
		newArchiveOrFolderLen := length(withTrailingSlash(newArchiveOrFolder));

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

					if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
						cleanUpOldFolder(copyToFolder, elTemp, 1); // [+/m] folder
				end;
			end
			else
				oldFolderList.Delete(nTemp);
		end;

		for elTemp in oldFolderList do // add to deleted list
		begin
			if not hasToDeletedParentFolder(copyToFolder, elTemp, deletedFolderList) then
				deletedFolderList.Add(elTemp);

			if copyToFolder <> '' then // if needs file copy
			begin
				if not hasToDeletedParentFolder(copyToFolder, elTemp, deletedFolderList) then
					createDirRecur(withTrailingSlash(copyToFolder) + elTemp + ToDeleteExt);

				if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
					cleanUpOldFolder(copyToFolder, elTemp, 2); // [-]   folder
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

					if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
						cleanUpOldFolder(copyToFolder, elTemp, 3); // [+/m] file
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
									archSimpNew.extract(withTrailingSlash(copyToFolder) + elTemp + MMArchiveExt, elTemp2);

									if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
										cleanUpOldFolder(copyToFolder, withTrailingSlash(elTemp + MMArchiveExt) + elTemp2, 5); // [+/m] resourcefile
								end;
							end;

							for elTemp2 in modifiedFileListTemp do
							begin
								modifiedFileList.Add(elTemp + archResSeparator + elTemp2);
								
								if copyToFolder <> '' then // if needs file copy
								begin
									archSimpNew.extract(withTrailingSlash(copyToFolder) + elTemp + MMArchiveExt, elTemp2);

									if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
										cleanUpOldFolder(copyToFolder, withTrailingSlash(elTemp + MMArchiveExt) + elTemp2, 5); // [+/m] resourcefile
								end;
							end;

							for elTemp2 in deletedFileListTemp do
							begin
								deletedFileList.Add(elTemp + archResSeparator + elTemp2);

								if copyToFolder <> '' then // if needs file copy
								begin
									createEmptyFile(withTrailingSlash(withTrailingSlash(copyToFolder) + elTemp + MMArchiveExt) + elTemp2 + ToDeleteExt);

									if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
										cleanUpOldFolder(copyToFolder, withTrailingSlash(elTemp + MMArchiveExt) + elTemp2, 6); // [-]   resourcefile
								end;
							end;

							modifiedFileList.Add(elTemp + archResSeparator);

						except // very unlikely to be MM Archive file
							modifiedFileList.Add(elTemp);

							if copyToFolder <> '' then // if needs file copy
							begin
								copyFile0(withTrailingSlash(newArchiveOrFolder) + elTemp, withTrailingSlash(copyToFolder) + elTemp);

								if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
									cleanUpOldFolder(copyToFolder, elTemp, 3); // [+/m] file
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

							if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
								cleanUpOldFolder(copyToFolder, elTemp, 3); // [+/m] file
						end;
					end;

			end;
		end;

		for elTemp in oldFileList do // add to deleted list
		begin
			if not hasToDeletedParentFolder(copyToFolder, elTemp, deletedFolderList) then
				deletedFileList.Add(elTemp);

			if copyToFolder <> '' then // if needs file copy
				if not hasToDeletedParentFolder(copyToFolder, elTemp, deletedFolderList) then
					createEmptyFile(withTrailingSlash(copyToFolder) + elTemp + ToDeleteExt);

				if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
					cleanUpOldFolder(copyToFolder, elTemp, 4); // [-]   file
		end;

		if (addedFileList.Count = 0) and
		(modifiedFileList.Count = 0) and
		(deletedFileList.Count = 0) and
		(addedFolderList.Count = 0) and
		(deletedFolderList.Count = 0) then
		begin
			WriteLn(FoldersAreSame);
			Result := true;
		end
		else
		begin
			colorPrintFileList(addedFileList, modifiedFileList, deletedFileList, addedFolderList, deletedFolderList);
			Result := false;
		end;

		if deletedFolderList0 <> nil then // send results to deletedFolderList0, deletedNonResFileList, deletedResFileList, modifiedArchiveList
		begin

			deletedFolderList0.Assign(deletedFolderList);

			for elTemp in deletedFileList do
				if AnsiContainsStr(elTemp, archResSeparator) then
					deletedResFileList.Add(elTemp)
				else
					if deletedFolderList.IndexOf(trimCharsRight(ExtractFilePath(elTemp), '\', '/')) = -1 then
						deletedNonResFileList.Add(elTemp);

			for elTemp in modifiedFileList do
				if AnsiRightStr(elTemp, 1) = archResSeparator then
					modifiedArchiveList.Add(elTemp);

		end;

		oldFolderList.Free;
		newFolderList.Free;
		oldFileList.Free;
		newFileList.Free;
		addedFolderList.Free;
		deletedFolderList.Free;
		addedFileList.Free;
		modifiedFileList.Free;
		deletedFileList.Free;

	end
	else
	begin
		
		if FileExists(oldArchiveOrFolder) and FileExists(newArchiveOrFolder) and
			MatchStr(AnsiLowerCase(ExtractFileExt(oldArchiveOrFolder)), supportedExts) and
			MatchStr(AnsiLowerCase(ExtractFileExt(newArchiveOrFolder)), supportedExts)
		then // oldArchiveOrFolder and newArchiveOrFolder are likely MM Archive files
		begin
			Result := compareFile(oldArchiveOrFolder, newArchiveOrFolder);
			if not Result then
			begin
				addedFileList := TStringList.Create;
				modifiedFileList := TStringList.Create;
				deletedFileList := TStringList.Create;

				try // try it as if it's MM Archive file
					compareArchive(oldArchiveOrFolder, newArchiveOrFolder,
									addedFileList, modifiedFileList, deletedFileList);
					archSimpNew := MMArchSimple.load(newArchiveOrFolder);

					nameTemp := ExtractFileName(newArchiveOrFolder);

					for elTemp in addedFileList do
					begin
						if copyToFolder <> '' then // if needs file copy
						begin
							archSimpNew.extract(withTrailingSlash(copyToFolder) + nameTemp + MMArchiveExt, elTemp);

							if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
								cleanUpOldFolder(copyToFolder, withTrailingSlash(nameTemp + MMArchiveExt) + elTemp, 5); // [+/m] resourcefile
						end;
					end;

					for elTemp in modifiedFileList do
					begin
						if copyToFolder <> '' then // if needs file copy
						begin
							archSimpNew.extract(withTrailingSlash(copyToFolder) + nameTemp + MMArchiveExt, elTemp);

							if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
								cleanUpOldFolder(copyToFolder, withTrailingSlash(nameTemp + MMArchiveExt) + elTemp, 5); // [+/m] resourcefile
						end;
					end;

					for elTemp in deletedFileList do
					begin
						if copyToFolder <> '' then // if needs file copy
						begin
							createEmptyFile(withTrailingSlash(withTrailingSlash(copyToFolder) + nameTemp + MMArchiveExt) + elTemp + ToDeleteExt);

							if copyToFolderExists and (deletedFolderList0 = nil) then // (deletedFolderList0 = nil) means it's `filesonly`
								cleanUpOldFolder(copyToFolder, withTrailingSlash(nameTemp + MMArchiveExt) + elTemp, 6); // [-]   resourcefile
						end;
					end;

					// whether `FilesAreSame` has already been checked
					colorPrintFileList(addedFileList, modifiedFileList, deletedFileList);

					if deletedFolderList0 <> nil then // send results to deletedFolderList0, deletedNonResFileList, deletedResFileList, modifiedArchiveList
					begin

						// add nothing to deletedFolderList0 and deletedNonResFileList

						for elTemp in deletedFileList do
							deletedResFileList.Add(nameTemp + archResSeparator + elTemp);

						if (addedFileList.Count > 0) and (modifiedFileList.Count > 0) then
							modifiedArchiveList.Add(nameTemp + archResSeparator);

					end;

				except // very unlikely to be MM Archive file
					raise Exception.Create(IncorrectMMArchive);
				end;

				addedFileList.Free;
				modifiedFileList.Free;
				deletedFileList.Free;
			end
			else // compareFile(oldArchiveOrFolder, newArchiveOrFolder) = true
			begin
				WriteLn(FilesAreSame);
			end;

		end
		else // oldArchiveOrFolder and newArchiveOrFolder are not folders, and not MMArchive files
		begin
			raise Exception.Create(IncorrectFoldersOrMMArchives);
		end;

	end;

end;


function compareBase(oldArchiveOrFolder, newArchiveOrFolder: string; copyToFolder: string = ''): boolean; overload;
var
	deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;

begin

	deletedFolderList := nil;
	deletedNonResFileList := nil;
	deletedResFileList := nil;
	modifiedArchiveList := nil;

	Result := compareBase(oldArchiveOrFolder, newArchiveOrFolder, copyToFolder,
				deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList);

end;


procedure getListFromDiffFiles(oldDiffFileFolder: string; var deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList);
var
	elTemp: string;
	extListTemp: TStringList;
	oldDiffFileFolderLen, i: integer;

begin

	extListTemp := TStringList.Create;

	oldDiffFileFolderLen := length(withTrailingSlash(oldDiffFileFolder));

	// deletedFolderList: .todelete [folder]
	extListTemp.Add(ToDeleteExt);
	addAllFilesToFileList(deletedFolderList, oldDiffFileFolder, 1, true, false, extListTemp);
	for i := 0 to deletedFolderList.Count - 1 do
	begin
		elTemp := deletedFolderList[i];
		System.Delete(elTemp, 1, oldDiffFileFolderLen);
		System.Delete(elTemp, length(elTemp) - 8, 9);
		deletedFolderList[i] := elTemp;
	end;

	// deletedNonResFileList: .todelete [file, not in .mmarchive folder]
	addAllFilesToFileList(deletedNonResFileList, oldDiffFileFolder, 1, false, false, extListTemp);
	for i := deletedNonResFileList.Count - 1 downto 0 do
	begin

		elTemp := deletedNonResFileList[i];
		System.Delete(elTemp, 1, oldDiffFileFolderLen);
		System.Delete(elTemp, length(elTemp) - 8, 9);
		deletedNonResFileList[i] := elTemp;

		// deletedResFileList: .todelete [file, in .mmarchive folder]
		if AnsiContainsStr(elTemp, MMArchiveExt + '\') then
		begin
			elTemp := StringReplace(elTemp, MMArchiveExt + '\', archResSeparator, [rfReplaceAll, rfIgnoreCase]);
			deletedResFileList.Add(elTemp);
			deletedNonResFileList.Delete(i);
		end

	end;

	// modifiedArchiveList: .mmarchive [folder]
	extListTemp.Delete(0);
	extListTemp.Add(MMArchiveExt);
	addAllFilesToFileList(modifiedArchiveList, oldDiffFileFolder, 1, true, false, extListTemp);
	for i := 0 to modifiedArchiveList.Count - 1 do
	begin
		elTemp := modifiedArchiveList[i];
		System.Delete(elTemp, 1, oldDiffFileFolderLen);
		System.Delete(elTemp, length(elTemp) - 9, 10);
		modifiedArchiveList[i] := elTemp + archResSeparator;
	end;

	extListTemp.Free;

end;


end.
