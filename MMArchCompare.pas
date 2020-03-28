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

type
	MissingParamException = class(Exception);

procedure colorWriteLn(str: string; color: word);
function compareFile(oldFile, newFile: string): boolean;
function compareInArchiveFile(oldRaw, newRaw: TRSMMFiles; oldIndex, newIndex: integer): boolean;
procedure compareArchive(oldArchive, newArchive: string; var addedFileList, modifiedFileList, deletedFileList: TStringList);
procedure generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
			scriptFilePath, diffFileFolderName: string; isNsis: boolean);
procedure compareBase(oldArchiveOrFolder, newArchiveOrFolder, copyToFolder: string;
			var deletedFolderList0, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList); overload;
procedure compareBase(oldArchiveOrFolder, newArchiveOrFolder: string; copyToFolder: string = ''); overload;
procedure getListFromDiffFiles(oldDiffFileFolder: string; var deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList);


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


procedure generateScript(deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList;
			scriptFilePath, diffFileFolderName: string; isNsis: boolean);
// isNsis is true : NSIS
// isNsis is false: Batch
var
	scriptString, elTemp, strTemp: string;
	tslTemp: TStringList;

begin

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
			'	SetOutPath $INSTDIR'#13#10 +
			'	File mmarch.exe'#13#10 +
			#13#10 +
			'	File /r /x *.todelete ' + withTrailingSlash(beautifyPath(diffFileFolderName)) + '*.*'#13#10 +
			#13#10#13#10;

		for elTemp in deletedFolderList do
		begin
			scriptString := scriptString + '	RMDir /r /REBOOTOK "$INSTDIR\' + elTemp + '"'#13#10;
		end;
		scriptString := scriptString + #13#10;

		for elTemp in deletedNonResFileList do
		begin
			scriptString := scriptString + '	Delete "' + elTemp + '"'#13#10;
		end;
		scriptString := scriptString + #13#10;

		for elTemp in deletedResFileList do
		begin
			tslTemp := TStringList.Create;
			Split(archResSeparator, elTemp, tslTemp);
			scriptString := scriptString +
				'	nsExec::Exec ''mmarch delete "' + tslTemp[0] + '" "' + tslTemp[1] + '"'''#13#10 +
				'	Delete "' + tslTemp[0] + '.mmarchive\' + tslTemp[1] + '"'#13#10;
		end;
		scriptString := scriptString + #13#10;

		for elTemp in modifiedArchiveList do
		begin
			strTemp := System.Copy(elTemp, 1, length(elTemp) - 1);
			scriptString := scriptString +
				'	nsExec::Exec ''mmarch add "' + strTemp + '" "' + strTemp + '.mmarchive\*.*"'''#13#10 +
				'	RMDir /r /REBOOTOK "$INSTDIR\' + strTemp + '.mmarchive"'#13#10;
		end;
		scriptString := scriptString + #13#10;

		scriptString := scriptString + #13#10 +
			'	Delete "mmarch.exe"'#13#10 +
			#13#10 +
			';-----FILE COPYING (MODIFYING, DELETING) ENDS HERE-----'#13#10 +
			#13#10 +
			'SectionEnd'#13#10;

	end
	else // Batch
	begin

		scriptString := 'cd %~dp0'#13#10 +
			#13#10 +
			'echo .todelete>excludelist.txt'#13#10 +
			'Xcopy files . /s /e /y /EXCLUDE:excludelist.txt'#13#10 +
			'del excludelist.txt'#13#10 +
			#13#10#13#10;

		for elTemp in deletedFolderList do
		begin
			scriptString := scriptString + 'rmdir /s /q "' + elTemp + '"'#13#10;
		end;
		scriptString := scriptString + #13#10;

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
				'mmarch delete "' + tslTemp[0] + '" "' + tslTemp[1] + '"'#13#10 +
				'Delete "' + tslTemp[0] + '.mmarchive\' + tslTemp[1] + '"'#13#10;
		end;
		scriptString := scriptString + #13#10;

		for elTemp in modifiedArchiveList do
		begin
			strTemp := System.Copy(elTemp, 1, length(elTemp) - 1);
			scriptString := scriptString +
				'mmarch add "' + strTemp + '" "' + strTemp + '.mmarchive\*.*"'#13#10 +
				'rmdir /s /q "' + strTemp + '.mmarchive"'#13#10;
		end;

	end;

	StrToFile(scriptFilePath, scriptString);

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

	elTemp, elTemp2,
	oldArchivePath, newArchivePath, pathTemp: string;

	oldArchiveOrFolderLen, newArchiveOrFolderLen,

	i, nTemp: integer;

	color: word;

	archSimpNew: MMArchSimple;

begin

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
			if deletedFolderList.IndexOf(trimCharsRight(ExtractFilePath(elTemp), '\', '/')) = -1 then
				createEmptyFile(withTrailingSlash(copyToFolder) + elTemp + '.todelete');
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

		if AnsiContainsStr(elTemp, archResSeparator) then
			color := color or BACKGROUND_BLUE;
		colorWriteLn(elTemp, color);

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
	allList.Free;

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


procedure getListFromDiffFiles(oldDiffFileFolder: string; var deletedFolderList, deletedNonResFileList, deletedResFileList, modifiedArchiveList: TStringList);
var
	elTemp: string;
	extListTemp: TStringList;
	oldDiffFileFolderLen, i: integer;

begin

	extListTemp := TStringList.Create;

	oldDiffFileFolderLen := length(withTrailingSlash(beautifyPath(oldDiffFileFolder)));

	// deletedFolderList: .todelete [folder]
	extListTemp.Add('.todelete');
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
		if AnsiContainsStr(elTemp, '.mmarchive\') then
		begin
			elTemp := StringReplace(elTemp, '.mmarchive\', archResSeparator, [rfReplaceAll, rfIgnoreCase]);
			deletedResFileList.Add(elTemp);
			deletedNonResFileList.Delete(i);
		end

	end;

	// modifiedArchiveList: .mmarchive [folder]
	extListTemp.Delete(0);
	extListTemp.Add('.mmarchive');
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