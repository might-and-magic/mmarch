// MMArchChecksum unit
// Part of mmarch
// Command line tool to handle Heroes 3 and Might and Magic 6, 7, 8
// resource archive files (e.g. lod files). Based on GrayFace's MMArchive.
// By Tom CHEN <tomchen.org@gmail.com> (tomchen.org)
// MIT License
// https://github.com/might-and-magic/mmarch



unit MMArchChecksum;

interface

uses
	SysUtils, Classes;

function CRC32Stream(Stream: TStream): Cardinal;
function CRC32File(const FileName: string): Cardinal;
function CRC32ToHex(Value: Cardinal): string;

// High-level checksum functions for MMArchSimple
// (defined here to keep MMArchMain lean; implementation uses MMArchMain)
function ChecksumResourceFile(archSimp: TObject; fileName: string): string;
function ChecksumAllResources(archSimp: TObject; ext: string = '*'): string;

implementation

uses
	MMArchMain, MMArchPath, RSLod;

var
	CRC32Table: array[0..255] of Cardinal;
	CRC32TableInit: Boolean = False;

procedure InitCRC32Table;
var
	i, j: Integer;
	CRC: Cardinal;
begin
	if CRC32TableInit then
		Exit;
	for i := 0 to 255 do
	begin
		CRC := Cardinal(i);
		for j := 0 to 7 do
		begin
			if (CRC and 1) <> 0 then
				CRC := (CRC shr 1) xor $EDB88320
			else
				CRC := CRC shr 1;
		end;
		CRC32Table[i] := CRC;
	end;
	CRC32TableInit := True;
end;

function CRC32Stream(Stream: TStream): Cardinal;
var
	Buffer: array[0..65535] of Byte;
	BytesRead: Integer;
	CRC: Cardinal;
	i: Integer;
begin
	InitCRC32Table;
	CRC := $FFFFFFFF;
	Stream.Position := 0;
	repeat
		BytesRead := Stream.Read(Buffer, SizeOf(Buffer));
		for i := 0 to BytesRead - 1 do
			CRC := CRC32Table[(CRC xor Buffer[i]) and $FF] xor (CRC shr 8);
	until BytesRead = 0;
	Result := CRC xor $FFFFFFFF;
end;

function CRC32File(const FileName: string): Cardinal;
var
	Stream: TFileStream;
begin
	Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
	try
		Result := CRC32Stream(Stream);
	finally
		Stream.Free;
	end;
end;

function CRC32ToHex(Value: Cardinal): string;
begin
	Result := IntToHex(Value, 8);
end;


function ChecksumResourceFile(archSimp: TObject; fileName: string): string;
var
	a: MMArchSimple;
	arch: TRSMMArchive;
	fileIndex: integer;
	ms: TMemoryStream;
	crc: Cardinal;
	extractName: string;
begin
	a := MMArchSimple(archSimp);
	arch := a.getTRSMMArchive;
	fileIndex := a.getIndexByFileName(fileName);
	if fileIndex = -1 then
		raise Exception.CreateFmt('File `%s` is not found in the archive', [fileName]);

	ms := TMemoryStream.Create;
	try
		extractName := arch.Extract(fileIndex, ms);
		crc := CRC32Stream(ms);
		Result := CRC32ToHex(crc) + '  ' + extractName;
	finally
		ms.Free;
	end;
end;


function ChecksumAllResources(archSimp: TObject; ext: string = '*'): string;
var
	a: MMArchSimple;
	arch: TRSMMArchive;
	fFiles: TRSMMFiles;
	fileCountMinusOne, i: integer;
	ms: TMemoryStream;
	crc: Cardinal;
	extractName: string;
begin
	a := MMArchSimple(archSimp);
	arch := a.getTRSMMArchive;
	fFiles := arch.RawFiles;
	fileCountMinusOne := fFiles.Count - 1;
	Result := '';

	for i := 0 to fileCountMinusOne do
	begin
		if a.matchesExtFilter(i, ext) then
		begin
			ms := TMemoryStream.Create;
			try
				try
					extractName := arch.Extract(i, ms);
					crc := CRC32Stream(ms);
					Result := Result + CRC32ToHex(crc) + '  ' + extractName + #13#10;
				except
					on E: Exception do
					begin
						WriteLn(format('File `%s` in archive `%s` error:', [beautifyPath(fFiles.Name[i]), beautifyPath(fFiles.FileName)]));
						WriteLn(E.Message);
					end;
				end;
			finally
				ms.Free;
			end;
		end;
	end;
end;


end.
