unit MMArchChecksum;

interface

uses
	SysUtils, Classes;

function CRC32Stream(Stream: TStream): Cardinal;
function CRC32Buf(Data: Pointer; Size: Integer): Cardinal;
function CRC32File(const FileName: string): Cardinal;
function CRC32ToHex(Value: Cardinal): string;

// High-level checksum functions for MMArchSimple
// (defined here to keep MMArchMain lean; implementation uses MMArchMain)
function ChecksumResourceFile(archSimp: TObject; fileName: string): string;
function ChecksumAllResources(archSimp: TObject; ext: string = '*'): string;

implementation

uses
	MMArchMain, MMArchPath, RSLod, {ZLib}dzlib;

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

function CRC32Buf(Data: Pointer; Size: Integer): Cardinal;
var
	CRC: Cardinal;
	i: Integer;
	p: PByte;
begin
	InitCRC32Table;
	CRC := $FFFFFFFF;
	p := PByte(Data);
	for i := 0 to Size - 1 do
	begin
		CRC := CRC32Table[(CRC xor p^) and $FF] xor (CRC shr 8);
		Inc(p);
	end;
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


// Decompress zlib data from raw bytes into output stream.
// Returns true on success.
procedure ZlibDecompress(output: TStream; data: Pointer; dataSize: integer);
var
	inputMs: TMemoryStream;
	decompStream: TDecompressionStream;
	buf: array[0..65535] of Byte;
	bytesRead: integer;
begin
	inputMs := TMemoryStream.Create;
	try
		inputMs.Write(data^, dataSize);
		inputMs.Seek(0, 0);
		decompStream := TDecompressionStream.Create(inputMs);
		try
			repeat
				bytesRead := decompStream.Read(buf, SizeOf(buf));
				if bytesRead > 0 then
					output.Write(buf, bytesRead);
			until bytesRead = 0;
		finally
			decompStream.Free;
		end;
	finally
		inputMs.Free;
	end;
end;


// Extract raw data for CRC computation, matching Rust's read_entry_data behavior.
// This avoids format conversion (no PCX->BMP, no sprite unpacking) and instead
// returns the decompressed/unwrapped data as Rust does.
procedure ExtractRawForChecksum(arch: TRSMMArchive; fileIndex: integer; output: TStream);
var
	rawMs: TMemoryStream;
	raw: PByteArray;
	rawSize: integer;
	lod: TRSLod;
	ver: TRSLodVersion;
	nameSize, payloadStart: integer;
	bmpSize, dataSize, unpSize: integer;
	ext: string;
	hdrSize: integer;
begin
	if arch is TRSLod then
	begin
		lod := TRSLod(arch);
		ver := lod.Version;

		case ver of
			RSLodHeroes:
			begin
				// RawExtract decompresses if packed - same as Rust
				arch.RawFiles.RawExtract(fileIndex, output);
			end;

			RSLodBitmaps, RSLodIcons, RSLodMM8:
			begin
				// Raw data format: NameSize + TMMLodFile header (32 bytes) + payload
				// Rust uses fixed TMMLODFILE_NAME_SIZE=16 for all these types
				rawMs := TMemoryStream.Create;
				try
					arch.RawFiles.RawExtract(fileIndex, rawMs);
					rawSize := rawMs.Size;
					raw := rawMs.Memory;
					nameSize := 16; // Match Rust's TMMLODFILE_NAME_SIZE
					payloadStart := nameSize + 32; // 16 + SizeOf(TMMLodFile)

					if rawSize < payloadStart then
					begin
						// Too small for header - return as-is (matches Rust)
						output.Write(raw^, rawSize);
					end
					else
					begin
						// Read header fields from offset nameSize
						bmpSize := PInteger(@raw[nameSize])^;
						dataSize := PInteger(@raw[nameSize + 4])^;
						unpSize := PInteger(@raw[nameSize + 24])^; // UnpSize offset within header

						if bmpSize <> 0 then
						begin
							// Bitmap image: pixel data + palette
							if dataSize = 0 then
								output.Write(raw[payloadStart], rawSize - payloadStart)
							else
							begin
								// Decompress pixel data if needed
								if (unpSize > 0) and (dataSize <> unpSize) then
									ZlibDecompress(output, @raw[payloadStart], dataSize)
								else
									output.Write(raw[payloadStart], dataSize);
								// Append palette (768 bytes) if present
								if payloadStart + dataSize + 768 <= rawSize then
									output.Write(raw[payloadStart + dataSize], 768);
							end;
						end
						else
						begin
							if dataSize = 0 then
							begin
								// Palette (.act) - 768 bytes after header
								if rawSize >= payloadStart + 768 then
									output.Write(raw[payloadStart], 768)
								else
									output.Write(raw[payloadStart], rawSize - payloadStart);
							end
							else
							begin
								// Compressed non-image data
								if (unpSize > 0) and (dataSize <> unpSize) then
									ZlibDecompress(output, @raw[payloadStart], dataSize)
								else
									output.Write(raw[payloadStart], dataSize);
							end;
						end;
					end;
				finally
					rawMs.Free;
				end;
			end;

			RSLodGames, RSLodGames7, RSLodChapter, RSLodChapter7:
			begin
				ext := LowerCase(ExtractFileExt(arch.RawFiles.Name[fileIndex]));
				if (ext = '.blv') or (ext = '.dlv') or (ext = '.odm') or (ext = '.ddm') then
				begin
					// Compressed game file with header
					rawMs := TMemoryStream.Create;
					try
						arch.RawFiles.RawExtract(fileIndex, rawMs);
						rawSize := rawMs.Size;
						raw := rawMs.Memory;

						if ver in [RSLodGames7, RSLodChapter7] then
							hdrSize := 16
						else
							hdrSize := 8;

						if rawSize > hdrSize then
							ZlibDecompress(output, @raw[hdrSize], rawSize - hdrSize);
					finally
						rawMs.Free;
					end;
				end
				else
				begin
					// Non-compressed game file - raw data as-is
					arch.RawFiles.RawExtract(fileIndex, output);
				end;
			end;

			RSLodSprites:
			begin
				// Sprites: raw data as-is (matches Rust)
				arch.RawFiles.RawExtract(fileIndex, output);
			end;

		else
			// Fallback: use standard Extract
			arch.Extract(fileIndex, output);
		end;
	end
	else
	begin
		// Non-LOD (SND, VID) - use base Extract
		arch.Extract(fileIndex, output);
	end;
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

	extractName := arch.GetExtractName(fileIndex);
	ms := TMemoryStream.Create;
	try
		ExtractRawForChecksum(arch, fileIndex, ms);
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
					ExtractRawForChecksum(arch, i, ms);
					extractName := arch.GetExtractName(i);
					crc := CRC32Stream(ms);
					Result := Result + CRC32ToHex(crc) + '  ' + extractName + #13#10;
				except
					on E: Exception do
					begin
						WriteLn(ErrOutput, format('File `%s` in archive `%s` error:', [beautifyPath(fFiles.Name[i]), beautifyPath(fFiles.FileName)]));
						WriteLn(ErrOutput, E.Message);
					end;
				end;
			finally
				ms.Free;
			end;
		end;
	end;
end;


end.
