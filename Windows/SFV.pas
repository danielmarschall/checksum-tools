unit SFV;

interface

uses
  Classes;

function CalcFileCRC32(filename: string): string; overload;
procedure SFVFileToStringList(aSFVFile: string; slOut: TStringList);

implementation

uses
  Windows, SysUtils, CRC32, Common, LongFilenameOperations;

function CalcFileCRC32(filename: string): string; overload;
var
  checksum: DWORD;
  totalbytes: TInteger8;
  error: Word;
begin
  CRC32.CalcFileCRC32(filename, checksum, totalbytes, error);
  if error = 0 then
    result := IntToHex(checksum, 8)
  else
    result := '';
end;

procedure SFVFileToStringList(aSFVFile: string; slOut: TStringList);
var
  sLine: string;
  originalFilename: string;
  expectedChecksum: string;
  fil: THandle;
  csum: TChecksum;
  firstLinePassed: boolean;
  forceUTF8: boolean;
begin
  if not FileExists(aSFVFile) then
    exit;

  MyAssignFile(fil, aSFVFile);
  try
    MyReset(fil);
    firstLinePassed := false;
    forceUTF8 := false;
    while not MyEOF(fil) do
    begin
      MyReadLn(fil, sLine);

      {$REGION 'Try UTF8 decode'}
      if not firstLinePassed and (length(sLine)>2) and (sLine[1]=#$EF) and (sLine[2]=#$BB) and (sLine[3]=#$BF) then
      begin
        delete(sLine,1,3); // Remove BOM
        forceUTF8 := true;
      end;
      firstLinePassed := true;

      if forceUTF8 or (Pos(#$FFFD, Utf8ToString(RawByteString(sLine))) = 0) then
        sLine := Utf8ToString(RawByteString(sLine));
      {$ENDREGION}

      if Copy(Trim(sLine),1,1) = ';' then continue;
      // Example.doc 4323C92B
      sLine := TrimRight(sLine); // Trim right, because file names may have leading white spaces
      if sLine = '' then
        continue;
      expectedChecksum := Copy(sLine, 1+Length(sLine)-8, 8);
      sLine := TrimRight(Copy(sLine, 1, Length(sLine)-8));  // Trim right, because file names may have leading white spaces
      originalFilename := sLine;

      //slOut.Values[originalFilename] := expectedChecksum; // <-- with this, files cannot have an equal sign
      slOut.OwnsObjects := true;
      csum := TChecksum.Create;
      csum.checksum := expectedChecksum;
      slOut.AddObject(originalFilename, csum);
    end;
  finally
    MyCloseFile(fil);
  end;
end;

end.
