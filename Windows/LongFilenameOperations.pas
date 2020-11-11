unit LongFilenameOperations;

interface

// These procedures should replace the Delphi internal AssignFile(), Reset(), etc.
// functions. These functions should be able to support long file names
// by using the WinAPI (the "long filename" mode is switched when the file
// name format \\?\ is used).

procedure MyAssignFile(var hFile: THandle; filename: string);
procedure MyReset(hFile: THandle);
procedure MyReadLn(hFile: THandle; var s: string);
procedure MyCloseFile(hFile: THandle);
function MyEOF(hFile: THandle): boolean;
procedure MyBlockRead(var hFile: THandle; var Buffer; RecordCount: integer; var RecordsRead: integer);

implementation

uses
  Windows, SysUtils;

procedure MyAssignFile(var hFile: THandle; filename: string);
begin
  hFile := CreateFile(PChar(filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_READONLY, 0);
  if hFile = INVALID_HANDLE_VALUE then RaiseLastOSError;
end;

procedure MyReset(hFile: THandle);
begin
  SetFilePointer(hFile, 0, nil, FILE_BEGIN);
end;

procedure MyReadLn(hFile: THandle; var s: string);
var
  buf: array [0..0] of ansichar;
  dwread: LongWord;
begin
  s := '';
  ReadFile(hFile, buf, 1, dwread, nil);
  while (dwread > 0) do
  begin
    if buf[0] <> #13 then // Note: The first line of SFV files contains a comment ended with LF while the other lines end with CR-LF
    begin
      if buf[0] = #10 then exit;
      s := s + string(buf[0]);
    end;
    Readfile(hFile, buf, 1, dwread, nil);
  end;
end;

procedure MyCloseFile(hFile: THandle);
begin
  CloseHandle(hFile);
end;

function MyEOF(hFile: THandle): boolean;
var
  buf: array [0..0] of ansichar;
  dwread: LongWord;
begin
  Readfile(hFile, buf, 1, dwread, nil);
  if dwread > 0 then
  begin
    SetFilePointer(hFile, -dwread, nil, FILE_CURRENT);
    result := false;
  end
  else
  begin
    result := true;
  end;
end;

procedure MyBlockRead(var hFile: THandle; var Buffer; RecordCount: integer; var RecordsRead: integer);
var
  RecordCount2, RecordsRead2: Cardinal;
begin
  RecordCount2 := RecordCount;
  RecordsRead2 := RecordsRead;
  ReadFile(hFile, Buffer, RecordCount2, RecordsRead2, nil);
  //RecordCount := RecordCount2;
  RecordsRead := RecordsRead2;
end;

end.
