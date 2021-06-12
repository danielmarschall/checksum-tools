unit MD5;

interface

uses
  Classes, Common;

type
  TCheckSumFileMD5 = class(TCheckSumFile)
  protected
    Fmd5File: string;
  public
    constructor Create(AFileName: string); override;
    procedure ToStringList(slOut: TStringList); override;
    function SingleFileChecksum(AFileName: string): string; override;
    function MergeLine(AFileName, ACheckSum: string): string; override;
  end;

function md5file(const filename: string): string;
procedure MD5FileToStringList(amd5file: string; slOut: TStringList);

implementation

uses
  SysUtils, IdHashMessageDigest, idHash, LongFilenameOperations;

function md5file(const filename: string): string;
var
  IdMD5: TIdHashMessageDigest5;
  FS: TFileStream;
begin
  IdMD5 := TIdHashMessageDigest5.Create;
  FS := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
{$IFDEF UNICODE} // I actually do not know at which version of Delphi/Indy, this has changed.
    Result := IdMD5.HashStreamAsHex(FS);
{$ELSE}
    Result := IdMD5.AsHex(IdMD5.HashValue(FS));
{$ENDIF}
  finally
    FS.Free;
    IdMD5.Free;
  end;
end;

procedure MD5FileToStringList(amd5file: string; slOut: TStringList);
var
  sLine: string;
  originalFilename: string;
  expectedChecksum: string;
  fil: THandle;
  csum: TChecksum;
  firstLinePassed: boolean;
  forceUTF8: boolean;
begin
  if not FileExists(amd5file) then
    exit;

  MyAssignFile(fil, amd5file);
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
      // 25bfdef2651071efdd08bb3404797384 *Example.doc
      sLine := Trim(sLine);
      if sLine = '' then
        continue;
      expectedChecksum := Copy(sLine, 1, 32);
      Delete(sLine, 1, 32);
      sLine := Trim(sLine);
      if Copy(sLine, 1, 1) = '*' then
        Delete(sLine, 1, 1);
      sLine := Trim(sLine);
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

{ TCheckSumFileMD5 }

constructor TCheckSumFileMD5.Create(AFileName: string);
begin
  inherited;
  fmd5File := AFileName;
  if not SameText(ExtractFileExt(AFileName),'.sfv') then
    raise Exception.Create('Invalid checksum file extension.');
end;

function TCheckSumFileMD5.MergeLine(AFileName, ACheckSum: string): string;
begin
  result := ACheckSum + ' *' + AFileName;
end;

function TCheckSumFileMD5.SingleFileChecksum(AFileName: string): string;
begin
  result := md5file(AFileName);
end;

procedure TCheckSumFileMD5.ToStringList(slOut: TStringList);
begin
  inherited;
  MD5FileToStringList(fmd5File, slOut);
end;

end.
