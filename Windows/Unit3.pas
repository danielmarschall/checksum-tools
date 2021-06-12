unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Memo2: TMemo;
    Label2: TLabel;
    Memo3: TMemo;
    Label3: TLabel;
    Memo4: TMemo;
    Label4: TLabel;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FChecksumFile: string;
  public
    function ParamChecksumFile: string;
    procedure LoadSFV;
    procedure SaveSFV;
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses
  Common, SFV, MD5, LongFilenameOperations;

procedure TForm3.Button1Click(Sender: TObject);
begin
  SaveSFV;
  LoadSFV;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  Caption := ParamChecksumFile;
  LoadSFV;
end;

procedure TForm3.LoadSFV;
var
  FileName: string;
  ADirectory: string;
  slSFV: TStringList;
  SR: TSearchRec;
  IsFound: Boolean;
  i: Integer;
  TestFilename: string;
  SollChecksum: string;
  IstChecksum: string;
  existingFiles: TStringList;
  j: Integer;
  csman: TCheckSumFile;
begin
  FileName := ParamChecksumFile;
  ADirectory := ExtractFilePath(FileName);

  if not FileExists(FileName) then
  begin
    ShowMessageFmt('File not found: %s', [FileName]);
    Close;
  end;

  Memo1.Clear;
  Memo2.Clear;
  Memo3.Clear;
  Memo4.Clear;

  if SameText(ExtractFileExt(FileName), '.sfv') then
    csman := TCheckSumFileSFV.Create(FileName)
  else if SameText(ExtractFileExt(FileName), '.md5') then
    csman := TCheckSumFileMD5.Create(FileName)
  else
    Exception.Create('Unknown file extension. Only supporting MD5 and SFV.');

  slSFV := TStringList.Create;
  existingFiles := TStringList.Create;
  try
    // Read SFV/MD5 file
    csman.ToStringList(slSFV);

    // List existing files
    IsFound := FindFirst(ADirectory + '*', faAnyFile, SR) = 0;
    while IsFound do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        existingFiles.Add(LowerCase(SR.Name));
      end;
      IsFound := FindNext(SR) = 0;
    end;
    FindClose(SR);

    // Checksum mismatch or missing files
    for i := 0 to slSFV.Count-1 do
    begin
      TestFilename := IncludeTrailingPathDelimiter(ADirectory) + slSFV.Strings[i];
      SollChecksum := TCheckSum(slSFV.Objects[i]).checksum;
      if not FileExists(TestFilename) then
      begin
        Memo3.Lines.Add(csman.MergeLine(slSFV.Strings[i], SollChecksum));
      end
      else
      begin
        IstChecksum  := CalcFileCRC32(TestFilename);
        if SameText(SollChecksum, IstChecksum) then
        begin
          Memo1.Lines.Add(csman.MergeLine(slSFV.Strings[i], SollChecksum));
        end
        else
        begin
          Memo2.Lines.Add('; [CURRENT FILE CHECKSUM] ' + csman.MergeLine(slSFV.Strings[i], IstChecksum));
          Memo2.Lines.Add(csman.MergeLine(slSFV.Strings[i], SollChecksum));
        end;

        j := existingFiles.IndexOf(LowerCase(slSFV.Strings[i]));
        if j >= 0 then existingFiles.Delete(j);
      end;
    end;

    // Check non-indexed files
    for i := 0 to existingFiles.Count-1 do
    begin
      TestFileName := existingFiles[i];
      if not SameText(ExtractFileExt(TestFileName), '.sfv') and
         not SameText(ExtractFileExt(TestFileName), '.md5') and
         not SameText(TestFileName, 'Thumbs.db') then
      begin
        IstChecksum  := CalcFileCRC32(IncludeTrailingPathDelimiter(ADirectory) + TestFilename);
        Memo4.Lines.Add(csman.MergeLine(TestFileName, IstChecksum));
      end;
    end;
  finally
    FreeAndNil(slSFV);
    FreeAndNil(existingFiles);
    FreeAndNil(csman);
  end;
end;

function TForm3.ParamChecksumFile: string;
begin
  if FChecksumFile <> '' then
  begin
    result := FChecksumFile;
  end
  else
  begin
    result := ParamStr(1);
    if result = '' then
    begin
      if not OpenDialog1.Execute then
      begin
        Close;
        Abort;
      end;
      result := OpenDialog1.FileName;
    end;
    FChecksumFile := result;
  end;
end;

procedure TForm3.SaveSFV;
var
  hFile: THandle;
  i: Integer;
begin
  MyAssignFile(hFile, ParamChecksumFile);
  MyRewrite(hFile); // clear File
  for i := 0 to memo1.Lines.Count-1 do
  begin
    if Trim(Memo1.Lines[i]) <> '' then
      MyWriteLn(hFile, AnsiString(utf8encode(Memo1.Lines[i])));
  end;
  for i := 0 to memo2.Lines.Count-1 do
  begin
    if Trim(Memo2.Lines[i]) <> '' then
      MyWriteLn(hFile, AnsiString(utf8encode(Memo2.Lines[i])));
  end;
  for i := 0 to memo3.Lines.Count-1 do
  begin
    if Trim(Memo3.Lines[i]) <> '' then
      MyWriteLn(hFile, AnsiString(utf8encode(Memo3.Lines[i])));
  end;
  for i := 0 to memo4.Lines.Count-1 do
  begin
    if Trim(Memo4.Lines[i]) <> '' then
      MyWriteLn(hFile, AnsiString(utf8encode(Memo4.Lines[i])));
  end;
  MyCloseFile(hFile);
end;

end.
