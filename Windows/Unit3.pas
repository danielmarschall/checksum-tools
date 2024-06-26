unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

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
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
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

procedure TForm3.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = []) and (Key = VK_F6) then
  begin
    Key := 0;
    SaveSFV;
    LoadSFV;
  end
  else if (Shift = []) and (Key = VK_ESCAPE) then
  begin
    Key := 0;
    Close;
  end
  else if (Shift = []) and (Key = VK_F5) then
  begin
    Key := 0;
    LoadSFV;
  end;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  Caption := ParamChecksumFile;
  Timer1.Enabled := true;
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
  Color := clGray;
  Refresh;

  FileName := ParamChecksumFile;
  if Copy(FileName, 2, 1) = ':' then FileName := '\\?\' + FileName; // To allow long filenames
  ADirectory := ExtractFilePath(FileName);

  if not FileExists(FileName) then
  begin
    ShowMessageFmt('File not found: %s', [FileName]);
    Close;
    exit;
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
    IsFound := FindFirst(ADirectory + '*', faAnyFile xor faDirectory, SR) = 0;
    while IsFound do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        existingFiles.Add(SR.Name);
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
        IstChecksum  := csman.SingleFileChecksum(TestFilename);
        if SameText(SollChecksum, IstChecksum) then
        begin
          Memo1.Lines.Add(csman.MergeLine(slSFV.Strings[i], SollChecksum));
        end
        else
        begin
          Memo2.Lines.Add('; [CURRENT FILE CHECKSUM] ' + csman.MergeLine(slSFV.Strings[i], IstChecksum));
          Memo2.Lines.Add(csman.MergeLine(slSFV.Strings[i], SollChecksum));
        end;

        {$IFDEF MSWINDOWS}
        existingFiles.CaseSensitive := false;
        {$ELSE}
        existingFiles.CaseSensitive := true;
        {$ENDIF}
        j := existingFiles.IndexOf(slSFV.Strings[i]);
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
        IstChecksum  := csman.SingleFileChecksum(IncludeTrailingPathDelimiter(ADirectory) + TestFilename);
        Memo4.Lines.Add(csman.MergeLine(TestFileName, IstChecksum));
      end;
    end;

    if (Memo2.Text = '') and (Memo3.Text = '') and (Memo4.Text = '') then
      Color := clMoneyGreen
    else if (Memo2.Text <> '') then
      Color := clRed
    else
      Color := clYellow;

    Memo1.SelStart := 0;
    Memo1.SelLength := 0;
    Memo2.SelStart := 0;
    Memo2.SelLength := 0;
    Memo3.SelStart := 0;
    Memo3.SelLength := 0;
    Memo4.SelStart := 0;
    Memo4.SelLength := 0;

    if Memo2.Text <> '' then Memo2.SetFocus
    else if Memo3.Text <> '' then Memo3.SetFocus
    else if Memo4.Text <> '' then Memo4.SetFocus
    else Memo1.SetFocus;
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
  slOut: TStringList;
begin
  MyAssignFile(hFile, ParamChecksumFile);
  try
    MyRewrite(hFile); // clear File

    slOut := TStringList.Create;
    try
      // Fill slOut with the Memo contents
      for i := 0 to memo1.Lines.Count-1 do
      begin
        slOut.Add(Memo1.Lines[i]);
      end;
      for i := 0 to memo2.Lines.Count-1 do
      begin
        slOut.Add(Memo2.Lines[i]);
      end;
      for i := 0 to memo3.Lines.Count-1 do
      begin
        slOut.Add(Memo3.Lines[i]);
      end;
      for i := 0 to memo4.Lines.Count-1 do
      begin
        slOut.Add(Memo4.Lines[i]);
      end;

      // Sort
      slOut.Sort;

      // Write to SFV/MD5 file
      for i := 0 to slOut.Count-1 do
      begin
        if Trim(slOut[i]) <> '' then
          MyWriteLn(hFile, AnsiString(utf8encode(slOut[i])));
      end;
    finally
      FreeAndNil(slOut);
    end;
  finally
    MyCloseFile(hFile);
  end;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  LoadSFV;
end;

end.
