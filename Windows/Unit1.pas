unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TSeverity = (seOK, seWarning, seCritical);

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    cbVerbose: TCheckBox;
    LabeledEdit1: TLabeledEdit;
    cbWarnChecksumFileMissing: TCheckBox;
    cbWarningMissingChecksumFileEntry: TCheckBox;
    cbWarnVanishedFile: TCheckBox;
    cbWarnChecksumMismatch: TCheckBox;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CheckSumFileCount: integer;
    function CheckDirectory(ADirectory: string; recursive: boolean): TSeverity;
    function VerifyChecksumFile(aChecksumFile: string): TSeverity;
    function GetChecksumSafe(const filename: string): string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  MD5, SFV, Common;

const
  DUMMY_FILE = 'DUMMY.$$$';

procedure TForm1.Button1Click(Sender: TObject);
var
  sev: TSeverity;
begin
  Memo1.Clear;
  if not DirectoryExists(LabeledEdit1.Text) then
  begin
    showmessage('Directory does not exist');
    exit;
  end;
  Application.ProcessMessages;
  CheckSumFileCount := 0;
  sev := CheckDirectory(LabeledEdit1.Text, true);
  Beep;
  case sev of
    seOK:
      showmessage('OK');
    seWarning:
      showmessage('Warning');
    seCritical:
      showmessage('Critical');
  end;
  Caption := Format('Done. Checked %d checksum files.', [CheckSumFileCount]);
end;

function SevMax(a, b: TSeverity): TSeverity;
begin
  if Ord(a) > Ord(b) then
    Result := a
  else
    Result := b;
end;

function TForm1.VerifyChecksumFile(aChecksumFile: string): TSeverity;
var
  slFile: TStringList;
  i: integer;
  originalFilename: string;
  expectedChecksum: string;
  IsFound: boolean;
  SR: TSearchRec;
  fullfilename: string;
  ADirectory: string;
  originalFilenameFull: string;
begin
  if ExtractFileName(aChecksumFile) <> DUMMY_FILE then
  begin
    Inc(CheckSumFileCount);
  end;

  if cbVerbose.Checked then
  begin
    Form1.Memo1.Lines.Add('Check: ' + aChecksumFile);
  end;

  Result := seOK;
  ADirectory := IncludeTrailingPathDelimiter(ExtractFilePath(aChecksumFile));

  try
    slFile := TStringList.Create;
    try
      slFile.CaseSensitive := false;
      slFile.OwnsObjects := true;

      if radiogroup1.itemindex = 0 then
        SFVFileToStringList(aChecksumFile, slFile)
      else
        MD5FileToStringList(aChecksumFile, slFile);
      // TODO: If multiple checksum files => put them together into a single array (beware conflicts!)

      // 1. Check existing entries in the checksum file

      for i := 0 to slFile.Count - 1 do
      begin
        originalFilename := slFile.Strings[i];
        expectedChecksum := TChecksum(slFile.Objects[i]).checksum;

        originalFilenameFull := ADirectory + originalFilename;

        if not FileExists(originalFilenameFull) then
        begin
          if cbWarnVanishedFile.Checked then
          begin
            Form1.Memo1.Lines.Add('FILE VANISHED: ' + originalFilenameFull);
            Result := SevMax(Result, seCritical);
          end;
        end
        else if LowerCase(GetChecksumSafe(originalFilenameFull))
          = LowerCase(expectedChecksum) then
        begin
          if cbVerbose.Checked then
          begin
            Form1.Memo1.Lines.Add('OK: ' + originalFilenameFull + ' = ' +
              expectedChecksum);
          end;
          Result := SevMax(Result, seOK);
        end
        else
        begin
          if cbWarnChecksumMismatch.Checked then
          begin
            Form1.Memo1.Lines.Add('CHECKSUM MISMATCH: ' + originalFilenameFull +
              ' <> ' + expectedChecksum);
            Result := SevMax(Result, seCritical);
          end;
        end;
      end;

      // 2. Checking for entries which are NOT in the checksum file

      IsFound := FindFirst(ADirectory + '*', faAnyFile - faDirectory, SR) = 0;
      while IsFound do
      begin
        fullfilename := ADirectory + SR.Name;
        if (LowerCase(ExtractFileExt(fullfilename)) <> '.md5') and
           (LowerCase(ExtractFileExt(fullfilename)) <> '.sfv') and
           (LowerCase(ExtractFileName(fullfilename)) <> 'thumbs.db') then
        begin
          if slFile.IndexOf(SR.Name) = -1 then //if slFile.Values[SR.Name] = '' then
          begin
            if ExtractFileName(aChecksumFile) = DUMMY_FILE then
            begin
              if cbWarnChecksumFileMissing.Checked then
              begin
                Form1.Memo1.Lines.Add('NEW FILE WITHOUT CHECKSUM FILE: ' +
                  fullfilename);
                Result := SevMax(Result, seWarning);
              end;
            end
            else
            begin
              if cbWarningMissingChecksumFileEntry.Checked then
              begin
                Form1.Memo1.Lines.Add('NEW FILE WITHOUT CHECKSUM ENTRY: ' +
                  fullfilename);
                Result := SevMax(Result, seWarning);
              end;
            end;
          end;
        end;
        IsFound := FindNext(SR) = 0;
      end;
      FindClose(SR);
    finally
      slFile.Free;
    end;
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('Invalid checksum file: ' + aChecksumFile + ' : ' + E.Message);
      Result := seCritical;
    end;
  end;
end;

function TForm1.CheckDirectory(ADirectory: string; recursive: boolean)
  : TSeverity;
var
  IsFound: boolean;
  SR: TSearchRec;
  fullfilename: string;
begin
  Caption := ADirectory;
  Application.ProcessMessages;
  if Application.Terminated then Abort;

  Result := seOK;
  ADirectory := IncludeTrailingPathDelimiter(ADirectory);

  // Check checksum files
  if radiogroup1.itemindex = 0 then
    IsFound := FindFirst(ADirectory + '*.sfv', faAnyFile - faDirectory, SR) = 0
  else
    IsFound := FindFirst(ADirectory + '*.md5', faAnyFile - faDirectory, SR) = 0;
  if not IsFound then
  begin
    fullfilename := ADirectory + DUMMY_FILE; // virtual "empty" file
    Result := SevMax(Result, VerifyChecksumFile(fullfilename));
  end
  else
  begin
    while IsFound do
    begin
      fullfilename := ADirectory + SR.Name;

      Caption := fullfilename;
      Application.ProcessMessages;
      if Application.Terminated then Abort;

      Result := SevMax(Result, VerifyChecksumFile(fullfilename));
      IsFound := FindNext(SR) = 0;
    end;
  end;
  FindClose(SR);

  // Check other dirs
  if recursive then
  begin
    IsFound := FindFirst(ADirectory + '*', faAnyFile, SR) = 0;
    while IsFound do
    begin
      fullfilename := ADirectory + SR.Name;
      if DirectoryExists(fullfilename) and (SR.Name <> '.') and (SR.Name <> '..')
      then
      begin
        Result := SevMax(Result, CheckDirectory(fullfilename, recursive));
      end;
      IsFound := FindNext(SR) = 0;
    end;
    FindClose(SR);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if ParamCount >= 1 then
  begin
    LabeledEdit1.Text := ParamStr(1);
  end;
end;

function TForm1.GetChecksumSafe(const filename: string): string;
begin
  Caption := filename;
  Application.ProcessMessages;
  if Application.Terminated then Abort;

  try
    if radiogroup1.itemindex = 0 then
      Result := CalcFileCRC32(filename)
    else
      Result := md5file(filename);
  except
    on E: Exception do
    begin
      Memo1.Lines.Add('Cannot read file ' + filename + ' : ' + E.Message);
      Result := 'ERROR';
    end;
  end;
end;

end.
