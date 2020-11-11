program MD5DirCheck;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  SFV in 'SFV.pas',
  MD5 in 'MD5.pas',
  CRC32 in 'CRC32.pas',
  Common in 'Common.pas',
  LongFilenameOperations in 'LongFilenameOperations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
