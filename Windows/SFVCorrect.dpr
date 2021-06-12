program SFVCorrect;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Form3},
  SFV in 'SFV.pas',
  Common in 'Common.pas',
  MD5 in 'MD5.pas',
  LongFilenameOperations in 'LongFilenameOperations.pas',
  CRC32 in 'CRC32.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
