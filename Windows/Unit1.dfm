object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Recursive MD5/SFV Directory Checksum checker'
  ClientHeight = 621
  ClientWidth = 1006
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    1006
    621)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 571
    Width = 958
    Height = 42
    Anchors = [akLeft, akRight, akBottom]
    Caption = 
      'Hints: MD5/SFV files can be created by TotalCommander or other t' +
      'ools (also recursively).  Multiple MD5/SFV files per directory a' +
      're not permitted.  To check directories with path names longer t' +
      'han 255 characters, use the "long filename" path format \\?\<GUI' +
      'D>\  (find out GUID by running "mountvol"), instead of the usual' +
      ' C:\  file name format.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 776
    Top = 72
    Width = 217
    Height = 33
    Anchors = [akTop, akRight]
    Caption = 'Check'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 184
    Width = 985
    Height = 381
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitHeight = 376
  end
  object cbVerbose: TCheckBox
    Left = 8
    Top = 59
    Width = 105
    Height = 17
    Caption = 'Verbose output'
    TabOrder = 2
  end
  object LabeledEdit1: TLabeledEdit
    Left = 8
    Top = 32
    Width = 985
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 91
    EditLabel.Height = 13
    EditLabel.Caption = 'Directory to check:'
    TabOrder = 3
  end
  object cbWarnChecksumFileMissing: TCheckBox
    Left = 8
    Top = 82
    Width = 353
    Height = 17
    Caption = 'Warning if directory with files does not contain a checksum file'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object cbWarningMissingChecksumFileEntry: TCheckBox
    Left = 8
    Top = 105
    Width = 417
    Height = 17
    Caption = 
      'If checksum file is present: Warn if there are files which do no' +
      't have a checksum'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object cbWarnVanishedFile: TCheckBox
    Left = 8
    Top = 128
    Width = 401
    Height = 17
    Caption = 
      'If checksum file is present: Warn if files with checksum entries' +
      ' vanished'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object cbWarnChecksumMismatch: TCheckBox
    Left = 8
    Top = 151
    Width = 417
    Height = 17
    Caption = 
      'If checksum file is present: Warn if a checksum does not match t' +
      'he checksum file'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object RadioGroup1: TRadioGroup
    Left = 440
    Top = 72
    Width = 105
    Height = 97
    Caption = 'Method'
    ItemIndex = 0
    Items.Strings = (
      'SFV (CRC32)'
      'MD5')
    TabOrder = 8
  end
end
