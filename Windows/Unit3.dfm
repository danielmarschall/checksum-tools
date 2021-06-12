object Form3: TForm3
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  ClientHeight = 619
  ClientWidth = 630
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 42
    Height = 13
    Caption = 'Files OK:'
  end
  object Label2: TLabel
    Left = 8
    Top = 235
    Width = 99
    Height = 13
    Caption = 'Checksum mismatch:'
  end
  object Label3: TLabel
    Left = 8
    Top = 346
    Width = 62
    Height = 13
    Caption = 'Files missing:'
  end
  object Label4: TLabel
    Left = 8
    Top = 457
    Width = 113
    Height = 13
    Caption = 'Files without checksum:'
  end
  object Memo1: TMemo
    Left = 8
    Top = 27
    Width = 601
    Height = 190
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 8
    Top = 254
    Width = 601
    Height = 86
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 8
    Top = 365
    Width = 601
    Height = 86
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Memo4: TMemo
    Left = 8
    Top = 476
    Width = 601
    Height = 86
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object Button1: TButton
    Left = 8
    Top = 568
    Width = 167
    Height = 38
    Caption = 'Merge, save and re-check'
    TabOrder = 4
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'SFV Files (*.sfv)|*.sfv|MD5 files (*.md5)|*.md5'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 560
    Top = 552
  end
end
