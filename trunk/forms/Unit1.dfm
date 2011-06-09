object Form1: TForm1
  Left = 232
  Top = 92
  Caption = 'Form1'
  ClientHeight = 513
  ClientWidth = 847
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 273
    Width = 847
    Height = 8
    Cursor = crVSplit
    Align = alTop
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 847
    Height = 273
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'VAR x, squ;'
      ' '
      'PROCEDURE square;'
      'BEGIN'
      '   squ := x * x'
      'END;'
      ' '
      'BEGIN'
      '   x := 1;'
      '   WHILE x <= 10 DO'
      '   BEGIN'
      '      CALL square;'
      '      x := x + 1'
      '   END'
      'END.')
    ParentFont = False
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 0
    Top = 281
    Width = 847
    Height = 191
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 472
    Width = 847
    Height = 41
    Align = alBottom
    TabOrder = 2
    object Button1: TButton
      Left = 400
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Parse'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
