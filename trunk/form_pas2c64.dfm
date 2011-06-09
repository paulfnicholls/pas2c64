object FormMainForm: TFormMainForm
  Left = 305
  Top = 87
  Caption = 'Pas2C64 Compiler'
  ClientHeight = 605
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 281
    Width = 862
    Height = 10
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitTop = 0
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 862
    Height = 281
    Align = alTop
    Caption = 'Source Code'
    TabOrder = 0
    object SynEdit_SourceCode: TSynEdit
      Left = 2
      Top = 15
      Width = 858
      Height = 264
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 0
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.ShowLineNumbers = True
      Highlighter = SynPasSyn
      Lines.Strings = (
        'program Test;'
        'begin'
        '  WriteLn('#39'hello world'#39');'
        'end.')
      WantTabs = True
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 548
    Width = 862
    Height = 57
    Align = alBottom
    TabOrder = 1
    object Label_CodeAddr: TLabel
      Left = 300
      Top = 29
      Width = 79
      Height = 13
      Caption = 'Code Address'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Button_Compile: TButton
      Left = 8
      Top = 6
      Width = 97
      Height = 25
      Caption = 'Compile (Ctrl+F9)'
      TabOrder = 0
      OnClick = Button_CompileClick
    end
    object Button_CompileAndRun: TButton
      Left = 128
      Top = 6
      Width = 133
      Height = 25
      Caption = 'Compile and  run (F9)'
      TabOrder = 1
      OnClick = Button_CompileAndRunClick
    end
    object CheckBox_UseBasicLoader: TCheckBox
      Left = 284
      Top = 6
      Width = 121
      Height = 17
      Caption = 'Use BASIC loader'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox_UseBasicLoaderClick
    end
    object Edit_CodeAddr: TEdit
      Left = 385
      Top = 29
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 3
      Text = '$c000'
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 291
    Width = 862
    Height = 257
    Align = alClient
    Caption = 'Assembly Output'
    TabOrder = 2
    ExplicitHeight = 278
    object SynEdit_AssemblyOutput: TSynEdit
      Left = 2
      Top = 15
      Width = 858
      Height = 240
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 0
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.ShowLineNumbers = True
      Highlighter = SynPasSyn
      ReadOnly = True
      WantTabs = True
      ExplicitHeight = 261
    end
  end
  object SynPasSyn: TSynPasSyn
    Left = 424
    Top = 52
  end
end
