object FormMacro11Source: TFormMacro11Source
  Left = 0
  Top = 0
  Caption = 'MACRO11 Source'
  ClientHeight = 353
  ClientWidth = 489
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
  object PanelT: TPanel
    Left = 0
    Top = 0
    Width = 489
    Height = 41
    Align = alTop
    TabOrder = 0
    object LoadButton: TButton
      Left = 78
      Top = 8
      Width = 48
      Height = 21
      Caption = 'Load'
      TabOrder = 0
      OnClick = LoadButtonClick
    end
    object SaveAsButton: TButton
      Left = 178
      Top = 8
      Width = 75
      Height = 21
      Caption = 'Save as ...'
      TabOrder = 1
      OnClick = SaveAsButtonClick
    end
    object CompileButton: TButton
      Left = 286
      Top = 8
      Width = 89
      Height = 21
      Hint = 'Save your source before you compile'
      Caption = 'Run MACRO11'
      TabOrder = 2
      OnClick = CompileButtonClick
    end
    object SaveButton: TButton
      Left = 132
      Top = 8
      Width = 40
      Height = 21
      Caption = 'Save'
      TabOrder = 3
      OnClick = SaveButtonClick
    end
    object NewButton: TButton
      Left = 10
      Top = 8
      Width = 47
      Height = 21
      Caption = 'New'
      TabOrder = 4
      OnClick = NewButtonClick
    end
  end
  object Editor: TJvEditor
    Left = 0
    Top = 41
    Width = 489
    Height = 312
    Cursor = crIBeam
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    GutterWidth = 36
    Completion.ItemHeight = 13
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '9 17 25 33 41 49 57 65'
    SmartTab = False
    BackSpaceUnindents = False
    AutoIndent = False
    BracketHighlighting.StringEscape = #39#39
    OnResize = EditorResize
    OnChange = EditorChange
    OnPaintGutter = EditorPaintGutter
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Lucida Console'
    Font.Style = []
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MACRO-11 sources|*.mac|All files|*.*'
    Title = 'Open MACRO-11 source file'
    Left = 64
    Top = 73
  end
  object SaveDialog1: TSaveDialog
    Filter = 'MACRO-11 sources|*.mac|All files|*.*'
    Title = 'Save MACRO-11 source file'
    Left = 323
    Top = 19
  end
end
