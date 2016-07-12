object FormMacro11Listing: TFormMacro11Listing
  Left = 0
  Top = 0
  Caption = 'MACRO11 Listing'
  ClientHeight = 293
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Editor: TJvEditor
    Left = 0
    Top = 41
    Width = 535
    Height = 252
    Cursor = crIBeam
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    GutterWidth = 36
    ReadOnly = True
    Completion.ItemHeight = 13
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '9 17 25 33 41 49 57 65'
    SmartTab = False
    BackSpaceUnindents = False
    AutoIndent = False
    BracketHighlighting.StringEscape = #39#39
    OnResize = EditorResize
    OnPaintGutter = EditorPaintGutter
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
  end
  object PanelT: TPanel
    Left = 0
    Top = 0
    Width = 535
    Height = 41
    Align = alTop
    TabOrder = 1
    object LoadButton: TButton
      Left = 17
      Top = 8
      Width = 59
      Height = 21
      Caption = 'Load'
      TabOrder = 0
      OnClick = LoadButtonClick
    end
    object ShowCodeMemFormButton: TButton
      Left = 210
      Top = 9
      Width = 103
      Height = 20
      Caption = 'Show octal code'
      TabOrder = 1
      OnClick = ShowCodeMemFormButtonClick
    end
    object DepositAllButton: TButton
      Left = 108
      Top = 8
      Width = 75
      Height = 21
      Caption = 'Deposit'
      TabOrder = 2
      OnClick = DepositAllButtonClick
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MACRO11 listings|*.lst|All files|*.*'
    Title = 'Open MACRO-11 listing file'
    Left = 204
    Top = 81
  end
end
