object FormDisas: TFormDisas
  Left = 0
  Top = 0
  Caption = 'FormDisas'
  ClientHeight = 150
  ClientWidth = 535
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 535
    Height = 45
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 53
      Height = 13
      Caption = 'Start addr:'
    end
    object Label2: TLabel
      Left = 141
      Top = 14
      Width = 47
      Height = 13
      Caption = 'End addr:'
    end
    object SetAddressButton: TButton
      Left = 273
      Top = 11
      Width = 57
      Height = 21
      Caption = 'Examine'
      TabOrder = 2
      OnClick = SetAddressButtonClick
    end
    object StartAddrEdit: TEdit
      Left = 62
      Top = 11
      Width = 73
      Height = 21
      TabOrder = 0
      Text = 'StartAddrEdit'
      OnKeyPress = AnyAddrEditKeyPress
    end
    object DecAddrButton: TButton
      Left = 328
      Top = 12
      Width = 50
      Height = 20
      Caption = 'Dec addr'
      TabOrder = 3
      OnClick = DecAddrButtonClick
    end
    object IncAddrButton: TButton
      Left = 384
      Top = 12
      Width = 46
      Height = 20
      Caption = 'Inc addr'
      TabOrder = 4
      OnClick = IncAddrButtonClick
    end
    object UseCacheCheckBox: TCheckBox
      Left = 436
      Top = 13
      Width = 99
      Height = 17
      Caption = 'use code cache'
      TabOrder = 5
      OnClick = UseCacheCheckBoxClick
    end
    object EndAddrEdit: TEdit
      Left = 194
      Top = 11
      Width = 73
      Height = 21
      TabOrder = 1
      Text = 'EndAddrEdit'
      OnKeyPress = AnyAddrEditKeyPress
    end
  end
  object DisasSourceEditor: TJvEditor
    Left = 0
    Top = 45
    Width = 535
    Height = 105
    Cursor = crIBeam
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Lines.Strings = (
      'line1'
      'line2'
      'line3'
      'line4'
      'line5'
      'line6')
    GutterWidth = 32
    ReadOnly = True
    Completion.ItemHeight = 13
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '9 17 25 33 41 49 57 65'
    SmartTab = False
    BackSpaceUnindents = False
    AutoIndent = False
    BracketHighlighting.StringEscape = #39#39
    OnResize = DisasSourceEditorResize
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
  end
end
