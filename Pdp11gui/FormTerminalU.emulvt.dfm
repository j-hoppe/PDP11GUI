object FormTerminal: TFormTerminal
  Left = 0
  Top = 0
  Caption = 'Terminal'
  ClientHeight = 517
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 70
    Top = 76
    Width = 534
    Height = 117
    Caption = 
      'This space is filled by an '#13#10'OverbyteIcsEmulVT Terminal. '#13#10'It is' +
      ' constructed programatically.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 488
    Width = 629
    Height = 29
    Panels = <>
  end
  object TerminalPopupMenu: TPopupMenu
    Left = 357
    Top = 178
    object Cut1: TMenuItem
      Caption = 'Copy'
      OnClick = Copy1Click
    end
    object Paste1: TMenuItem
      Caption = 'Paste'
      OnClick = Paste1Click
    end
    object Clear1: TMenuItem
      Caption = 'Clear'
      OnClick = Clear1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Selectfont1: TMenuItem
      Caption = 'Select font'
      OnClick = Selectfont1Click
    end
  end
  object TerminalFontDialog: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clYellow
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Options = [fdEffects, fdFixedPitchOnly]
    Left = 460
    Top = 162
  end
end
