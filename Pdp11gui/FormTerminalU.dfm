object FormTerminal: TFormTerminal
  Left = 0
  Top = 0
  Caption = 'Terminal'
  ClientHeight = 517
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clLime
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 488
    Width = 629
    Height = 29
    Panels = <>
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 0
    Width = 629
    Height = 488
    Align = alClient
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    HideScrollBars = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    OnKeyDown = RichEdit1KeyDown
    OnKeyPress = RichEdit1KeyPress
    OnMouseUp = RichEdit1MouseUp
  end
  object TerminalPopupMenu: TPopupMenu
    Left = 357
    Top = 178
    object Clear1: TMenuItem
      Caption = 'Clear'
      OnClick = Clear1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Caption = 'Copy'
      OnClick = Copy1Click
    end
    object Copyall1: TMenuItem
      Caption = 'Copy all'
      OnClick = Copyall1Click
    end
    object Paste1: TMenuItem
      Caption = 'Paste'
      OnClick = Paste1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Showcontrolchars1: TMenuItem
      Caption = 'Show control chars'
      OnClick = Showcontrolchars1Click
    end
    object Selectfont1: TMenuItem
      Caption = 'Select font'
      OnClick = Selectfont1Click
    end
    object Selectcolor1: TMenuItem
      Caption = 'Select background color'
      OnClick = Selectcolor1Click
    end
  end
  object TerminalFontDialog: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clYellow
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Left = 460
    Top = 162
  end
  object TerminalColorDialog: TColorDialog
    Left = 452
    Top = 258
  end
end
