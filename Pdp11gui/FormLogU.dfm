object FormLog: TFormLog
  Left = 0
  Top = 0
  Caption = 'Log'
  ClientHeight = 293
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LogMemo: TMemo
    Left = 0
    Top = 0
    Width = 426
    Height = 293
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    OnMouseUp = LogMemoMouseUp
  end
  object PopupMenu1: TPopupMenu
    Left = 265
    Top = 119
    object Clear1: TMenuItem
      Caption = 'Clear'
      OnClick = Clear1Click
    end
  end
end
