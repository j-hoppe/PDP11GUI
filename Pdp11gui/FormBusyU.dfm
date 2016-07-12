object BusyForm: TBusyForm
  Left = 0
  Top = 0
  Anchors = []
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'PDP11GUI - Work in progress'
  ClientHeight = 129
  ClientWidth = 572
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    572
    129)
  PixelsPerInch = 96
  TextHeight = 13
  object InfoLabel: TLabel
    Left = 12
    Top = 16
    Width = 58
    Height = 14
    Caption = 'InfoLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ProgressBar1: TProgressBar
    Left = 14
    Top = 43
    Width = 536
    Height = 25
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Anchors = []
    Smooth = True
    TabOrder = 0
    ExplicitLeft = 9
    ExplicitTop = 39
  end
  object AbortButton: TButton
    Left = 221
    Top = 84
    Width = 83
    Height = 25
    Anchors = []
    Caption = 'Abort'
    TabOrder = 1
    OnClick = AbortButtonClick
    ExplicitLeft = 216
    ExplicitTop = 77
  end
end
