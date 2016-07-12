object FormPdp1170Panel: TFormPdp1170Panel
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Formpdp1170Panel'
  ClientHeight = 538
  ClientWidth = 1028
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inline pdp1170panelImplementorFrame1: Tpdp1170panelImplementorFrame
    Left = 0
    Top = 0
    Width = 1028
    Height = 538
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    inherited img_panel_background: TImage
      ExplicitWidth = 1028
      ExplicitHeight = 538
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 216
    Top = 128
  end
end
