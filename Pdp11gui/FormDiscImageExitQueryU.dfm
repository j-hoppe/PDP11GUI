object FormDiscImageExitQueryForm: TFormDiscImageExitQueryForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Disc driver is running'
  ClientHeight = 167
  ClientWidth = 295
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
  object Label1: TLabel
    Left = 11
    Top = 0
    Width = 262
    Height = 91
    Alignment = taCenter
    Caption =
      'WARNING:'#13#10#13#10'The disk driver is running, '#13#10'the PDP-11 will not be' +
      ' accessible over console.'#13#10#13#10'Really exit disc image window?'#13#10
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ExitWindowButton: TButton
    Left = 218
    Top = 110
    Width = 77
    Height = 25
    Caption = 'Exit window'
    Default = True
    TabOrder = 0
    OnClick = ExitWindowButtonClick
  end
  object StayInWindowButton: TButton
    Left = 0
    Top = 110
    Width = 89
    Height = 25
    Cancel = True
    Caption = 'Stay in window'
    TabOrder = 1
    OnClick = StayInWindowButtonClick
  end
  object StopDriverButton: TButton
    Left = 95
    Top = 110
    Width = 117
    Height = 25
    Caption = 'Stop driver, then exit.'
    TabOrder = 2
    OnClick = StopDriverButtonClick
  end
  object DoNotShowAgainCheckBox: TCheckBox
    Left = 3
    Top = 150
    Width = 127
    Height = 17
    Caption = 'Do not show again'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = DoNotShowAgainCheckBoxClick
  end
end
