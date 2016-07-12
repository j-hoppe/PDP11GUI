object FormExecuteBlinkenlight: TFormExecuteBlinkenlight
  Left = 0
  Top = 0
  Caption = 'Instructions, how to load and run with blinkenlight console'
  ClientHeight = 602
  ClientWidth = 640
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
    Width = 640
    Height = 602
    Align = alClient
    TabOrder = 0
    object Label1: TLabel
      Left = 245
      Top = 24
      Width = 44
      Height = 13
      Caption = 'Start PC:'
    end
    object StartPCEdit: TEdit
      Left = 297
      Top = 21
      Width = 51
      Height = 21
      TabOrder = 0
      Text = '123456'
      OnChange = StartPCEditChange
    end
    object NewPgmButton: TButton
      Left = 16
      Top = 16
      Width = 213
      Height = 25
      Caption = 'Compile program and show'
      TabOrder = 1
      OnClick = NewPgmButtonClick
    end
    object BlinkenLightInstructionMemo: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 51
      Width = 632
      Height = 547
      Margins.Top = 50
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Lucida Console'
      Font.Style = []
      Lines.Strings = (
        'BlinkenLightInstructionMemo')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 2
    end
    object SaveButton: TButton
      Left = 398
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 3
      OnClick = SaveButtonClick
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 522
    Top = 10
  end
end
