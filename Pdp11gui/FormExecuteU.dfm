object FormExecute: TFormExecute
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Execution'
  ClientHeight = 277
  ClientWidth = 242
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
    Width = 242
    Height = 277
    Align = alClient
    TabOrder = 0
    object Label1: TLabel
      Left = 19
      Top = 144
      Width = 44
      Height = 13
      Caption = 'Start PC:'
    end
    object Label2: TLabel
      Left = 17
      Top = 217
      Width = 57
      Height = 13
      Caption = 'Current PC:'
    end
    object ResetButton: TButton
      Left = 16
      Top = 173
      Width = 73
      Height = 21
      Caption = 'Reset'
      TabOrder = 0
      OnClick = ResetButtonClick
    end
    object ResetAndStartButton: TButton
      Left = 104
      Top = 173
      Width = 89
      Height = 21
      Caption = 'Reset and Start'
      TabOrder = 1
      OnClick = ResetAndStartButtonClick
    end
    object HaltButton: TButton
      Left = 161
      Top = 241
      Width = 63
      Height = 21
      Caption = 'Halt'
      TabOrder = 2
      OnClick = HaltButtonClick
    end
    object SingleStepButton: TButton
      Left = 88
      Top = 241
      Width = 61
      Height = 21
      Caption = 'Single Step'
      TabOrder = 3
      OnClick = SingleStepButtonClick
    end
    object StartPCEdit: TEdit
      Left = 71
      Top = 141
      Width = 51
      Height = 21
      TabOrder = 4
      Text = '123456'
      OnChange = StartPCEditChange
    end
    object CurPCEdit: TEdit
      Left = 88
      Top = 214
      Width = 51
      Height = 21
      TabOrder = 5
      Text = '123456'
      OnKeyPress = CurPCEditKeyPress
    end
    object NewPgmButton: TButton
      Left = 16
      Top = 8
      Width = 213
      Height = 38
      Caption = 'New program: Compile, Load and Reset'
      TabOrder = 6
      OnClick = NewPgmButtonClick
    end
    object ContinueButton: TButton
      Left = 17
      Top = 241
      Width = 57
      Height = 21
      Caption = 'Continue'
      TabOrder = 7
      OnClick = ContinueButtonClick
    end
    object RunModeGroupBox: TGroupBox
      Left = 13
      Top = 49
      Width = 216
      Height = 86
      Caption = ' Physical Run/Halt switch'
      Color = clBtnFace
      ParentBackground = False
      ParentColor = False
      TabOrder = 8
      object RunRadioButton: TRadioButton
        Left = 10
        Top = 20
        Width = 97
        Height = 17
        Caption = 'RUN/ENABLE'
        TabOrder = 0
        OnClick = RunRadioButtonClick
      end
      object HaltRadioButton: TRadioButton
        Left = 117
        Top = 20
        Width = 55
        Height = 17
        Caption = 'HALT'
        TabOrder = 1
        OnClick = HaltRadioButtonClick
      end
      object RunHaltInfoMemo: TMemo
        Left = 2
        Top = 40
        Width = 212
        Height = 44
        Align = alBottom
        BorderStyle = bsNone
        Color = clBtnFace
        Lines.Strings = (
          'Keep these setting synchronous with  the '
          'RUN/HALT switch on the physical machine! '
          'It enables Reset, Start, Cont, Single Step.')
        ReadOnly = True
        TabOrder = 2
      end
    end
    object ShowPCButton: TButton
      Left = 161
      Top = 216
      Width = 63
      Height = 19
      Caption = 'Set/Show'
      TabOrder = 9
      OnClick = SetPCButtonClick
    end
  end
end
