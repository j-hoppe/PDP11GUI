object FormMemoryList: TFormMemoryList
  Left = 0
  Top = 0
  Caption = 'FormMemoryList'
  ClientHeight = 301
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PanelT: TPanel
    Left = 0
    Top = 0
    Width = 467
    Height = 57
    Align = alTop
    TabOrder = 0
    object InfoLabel: TLabel
      Left = 12
      Top = 36
      Width = 45
      Height = 13
      Caption = 'InfoLabel'
    end
    object DepositChangedButton: TButton
      Left = 194
      Top = 6
      Width = 98
      Height = 21
      Caption = 'Deposit changed'
      TabOrder = 0
      OnClick = DepositChangedButtonClick
    end
    object DepositAllButton: TButton
      Left = 298
      Top = 6
      Width = 75
      Height = 21
      Caption = 'Deposit all'
      TabOrder = 1
      OnClick = DepositAllButtonClick
    end
    object ExamineAllButton: TButton
      Left = 106
      Top = 7
      Width = 75
      Height = 21
      Caption = 'Examine all'
      TabOrder = 2
      OnClick = ExamineAllButtonClick
    end
    object ExamineCurrentButton: TButton
      Left = 18
      Top = 6
      Width = 75
      Height = 21
      Caption = 'Examine cur.'
      TabOrder = 3
      OnClick = ExamineCurrentButtonClick
    end
  end
  inline MemoryList: TFrameMemoryCellGroupList
    Left = 0
    Top = 57
    Width = 467
    Height = 244
    Align = alClient
    TabOrder = 1
    ExplicitTop = 57
    ExplicitWidth = 467
    ExplicitHeight = 244
    inherited MemoryCellsStringGrid: TJvStringGrid
      Width = 467
      Height = 244
      ExplicitWidth = 467
      ExplicitHeight = 244
    end
  end
end
