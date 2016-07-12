object FormIopageScanner: TFormIopageScanner
  Left = 0
  Top = 0
  Caption = 'FormIopageScanner'
  ClientHeight = 368
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 214
    Width = 505
    Height = 9
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
    ExplicitLeft = -8
    ExplicitTop = 39
    ExplicitWidth = 426
  end
  object PanelT: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 72
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 108
      Top = 14
      Width = 216
      Height = 13
      Caption = 'WARNING: scanning is VERY time consuming!'
    end
    object StartScanButton: TButton
      Left = 19
      Top = 12
      Width = 75
      Height = 20
      Caption = 'Start scan'
      TabOrder = 0
      OnClick = StartScanButtonClick
    end
    object ExamineCurrentButton: TButton
      Left = 19
      Top = 38
      Width = 75
      Height = 21
      Caption = 'Examine cur.'
      TabOrder = 1
      OnClick = ExamineCurrentButtonClick
    end
    object ExamineAllButton: TButton
      Left = 108
      Top = 39
      Width = 75
      Height = 21
      Caption = 'Examine all'
      TabOrder = 2
      OnClick = ExamineAllButtonClick
    end
    object DepositChangedButton: TButton
      Left = 195
      Top = 38
      Width = 98
      Height = 21
      Caption = 'Deposit changed'
      TabOrder = 3
      OnClick = DepositChangedButtonClick
    end
  end
  inline MemoryList: TFrameMemoryCellGroupList
    Left = 0
    Top = 72
    Width = 505
    Height = 142
    Align = alClient
    TabOrder = 1
    ExplicitTop = 72
    ExplicitWidth = 505
    ExplicitHeight = 142
    inherited MemoryCellsStringGrid: TJvStringGrid
      Width = 505
      Height = 142
      ExplicitWidth = 505
      ExplicitHeight = 142
    end
  end
  object IoPageMachineDescriptionMemo: TMemo
    Left = 0
    Top = 223
    Width = 505
    Height = 145
    Align = alBottom
    Lines.Strings = (
      'IoPageMachineDescriptionMemo')
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
