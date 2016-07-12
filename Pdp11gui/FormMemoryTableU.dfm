object FormMemoryTable: TFormMemoryTable
  Left = 0
  Top = 0
  Caption = 'FormMemoryTable'
  ClientHeight = 285
  ClientWidth = 418
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
    Width = 418
    Height = 57
    Align = alTop
    TabOrder = 0
    OnClick = PanelTClick
    object Label1: TLabel
      Left = 12
      Top = 6
      Width = 53
      Height = 13
      Caption = 'Start addr:'
    end
    object Label2: TLabel
      Left = 139
      Top = 6
      Width = 21
      Height = 13
      Caption = 'Len:'
    end
    object ExamineAllButton: TButton
      Left = 109
      Top = 31
      Width = 75
      Height = 21
      Caption = 'Examine All'
      TabOrder = 0
      OnClick = ExamineAllButtonClick
    end
    object DepositChangedButton: TButton
      Left = 190
      Top = 30
      Width = 98
      Height = 21
      Caption = 'Deposit changed'
      TabOrder = 1
      OnClick = DepositChangedButtonClick
    end
    object DepositAllButton: TButton
      Left = 306
      Top = 30
      Width = 75
      Height = 21
      Caption = 'Deposit all'
      TabOrder = 2
      OnClick = DepositAllButtonClick
    end
    object StartAddrEdit: TEdit
      Left = 67
      Top = 3
      Width = 66
      Height = 21
      TabOrder = 3
      Text = '17777777'
      OnKeyPress = StartAddrEditKeyPress
    end
    object SetStartAddrButton: TButton
      Left = 213
      Top = 3
      Width = 33
      Height = 21
      Caption = 'Set'
      TabOrder = 4
      OnClick = SetStartAddrButtonClick
    end
    object DecAddrButton: TButton
      Left = 268
      Top = 3
      Width = 50
      Height = 21
      Caption = 'Dec addr'
      TabOrder = 5
      OnClick = IncDecAddrButtonClick
    end
    object IncAddrButton: TButton
      Left = 326
      Top = 3
      Width = 55
      Height = 21
      Caption = 'Inc addr'
      TabOrder = 6
      OnClick = IncDecAddrButtonClick
    end
    object ExamineCurrentButton: TButton
      Left = 21
      Top = 31
      Width = 75
      Height = 21
      Caption = 'Examine cur.'
      TabOrder = 7
      OnClick = ExamineCurrentButtonClick
    end
    object MemoryBlockSizeEdit: TEdit
      Left = 166
      Top = 3
      Width = 37
      Height = 21
      TabOrder = 8
      Text = '377'
      OnKeyPress = MemoryBlockSizeEditKeyPress
    end
  end
  inline MemoryGrid: TFrameMemoryCellGroupGrid
    Left = 0
    Top = 57
    Width = 418
    Height = 228
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitTop = 57
    ExplicitWidth = 418
    ExplicitHeight = 228
    inherited MemoryCellsStringGrid: TJvStringGrid
      Width = 418
      Height = 228
      ExplicitWidth = 418
      ExplicitHeight = 228
    end
    inherited PopupMenu1: TPopupMenu
      inherited Verify1: TMenuItem
        OnClick = MemoryGridVerify1Click
      end
    end
  end
end
