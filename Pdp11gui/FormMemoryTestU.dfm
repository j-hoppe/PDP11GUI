object FormMemoryTest: TFormMemoryTest
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'FormMemoryTest'
  ClientHeight = 295
  ClientWidth = 464
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
    Width = 464
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
    object Label3: TLabel
      Left = 133
      Top = 6
      Width = 49
      Height = 13
      Caption = 'Last addr:'
    end
    object Label2: TLabel
      Left = 253
      Top = 6
      Width = 46
      Height = 13
      Caption = 'Chip size:'
    end
    object StartAddrEdit: TEdit
      Left = 67
      Top = 3
      Width = 60
      Height = 21
      TabOrder = 0
      Text = '17777777'
      OnKeyPress = OctalEditKeyPress
    end
    object SetStartAddrButton: TButton
      Left = 420
      Top = 3
      Width = 33
      Height = 21
      Caption = 'Set'
      TabOrder = 2
      OnClick = SetStartAddrButtonClick
    end
    object TestAddressLinesButton: TButton
      Left = 102
      Top = 30
      Width = 94
      Height = 21
      Caption = 'Test address lines'
      TabOrder = 3
      OnClick = TestAddressLinesButtonClick
    end
    object TestDatabitsButton: TButton
      Left = 202
      Top = 30
      Width = 97
      Height = 21
      Caption = 'Test data bit chips'
      TabOrder = 4
      OnClick = TestDatabitsButtonClick
    end
    object TestRandomButton: TButton
      Left = 320
      Top = 30
      Width = 94
      Height = 21
      Caption = 'Test random'
      TabOrder = 5
      OnClick = TestRandomButtonClick
    end
    object EndAddrEdit: TEdit
      Left = 188
      Top = 3
      Width = 59
      Height = 21
      TabOrder = 1
      Text = '17777777'
      OnKeyPress = OctalEditKeyPress
    end
    object TestDataLinesButton: TButton
      Left = 15
      Top = 30
      Width = 81
      Height = 21
      Caption = 'Test data lines'
      TabOrder = 6
      OnClick = TestDataLinesButtonClick
    end
    object ChipAddrSizeComboBox: TComboBox
      Left = 305
      Top = 3
      Width = 106
      Height = 21
      Style = csDropDownList
      TabOrder = 7
      Items.Strings = (
        '2 (single wordl)'
        '1 K / 2000'
        '2 K / 4000'
        '4 K / 10000'
        '16 K / 40000'
        '64 K / 200000'
        '256 K / 1000000')
    end
  end
  inline MemoryGrid: TFrameMemoryCellGroupGrid
    Left = 0
    Top = 57
    Width = 464
    Height = 238
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitTop = 57
    ExplicitWidth = 464
    ExplicitHeight = 238
    inherited MemoryCellsStringGrid: TJvStringGrid
      Width = 464
      Height = 238
      ExplicitWidth = 464
      ExplicitHeight = 238
    end
    inherited PopupMenu1: TPopupMenu
      inherited Verify1: TMenuItem
        OnClick = MemoryGridVerify1Click
      end
    end
  end
  object TestLogMemo: TMemo
    Left = 0
    Top = 57
    Width = 464
    Height = 238
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
