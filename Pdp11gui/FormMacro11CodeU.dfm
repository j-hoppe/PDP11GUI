object FormMacro11Code: TFormMacro11Code
  Left = 0
  Top = 0
  Caption = 'FormMacro11Code'
  ClientHeight = 293
  ClientWidth = 426
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
    Width = 426
    Height = 33
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 13
      Top = 7
      Width = 53
      Height = 13
      Caption = 'Start addr:'
    end
    object DepositAllButton: TButton
      Left = 214
      Top = 4
      Width = 75
      Height = 21
      Caption = 'Deposit all'
      TabOrder = 0
      OnClick = DepositAllButtonClick
    end
    object StartAddrEdit: TEdit
      Left = 75
      Top = 4
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'StartAddrEdit'
    end
  end
  inline MemoryGrid: TFrameMemoryCellGroupGrid
    Left = 0
    Top = 33
    Width = 426
    Height = 260
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitTop = 33
    ExplicitWidth = 426
    ExplicitHeight = 260
    inherited MemoryCellsStringGrid: TJvStringGrid
      Width = 426
      Height = 260
      ExplicitWidth = 426
      ExplicitHeight = 260
    end
  end
end
