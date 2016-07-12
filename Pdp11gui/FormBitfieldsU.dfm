object FormBitfields: TFormBitfields
  Left = 0
  Top = 0
  Caption = 'Bitfields'
  ClientHeight = 293
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBackground: TPanel
    Left = 0
    Top = 55
    Width = 507
    Height = 238
    Align = alClient
    Caption = 'No  bitfield definitions available'
    TabOrder = 2
  end
  object PanelT: TPanel
    Left = 0
    Top = 0
    Width = 507
    Height = 55
    Align = alTop
    TabOrder = 0
    object AddrLabel: TLabel
      Left = 11
      Top = 12
      Width = 30
      Height = 13
      Caption = 'Addr: '
    end
    object Label3: TLabel
      Left = 115
      Top = 11
      Width = 33
      Height = 13
      Caption = 'Value: '
    end
    object InfoLabel: TLabel
      Left = 18
      Top = 35
      Width = 23
      Height = 13
      Caption = 'Info '
    end
    object AddrEdit: TEdit
      Left = 40
      Top = 8
      Width = 57
      Height = 21
      ReadOnly = True
      TabOrder = 0
      Text = '12345678'
      OnKeyPress = AddrEditKeyPress
    end
    object ValueEdit: TEdit
      Left = 148
      Top = 8
      Width = 46
      Height = 21
      TabOrder = 1
      Text = '123456'
      OnChange = ValueEditChange
      OnKeyPress = ValueEditKeyPress
    end
    object DepositButton: TButton
      Left = 265
      Top = 8
      Width = 58
      Height = 21
      Caption = 'Deposit'
      TabOrder = 2
      OnClick = DepositButtonClick
    end
    object ExamineButton: TButton
      Left = 205
      Top = 7
      Width = 54
      Height = 23
      Caption = 'Examine'
      TabOrder = 3
      OnClick = ExamineButtonClick
    end
  end
  object BitfieldsStringGrid: TJvStringGrid
    Left = 0
    Top = 55
    Width = 507
    Height = 238
    Align = alClient
    ColCount = 6
    DefaultRowHeight = 16
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 1
    OnDrawCell = BitfieldsStringGridDrawCell
    OnKeyPress = BitfieldsStringGridKeyPress
    OnMouseUp = BitfieldsStringGridMouseUp
    OnSelectCell = BitfieldsStringGridSelectCell
    OnSetEditText = BitfieldsStringGridSetEditText
    Alignment = taLeftJustify
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -11
    FixedFont.Name = 'Tahoma'
    FixedFont.Style = []
  end
end
