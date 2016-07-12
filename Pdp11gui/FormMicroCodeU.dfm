object FormMicroCode: TFormMicroCode
  Left = 0
  Top = 0
  Caption = 'FormMicroCode'
  ClientHeight = 293
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 524
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 209
      Top = 15
      Width = 62
      Height = 13
      Caption = #181'Instruction:'
    end
    object Label2: TLabel
      Left = 88
      Top = 15
      Width = 52
      Height = 13
      Caption = 'Search by:'
    end
    object MicroCodeSearchComboBox: TComboBox
      Left = 277
      Top = 11
      Width = 68
      Height = 21
      CharCase = ecUpperCase
      TabOrder = 0
      Text = 'MICROCODESEARCHCOMBOBOX'
      OnChange = MicroCodeSearchComboBoxChange
    end
    object NextMicroInstructionButton: TButton
      Left = 363
      Top = 12
      Width = 92
      Height = 20
      Caption = 'Next instruction'
      TabOrder = 1
      OnClick = NextMicroInstructionButtonClick
    end
    object MicroCodeSearchModeComboBox: TComboBox
      Left = 142
      Top = 11
      Width = 49
      Height = 21
      TabOrder = 2
      Text = #181'PC'
      OnChange = MicroCodeSearchModeComboBoxChange
      Items.Strings = (
        #181'PC'
        'tag'
        'line#')
    end
    object MicroCodeLoadButton: TButton
      Left = 15
      Top = 12
      Width = 43
      Height = 20
      Caption = 'Load'
      TabOrder = 3
      OnClick = MicroCodeLoadButtonClick
    end
  end
  object MicroInstructionStringGrid: TJvStringGrid
    Left = 0
    Top = 41
    Width = 524
    Height = 252
    Align = alClient
    ColCount = 3
    DefaultRowHeight = 16
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 1
    OnDrawCell = MicroInstructionStringGridDrawCell
    OnMouseUp = MicroInstructionStringGridMouseUp
    Alignment = taLeftJustify
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -11
    FixedFont.Name = 'Tahoma'
    FixedFont.Style = []
  end
  object OpenDialog1: TOpenDialog
    Options = [ofEnableSizing]
    Title = 'Select 1st file of micro code listings'
    Left = 92
    Top = 32
  end
end
