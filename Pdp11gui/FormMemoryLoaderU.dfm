object FormMemoryLoader: TFormMemoryLoader
  Left = 0
  Top = 0
  Caption = 'FormMemoryLoader'
  ClientHeight = 511
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
    Height = 179
    Align = alTop
    TabOrder = 0
    object StartAddrLabel: TLabel
      Left = 36
      Top = 100
      Width = 56
      Height = 13
      Caption = 'Start addr :'
    end
    object Label2: TLabel
      Left = 34
      Top = 13
      Width = 58
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = 'File format :'
      ParentBiDiMode = False
    end
    object File1Label: TLabel
      Left = 43
      Top = 46
      Width = 47
      Height = 13
      Alignment = taRightJustify
      Caption = 'File1Label'
    end
    object File2Label: TLabel
      Left = 43
      Top = 71
      Width = 47
      Height = 13
      Alignment = taRightJustify
      Caption = 'File2Label'
    end
    object Label1: TLabel
      Left = 205
      Top = 100
      Width = 50
      Height = 13
      Caption = 'End addr :'
    end
    object EntryAddrLabel: TLabel
      Left = 12
      Top = 156
      Width = 58
      Height = 13
      Caption = 'Entry addr :'
    end
    object VerifyAllButton: TButton
      Left = 286
      Top = 125
      Width = 75
      Height = 21
      Caption = 'Verify all'
      TabOrder = 7
      OnClick = VerifyAllButtonClick
    end
    object StartAddrEdit: TEdit
      Left = 102
      Top = 97
      Width = 90
      Height = 21
      TabOrder = 3
      OnKeyPress = EntryAddrEditKeyPress
    end
    object LoadFileButton: TButton
      Left = 101
      Top = 124
      Width = 75
      Height = 21
      Caption = 'Load'
      TabOrder = 5
      OnClick = LoadFileButtonClick
    end
    object LoaderFileFormatComboBox: TComboBox
      Left = 102
      Top = 10
      Width = 280
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = LoaderFileFormatComboBoxChange
      Items.Strings = (
        'Byte stream: low byte, high byte, low byte ...'
        'Two 8 bit ROMs: Low-file, High-file'
        'Text file with one addr and a value list per line'
        'Standard Absolute Paper Tape image')
    end
    object Filename2Edit: TEdit
      Left = 103
      Top = 70
      Width = 279
      Height = 21
      ReadOnly = True
      TabOrder = 6
      Text = 'Filename2Edit'
    end
    object Filename1Edit: TEdit
      Left = 103
      Top = 43
      Width = 279
      Height = 21
      ReadOnly = True
      TabOrder = 8
      Text = 'Filename1Edit'
    end
    object BrowseFile1Button: TButton
      Left = 388
      Top = 44
      Width = 29
      Height = 19
      Caption = '...'
      TabOrder = 1
      OnClick = BrowseFile1ButtonClick
    end
    object BrowseFile2Button: TButton
      Left = 388
      Top = 69
      Width = 29
      Height = 20
      Caption = '...'
      TabOrder = 2
      OnClick = BrowseFile1ButtonClick
    end
    object EndAddrEdit: TEdit
      Left = 257
      Top = 97
      Width = 90
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object EntryAddrEdit: TEdit
      Left = 102
      Top = 152
      Width = 90
      Height = 21
      TabOrder = 9
      OnKeyPress = EntryAddrEditKeyPress
    end
    object DepositAllButton: TButton
      Left = 198
      Top = 125
      Width = 75
      Height = 21
      Caption = 'Deposit all'
      TabOrder = 10
      OnClick = DepositAlllButtonClick
    end
  end
  inline MemoryGrid: TFrameMemoryCellGroupGrid
    Left = 0
    Top = 179
    Width = 426
    Height = 332
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitTop = 179
    ExplicitWidth = 426
    ExplicitHeight = 332
    inherited MemoryCellsStringGrid: TJvStringGrid
      Width = 426
      Height = 332
      ExplicitWidth = 426
      ExplicitHeight = 332
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 8
    Top = 48
  end
end
