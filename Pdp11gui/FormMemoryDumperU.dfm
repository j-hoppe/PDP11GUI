object FormMemoryDumper: TFormMemoryDumper
  Left = 0
  Top = 0
  Caption = 'FormMemoryDumper'
  ClientHeight = 443
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
    Height = 188
    Align = alTop
    TabOrder = 0
    object StartAddrLabel: TLabel
      Left = 33
      Top = 9
      Width = 56
      Height = 13
      Caption = 'Start addr :'
    end
    object Label2: TLabel
      Left = 30
      Top = 63
      Width = 58
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = 'File format :'
      ParentBiDiMode = False
    end
    object File1Label: TLabel
      Left = 39
      Top = 101
      Width = 47
      Height = 13
      Alignment = taRightJustify
      Caption = 'File1Label'
    end
    object File2Label: TLabel
      Left = 39
      Top = 126
      Width = 47
      Height = 13
      Alignment = taRightJustify
      Caption = 'File2Label'
    end
    object Label1: TLabel
      Left = 206
      Top = 9
      Width = 50
      Height = 13
      Caption = 'End addr :'
    end
    object EntryAddrLabel: TLabel
      Left = 31
      Top = 36
      Width = 58
      Height = 13
      Caption = 'Entry addr :'
    end
    object ExamineAllButton: TButton
      Left = 270
      Top = 33
      Width = 75
      Height = 21
      Caption = 'Examine all'
      TabOrder = 2
      OnClick = ExamineAllButtonClick
    end
    object StartAddrEdit: TEdit
      Left = 100
      Top = 6
      Width = 90
      Height = 21
      TabOrder = 0
      Text = 'StartAddrEdit'
      OnKeyPress = AddrEditKeyPress
    end
    object DumpFileButton: TButton
      Left = 301
      Top = 152
      Width = 75
      Height = 21
      Caption = 'Save'
      TabOrder = 6
      OnClick = DumpFileButtonClick
    end
    object DumperFileFormatComboBox: TComboBox
      Left = 100
      Top = 60
      Width = 280
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = DumperFileFormatComboBoxChange
      Items.Strings = (
        'Byte stream: low byte, high byte, low byte ...'
        'Two 8 bit ROMs: Low-file, High-file'
        'Text file with one addr and a value list per line'
        'Instructions for blinkenlight  console'
        'Standard Absolute Papertape image')
    end
    object Filename2Edit: TEdit
      Left = 99
      Top = 125
      Width = 279
      Height = 21
      ReadOnly = True
      TabOrder = 8
      Text = 'Filename2Edit'
    end
    object Filename1Edit: TEdit
      Left = 99
      Top = 98
      Width = 279
      Height = 21
      ReadOnly = True
      TabOrder = 7
      Text = 'Filename1Edit'
    end
    object BrowseFile1Button: TButton
      Left = 384
      Top = 99
      Width = 29
      Height = 19
      Caption = '...'
      TabOrder = 4
      OnClick = BrowseFile1ButtonClick
    end
    object BrowseFile2Button: TButton
      Left = 384
      Top = 124
      Width = 29
      Height = 20
      Caption = '...'
      TabOrder = 5
      OnClick = BrowseFile1ButtonClick
    end
    object EndAddrEdit: TEdit
      Left = 273
      Top = 6
      Width = 90
      Height = 21
      TabOrder = 1
      OnKeyPress = AddrEditKeyPress
    end
    object EntryAddrEdit: TEdit
      Left = 99
      Top = 33
      Width = 90
      Height = 21
      TabOrder = 9
      OnKeyPress = EntryAddrEditKeyPress
    end
  end
  inline MemoryGrid: TFrameMemoryCellGroupGrid
    Left = 0
    Top = 188
    Width = 426
    Height = 255
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitTop = 188
    ExplicitWidth = 426
    ExplicitHeight = 255
    inherited MemoryCellsStringGrid: TJvStringGrid
      Width = 426
      Height = 255
      ExplicitWidth = 426
      ExplicitHeight = 255
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 24
    Top = 10
  end
end
