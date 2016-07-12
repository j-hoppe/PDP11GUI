object FormMMU: TFormMMU
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'MMU'
  ClientHeight = 407
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 65
    Width = 396
    Height = 342
    ActivePage = DataSpaceTabSheet
    Align = alClient
    TabOrder = 0
    object DataSpaceTabSheet: TTabSheet
      Caption = 'Data space'
      object DataSpaceStringGrid: TJvStringGrid
        Left = 0
        Top = 0
        Width = 388
        Height = 314
        Align = alClient
        DefaultRowHeight = 16
        TabOrder = 0
        Alignment = taLeftJustify
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'Tahoma'
        FixedFont.Style = []
      end
    end
    object InstructionSpaceTabSheet: TTabSheet
      Caption = 'Instruction space'
      ImageIndex = 1
      object InstructionSpaceStringGrid: TJvStringGrid
        Left = 0
        Top = 0
        Width = 388
        Height = 314
        Align = alClient
        DefaultRowHeight = 16
        TabOrder = 0
        Alignment = taLeftJustify
        FixedFont.Charset = DEFAULT_CHARSET
        FixedFont.Color = clWindowText
        FixedFont.Height = -11
        FixedFont.Name = 'Tahoma'
        FixedFont.Style = []
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 396
    Height = 65
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 105
      Top = 15
      Width = 109
      Height = 13
      Caption = 'Cur CPU mode (PSW): '
    end
    object SpecialInfoLabel: TLabel
      Left = 25
      Top = 42
      Width = 78
      Height = 13
      Caption = 'SpecialInfoLabel'
    end
    object RefreshButton: TButton
      Left = 19
      Top = 12
      Width = 75
      Height = 21
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = RefreshButtonClick
    end
    object CpuModeEdit: TEdit
      Left = 220
      Top = 12
      Width = 61
      Height = 21
      ReadOnly = True
      TabOrder = 1
      Text = 'Supervisor'
    end
  end
end
