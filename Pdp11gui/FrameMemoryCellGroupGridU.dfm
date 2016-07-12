object FrameMemoryCellGroupGrid: TFrameMemoryCellGroupGrid
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object MemoryCellsStringGrid: TJvStringGrid
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    DefaultRowHeight = 16
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
    OnDrawCell = MemoryCellsStringGridDrawCell
    OnGetEditText = MemoryCellsStringGridGetEditText
    OnKeyPress = MemoryCellsStringGridKeyPress
    OnMouseDown = AnyControlMouseDown
    OnSelectCell = MemoryCellsStringGridSelectCell
    OnSetEditText = MemoryCellsStringGridSetEditText
    Alignment = taLeftJustify
    FixedFont.Charset = DEFAULT_CHARSET
    FixedFont.Color = clWindowText
    FixedFont.Height = -11
    FixedFont.Name = 'Tahoma'
    FixedFont.Style = []
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      16
      16
      16
      16
      16)
  end
  object PopupMenu1: TPopupMenu
    Left = 224
    Top = 156
    object Cleardata1: TMenuItem
      Caption = 'Clear data'
      OnClick = Cleardata1Click
    end
    object Filldatawithaddr1: TMenuItem
      Caption = 'Fill data with addr'
      OnClick = Filldatawithaddr1Click
    end
    object Verify1: TMenuItem
      Caption = 'Verify data'
      OnClick = Verify1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ExportasSimHDOscript1: TMenuItem
      Caption = 'Export as SimH script'
      OnClick = ExportasSimHDOscript1Click
    end
  end
  object ExportSaveDialog: TSaveDialog
    Filter = 'SimH Script|*.sim|All files|*.*'
    Title = 'Save code as SimH script'
    Left = 265
    Top = 104
  end
end
