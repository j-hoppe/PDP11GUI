object FrameMemoryCellGroupList: TFrameMemoryCellGroupList
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  OnResize = FrameResize
  object MemoryCellsStringGrid: TJvStringGrid
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    ColCount = 4
    DefaultRowHeight = 16
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goThumbTracking]
    TabOrder = 0
    OnDrawCell = MemoryCellsStringGridDrawCell
    OnKeyPress = MemoryCellsStringGridKeyPress
    OnMouseUp = MemoryCellsStringGridMouseUp
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
      74
      95
      226)
  end
end
