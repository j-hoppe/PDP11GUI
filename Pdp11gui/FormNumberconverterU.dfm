object FormNumberConverter: TFormNumberConverter
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsSingle
  Caption = 'Number converter'
  ClientHeight = 277
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 298
    Top = 1
    Width = 75
    Height = 44
    Caption = '&hex'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -41
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 300
    Top = 110
    Width = 75
    Height = 44
    Caption = '&oct'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -41
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 300
    Top = 222
    Width = 75
    Height = 44
    Caption = '&dec'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -41
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object HexEdit: TEdit
    Left = 76
    Top = 0
    Width = 207
    Height = 55
    Alignment = taRightJustify
    AutoSelect = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -41
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = '12345678'
    OnChange = NumberEditChange
    OnEnter = NumberEditEnter
  end
  object OctalEdit: TEdit
    Left = 0
    Top = 105
    Width = 283
    Height = 55
    Alignment = taRightJustify
    AutoSelect = False
    Color = clInfoBk
    Ctl3D = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -41
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 1
    Text = '37777777777'
    OnChange = NumberEditChange
    OnEnter = NumberEditEnter
  end
  object DecimalEdit: TEdit
    Left = 20
    Top = 222
    Width = 263
    Height = 55
    Alignment = taRightJustify
    AutoSelect = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -41
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    Text = '4294967295'
    OnChange = NumberEditChange
    OnEnter = NumberEditEnter
  end
  object HexAsBinaryEdit: TEdit
    Left = 76
    Top = 48
    Width = 207
    Height = 20
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '1111 0000 1111 0000 1111 0000 1111 0000'
  end
  object OctalAsBinaryEdit: TEdit
    Left = 0
    Top = 166
    Width = 283
    Height = 20
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = '  00  111  000  111  000  111  000  111  000  111  000'
  end
end
