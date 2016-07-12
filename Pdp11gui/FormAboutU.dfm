object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'About'
  ClientHeight = 267
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    AlignWithMargins = True
    Left = 30
    Top = 233
    Width = 301
    Height = 25
    Margins.Left = 30
    Margins.Top = 9
    Margins.Right = 30
    Margins.Bottom = 9
    Align = alBottom
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 355
    Height = 218
    Align = alClient
    Color = clBtnFace
    Lines.Strings = (
      ''
      'DO NOT EDIT!'
      'Text ist inserted in FormCreate!')
    ReadOnly = True
    TabOrder = 1
  end
end
