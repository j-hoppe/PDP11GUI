object FormDiscImage: TFormDiscImage
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  ClientHeight = 636
  ClientWidth = 581
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object DriverInfoLabel: TLabel
    Left = 16
    Top = 35
    Width = 88
    Height = 13
    Caption = 'DriverInfoLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ImageFileInfoLabel: TLabel
    Left = 16
    Top = 19
    Width = 109
    Height = 13
    Caption = 'ImageFileInfoLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object DiscInfoLabel: TLabel
    Left = 16
    Top = 0
    Width = 76
    Height = 13
    Caption = 'DiscInfoLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object InfoLabel: TLabel
    Left = 16
    Top = 54
    Width = 53
    Height = 13
    Caption = 'InfoLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 216
    Width = 527
    Height = 217
    Caption = ' Image file '
    TabOrder = 0
    object Label10: TLabel
      Left = 229
      Top = -270
      Width = 37
      Height = 13
      Caption = 'Label10'
    end
    object SaveImageButton: TButton
      Left = 215
      Top = 21
      Width = 97
      Height = 25
      Caption = 'Save image as ...'
      TabOrder = 0
      OnClick = SaveImageButtonClick
    end
    object LoadImageFileButton: TButton
      Left = 99
      Top = 20
      Width = 98
      Height = 25
      Caption = 'Load image file'
      TabOrder = 1
      OnClick = LoadImageFileButtonClick
    end
    object ClearImageButton: TButton
      Left = 7
      Top = 20
      Width = 75
      Height = 24
      Caption = 'Clear image'
      TabOrder = 2
      OnClick = ClearImageButtonClick
    end
    object OverlayFileButton: TButton
      Left = 321
      Top = 21
      Width = 75
      Height = 25
      Caption = 'Overlay file'
      TabOrder = 3
      OnClick = OverlayFileButtonClick
    end
    object MetainformationGroupBox: TGroupBox
      Left = 2
      Top = 46
      Width = 513
      Height = 171
      Caption = ' Meta information / Bad block list '
      TabOrder = 4
      object Label9: TLabel
        Left = 12
        Top = 125
        Width = 84
        Height = 13
        Caption = 'Media Serial#: 0x'
      end
      object BadBlockListStringGrid: TStringGrid
        Left = 9
        Top = 25
        Width = 496
        Height = 89
        Hint = 'LOad with iamge file, or set by "Read all" operation.'
        DefaultRowHeight = 16
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goThumbTracking]
        TabOrder = 0
      end
      object ReadBadBlocksFromBadSectorFileButton: TButton
        Left = 266
        Top = 120
        Width = 229
        Height = 20
        Caption = 'DEC std 144 bad sector file -> bad sector list'
        TabOrder = 1
        OnClick = ReadBadBlocksFromBadSectorFileButtonClick
      end
      object WriteBadBlocksToBadSectorFileButton: TButton
        Left = 266
        Top = 143
        Width = 229
        Height = 20
        Caption = 'Bad sector list -> DEC std 144 bad sector file'
        TabOrder = 2
        OnClick = WriteBadBlocksToBadSectorFileButtonClick
      end
      object MediaSerialNumberEdit: TEdit
        Left = 97
        Top = 123
        Width = 57
        Height = 21
        TabOrder = 3
        Text = '88888888'
        OnChange = MediaSerialNumberEditChange
        OnExit = MediaSerialNumberEditExit
      end
    end
    object SaveMetaInfoButton: TButton
      Left = 428
      Top = 20
      Width = 87
      Height = 25
      Caption = 'Save meta info'
      TabOrder = 5
      OnClick = SaveMetaInfoButtonClick
    end
  end
  object ReadWriteGroupBox: TGroupBox
    Left = 0
    Top = 442
    Width = 527
    Height = 127
    Caption = ' Read/Write operation '
    TabOrder = 1
    object BlockNrLabel: TLabel
      Left = 11
      Top = 19
      Width = 57
      Height = 13
      Caption = 'Next block: '
    end
    object BlocksPerTransferLabel: TLabel
      Left = 146
      Top = 20
      Width = 82
      Height = 13
      Caption = 'blocks / transfer:'
    end
    object StopOnErrorCheckBox: TCheckBox
      Left = 288
      Top = 12
      Width = 97
      Height = 17
      Caption = 'Stop on error'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = StopOnErrorCheckBoxClick
    end
    object ProtectVendorAreaCheckBox: TCheckBox
      Left = 288
      Top = 35
      Width = 133
      Height = 17
      Caption = 'Protect vendor area'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = ProtectVendorAreaCheckBoxClick
    end
    object ReadImageButton: TButton
      Left = 11
      Top = 61
      Width = 160
      Height = 24
      Caption = 'Read all from '#39'blocknr'#39' to end'
      TabOrder = 2
      OnClick = ReadImageButtonClick
    end
    object StopButton: TButton
      Left = 440
      Top = 24
      Width = 75
      Height = 23
      Caption = 'Stop'
      TabOrder = 3
      OnClick = StopButtonClick
    end
    object BlockNrEdit: TEdit
      Left = 88
      Top = 18
      Width = 47
      Height = 21
      TabOrder = 4
      Text = '0'
    end
    object WriteImageButton: TButton
      Left = 182
      Top = 61
      Width = 160
      Height = 24
      Caption = 'Write all from '#39'blocknr'#39' to end'
      TabOrder = 5
      OnClick = WriteImageButtonClick
    end
    object ReadSingleBlockButton: TButton
      Left = 11
      Top = 91
      Width = 160
      Height = 25
      Caption = 'Read single sector '#39'Block nr'#39
      TabOrder = 6
      OnClick = ReadSingleBlockButtonClick
    end
    object WriteSingleBlockButton: TButton
      Left = 182
      Top = 91
      Width = 160
      Height = 25
      Caption = 'Write single block '#39'Block nr'#39
      TabOrder = 7
      OnClick = WriteSingleBlockButtonClick
    end
    object CheckSingleBlockButton: TButton
      Left = 355
      Top = 91
      Width = 160
      Height = 25
      Caption = 'Check single block '#39'Block nr'#39
      TabOrder = 8
      OnClick = ReadSingleBlockButtonClick
    end
    object CheckImageButton: TButton
      Left = 355
      Top = 61
      Width = 160
      Height = 24
      Caption = 'Check all from '#39'blocknr'#39' to end'
      TabOrder = 9
      OnClick = ReadImageButtonClick
    end
    object UpdateCurBlockEditCheckBox: TCheckBox
      Left = 11
      Top = 42
      Width = 122
      Height = 17
      Caption = 'Track current  block'
      TabOrder = 10
      OnClick = UpdateCurBlockEditCheckBoxClick
    end
    object BlockPerTransferEdit: TEdit
      Left = 229
      Top = 18
      Width = 33
      Height = 21
      TabOrder = 11
      Text = '0'
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 572
    Width = 527
    Height = 64
    Caption = ' Communication self test '
    TabOrder = 2
    object Label6: TLabel
      Left = 94
      Top = 21
      Width = 60
      Height = 13
      Caption = 'Word count:'
    end
    object Label7: TLabel
      Left = 205
      Top = 20
      Width = 39
      Height = 13
      Caption = 'Repeat:'
    end
    object Label5: TLabel
      Left = 299
      Top = 20
      Width = 40
      Height = 13
      Caption = 'Pattern:'
    end
    object SelftestWordcountEdit: TEdit
      Left = 160
      Top = 18
      Width = 39
      Height = 21
      TabOrder = 0
      Text = '4096'
    end
    object SelftestButton: TButton
      Left = 14
      Top = 43
      Width = 69
      Height = 17
      Caption = 'Test'
      TabOrder = 1
      OnClick = SelftestButtonClick
    end
    object LoadDriverButton: TButton
      Left = 11
      Top = 18
      Width = 75
      Height = 19
      Caption = 'Load driver'
      TabOrder = 2
      OnClick = LoadDriverButtonClick
    end
    object SelfTestRepeatCountEdit: TEdit
      Left = 250
      Top = 17
      Width = 39
      Height = 21
      TabOrder = 3
      Text = '1'
    end
    object SelftestRLECompressionCheckBox: TCheckBox
      Left = 102
      Top = 44
      Width = 135
      Height = 17
      Caption = 'Test RLE compression'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object SelftestPatternComboBox: TComboBox
      Left = 342
      Top = 18
      Width = 173
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 5
      Text = 'random with constant dword block'
      Items.Strings = (
        'random'
        'random with constant word block'
        'random with constant dword block'
        'constant data, max RLE block len'
        'count up'
        'count down from 177777')
    end
    object SelftestRandomWordCountCheckBox: TCheckBox
      Left = 251
      Top = 44
      Width = 122
      Height = 17
      Caption = 'Random word count'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object GroupBox4: TGroupBox
    Left = 2
    Top = 73
    Width = 525
    Height = 141
    Caption = ' Disc or Tape selection '
    TabOrder = 3
    object DriveInfoLabel: TLabel
      Left = 20
      Top = 86
      Width = 50
      Height = 13
      Caption = 'Drive info:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 8
      Top = 19
      Width = 76
      Height = 13
      Caption = 'Controller type:'
    end
    object Label3: TLabel
      Left = 297
      Top = 19
      Width = 23
      Height = 13
      Caption = 'Unit:'
    end
    object Label4: TLabel
      Left = 375
      Top = 19
      Width = 39
      Height = 13
      Caption = 'Device: '
    end
    object Label11: TLabel
      Left = 186
      Top = 19
      Width = 52
      Height = 13
      Caption = 'Cntrl addr:'
    end
    object DriveInfoMemo: TMemo
      Left = 76
      Top = 83
      Width = 437
      Height = 47
      Lines.Strings = (
        'DriveInfoMemo')
      TabOrder = 0
    end
    object OpenDeviceButton: TButton
      Left = 249
      Top = 45
      Width = 112
      Height = 25
      Caption = 'Reset && Open device'
      TabOrder = 1
      OnClick = OpenDeviceButtonClick
    end
    object ControllerComboBox: TComboBox
      Left = 85
      Top = 16
      Width = 95
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnChange = ControllerComboBoxChange
    end
    object UnitNumberEdit: TEdit
      Left = 326
      Top = 16
      Width = 24
      Height = 21
      TabOrder = 3
      Text = '0'
      OnChange = UnitNumberEditChange
    end
    object UnitNrUpDown: TUpDown
      Left = 350
      Top = 16
      Width = 15
      Height = 21
      Associate = UnitNumberEdit
      Max = 7
      TabOrder = 4
    end
    object DeviceComboBox: TComboBox
      Left = 420
      Top = 16
      Width = 93
      Height = 21
      TabOrder = 5
      OnChange = DeviceComboBoxChange
    end
    object ControllerBaseAddressEdit: TEdit
      Left = 241
      Top = 16
      Width = 46
      Height = 21
      TabOrder = 6
      Text = '161234'
      OnChange = ControllerBaseAddressEditChange
      OnKeyPress = ControllerBaseAddressEditKeyPress
    end
    object DriverStopButton: TButton
      Left = 114
      Top = 43
      Width = 66
      Height = 25
      Caption = 'Stop driver'
      TabOrder = 7
      OnClick = DriverStopButtonClick
    end
    object DriverInvalidateButton: TButton
      Left = 14
      Top = 43
      Width = 76
      Height = 26
      Caption = 'Unload driver'
      TabOrder = 8
      OnClick = DriverInvalidateButtonClick
    end
  end
  object OpenDiskImageDialog: TOpenDialog
    Filter = 'Byte stream picture|*.bmp|All|*.*'
    Left = 18
    Top = 414
  end
  object SaveDiskImageDialog: TSaveDialog
    Filter = 'Byte stream picture|*.bmp|All|*.*'
    Left = 112
    Top = 384
  end
  object OpenDiskImageOverlayDialog: TOpenDialog
    Left = 50
    Top = 404
  end
  object SaveDiskMetaInfoDialog: TSaveDialog
    Left = 170
    Top = 388
  end
end
