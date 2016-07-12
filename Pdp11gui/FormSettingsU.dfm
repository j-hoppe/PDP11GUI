object FormSettings: TFormSettings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Connection settings'
  ClientHeight = 256
  ClientWidth = 538
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ComportLabel: TLabel
    Left = 59
    Top = 121
    Width = 51
    Height = 13
    Caption = 'COM-Port:'
  end
  object BaudrateLabel: TLabel
    Left = 62
    Top = 151
    Width = 48
    Height = 13
    Caption = 'Baudrate:'
  end
  object Label3: TLabel
    Left = 38
    Top = 26
    Width = 72
    Height = 13
    Caption = 'PDP-11 target:'
  end
  object HostnameLabel: TLabel
    Left = 58
    Top = 61
    Width = 52
    Height = 13
    Caption = 'Hostname:'
  end
  object TelnetPortLabel: TLabel
    Left = 53
    Top = 94
    Width = 57
    Height = 13
    Caption = 'Telnet port:'
  end
  object MonitorEntryAddressLabel: TLabel
    Left = 0
    Top = 212
    Width = 110
    Height = 13
    Caption = 'Monitor entry address:'
  end
  object SerialFormatLabel: TLabel
    Left = 74
    Top = 178
    Width = 38
    Height = 13
    Caption = 'Format:'
  end
  object ComportComboBox: TComboBox
    Left = 122
    Top = 118
    Width = 103
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = ComportComboBoxChange
    Items.Strings = (
      'COM1'
      'COM2'
      'COM3'
      'COM4'
      'COM5'
      'COM6'
      'COM7'
      'COM8'
      'COM9'
      'COM10'
      'COM11'
      'COM12'
      'COM13'
      'COM14'
      'COM15'
      'COM16'
      'COM17'
      'COM18'
      'COM19'
      'COM21'
      'COM22'
      'COM23'
      'COM24'
      'COM25'
      'COM26'
      'COM27'
      'COM28'
      'COM29'
      'COM30'
      'COM31'
      'COM32')
  end
  object OKButton: TButton
    Left = 154
    Top = 235
    Width = 75
    Height = 21
    Caption = 'Connect'
    ModalResult = 1
    TabOrder = 1
  end
  object BaudrateComboBox: TComboBox
    Left = 122
    Top = 148
    Width = 103
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = BaudrateComboBoxChange
    Items.Strings = (
      '1200'
      '2400'
      '4800'
      '9600'
      '19200'
      '38400'
      '57600'
      '115200')
  end
  object Pdp11SelectComboBox: TComboBox
    Left = 122
    Top = 23
    Width = 389
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = Pdp11SelectComboBoxChange
  end
  object HostnameEdit: TEdit
    Left = 122
    Top = 58
    Width = 389
    Height = 21
    TabOrder = 4
    OnChange = HostnameEditChange
  end
  object TelnetPortEdit: TEdit
    Left = 122
    Top = 91
    Width = 79
    Height = 21
    TabOrder = 5
    OnChange = TelnetPortEditChange
  end
  object MonitorEntryAddressEdit: TEdit
    Left = 122
    Top = 205
    Width = 79
    Height = 21
    Hint = 
      'Needed to jump into monitor ROM isntead of HALT in the disk driv' +
      'er.'
    TabOrder = 6
    OnChange = MonitorEntryAddressEditChange
  end
  object ShowFakeConsolesCheckBox: TCheckBox
    Left = 122
    Top = 0
    Width = 247
    Height = 17
    Caption = ' allow selection of self test consoles ("fakes")'
    TabOrder = 7
    OnClick = ShowFakeConsolesCheckBoxClick
  end
  object SerialFormatComboBox: TComboBox
    Left = 122
    Top = 175
    Width = 103
    Height = 21
    Style = csDropDownList
    TabOrder = 8
    OnChange = SerialFormatComboBoxChange
    Items.Strings = (
      '1200'
      '2400'
      '4800'
      '9600'
      '19200'
      '38400'
      '57600'
      '115200')
  end
end
