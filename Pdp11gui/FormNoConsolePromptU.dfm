object FormNoConsolePrompt: TFormNoConsolePrompt
  Left = 0
  Top = 0
  Caption = 'No console prompt'
  ClientHeight = 425
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
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 406
    Height = 362
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alTop
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'PDP11GUI could not see the PDP-11 console prompt!'
      ''
      
        'First try to synchronize to the PDP-11 with "Connections/Reconne' +
        'ct".'
      ''
      'Then check for possible error reasons:'
      ''
      '1) No serial connection to target machine.'
      
        '  This is true if you cannot see good output in the Terminal win' +
        'dow.'
      '1.1)  If you use a serial connection, check'
      '  - COM port and baudrate on your PC.'
      '  - serial console port and baudrate settings on your PDP-11'
      '  - the interconnecting cable (is it "null modem"?)'
      '1.2)  If you use a telnet connection, check'
      '  - host name and port.'
      '  - your network and your firewall settings.'
      '  - is your telnet server running?'
      
        '  - if connecting to SimH: has "set stdio telnet=<port>" timed o' +
        'ut?'
      ''
      
        '  You can verify the connection with any external Terminal emula' +
        'tor'
      '  program too!'
      ''
      '2) PDP-11 is not in console mode.'
      
        '  If you power on your PDP-11, it will likely boot directly from' +
        ' disc or tape.'
      '  To get the console prompt, flip the RUN/HALT switch to the'
      '  HALT position. Let it stay at HALT or reset it to RUN, but'
      '  update the switch setting in the Execution window accordingly.'
      ''
      '3) PDP-11 is in console mode, but you choose the wrong'
      '  machine type. Check selection in "Connections/Settings".')
    ParentFont = False
    TabOrder = 0
  end
  object OkButton: TButton
    Left = 126
    Top = 389
    Width = 154
    Height = 25
    Caption = 'Check connection settings'
    ModalResult = 1
    TabOrder = 1
  end
end
