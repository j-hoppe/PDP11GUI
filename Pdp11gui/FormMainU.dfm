object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 824
  ClientWidth = 781
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Scaled = False
  WindowMenu = Windows1
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 781
    Height = 824
    Align = alClient
    Proportional = True
    ExplicitLeft = 8
    ExplicitTop = -8
    ExplicitWidth = 733
    ExplicitHeight = 86
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = UpdateTimerTimer
    Left = 160
    Top = 64
  end
  object MainMenu1: TMainMenu
    Left = 239
    Top = 43
    object File1: TMenuItem
      Caption = 'File'
      object Loadmachinedescription1: TMenuItem
        Caption = 'Load machine description'
        OnClick = Loadmachinedescription1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Connection1: TMenuItem
      Caption = 'Connection'
      object Terminal1: TMenuItem
        Caption = 'Terminal'
        OnClick = FormEnableMenuItemClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Settings2: TMenuItem
        Caption = 'Settings'
        OnClick = Settings1Click
      end
      object Reconnect1: TMenuItem
        Caption = 'Reconnect'
        OnClick = ReconnectButtonClick
      end
    end
    object Programming1: TMenuItem
      Caption = 'Programming'
      OnClick = Programming1Click
      object Macro11Source1: TMenuItem
        Caption = 'MACRO11 Source'
        OnClick = FormEnableMenuItemClick
      end
      object Macro11Listing1: TMenuItem
        Caption = 'MACRO11 Listing'
        OnClick = FormEnableMenuItemClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Execute1: TMenuItem
        Caption = 'Execution Control'
        OnClick = FormEnableMenuItemClick
      end
      object Disassembly1: TMenuItem
        Caption = 'Disassembly'
        OnClick = FormEnableMenuItemClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Blinkenlightinstructions1: TMenuItem
        Caption = 'Blinkenlight instructions'
        OnClick = FormEnableMenuItemClick
      end
    end
    object IOpageMenuItem: TMenuItem
      Caption = 'I/O page'
      object Bitfieldshelper1: TMenuItem
        Caption = 'Bitfields'
        OnClick = FormEnableMenuItemClick
      end
      object IOpagescanner1: TMenuItem
        Caption = 'I/O page scanner'
        OnClick = FormEnableMenuItemClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
    end
    object MemoryMenuItem: TMenuItem
      Caption = 'Memory'
      object MemoryLoader1: TMenuItem
        Caption = 'Memory Loader'
        OnClick = FormEnableMenuItemClick
      end
      object MemoryDumper1: TMenuItem
        Caption = 'Memory Dumper'
        OnClick = FormEnableMenuItemClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Mem11: TMenuItem
        Caption = 'Mem1'
        OnClick = FormEnableMenuItemClick
      end
      object Mem21: TMenuItem
        Caption = 'Mem2'
        OnClick = FormEnableMenuItemClick
      end
      object Mem31: TMenuItem
        Caption = 'Mem3'
        OnClick = FormEnableMenuItemClick
      end
      object Mem41: TMenuItem
        Caption = 'Mem4'
        OnClick = FormEnableMenuItemClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object MemoryTest1: TMenuItem
        Caption = 'Memory Test'
        OnClick = FormEnableMenuItemClick
      end
    end
    object View1: TMenuItem
      Caption = 'Tools'
      object Code1: TMenuItem
        Caption = #181' Code'
        OnClick = FormEnableMenuItemClick
      end
      object MMU1: TMenuItem
        Caption = 'MMU'
        OnClick = FormEnableMenuItemClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object PDP1170panel1: TMenuItem
        Caption = 'PDP-11/70 panel'
        OnClick = FormEnableMenuItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object RL02image1: TMenuItem
        Caption = 'Read/write disc images'
        OnClick = FormEnableMenuItemClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Numberconverter1: TMenuItem
        Caption = 'Number converter'
        OnClick = FormEnableMenuItemClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Log1: TMenuItem
        Caption = 'Log'
        OnClick = FormEnableMenuItemClick
      end
    end
    object Windows1: TMenuItem
      Caption = 'Windows'
      object Cascade1: TMenuItem
        Caption = 'Cascade'
        OnClick = Cascade1Click
      end
      object Arangeicons1: TMenuItem
        Caption = 'Arange Icons'
        OnClick = Arangeicons1Click
      end
      object MinimizeAll1: TMenuItem
        Caption = 'Minimize All'
        OnClick = MinimizeAll1Click
      end
      object Minimizeallbutactive1: TMenuItem
        Caption = 'Minimize All but Active'
        OnClick = Minimizeallbutactive1Click
      end
      object RestoreAll1: TMenuItem
        Caption = 'Restore All'
        OnClick = RestoreAll1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object Onlinetutorial1: TMenuItem
        Caption = 'Online tutorial'
        OnClick = Onlinetutorial1Click
      end
      object Onlinedocumentation1: TMenuItem
        Caption = 'Online documentation'
        OnClick = Onlinedocumentation1Click
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'ini-files|*.ini|all files|*.*'
    Left = 242
    Top = 108
  end
  object StartupTimer: TTimer
    Enabled = False
    OnTimer = StartupTimerTimer
    Left = 365
    Top = 72
  end
end
