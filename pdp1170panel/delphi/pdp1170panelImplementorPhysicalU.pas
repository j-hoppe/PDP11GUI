unit pdp1170panelImplementorPhysicalU; 

interface 

uses 
  Windows, Classes, 
  iowkit, 
  pdp1170PanelU ; 

{
 Ansteuerung eines PDP11/70 Console Panels über eine IO-Warrior56 Schaltung
 ein IOWarrior  überträgt einen HID-Report, der aus 7 BYtes besteht.
 Die 7 bytes entsprechen den Ports P0..P6

 Verwendet werden nur 36 bits der Ports P0..P3 sowie P4.0..5
 Diese 36 Signalleitungen bilden auf der Zusatzplatinen einen Bustruktur,
 sie heissen IOBUS0..IOBUS35
 Daran sind 3 "Register" angeschlossen.
 SWITCHES (readonly, 74LS244) schaltet 34 Switch-Signale von J3 auf den IOBUs
     IOBUS3..34 gehen an 5 74LS244 (8fach Octal line driver) = SWITCHREG_A..E
     Steuerleitung SWITCHENABLE = IOBUS0 geht an alle 1G/2G der 74LS244
     Low an IOBUS0 = tristte schalter Dten auf IOBUS
     Stteurt im wesentlichen J3
 LEDS0 (write only), IOBUS3..IOBUS36 gehen an 4x 74LS373,
         Datenüernahme mit fallender Flanke an, steuert J2 des Panels an)
        p0.1 = LEDS0LATCH  geht an 4 74LS373
 LEDS1, wie LEDS0, Steuerleitung P 0.2 = LEDs1 Enable
  geht an 2x 74ls373, steueer im wesentlichen J1 des Panels an.

 p0.3 is not used

 iobus0   p0.0  L =
 Diese steuern über eine Zeile Bank von Input-Tristatete

}

type 

  // Ist ein Implementor für pdp1170panel
  Tpdp1170panelImplementorPhysical = class(TObject) 
    private 
      // Iowkit-Handles
      iowkitDevHandle: IOWKIT_HANDLE; 
      numIowkitDevices: integer ; 
      iowkitApiLoaded: boolean ; // false, wenn die DLL nicht gefunden wurde

      OpenDevice_NextTime: dword ; // TickCount des nächsten OpenDevice-Versuchs
    public 
      pdp1170panel: Tpdp1170panel ; // Das Basipanel.

      // USB-Device angesteckt?
      Connected : boolean ; 

      constructor Create ; 
      destructor Destroy ; override ; 

      procedure OpenDevice ; 

      procedure ReadInputRegisters(var switchreg0 : dword ; var switchreg1 : dword) ; 
      procedure WriteOutputRegisters(ledsreg0 : dword ; ledsreg1 : dword) ; 

      // gleicht das pdp1170panel-Object mit der Hardware ab.
      // schreibt die gelesenen neuen Control-States nach implementor_active_state.
      procedure SyncLedsFromBasePanel(Sender: TObject) ; 
      procedure SyncSwitchesToBasePanel(Sender: TObject) ; 


    end{ "TYPE Tpdp1170panelImplementorPhysical = class(TObject)" } ; 


implementation 

uses 
  SysUtils, 
  Dialogs, 
  Forms, 
  FormMainU ; 


// gibt die Bitmaske zurück, die die Werte 0..value halten kann
// Bsp: 6 -> $07, 10 -> $0f
function value2bitmask(val: dword): dword ; 
  begin 
    result := 0 ; 
    while val > 0 do begin 
      val := val shr 1 ; 
      result := (result shl 1) or 1 ; 
    end; 
  end; 


// gibt das niedrigste in "mask" gesetzt Bit zurück
function bitmask2lsb(mask: dword): integer ; 
  begin 
    if mask = 0 then 
      result := 0 
    else begin 
      result := 0 ; 
      while (mask and 1) = 0 do begin 
        mask := mask shr 1 ; 
        inc(result) ; 
      end; 
    end; 
  end; 


constructor Tpdp1170panelImplementorPhysical.Create ; 
  var 
    ctrl: Tpdp1170PanelControl ; 
    i: integer ; 
  begin 
    inherited ; 

    Connected := false ; 
    iowkitDevHandle := nil ; 
    OpenDevice_NextTime := 0 ; 

    pdp1170panel := Tpdp1170panel.Create; 

    pdp1170panel.OnSyncSwitchesFromImplementor := SyncSwitchesToBasePanel ; 
    pdp1170panel.OnSyncLedsToImplementor := SyncLedsFromBasePanel ; 

    ////////////////// Registerbits an die LEDs binden ////////////////////////////
    ///  // leds gehen an die Outputregister
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'KERNEL-D') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'SUPER-D') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'USER-D') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'KERNEL-I') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'SUPER-I') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'USER-I') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'CONS PHY') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_ADDRESS_SELECT, 'PROG PHY') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden

    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_DATA_SELECT, 'DATA PATHS') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_DATA_SELECT, 'BUS REG') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_DATA_SELECT, 'µADRS FPP/CPU') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_DATA_SELECT, 'DISPLAY REGISTER') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ; // kann nicht direkt gesetzt werden

    // Adress-Leds: über Index finden
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000001 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 1) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000002 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 2) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000004 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 3) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000008 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 4) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000010 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 5) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000020 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 6) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000040 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 7) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000080 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 8) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000100 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 9) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000200 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 10) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000400 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 11) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000800 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 12) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00001000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 13) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00002000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 14) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00004000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 15) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00008000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 16) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00010000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 17) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00020000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 18) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00040000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 19) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00080000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 20) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00100000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESS, 21) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00200000 ; 

    // DATA-Leds: über Index finden
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00010000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 1) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00020000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 2) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00040000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 3) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00080000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 4) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00100000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 5) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00200000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 6) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00400000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 7) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00800000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 8) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $01000000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 9) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $02000000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 10) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $04000000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 11) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $08000000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 12) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $10000000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 13) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $20000000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 14) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $40000000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA, 15) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $80000000 ; 

    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_PARITY, 'PARITY LOW') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000002 ; 
    ctrl := pdp1170panel.getControlByName(_Led, pdp1170Led_PARITY, 'PARITY HIGH') ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000001 ; 

    // Diese LEDS sind einzeln in ihrer Gruppe, kein Name nötig
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_PAR_ERR, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000008 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADRS_ERR, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000004 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_RUN, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000040 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_PAUSE, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000020 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_MASTER, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000010 ; 

    // 00000000=Kernel, 00000100 = OFF, 00000200 = SUPER 00000300=USER
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_USER_SUPER_KERNEL, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000300 ; 

    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESSING_16, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000800 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESSING_18, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00001000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_ADDRESSING_22, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00002000 ; 
    ctrl := pdp1170panel.getControlByID(_Led, pdp1170Led_DATA_SPACE, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000400 ; 


    ////////////////// die Images an die Switches binden ////////////////////////////
    ///  // Switches kommen aus den Inpoutregistern
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_Power, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ;  // kann nicht abgefragt werden
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_LampTest, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := -1 ;  // kann nicht abgefragt werden
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_LOAD_ADRS, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $1000000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_EXAM, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $4000000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DEP, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $8000000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_CONT, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00400000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_ENABLE_HALT, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $10000000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_SINST_SBUS_CYCLE, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00800000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_START, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $02000000 ; 
    // TODO: Schalterstellungen 0..7 auf logische States abgleichen!
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_ADDRESS_SELECT, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $E0000000 ; 
    // TODO: Schalterstellungen 0..7 auf logische States abgleichen!
    // Hier: immer zwei Schalterstellungen für einen State:
    // "getStateByName" findet den ersten State, der nächste kommt 4 states später
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA_SELECT, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 1 ; ctrl.register_mask := $00000003 ; 

    // die 22 Data-Schalter
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 0) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000001 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 1) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000002 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 2) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000004 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 3) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000008 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 4) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000010 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 5) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000020 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 6) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000040 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 7) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000080 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 8) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000100 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 9) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000200 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 10) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000400 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 11) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00000800 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 12) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00001000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 13) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00002000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 14) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00004000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 15) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00008000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 16) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00010000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 17) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00020000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 18) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00040000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 19) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00080000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 20) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00100000 ; 
    ctrl := pdp1170panel.getControlByID(_Switch, pdp1170Sw_DATA, 21) ; assert(ctrl <> nil) ; 
    ctrl.register_nr := 0 ; ctrl.register_mask := $00200000 ; 

    // aus register_mask register_lsb berechnen
    for i := 0 to pdp1170panel.ControlCount - 1 do begin 
      ctrl := pdp1170panel.Controls[i] ; 
      ctrl.register_lsb := bitmask2lsb(ctrl.register_mask) ; 
    end; 

    iowkitApiLoaded := LoadIowKitAPI ; 

//    if not iowkitApiLoaded then begin
//      buff := Format('LoadIowKitAPI() failed', [0]) ;
//      MessageDlg(buff, mtError, mbOKCancel, 0);
//    end ; 
  end{ "constructor Tpdp1170panelImplementorPhysical.Create" } ; 


destructor Tpdp1170panelImplementorPhysical.Destroy ; 
  begin 
    if iowkitDevHandle <> nil then begin 
      IowKitCancelIo(iowkitDevHandle, IOW_PIPE_IO_PINS) ; 
      IowKitCancelIo(iowkitDevHandle, IOW_PIPE_SPECIAL_MODE) ; 
      IowKitCloseDevice(iowkitDevHandle) ; 
    end; 
    UnloadIowKitAPI ; 
    pdp1170panel.Free ; 
    inherited ; 
  end; 


// Open device, wenn eins angesteckt.
// stellt auch neu die Anzahl der eingesteckten IOwarrior fest
// Dazu muss die Library re-loaded werden.
// Hat einen eingebauten Aufruf-Frequenz-Begrenzer!
procedure Tpdp1170panelImplementorPhysical.OpenDevice ;
  var
    buff1: array[0..256] of WCHAR ;
  begin

    if not Connected then // reconnect-Versuche zeitlich begrenzen
      // da Aufruf aus high-frequency routinen heraus
      if GetTickCount < OpenDevice_NextTime then 
        Exit ; 

    // nur jede Sekunde ein versuch!
    OpenDevice_NextTime := GetTickCount + 1000 ; 

    try 

      Connected := false ; 
      // Notbremse: wenn DLL nicht gefunden wurde, immer deconnected!
      if not iowkitApiLoaded then 
        Exit ; 

      IowKitCloseDevice(nil) ; // close all ... for clean reopening

      // das hier ist nötig, um die Anzahl der devices neu zu bestimmen
      UnloadIowKitAPI ; 
      iowkitApiLoaded := LoadIowKitAPI ; 

      iowkitDevHandle := IowKitOpenDevice ; 
      numIowkitDevices :=IowKitGetNumDevs ; 
      if numIowkitDevices = 0 then 
        Exit ; 

      if iowkitDevHandle <> nil then begin 

        Connected := true ; 
        Log('productid = 0x%x', [IowKitGetProductId(iowkitDevHandle)]);
        Log('revision = 0x%x', [IowKitGetRevision(iowkitDevHandle)]);
        IowKitGetSerialNumber(iowkitDevHandle, buff1) ;
        Log('serial = %S', [buff1]); 
        //
      end ; 

    except // Fehler ignorieren ... device muss gar nicht vorhanden sein.

    end{ "try" } ; 
  end { "procedure Tpdp1170panelImplementorPhysical.OpenDevice" } ; 


// 2*32 bit Registerwerte in den hid-report schreiben
// problem: register 0..32 wird in iobit 3..36 übertragen,
// ist also um 4 bit verschoben,
// also reg.0 liegt auf byte[0].3, etc.
procedure Reg2hidIoReport(reg_bits0_31: dword ; 
        reg_bits32_63: dword ; 
        var rep: IOWKIT56_IO_REPORT) ; 
  var 
    bits: dword ; 
  begin 

    rep.Bytes[0] := rep.Bytes[0] and $0f ; // Steuerleitungen erhalten
    rep.Bytes[1] := 0 ; rep.Bytes[2] := 0 ; rep.Bytes[3] := 0 ; 
    rep.Bytes[4] := 0 ; rep.Bytes[5] := 0 ; rep.Bytes[6] := 0 ; 

    bits := reg_bits0_31 and $0f ; 
    rep.Bytes[0] := rep.Bytes[0] or (bits shl 4) ; 

    bits := (reg_bits0_31 shr 4) and $ff ; 
    rep.Bytes[1] := bits ; 

    bits := (reg_bits0_31 shr 12) and $ff ; 
    rep.Bytes[2] := bits ; 

    bits := (reg_bits0_31 shr 20) and $ff ; 
    rep.Bytes[3] := bits ; 

    bits := (reg_bits0_31 shr 28) and $ff ; 
    rep.Bytes[4] := bits ; 


    bits := reg_bits32_63 and $0f ; 
    rep.Bytes[4] := rep.Bytes[4] or (bits shl 4) ; 

    bits := (reg_bits32_63 shr 4) and $ff ; 
    rep.Bytes[5] := bits ; 

    bits := (reg_bits32_63 shr 12) and $ff ; 
    rep.Bytes[6] := bits ; 
  end { "procedure Reg2hidIoReport" } ; 


// einen IO report mit 8 bytes auslesen
// die bits0 ..3 des IOwarrior ignorieren .. es sind die
// Control-Leitungen
procedure hidSpecialReport2Reg(rep: IOWKIT56_SPECIAL_REPORT ; 
        var result_bits0_31: dword ; 
        var result_bits32_63: dword) ; 
  var 
    bits: dword ; 
    reg: dword ; 
  begin 
    reg := 0 ; 
    bits := rep.Bytes[0] and $f0 ; 
    reg := reg or (bits shr 4) ; 

    bits := rep.Bytes[1] ; 
    reg := reg or (bits shl 4) ; 

    bits := rep.Bytes[2] ; 
    reg := reg or (bits shl 12) ; 

    bits := rep.Bytes[3] ; 
    reg := reg or (bits shl 20) ; 

    bits := rep.Bytes[4] and $0f ; 
    reg := reg or (bits shl 28) ; 


    result_bits0_31 := reg ; 

    reg := 0 ; 

    bits := rep.Bytes[4] and $f0 ; 
    reg := reg or (bits shr 4) ; 

    bits := rep.Bytes[5] ; 
    reg := reg or (bits shl 4) ; 

    bits := rep.Bytes[6] ; 
    reg := reg or (bits shl 12) ; 

    bits := rep.Bytes[7] ; 
    reg := reg or (bits shl 20) ; 

    result_bits32_63 := reg ; 

  end { "procedure hidSpecialReport2Reg" } ; 

{
     UpdateRegisters
     zugriff auf die regsiter auf der Zusatzplatine.
     Kombinationssequenz, schreibt leds0reg und leds1reg,
     liest switchreg

    Update() legt die LED-Zustände aus pdp1170panel->LEDS
    auf die Outputregister und liest die Switchzustände wieder ein
    Sequenz:
    1. Report :                      input tristate, output data 0 auf iobus
       iobus0 switchenable     1
       iobus1  leds0latch     1
        iobus2  leds1enable    0
        ..
        data    data for LEDS0-REgsiter

    2. Report:    input tristate, clock output data 0 into regsiter 0
                Data für  leds1 auf Bus
                (geht, da P1,2,3 vom IOWarrior erst nach P0 geschaltet werden.
       iobus0 switchenable     1
       iobus1  leds0latch     0   fallende Flanke
        iobus2  leds1enable    1   reg1 öffnen
        data for LED1-Register

    3. Report :                      input tristate, clock output data 1 into register
                clear data, so IOWarrior-Pins act as Inputs
       iobus0 switchenable     1
       iobus1  leds0latch     0
        iobus2  leds1enable    0    fallende Flanke
        ..
        data    all 0xFF        IOWarior Pins open

    4. Report    input onto iobus
       iobus0 switchenable     0    // inputregister treibt iobus.
       iobus1  leds0latch     0
        iobus2  leds1enable    0
        DATA     ALL 0xFF

    5. Report   : read iobus = Switch-Signals
}

// NB: Input enabled
procedure Tpdp1170panelImplementorPhysical.ReadInputRegisters( 
        var switchreg0 : dword ; var switchreg1 : dword) ; 
  var 
    io_rep: IOWKIT56_IO_REPORT ; // der report
    special_rep: IOWKIT56_SPECIAL_REPORT ; // der report
    i: integer ; 
  begin 
    try 
      // Device connected oder re-connected?
      if not Connected then OpenDevice ; 
      if not Connected then Exit ; 

      io_rep.ReportID := 0; 

      // report 3: IOWarrior pins auf input. alle 56 bits (interner bus tristate)
      io_rep.Bytes[0] := $01 ; 
      Reg2hidIoReport($ffffffff, $ffffffff, io_rep) ; 
      if IowKitWrite(iowkitDevHandle, IOW_PIPE_IO_PINS, pchar(@io_rep), IOWKIT56_IO_REPORT_SIZE) 
              <> IOWKIT56_IO_REPORT_SIZE then raise Exception.Create('IowKitWrite failed') ; 

      // report 4: enable input buffers: bus = input
      io_rep.Bytes[0] := $00 ; 
      Reg2hidIoReport($ffffffff, $ffffffff, io_rep) ; 
      if IowKitWrite(iowkitDevHandle, IOW_PIPE_IO_PINS, pchar(@io_rep), IOWKIT56_IO_REPORT_SIZE) 
              <> IOWKIT56_IO_REPORT_SIZE then raise Exception.Create('IowKitWrite failed') ; 

      // wie inputs abfragen?
      // report 5: request
      special_rep.ReportID := $ff ; 
      for i := 0 to length(special_rep.Bytes)-1 do special_rep.Bytes[i] := 0 ; 

      // An Interface 1 senden!!!
      IowKitWrite(iowkitDevHandle, IOW_PIPE_SPECIAL_MODE, pchar(@special_rep), IOWKIT56_SPECIAL_REPORT_SIZE) ; 

      IowKitRead(iowkitDevHandle, IOW_PIPE_SPECIAL_MODE, pchar(@special_rep), IOWKIT56_SPECIAL_REPORT_SIZE); 

      hidSpecialReport2Reg(special_rep, switchreg0, switchreg1) ; 

    except 
      // wahrscheinlichster Fehler: abgezogen
      Connected := false ; 
    end{ "try" } ; 

  end{ "procedure Tpdp1170panelImplementorPhysical.ReadInputRegisters" } ; 


// Auf Ausgabe schalten, 2 register schreiben
// NB: Input disabled
procedure Tpdp1170panelImplementorPhysical.WriteOutputRegisters( 
        ledsreg0 : dword ; ledsreg1 : dword) ; 
  var 
    io_rep: IOWKIT56_IO_REPORT ; // der report
  begin 
    try 
      // Device connected oder re-connected?
      if not Connected then OpenDevice ; 
      if not Connected then Exit ; 

      io_rep.ReportID := 0; 

      // Unoptimierte Ausgabe: jeder state einzeln

      // Reportdefinitionen siehe oben!
      // report 1: input buffer tristate, output data 0 auf iobus
      // mögliche optimierung: nur ausgeben, wenn sich "leds0reg" geändert hat
      io_rep.Bytes[0] := $03 ; 
      Reg2hidIoReport(ledsreg0, 0, io_rep) ; 
      if IowKitWrite(iowkitDevHandle, IOW_PIPE_IO_PINS, pchar(@io_rep), IOWKIT56_IO_REPORT_SIZE) 
              <> IOWKIT56_IO_REPORT_SIZE then raise Exception.Create('IowKitWrite failed') ; 

      // report 1b:  enable H->L, save output data 0 in 373 latches
      io_rep.Bytes[0] := $01 ; 
      Reg2hidIoReport(ledsreg0, 0, io_rep) ; // Output nur 32 bit
      if IowKitWrite(iowkitDevHandle, IOW_PIPE_IO_PINS, pchar(@io_rep), IOWKIT56_IO_REPORT_SIZE) 
              <> IOWKIT56_IO_REPORT_SIZE then raise Exception.Create('IowKitWrite failed') ; 

      // report 2: input tristate, output data 1 auf iobus
      // mögliche optimierung: nur ausgeben, wenn sich "leds1reg" geändert hat
      io_rep.Bytes[0] := $05 ; 
      Reg2hidIoReport(ledsreg1, 0, io_rep) ; // Output nur 32 bit
      if IowKitWrite(iowkitDevHandle, IOW_PIPE_IO_PINS, pchar(@io_rep), IOWKIT56_IO_REPORT_SIZE) 
              <> IOWKIT56_IO_REPORT_SIZE then raise Exception.Create('IowKitWrite failed') ; 

      // report 2b:  enable H->L, save output data 1 in 373 latches
      io_rep.Bytes[0] := $01 ; 
      Reg2hidIoReport(ledsreg1, 0, io_rep) ;  // Output nur 32 bit
      if IowKitWrite(iowkitDevHandle, IOW_PIPE_IO_PINS, pchar(@io_rep), IOWKIT56_IO_REPORT_SIZE) 
              <> IOWKIT56_IO_REPORT_SIZE then raise Exception.Create('IowKitWrite failed') ; 

    except 
      // wahrscheinlichster Fehler: abgezogen
      Connected := false ; 
    end{ "try" } ; 

  end{ "procedure Tpdp1170panelImplementorPhysical.WriteOutputRegisters" } ; 


// gleicht das pdp1170panel-Object mit der Hardware ab.
// schreibt die gelesenen neuen Control-States nach implementor_active_state.
procedure Tpdp1170panelImplementorPhysical.SyncLedsFromBasePanel(Sender: TObject) ; 
  var 
    cur_led_reg: array[0..1] of dword ; 
    i: integer ; 
    ctrl : Tpdp1170PanelControl ; 
    value: dword ; 
  begin 
    // LEDS codieren
    cur_led_reg[0] := 0 ; cur_led_reg[1] := 0 ; 
    for i := 0 to pdp1170panel.ControlCount - 1 do begin 
      ctrl := pdp1170panel.Controls[i] ; 
      if (ctrl.ctltype = _Led) and (ctrl.register_nr >= 0) then begin 
        // bits für diese LEd in das passende output register ORen
        assert(ctrl.implementor_active_state_index < ctrl.StateCount) ; 
        value := ctrl.States[ctrl.implementor_active_state_index].value ; 
        value := value shl ctrl.register_lsb ; 
        cur_led_reg[ctrl.register_nr] := cur_led_reg[ctrl.register_nr] or value ; 
      end; 
    end; 

    // LEDS im Panel setzen
    WriteOutputRegisters(cur_led_reg[0], cur_led_reg[1]) ; 

  end{ "procedure Tpdp1170panelImplementorPhysical.SyncLedsFromBasePanel" } ; 


procedure Tpdp1170panelImplementorPhysical.SyncSwitchesToBasePanel(Sender: TObject) ; 
  var 
    cur_switch_reg: array[0..1] of dword ; 
    i: integer ; 
    ctrl : Tpdp1170PanelControl ; 
    state: Tpdp1170PanelControlState ; 
    reg, value: dword ; 
  begin 
    // Switches einlesen
    ReadInputRegisters(cur_switch_reg[0], cur_switch_reg[1]) ; 

    // Device nicht (mehr) da?
    if Connected then begin 
      // gelesene switch register nach Control-States decodieren
      for i := 0 to pdp1170panel.ControlCount - 1 do begin 
        ctrl := pdp1170panel.Controls[i] ; 
        if (ctrl.ctltype = _Switch) and (ctrl.register_nr >= 0) then begin 
          // die richtige Bitgruppe aus dem passenden inputregister wählen
          reg := cur_switch_reg[ctrl.register_nr] ; 
          reg := reg and ctrl.register_mask ; 
          value := reg shr ctrl.register_lsb ; // die bits von value nach bit 0 shiften
          state := ctrl.getStateByValue(value) ; 
          ctrl.implementor_active_state_index := state.index ; 
          // wird erst von pdp1170panel an die Application signalisiert
        end; 
      end; 
    end{ "if Connected" } ; 

  end{ "procedure Tpdp1170panelImplementorPhysical.SyncSwitchesToBasePanel" } ; 


end{ "unit pdp1170panelImplementorPhysicalU" } . 
