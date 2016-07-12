unit FormSettingsU;
{
   Copyright (c) 2016, Joerg Hoppe
   j_hoppe@t-online.de, www.retrocmp.com

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
   JOERG HOPPE BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

{
Parameter für die verschiedenen PDP-11, an die
sich pdp11gui anschliessen kann.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  FormChildU,
  AddressU,
  ConsoleGenericU,
  SerialIoHubU ;

type

{
  Zur Steuerung des Dialogs
  - Liste von Machinen
  - pro Machine: Liste der erlaubten COnnections

  Ergebnis nach Auswahl:
  - 1 machine
  - 1 TConnectionSetting* record
}

  TConsoleSettings = class(TObject)
      consoleType: TConsoleType ;
      hasMonitorEntryAddress: boolean ; // M9312 hat eine Einsprungadresse
      defaultMonitorEntryAddress: string ;
      monitorentryaddress: TMemoryAddress ;

      // list of possible connection
      connections: TList ;
    end ;


(*
// beschreibt Einstellung für einen Typ von Console
// Zugeordnet eine Liste von möglichen COnnections
 TSettingsConfigurationConsole = class(TObject)
   console: TConsoleGeneric ;
   connections: TList ; // of TSettingsConfigurationConnection
 end;



*)



  // beschreibt eine auswählbare Configuration,
  // steuert das Auswahlformular
  TFormSettingsConfiguration = class(TCollectionItem)
    private
      comboboxItemText: string ; // anzeige in Combobox
      registryPrefix: string ; // prefix für werte in registry
      hasSerialComPort: boolean ; // COM port?
      hasSerialBaudrate: boolean ; // Baudrate eingabe?
      hasTelnet: boolean ; // telnet parameter?
      hasMonitorEntryAddress: boolean ; // M9312 hat eine Einsprungadresse
      defaultMonitorEntryAddress: string ;
    public
      consoleType: TConsoleType ;
      connectionType: TSerialIoHubPhysicalConnectionType ;

      // eingestellte Properties, zur Auswertung durch caller
      serialComport: integer ; // 0 = none
      serialBaudrate: integer ;
      serialFormat: TSerialFormat ;
      telnetHostname : string ;
      telnetPort : integer ;

      // manche consolen (M9312) brauchen eine Einsprungadresse zurück in
      // den console emulator.
      // da ist hier nicht zu lösen: die maske zeigt nur 1
      // edit an, der Wert ist aber pro Console verschieden und muss
      // pro Console gemangt werden!
      monitorentryaddress: TMemoryAddress ;

    end { "TYPE TFormSettingsConfiguration = class(TCollectionItem)" } ;

  (*
 quatsch! ist teil von current TFormSettingsConfiguration


TSelectedTargetParams = record
          // mit welcher Art pdp11 soll kommuniziert werden?
    consoleType : TConsoleType ;
    connectionType : TSerialIoHubPhysicalConnectionType ;

    // eingestellte Properties
    comport: integer ; // 0 = none
    baudrate: integer ;
    hostname : string ;
    telnetport : integer ;

    // manche consolen (M9312) brauchen eine Einsprungadresse zurück in
    // den console emulator.
    // da ist hier nicht zu lösen: die maske zeigt nur 1
    // edit an, der Wert ist aber pro Console verschieden und muss
    // pro Console gemangt werden!
    monitorentryaddress: TMemoryAddress ;
  end { "TYPE TSelectedTargetParams = record /" } ;
    *)


  TFormSettings = class(TFormChild)
      ComportComboBox: TComboBox;
      OKButton: TButton;
      ComportLabel: TLabel;
      BaudrateLabel: TLabel;
      BaudrateComboBox: TComboBox;
      Pdp11SelectComboBox: TComboBox;
      Label3: TLabel;
      HostnameLabel: TLabel;
      HostnameEdit: TEdit;
      TelnetPortLabel: TLabel;
      TelnetPortEdit: TEdit;
      MonitorEntryAddressLabel: TLabel;
      MonitorEntryAddressEdit: TEdit;
      ShowFakeConsolesCheckBox: TCheckBox;
      SerialFormatLabel: TLabel;
      SerialFormatComboBox: TComboBox;
      procedure FormCreate(Sender: TObject);
      procedure ComportComboBoxChange(Sender: TObject);
      procedure BaudrateComboBoxChange(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure Pdp11SelectComboBoxChange(Sender: TObject);
      procedure HostnameEditChange(Sender: TObject);
      procedure TelnetPortEditChange(Sender: TObject);
      procedure MonitorEntryAddressEditChange(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure ShowFakeConsolesCheckBoxClick(Sender: TObject);
      procedure SerialFormatComboBoxChange(Sender: TObject);
    private
      { Private-Deklarationen }
      loadingConfigurationEditControlsFromRegistry: boolean ;


      procedure createConfigurations(showFakes: boolean) ;
      procedure UpdateProperties ;

      procedure loadConfigurationEditControlsFromRegistry ;
      procedure saveConfigurationEditControlsToRegistry ;

    public
      { Public-Deklarationen }
      Configurations: TCollection ; // of TFormSettingsConfiguration

//      SelectedTargetParams: TSelectedTargetParams ;

      // as selected by combobox
      function getConfiguration(idx: integer): TFormSettingsConfiguration ;
      function selectedConfiguration: TFormSettingsConfiguration ;


    end{ "TYPE TFormSettings = class(TFormChild)" } ;

var
  FormSettings: TFormSettings;

implementation

{$R *.dfm}

uses
  RegistryU ;

procedure TFormSettings.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    UpdateProperties ;
  end;

procedure TFormSettings.FormCreate(Sender: TObject);
  begin
    loadingConfigurationEditControlsFromRegistry := false ;

    Configurations := TCollection.Create(TFormSettingsConfiguration) ;

    // load early, determines the PDP11 configurations
    TheRegistry.Load(ShowFakeConsolesCheckBox) ;

    // welche PDP-11's sollen auswählbar sein?
    createConfigurations(ShowFakeConsolesCheckBox.Checked) ;

    // load edit controls with values for current Configuration
    loadConfigurationEditControlsFromRegistry ;

    UpdateProperties ;
  end{ "procedure TFormSettings.FormCreate" } ;


procedure TFormSettings.FormDestroy(Sender: TObject);
  begin
    Configurations.Free ;
  end;

// zählt alle Configurationen des Formulars auf,
// Ergebnis in "Configurations
procedure TFormSettings.createConfigurations(showFakes: boolean);
  var
    cfg: TFormSettingsConfiguration ;
    idx: integer ;
  begin
    Configurations.Clear ;
    if showFakes then begin

      // interne primitiv-Simulation M9301 console emulator: nur baudrate anzeigen
      cfg := Configurations.Add as TFormSettingsConfiguration ;
      cfg.comboboxItemText := 'Internal PDP-11 with M9301 fake (self test)' ;
      cfg.registryPrefix := 'PDP11M9301fake' ;
      cfg.consoleType := consoleSelftest11M9301 ;
      cfg.connectionType := connectionInternal ;
      cfg.hasSerialComPort := false ;
      cfg.hasSerialBaudrate := true ;
      cfg.hasTelnet := false ;
      cfg.hasMonitorEntryAddress := false ;
      cfg.defaultMonitorEntryAddress := '' ;

      // interne primitiv-Simulation M9312 console emulator: nur baudrate anzeigen
      cfg := Configurations.Add as TFormSettingsConfiguration ;
      cfg.comboboxItemText := 'Internal PDP-11 with M9312 fake (self test)' ;
      cfg.registryPrefix := 'PDP11M9312fake' ;
      cfg.consoleType := consoleSelftest11M9312 ;
      cfg.connectionType := connectionInternal ;
      cfg.hasSerialComPort := false ;
      cfg.hasSerialBaudrate := true ;
      cfg.hasTelnet := false ;
      cfg.hasMonitorEntryAddress := false ;
      cfg.defaultMonitorEntryAddress := '' ;

      // interne primitiv-Simulation 11/44: nur baudrate anzeigen
      cfg := Configurations.Add as TFormSettingsConfiguration ;
      cfg.comboboxItemText := 'Internal PDP-11/44 fake (self test)' ;
      cfg.registryPrefix := 'PDP1144fake' ;
      cfg.consoleType := consoleSelftest1144 ;
      cfg.connectionType := connectionInternal ;
      cfg.hasSerialComPort := false ;
      cfg.hasSerialBaudrate := true ;
      cfg.hasTelnet := false ;
      cfg.hasMonitorEntryAddress := false ;
      cfg.defaultMonitorEntryAddress := '' ;

      cfg := Configurations.Add as TFormSettingsConfiguration ;
      cfg.comboboxItemText := 'Internal PDP-11/44 console v3.40C fake (self test)' ;
      cfg.registryPrefix := 'PDP1144v340Cfake' ;
      cfg.consoleType := consoleSelftest1144v340c ;
      cfg.connectionType := connectionInternal ;
      cfg.hasSerialComPort := false ;
      cfg.hasSerialBaudrate := true ;
      cfg.hasTelnet := false ;
      cfg.hasMonitorEntryAddress := false ;
      cfg.defaultMonitorEntryAddress := '' ;


      // interne primitiv-Simulation ODT, 16 bit adressen: nur baudrate anzeigen
      cfg := Configurations.Add as TFormSettingsConfiguration ;
      cfg.comboboxItemText := 'Internal PDP-11 ODT 16 bit fake (self test)' ;
      cfg.registryPrefix := 'PDP11ODT16fake' ;
      cfg.consoleType := consoleSelftest11ODT16 ;
      cfg.connectionType := connectionInternal ;
      cfg.hasSerialComPort := false ;
      cfg.hasSerialBaudrate := true ;
      cfg.hasTelnet := false ;
      cfg.hasMonitorEntryAddress := false ;
      cfg.defaultMonitorEntryAddress := '' ;

      // interne primitiv-Simulation ODT, 18 bit adressen: nur baudrate anzeigen
      cfg := Configurations.Add as TFormSettingsConfiguration ;
      cfg.comboboxItemText := 'Internal PDP-11 ODT 18 bit fake (self test)' ;
      cfg.registryPrefix := 'PDP11ODT18fake' ;
      cfg.consoleType := consoleSelftest11ODT18 ;
      cfg.connectionType := connectionInternal ;
      cfg.hasSerialComPort := false ;
      cfg.hasSerialBaudrate := true ;
      cfg.hasTelnet := false ;
      cfg.hasMonitorEntryAddress := false ;
      cfg.defaultMonitorEntryAddress := '' ;

      // interne primitiv-Simulation ODT 22 bit: nur baudrate anzeigen
      cfg := Configurations.Add as TFormSettingsConfiguration ;
      cfg.comboboxItemText := 'Internal PDP-11 ODT 22 bit fake (self test)' ;
      cfg.registryPrefix := 'PDP11ODT22fake' ;
      cfg.consoleType := consoleSelftest11ODT22 ;
      cfg.connectionType := connectionInternal ;
      cfg.hasSerialComPort := false ;
      cfg.hasSerialBaudrate := true ;
      cfg.hasTelnet := false ;
      cfg.hasMonitorEntryAddress := false ;
      cfg.defaultMonitorEntryAddress := '' ;

      // interne primitiv-Simulation : nur baudrate anzeigen
      cfg := Configurations.Add as TFormSettingsConfiguration ;
      cfg.comboboxItemText := 'Internal PDP-11 ODT K1630 fake (self test)' ;
      cfg.registryPrefix := 'PDP11ODTK1630fake' ;
      cfg.consoleType := consoleSelftest11ODTK1630 ;
      cfg.connectionType := connectionInternal ;
      cfg.hasSerialComPort := false ;
      cfg.hasSerialBaudrate := true ;
      cfg.hasTelnet := false ;
      cfg.hasMonitorEntryAddress := false ;
      cfg.defaultMonitorEntryAddress := '' ;

    end{ "if showFakes" } ;

    // echter PDP-11 M9301 console emulator an serial: com und baudrate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 with M9301 over serial port' ;
    cfg.registryPrefix := 'PDP11M9301serial' ;
    cfg.consoleType := consolePDP11M9301 ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    // only M9312 has a monitor entry
    cfg.hasMonitorEntryAddress := true ;
    cfg.defaultMonitorEntryAddress := '0' ; // depends on switch settings, default ? =

    // echter PDP-11 M9312 console emulator an serial: com und baudrate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 with M9312 over serial port' ;
    cfg.registryPrefix := 'PDP11M9312serial' ;
    cfg.consoleType := consolePDP11M9312 ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    // only M9312 has a monitor entry
    cfg.hasMonitorEntryAddress := true ;
    cfg.defaultMonitorEntryAddress := '165020' ; // see M9312 doc

    // echter PDP-11 M9312 console emulator über telnet (mit konverter dazwischen)
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 with M9312 over telnet' ;
    cfg.registryPrefix := 'PDP11M9312telnet' ;
    cfg.consoleType := consolePDP11M9312 ;
    cfg.connectionType := connectionTelnet ;
    cfg.hasSerialComPort := false ;
    cfg.hasSerialBaudrate := false ;
    cfg.hasTelnet := true ;
    cfg.hasMonitorEntryAddress := true ;
    cfg.defaultMonitorEntryAddress := '165020' ; // see M9312 doc

    // echte PDP-11/44 an serial: com und baudrate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11/44 over serial port' ;
    cfg.registryPrefix := 'PDP1144serial' ;
    cfg.consoleType := consolePDP1144 ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // echte PDP-11/44 über telnet (mit konverter dazwischen)
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11/44 over telnet' ;
    cfg.registryPrefix := 'PDP1144telnet' ;
    cfg.consoleType := consolePDP1144 ;
    cfg.connectionType := connectionTelnet ;
    cfg.hasSerialComPort := false ;
    cfg.hasSerialBaudrate := false ;
    cfg.hasTelnet := true ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // 7: echte PDP-11/44 an serial: com und baudrate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11/44 v3.40c over serial port' ;
    cfg.registryPrefix := 'PDP1144v340cserial' ;
    cfg.consoleType := consolePDP1144v340c ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;


    // echte PDP-11 ODT 16 bit an serial: com und baudrate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 ODT 16 bit (LSI11-03) over serial port' ;
    cfg.registryPrefix := 'PDP11ODT16serial' ;
    cfg.consoleType := consolePDP11ODT16 ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // echte PDP-11 ODT 16 bit über telnet (mit konverter dazwischen)
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 ODT 16 bit (LSI11-03) over telnet' ;
    cfg.registryPrefix := 'PDP11ODT16telnet' ;
    cfg.consoleType := consolePDP11ODT16 ;
    cfg.connectionType := connectionTelnet ;
    cfg.hasSerialComPort := false ;
    cfg.hasSerialBaudrate := false ;
    cfg.hasTelnet := true ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // echte PDP-11 ODT 18 bit an serial: com und baudrate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 ODT 18 bit (11/23) over serial port' ;
    cfg.registryPrefix := 'PDP11ODT18serial' ;
    cfg.consoleType := consolePDP11ODT18 ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // echte PDP-11 ODT 18 bit über telnet (mit konverter dazwischen)
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 ODT 18 bit (11/23) over telnet' ;
    cfg.registryPrefix := 'PDP11ODT18telnet' ;
    cfg.consoleType := consolePDP11ODT18 ;
    cfg.connectionType := connectionTelnet ;
    cfg.hasSerialComPort := false ;
    cfg.hasSerialBaudrate := false ;
    cfg.hasTelnet := true ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // echte PDP-11 ODT 22 bit an serial: com und baudrate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 ODT 22 bit (11/53,73,93) over serial port' ;
    cfg.registryPrefix := 'PDP11ODT22serial' ;
    cfg.consoleType := consolePDP11ODT22 ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // echte PDP-11 ODT 22 bit über telnet (mit konverter dazwischen)
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical PDP-11 ODT 22 bit (11/53,73,93) over telnet' ;
    cfg.registryPrefix := 'PDP11ODT22telnet' ;
    cfg.consoleType := consolePDP11ODT22 ;
    cfg.connectionType := connectionTelnet ;
    cfg.hasSerialComPort := false ;
    cfg.hasSerialBaudrate := false ;
    cfg.hasTelnet := true ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // Robotron A6402 CPU K1630 an serial: com und baudrate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Physical Robotron CPU K1630 (ODT 18 bit 11/23) over serial port' ;

    cfg.registryPrefix := 'PDP11ODTK1630serial' ;
    cfg.consoleType := consolePDP11ODTK1630 ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;


    // SimH over serial: port and baud rate
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Modified SimH over serial port' ;
    cfg.registryPrefix := 'SimHserial' ;
    cfg.consoleType := consoleSimH ;
    cfg.connectionType := connectionSerial ;
    cfg.hasSerialComPort := true ;
    cfg.hasSerialBaudrate := true ;
    cfg.hasTelnet := false ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;

    // SimH over telnet: host und telnet port
    cfg := Configurations.Add as TFormSettingsConfiguration ;
    cfg.comboboxItemText := 'Modified SimH over telnet' ;
    cfg.registryPrefix := 'SimHtelnet' ;
    cfg.consoleType := consoleSimH ;
    cfg.connectionType := connectionTelnet ;
    cfg.hasSerialComPort := false ;
    cfg.hasSerialBaudrate := false ;
    cfg.hasTelnet := true ;
    cfg.hasMonitorEntryAddress := false ;
    cfg.defaultMonitorEntryAddress := '' ;


    // build selection combobox items
    Pdp11SelectComboBox.Clear ;
    for idx := 0 to Configurations.Count - 1 do
      Pdp11SelectComboBox.Items.Add(getConfiguration(idx).comboboxItemText) ;

    TheRegistry.Load(Pdp11SelectComboBox) ;
    Pdp11SelectComboBox.DropDownCount := Pdp11SelectComboBox.Items.Count ;
    if Pdp11SelectComboBox.ItemIndex < 0 then Pdp11SelectComboBox.ItemIndex := 0 ;

  end { "procedure TFormSettings.createConfigurations" } ;


// load edit controls with values for current Confuiguration
procedure TFormSettings.loadConfigurationEditControlsFromRegistry ;

  procedure setComboBox(cb: TComboBox; s: string) ;
    var i: integer ;
    begin
      i := cb.Items.IndexOf(s) ;
      if i >= 0 then cb.ItemIndex := i else cb.ItemIndex := -1 ;
      if cb.Style = csDropDown then
        cb.text := s ;
    end ;

  var s: string ;
    sf: TSerialFormat ;
  begin
    loadingConfigurationEditControlsFromRegistry := true ;
    try
      s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.ComportComboBox' ;
      setComboBox(ComportComboBox, TheRegistry.Load(s, 'COM1')) ;
      if ComportComboBox.ItemIndex < 0 then ComportComboBox.ItemIndex := 0 ;
      s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.BaudrateComboBox' ;
      setComboBox(BaudrateComboBox, TheRegistry.Load(s, '9600')) ;
      if BaudrateComboBox.ItemIndex < 0 then BaudrateComboBox.ItemIndex := 0 ;


      SerialFormatComboBox.Items.Clear ;
      // fill in Strings in enumeration order
      for sf := Low(TSerialFormat) to High(TSerialFormat) do
        SerialFormatComboBox.Items.Add(SerialFormatAstext(sf)) ;

      s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.SerialFormatComboBox' ;
      setComboBox(SerialFormatComboBox, TheRegistry.Load(s, '8N1')) ;
      if SerialFormatComboBox.ItemIndex < 0 then SerialFormatComboBox.ItemIndex := 0 ;

      s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.HostnameEdit' ;
      HostnameEdit.text := TheRegistry.Load(s, 'localhost') ;
      s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.TelnetPortEdit' ;
      TelnetPortEdit.text := TheRegistry.Load(s, '23') ;

      s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.MonitorEntryAddressEdit' ;
      MonitorEntryAddressEdit.text := TheRegistry.Load(s,
              selectedConfiguration.defaultMonitorEntryAddress) ;
    finally
      loadingConfigurationEditControlsFromRegistry := false ;
    end { "try" } ;
  end{ "procedure TFormSettings.loadConfigurationEditControlsFromRegistry" } ;


// save editcontrol values for current Configuration
procedure TFormSettings.saveConfigurationEditControlsToRegistry ;
  var s: string ;
  begin
    s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.ComportComboBox' ;
    TheRegistry.Save(s, ComportComboBox.text) ;
    s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.BaudrateComboBox' ;
    TheRegistry.Save(s, BaudrateComboBox.text) ;
    s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.SerialFormatComboBox' ;
    TheRegistry.Save(s, SerialFormatComboBox.text) ;

    s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.HostnameEdit' ;
    TheRegistry.Save(s, HostnameEdit.text) ;
    s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.TelnetPortEdit' ;
    TheRegistry.Save(s, TelnetPortEdit.text) ;

    s := 'FormSettings.' + selectedConfiguration.registryPrefix+'.MonitorEntryAddressEdit' ;
    TheRegistry.Save(s, MonitorEntryAddressEdit.text) ;
  end{ "procedure TFormSettings.saveConfigurationEditControlsToRegistry" } ;


// Einstellungen aus Form-Inhalten erzeugen
// muss ohne Exception immer irgendwelche Ergebnisse liefern!
procedure TFormSettings.UpdateProperties ;

  var
    curTop: integer ; // vertical position of current control ;
    vdist: integer ; // vertical distance of controls

  procedure showhidecontrol(l: TLabel ; c: Tcontrol; isvisible: boolean) ;
    begin
      if isvisible then begin
        curTop := curTop + vdist ;
        // adjust below prev control
        l.Top := curTop + (c.Height - l.Height) div 2 ; // label v-centered before control
        c.Top := curTop ;
        l.visible := true ;
        c.visible := true ;
      end else begin
        l.visible := false ;
        c.visible := false ;
      end;
    end{ "procedure showhidecontrol" } ;

  var
    s: string ;
    idx:  integer ;
  begin { "procedure TFormSettings.UpdateProperties" }

    // 1) Controls je nach Target-auswahl übereinander anzeigen:
    curTop := Pdp11SelectComboBox.Top ;
    vdist := Pdp11SelectComboBox.Height + 3 ; //  3 pix room between lines

    showhidecontrol(ComportLabel, ComportComboBox, selectedConfiguration.hasSerialComPort) ;

    showhidecontrol(BaudrateLabel, BaudrateComboBox, selectedConfiguration.hasSerialBaudrate) ;
    showhidecontrol(SerialFormatLabel, SerialFormatComboBox, selectedConfiguration.hasSerialBaudrate) ;

    showhidecontrol(HostnameLabel, HostnameEdit, selectedConfiguration.hasTelnet) ;
    showhidecontrol(TelnetPortLabel, TelnetPortEdit, selectedConfiguration.hasTelnet) ;

    showhidecontrol(MonitorEntryAddressLabel, MonitorEntryAddressEdit,
            selectedConfiguration.hasMonitorEntryAddress) ;

    // stack OK button below all with double space
    OKButton.Top := curTop + 2 * vdist ;

    // adjust form height
    ClientHeight := OKButton.Top + OKButton.Height + (OKButton.Height div 2) ;


    // 2) Controls auswerten
    selectedConfiguration.serialComport := ComportComboBox.ItemIndex+1 ;
    if selectedConfiguration.serialComport < 0 then selectedConfiguration.serialComport := 1 ;

    if not TryStrToInt(BaudrateComboBox.text, selectedConfiguration.serialBaudrate) then
      selectedConfiguration.serialBaudrate := 9600 ;

    if SerialFormatComboBox.ItemIndex < 0 then
      selectedConfiguration.serialFormat := serformat8N1
    else
      selectedConfiguration.serialFormat := TSerialFormat(SerialFormatComboBox.ItemIndex) ; // only this
// special Test for Rüdiger Kurth
//   selectedConfiguration.serialFormat := serformat87N1 ;

    selectedConfiguration.telnetHostname := HostnameEdit.text ;

    if not TryStrToInt(TelnetPortEdit.text, selectedConfiguration.telnetPort) then
      selectedConfiguration.telnetPort := 23 ;

    // maybe empty?
    if Trim(MonitorEntryAddressEdit.text) = '' then
      s := '0' else
      s := MonitorEntryAddressEdit.text ;

    selectedConfiguration.monitorentryaddress := OctalStr2Addr(s, matPhysical16) ;

(*
    // 2) Controls auswerten, Ergbnis in 'SelectedTargetParams'
    SelectedTargetParams.consoleType := selectedConfiguration.consoleType ;
    SelectedTargetParams.connectionType :=  selectedConfiguration.connectionType ;

    // 0 = none ... ?
    SelectedTargetParams.comport := ComportComboBox.ItemIndex+1 ;
    if SelectedTargetParams.comport < 0 then SelectedTargetParams.comport := 0 ;

    if not TryStrToInt(BaudrateComboBox.text, SelectedTargetParams.baudrate) then
      SelectedTargetParams.baudrate := 9600 ;
    SelectedTargetParams.hostname := HostnameEdit.text ;

    if not TryStrToInt(TelnetPortEdit.text, SelectedTargetParams.telnetport) then
      SelectedTargetParams.telnetport := 23 ;

    // maybe empty?
    SelectedTargetParams.monitorentryaddress :=
            OctalStr2Addr(MonitorEntryAddressEdit.text, matPhysical16) ;
            *)
  end{ "procedure TFormSettings.UpdateProperties" } ;


function TFormSettings.getConfiguration(idx: integer): TFormSettingsConfiguration ;
  begin
    if (idx < 0) or (idx >= Configurations.Count) then
      result := nil
    else    result := Configurations.Items[idx] as TFormSettingsConfiguration ;
  end;

// configuration as selected by combobox
// must always give a valid config
function TFormSettings.selectedConfiguration: TFormSettingsConfiguration ;
  var idx: integer ;
  begin
    idx := Pdp11SelectComboBox.ItemIndex ;
    if (idx < 0) or (idx >= Configurations.Count) then
      raise Exception.Create('TFormSettings.selectedConfiguration(): mismatch between combobox and config-collection!');

    result := getConfiguration(idx) ;
  end;



procedure TFormSettings.ShowFakeConsolesCheckBoxClick(Sender: TObject);
  begin
    if loadingConfigurationEditControlsFromRegistry then Exit ;
    TheRegistry.Save(ShowFakeConsolesCheckBox) ;

    createConfigurations(ShowFakeConsolesCheckBox.Checked) ;

    loadConfigurationEditControlsFromRegistry ;
    UpdateProperties ;
  end;

procedure TFormSettings.Pdp11SelectComboBoxChange(Sender: TObject);
  begin
    loadConfigurationEditControlsFromRegistry ;
    UpdateProperties ;
    TheRegistry.Save(Pdp11SelectComboBox) ;
(*
    if loadingConfigurationEditControlsFromRegistry then Exit ;
//    loadConfigurationEditControlsFromRegistry ;
    UpdateProperties ;
    TheRegistry.Save(Pdp11SelectComboBox) ;
*)
  end;

procedure TFormSettings.ComportComboBoxChange(Sender: TObject);
  begin
    if loadingConfigurationEditControlsFromRegistry then Exit ;
    UpdateProperties ;
    saveConfigurationEditControlsToRegistry ;
  end;

procedure TFormSettings.BaudrateComboBoxChange(Sender: TObject);
  begin
    if loadingConfigurationEditControlsFromRegistry then Exit ;
    UpdateProperties ;
    saveConfigurationEditControlsToRegistry ;
  end;

procedure TFormSettings.SerialFormatComboBoxChange(Sender: TObject);
  begin
    if loadingConfigurationEditControlsFromRegistry then Exit ;
    UpdateProperties ;
    saveConfigurationEditControlsToRegistry ;
  end;


procedure TFormSettings.HostnameEditChange(Sender: TObject);
  begin
    if loadingConfigurationEditControlsFromRegistry then Exit ;
    UpdateProperties ;
    saveConfigurationEditControlsToRegistry ;
  end;

procedure TFormSettings.TelnetPortEditChange(Sender: TObject);
  begin
    if loadingConfigurationEditControlsFromRegistry then Exit ;
    UpdateProperties ;
    saveConfigurationEditControlsToRegistry ;
  end;

procedure TFormSettings.MonitorEntryAddressEditChange(Sender: TObject);
  begin
    if loadingConfigurationEditControlsFromRegistry then Exit ;
    UpdateProperties ;
    saveConfigurationEditControlsToRegistry ;
  end;

end{ "unit FormSettingsU" } .
