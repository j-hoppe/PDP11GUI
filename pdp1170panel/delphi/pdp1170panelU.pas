unit pdp1170panelU;

{
  Pdp1170Panel
  Klasse, die ein Console-Panel für die PDP-11/70 kapselt.
  abgleitet: physische Klasse für USB-angestuerte Hardware
  ingebettet in: Frame für viruelles Console-Panel

  public model
  panel = 1:n Leds: Tpdp1170PanelControl
  panel = 1:n Switches: Tpdp1170PanelControl

  Ansteuerung:
  Jeder Switch/Led hat n States.

  1. Control suchen über Name/Gruppe/Typ

  2. Control setzen oder abfragen mit
     Tpdp1170PanelControl.getActiveState/setActiveStateByValue geändert werden.


  3. Danach muss für das ganze panel
    Tpdp1170Panel.SyncWithImplementor()
    aufgerufen werden: das gleicht den State vom Tpdp1170Panel mit
    der tatsächlichen Implementierung ab.
    Switches werden gelesen, LEDs werden geschrieben.

   Manche Implemantationen (Der Frame) oder später mal die USB-Hardware
   setzen die Switch-Controls on Event.


}

interface 

uses 
  Windows, 
  Classes,
  SysUtils, 
  ExtCtrls ; 

type 
  // Grundtypen von Controls
  Tpdp1170panelControlType = (_Led, _Switch) ; 

  Tpdp1170panelControlGroup = ( 
      // Gruppen von LEDs
      pdp1170Led_ADDRESS_SELECT, // 8 LED, directly driven, see Sw_ADRESS_SELECT
      // 1 Sw, 8 pos: KERNEL-D,SUPER-D,USER-D,
      // KERNEL-I, SUPER-I, USER-I, CONS PHY, PROG PHY
      pdp1170Led_DATA_SELECT, // 4 LED, directly driven, see Sw_DATA_SELECT
      // 1 Sw, 4 pos: DATA PATHS, BUS REG, uADRS FPP/CPU, DISPLAY REGISTER
      pdp1170Led_ADDRESS, // 22 Address LEDs
      pdp1170Led_DATA, // 16 Data LEDs
      pdp1170Led_PARITY, // 2 Leds: 0 = Low, 1 = High
      pdp1170Led_PAR_ERR, // 1 Led
      pdp1170Led_ADRS_ERR, // 1 Led
      pdp1170Led_RUN, // 1 Led
      pdp1170Led_PAUSE, // 1 Led
      pdp1170Led_MASTER, // 1 Led
      pdp1170Led_USER_SUPER_KERNEL, // 3 Led
      pdp1170Led_ADDRESSING_16, // 1 Led
      pdp1170Led_ADDRESSING_18, // 1 Led
      pdp1170Led_ADDRESSING_22, // 1 Led
      pdp1170Led_DATA_SPACE, // 1 Led

      // switches haben als Wert nicht nur 0 oder 1, manche haben auch mehr
      pdp1170Sw_Power, // 1 Sw, 0 = off, 1 = Power, 2 = Lock
      pdp1170Sw_LampTest, // 1 Sw
      pdp1170Sw_LOAD_ADRS, // 1 Sw, momentary action, down
      pdp1170Sw_EXAM, // 1 Sw, momentary action, down
      pdp1170Sw_DEP, // 1 Sw, momentary action, up
      pdp1170Sw_CONT, // 1 Sw, momentary action, down
      pdp1170Sw_ENABLE_HALT, // 1 Sw, bidi
      pdp1170Sw_SINST_SBUS_CYCLE, // 1 Sw, bidi
      pdp1170Sw_START, // 1 Sw, momentary action, down
      pdp1170Sw_ADDRESS_SELECT, // 1 Sw, 8 pos: KERNEL-D,SUPER-D,USER-D,
      // KERNEL-I, SUPER-I, USER-I, CONS PHY, PROG PHY
      pdp1170Sw_DATA_SELECT, // 1 Sw, 4 pos: DATA PATHS, BUS REG, uADRS FPP/CPU, DISPLAY REGISTER
      pdp1170Sw_DATA // 22 Sw
    ) { "TYPE Tpdp1170panelControlGroup" } ; 


const 
  // Werte für Tpdp1170PanelControl.flags
  PDP1170PANEL_FLAG_NONE = 0 ; 
  PDP1170PANEL_FLAG_SWITCH_ACTION = 1 ; // action switch: Taster
  PDP1170PANEL_FLAG_SWITCH_LAMPTEST = 2 ; // ist der Lamptest

  MAX_PDP1170PANEL_LED_COUNT = 100 ; // LEDs
  MAX_PDP1170PANEL_SWITCH_COUNT = 100 ; // Switches und Drehknöpfe
  // die Drehknöpfe haben 8 Zustände, die LEDs und Schalter nur jeweils 2
  MAX_PDP1170PANEL_CONTROL_STATES = 8 ; 

type 
  // Zustand eines LED (ON/OFF) oder Switches(0, 1, ...)
  Tpdp1170PanelControlState = class(TObject) 
    public 
      // index/value: Bei den 11/70 Drehschalter kodiert "index" die Raste,
      // "value ist der Wert für eine Raststellung!
      index: integer ; // Platznummer des States. "Reihenfolge" der States
      value: integer ; // ID, wie von Schaltung geliefert: 0, 1, 2, ...
      name: string ; // Beschreibung für den Zustand
      image: TImage ; // wenn Form: Bild für den Zustand

      constructor Create(_value: integer ; _name: string) ; 
    end ; 


  Tpdp1170Panel = class ; 

// Ein "Control" ist entweder eine LED, ein Switch oder ein Drehschalter.
// die aktuelle Schalterstellung bzw LED-Zustand wird in "active_state" gespeichert.
  Tpdp1170PanelControl = class(TObject) 

    public 
      ctltype: Tpdp1170panelControlType ; // Led oder Switch?
      ctltype_str: string ; // Textdarstellung des tyops: "LED", "Switch"

      index: integer ; // ID: nummer in gruppe

      panel: Tpdp1170Panel ; // uplink

      group: Tpdp1170panelControlGroup ; // ID: gruppe
      name: string ; // Beschreibung
      // Verhaltens-flags. 1 für Switches heisst: switch ist nicht bistabil,
      // switcht automatisch auf 0 zurück
      flags: integer ; 
      StateCount: integer ; // wieviel Zustände hat dieses Control ?
      // LEDs eigentlich nur zwei, die beiden Drehknöpfe haben aber 8!
      States: array [0..MAX_PDP1170PANEL_CONTROL_STATES] of Tpdp1170PanelControlState ; 

      active_state_index: integer  ; // aktuell aktiver Zustand

      implementor_active_state_index: integer  ; // wie vom implementor durch Sync erzeugt

      // nach UpdateEvents ist   active_state = implementor_active_state

      // wenn physikalisches Panel über Ansteuerungsplatine:
      register_nr: integer ; // Nummer des registers
      register_mask: dword ; // bitmask für die sate.values des registers
      register_lsb: dword ;  // abgeleitet: Nr des Bits, das LSB des State.value darstellt
      // Bsp: lsb = 3 -> value 1 -> maske 0x0008, value 5 -> maske 0x0028
      // Die Länge des belegten Bitblocks ergibt sich aus StateCount

      constructor Create( 
              _ctltype: Tpdp1170panelControlType ; 
              _panel: Tpdp1170Panel ; 
              _group: Tpdp1170panelControlGroup ; 
              _index: integer ;
              _flags: integer ; 
              _name: string) ; 
      destructor Destroy ; override ; 

      // zum registrieren der States
      function AddState(_state: Tpdp1170PanelControlState): Tpdp1170PanelControlState ; 

      // einen State über seine Zahl finden
      function getStateByValue(_value: integer): Tpdp1170PanelControlState ; 
      // einen State über seinen Namen finden
      function getStateByName(_name: string): Tpdp1170PanelControlState ; 

      function getActiveState: Tpdp1170PanelControlState ; virtual ; 
      procedure setActiveStateByValue(_value: integer) ; virtual ; 

    end{ "TYPE Tpdp1170PanelControl = class(TObject)" } ; 


// Basisklasse für das simulierte und das physikalische Panel
  Tpdp1170Panel = class(TObject) 
    private 
      function Add(ctrl: Tpdp1170PanelControl): Tpdp1170PanelControl ; 

    public 
      // Verzeichnis der LEDs und Switches
      ControlCount: integer ; 
      Controls: array [0..MAX_PDP1170PANEL_LED_COUNT+MAX_PDP1170PANEL_SWITCH_COUNT] of Tpdp1170PanelControl ; 

      lamptest_active: boolean ; // TRUE, wenn Lamptest active ist

      // Das hier muss der Implementor (Frame oder USB-hardwarew) auf seine Update-logik lenken
      OnSyncSwitchesFromImplementor: TNotifyEvent ; // Sender: geändertes Control
      OnSyncLedsToImplementor: TNotifyEvent ; // Sender: geändertes Control

      // Damit kriegt die Anwendung änderungen mit.
      OnControlStateChanged: TNotifyEvent ;// Sender: geändertes Control

    public 
      constructor Create ; 
      destructor Destroy ; override ; 

      function getControlByID(_ctltype: Tpdp1170panelControlType ; 
              _group: Tpdp1170panelControlGroup ; _index: integer): Tpdp1170PanelControl ; 
      // eine Switch/LED über group und name finden
      function getControlByName(_ctltype: Tpdp1170panelControlType ; 
              _group: Tpdp1170panelControlGroup ; _name: string): Tpdp1170PanelControl ; 
      // eine Control über eines ihrer State-Images finden
      // keine angabe, ob siwthc oder led gewünscht ist ..
      // das sollte sich wirklich aus dem image ergeben!
      //
      function getControlByImage(_image: TImage): Tpdp1170PanelControl ; 

      // wird von Anwendung aufgerufen, befor sie Switches auswerten will,
      // bzw nachdem sie LED-Controls gesetzt hat.
      procedure SyncWithImplementor ; 

    end{ "TYPE Tpdp1170Panel = class(TObject)" } ; 



implementation 


constructor Tpdp1170PanelControlState.Create(_value: integer ; _name: string) ; 
  begin 
    inherited Create ; 
    self.value := _value ; 
    self.name := _name ; 
    self.image := nil ; 
  end;


// Definition eines Controls erzeugen
constructor Tpdp1170PanelControl.Create( 
        _ctltype: Tpdp1170panelControlType ; 
        _panel: Tpdp1170Panel ; 
        _group: Tpdp1170panelControlGroup ; 
        _index: integer ; 
        _flags: integer ; 
        _name: string) ; 
  begin 
    inherited Create ; 
    self.ctltype := _ctltype ;
    case _ctltype of
      _Led: ctltype_str := 'LED' ;
      _Switch: ctltype_str := 'Switch' ;
    end;

    self.panel := _panel ; // uplink
    self.group := _group ; 
    self.index := _index ; 
    self.flags := _flags ; 
    self.name := _name ; 
    self.StateCount := 0 ; 
    self.active_state_index := 0 ; 
    self.implementor_active_state_index := 0 ; 
  end ; 


destructor Tpdp1170PanelControl.Destroy ; 
  var i: integer ; 
  begin 
    for i := 0 to StateCount-1 do 
      States[i].Free ; 
    inherited ; 
  end ;


function Tpdp1170PanelControl.AddState(_state: Tpdp1170PanelControlState): 
        Tpdp1170PanelControlState ; 
  begin 
    States[StateCount] := _state ;
    _state.index := StateCount ; // Platznummer in Liste
    inc(StateCount) ;
    result := _state ;
  end;


// einen State über seine Zahl finden
function Tpdp1170PanelControl.getStateByValue(_value: integer): Tpdp1170PanelControlState ;
  var i: integer ;
  begin
    result := nil ;
    for i := 0 to StateCount-1 do
      if States[i].value = _value then
        result := States[i] ;
  end ;


// einen State über seinen Namen finden
// gibt den ersten State mit diesem Namen zurück
function Tpdp1170PanelControl.getStateByName(_name: string): Tpdp1170PanelControlState ;
  var i: integer ;
  begin
    _name := Uppercase(_name) ;
    result := nil ;
    for i := 0 to StateCount-1 do
      if Uppercase(States[i].name) = _name then begin
        result := States[i] ;
        Exit ;
      end;
  end ;


function Tpdp1170PanelControl.getActiveState: Tpdp1170PanelControlState ; 
  begin 
    result := States[active_state_index] ; 
  end; 


// die Anwendung ändert erstmal "implementor_active_state"
// erst beim "SyncWithImplementor" wird wirksam, und
// dann werden auch die Chnge-Events ausgelöst
procedure Tpdp1170PanelControl.setActiveStateByValue(_value: integer) ; 
  var state: Tpdp1170PanelControlState ; 
    i: integer ; 
  begin 
    // ist der State für das Control überhaupt möglich?
    state := getStateByValue(_value) ; 
    if state = nil then 
      raise Exception.CreateFmt('state with value %d not defined für control "%s"', [_value, name]) ; 
    for i := 0 to StateCount-1 do // grmbl ... noch mal suchen, damit der Index bekannt wird.
      if (States[i] = state) and (implementor_active_state_index <> i) then begin 
        implementor_active_state_index := i ; 
        Exit ; 
      end; 
  end{ "procedure Tpdp1170PanelControl.setActiveStateByValue" } ; 


// ein Control (Switch/Led) über group und index finden
// result NULL: not found
function Tpdp1170Panel.getControlByID(_ctltype: Tpdp1170panelControlType ;
        _group: Tpdp1170panelControlGroup  ;
        _index: integer)
        : Tpdp1170PanelControl ;
  var i: integer ;
  begin
    result := nil ;
    for i := 0 to ControlCount - 1 do
      if (Controls[i].ctltype = _ctltype) and (Controls[i].group = _group)
              and (Controls[i].index = _index) then begin 
        result := Controls[i] ; 
        Exit ; 
      end; 
  end;


// ein Control (Switch/Led) über group und name finden
// result NULL: not found
function Tpdp1170Panel.getControlByName(_ctltype: Tpdp1170panelControlType ;
        _group : Tpdp1170panelControlGroup ;
        _name: string):
        Tpdp1170PanelControl ;
  var i: integer ;
  begin
    _name := Uppercase(_name) ;
    result := nil ;
    for i := 0 to ControlCount - 1 do
      if (Controls[i].ctltype = _ctltype) and (Controls[i].group = _group)
              and (Uppercase(Controls[i].name) = _name) then begin
        result := Controls[i] ;
        Exit ;
      end;
  end;


// ein Control (Switch/Led) über das image finden
// durchsucht alle States des Control.
// result NULL: not found
function Tpdp1170Panel.getControlByImage(_image: TImage): Tpdp1170PanelControl ; 
  var i, j: integer ; 
    ctrl: Tpdp1170PanelControl ; 
  begin 
    result := nil ; 
    for i := 0 to ControlCount - 1 do begin 
      ctrl := Controls[i] ; 
      for j := 0 to ctrl.StateCount-1 do 
        if ctrl.States[j].image = _image then begin 
          result := ctrl ; 
          Exit ; 
        end; 
    end; 
  end{ "function Tpdp1170Panel.getControlByImage" } ; 


function Tpdp1170Panel.Add(ctrl: Tpdp1170PanelControl): Tpdp1170PanelControl ;
  begin
    Controls[ControlCount] := ctrl ;
    inc(ControlCount) ;
    result := ctrl ;
  end ;


constructor Tpdp1170Panel.Create ;
  var 
    ctrl: Tpdp1170PanelControl ; 
    i: integer ; 
  begin 
    inherited Create ; 

    lamptest_active := false ; 
    OnSyncSwitchesFromImplementor := nil ; 
    OnSyncLedsToImplementor := nil ; 

    // die Datenstruktur für die LEDs und die Switches anlegen
    ControlCount := 0 ; 
    // LEDs haben immer nur zwei states

    // index 0..7 muss auf Drehschalterstellung passen:
    // index der LED wie index der Switch-Positionen
    // die DEC doku ist anders, ich habe gemessen:
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS_SELECT, 0, PDP1170PANEL_FLAG_NONE, 'PROG PHY')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS_SELECT, 1, PDP1170PANEL_FLAG_NONE, 'CONS PHY')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS_SELECT, 2, PDP1170PANEL_FLAG_NONE, 'KERNEL-D')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS_SELECT, 3, PDP1170PANEL_FLAG_NONE, 'SUPER-D')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS_SELECT, 4, PDP1170PANEL_FLAG_NONE, 'USER-D')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS_SELECT, 5, PDP1170PANEL_FLAG_NONE, 'USER-I')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS_SELECT, 6, PDP1170PANEL_FLAG_NONE, 'SUPER-I')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS_SELECT, 7, PDP1170PANEL_FLAG_NONE, 'KERNEL-I')) ; 
    // index wie index der switch-Positionen
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA_SELECT, 0, PDP1170PANEL_FLAG_NONE, 'BUS REG')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA_SELECT, 1, PDP1170PANEL_FLAG_NONE, 'DATA PATHS')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA_SELECT, 2, PDP1170PANEL_FLAG_NONE, 'µADRS FPP/CPU')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA_SELECT, 3, PDP1170PANEL_FLAG_NONE, 'DISPLAY REGISTER')) ; 

    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 0, PDP1170PANEL_FLAG_NONE, 'ADDRESS 0')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 1, PDP1170PANEL_FLAG_NONE, 'ADDRESS 1')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 2, PDP1170PANEL_FLAG_NONE, 'ADDRESS 2')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 3, PDP1170PANEL_FLAG_NONE, 'ADDRESS 3')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 4, PDP1170PANEL_FLAG_NONE, 'ADDRESS 4')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 5, PDP1170PANEL_FLAG_NONE, 'ADDRESS 5')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 6, PDP1170PANEL_FLAG_NONE, 'ADDRESS 6')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 7, PDP1170PANEL_FLAG_NONE, 'ADDRESS 7')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 8, PDP1170PANEL_FLAG_NONE, 'ADDRESS 8')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 9, PDP1170PANEL_FLAG_NONE, 'ADDRESS 9')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 10, PDP1170PANEL_FLAG_NONE, 'ADDRESS 10')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 11, PDP1170PANEL_FLAG_NONE, 'ADDRESS 11')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 12, PDP1170PANEL_FLAG_NONE, 'ADDRESS 12')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 13, PDP1170PANEL_FLAG_NONE, 'ADDRESS 13')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 14, PDP1170PANEL_FLAG_NONE, 'ADDRESS 14')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 15, PDP1170PANEL_FLAG_NONE, 'ADDRESS 15')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 16, PDP1170PANEL_FLAG_NONE, 'ADDRESS 16')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 17, PDP1170PANEL_FLAG_NONE, 'ADDRESS 17')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 18, PDP1170PANEL_FLAG_NONE, 'ADDRESS 18')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 19, PDP1170PANEL_FLAG_NONE, 'ADDRESS 19')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 20, PDP1170PANEL_FLAG_NONE, 'ADDRESS 20')) ;
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESS, 21, PDP1170PANEL_FLAG_NONE, 'ADDRESS 21')) ; 

    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 0, PDP1170PANEL_FLAG_NONE, 'DATA 0')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 1, PDP1170PANEL_FLAG_NONE, 'DATA 1')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 2, PDP1170PANEL_FLAG_NONE, 'DATA 2')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 3, PDP1170PANEL_FLAG_NONE, 'DATA 3')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 4, PDP1170PANEL_FLAG_NONE, 'DATA 4')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 5, PDP1170PANEL_FLAG_NONE, 'DATA 5')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 6, PDP1170PANEL_FLAG_NONE, 'DATA 6')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 7, PDP1170PANEL_FLAG_NONE, 'DATA 7')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 8, PDP1170PANEL_FLAG_NONE, 'DATA 8')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 9, PDP1170PANEL_FLAG_NONE, 'DATA 9')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 10, PDP1170PANEL_FLAG_NONE, 'DATA 10')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 11, PDP1170PANEL_FLAG_NONE, 'DATA 11')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 12, PDP1170PANEL_FLAG_NONE, 'DATA 12')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 13, PDP1170PANEL_FLAG_NONE, 'DATA 13')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 14, PDP1170PANEL_FLAG_NONE, 'DATA 14')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA, 15, PDP1170PANEL_FLAG_NONE, 'DATA 15')) ; 

    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_PARITY, 0, PDP1170PANEL_FLAG_NONE, 'PARITY LOW')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_PARITY, 1, PDP1170PANEL_FLAG_NONE, 'PARITY HIGH')) ;

    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_PAR_ERR, 0, PDP1170PANEL_FLAG_NONE, 'PAR ERR')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADRS_ERR, 0, PDP1170PANEL_FLAG_NONE, 'ADRS ERR')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_RUN, 0, PDP1170PANEL_FLAG_NONE, 'RUN')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_PAUSE, 0, PDP1170PANEL_FLAG_NONE, 'PAUSE')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_MASTER, 0, PDP1170PANEL_FLAG_NONE, 'MASTER')) ; 

    // Ausnahme: USER/SUPER/KERNEL ist 1 Control mit 4 Zuständen
    // 00000000=Kernel, 00000100 = OFF, 00000200 = SUPER 00000300=USER
    // index-reihenfolge wie auf display
    ctrl := Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_USER_SUPER_KERNEL, 0, PDP1170PANEL_FLAG_NONE, 'USER')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'OFF')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(3, 'USER')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(2, 'SUPER')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'KERNEL')) ; 

    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESSING_16, 0, PDP1170PANEL_FLAG_NONE, 'ADDRESSING 16')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESSING_18, 0, PDP1170PANEL_FLAG_NONE, 'ADDRESSING 18')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_ADDRESSING_22, 0, PDP1170PANEL_FLAG_NONE, 'ADDRESSING 22')) ; 
    Add(Tpdp1170PanelControl.Create(_Led, self, pdp1170Led_DATA_SPACE, 0, PDP1170PANEL_FLAG_NONE, 'DATA')) ; 

    // Die meisten LEDS haben nur den Zustand OFF/ON
    for i := 0 to ControlCount-1 do begin 
      ctrl := Controls[i] ; 
      if (ctrl.ctltype = _Led) and (ctrl.StateCount = 0) then begin 
        ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ; 
        ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'ON')) ; 
      end ; 
    end; 

    // Die meisten Switches haben nur zwei states, die Drehknöpfe haben 8
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_Power, 0, {flags=}0, 'Power')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'POWER')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(2, 'LOCK')) ; 

    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_LampTest, 0,
            PDP1170PANEL_FLAG_SWITCH_ACTION or PDP1170PANEL_FLAG_SWITCH_LAMPTEST, 
            'Lamp test')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'LAMP TEST')) ; 
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_LOAD_ADRS, 0, PDP1170PANEL_FLAG_SWITCH_ACTION, 'LOAD ADRS')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'LOAD ADRS')) ; 
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_EXAM, 0, PDP1170PANEL_FLAG_SWITCH_ACTION, 'EXAM')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'EXAM')) ; 
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DEP, 0, PDP1170PANEL_FLAG_SWITCH_ACTION, 'DEP')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'DEP')) ; 
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_CONT, 0, PDP1170PANEL_FLAG_SWITCH_ACTION, 'CONT')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'CONT')) ; 
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_ENABLE_HALT, 0, PDP1170PANEL_FLAG_NONE, 'ENABLE/HALT')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'ENABLE')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'HALT')) ; 
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_SINST_SBUS_CYCLE, 0, PDP1170PANEL_FLAG_NONE, 'S INST/S BUS CYCLE')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'S INST')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'S BUS CYCLE')) ; 
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_START, 0, PDP1170PANEL_FLAG_SWITCH_ACTION, 'START')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'START')) ; 
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_ADDRESS_SELECT, 0, PDP1170PANEL_FLAG_NONE, 'ADDR SELECT')) ; 
    // DEC-doku ist anders, ich habe gemessen:
    // Reihenfolge nicht wie "values", sondern wie Rotationsreihenfolge!
    // damit der frame sie in der richtigen Reihenfolge rotieren kann
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'PROG PHY')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(5, 'CONS PHY')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'KERNEL-D')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(2, 'SUPER-D')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(6, 'USER-D')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(7, 'USER-I')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(3, 'SUPER-I')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(4, 'KERNEL-I')) ;

    // der DATA-SELECT Switch hat 8 Stellungen, die 4 States codieren
    // Reihenfolge nicht wie "values", sondern wie Rotationsreihenfolge!
    // damit der frame sie in der richtigen Reihenfolge rotieren kann
    ctrl := Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA_SELECT, 0, PDP1170PANEL_FLAG_NONE, 'DATA SELECT')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'BUS REG')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'DATA PATHS')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(3, 'µADRS FPP/CPU')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(2, 'DISPLAY REGISTER')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(0, 'BUS REG')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(1, 'DATA PATHS')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(3, 'µADRS FPP/CPU')) ; 
    ctrl.AddState(Tpdp1170PanelControlState.Create(2, 'DISPLAY REGISTER')) ; 

    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 0, PDP1170PANEL_FLAG_NONE, 'DATA 0')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 1, PDP1170PANEL_FLAG_NONE, 'DATA 1')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 2, PDP1170PANEL_FLAG_NONE, 'DATA 2')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 3, PDP1170PANEL_FLAG_NONE, 'DATA 3')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 4, PDP1170PANEL_FLAG_NONE, 'DATA 4')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 5, PDP1170PANEL_FLAG_NONE, 'DATA 5')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 6, PDP1170PANEL_FLAG_NONE, 'DATA 6')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 7, PDP1170PANEL_FLAG_NONE, 'DATA 7')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 8, PDP1170PANEL_FLAG_NONE, 'DATA 8')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 9, PDP1170PANEL_FLAG_NONE, 'DATA 9')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 10, PDP1170PANEL_FLAG_NONE, 'DATA 10')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 11, PDP1170PANEL_FLAG_NONE, 'DATA 11')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 12, PDP1170PANEL_FLAG_NONE, 'DATA 12')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 13, PDP1170PANEL_FLAG_NONE, 'DATA 13')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 14, PDP1170PANEL_FLAG_NONE, 'DATA 14')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 15, PDP1170PANEL_FLAG_NONE, 'DATA 15')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 16, PDP1170PANEL_FLAG_NONE, 'DATA 16')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 17, PDP1170PANEL_FLAG_NONE, 'DATA 17')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 18, PDP1170PANEL_FLAG_NONE, 'DATA 18')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 19, PDP1170PANEL_FLAG_NONE, 'DATA 19')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 20, PDP1170PANEL_FLAG_NONE, 'DATA 20')) ; 
    Add(Tpdp1170PanelControl.Create(_Switch, self, pdp1170Sw_DATA, 21, PDP1170PANEL_FLAG_NONE, 'DATA 21')) ; 

    // alle Data-Switches können nur ON/OFF sein
    for i := 0 to ControlCount-1 do
      if Controls[i].group = pdp1170Sw_DATA then begin
        Controls[i].AddState(Tpdp1170PanelControlState.Create(0, 'OFF')) ;
        Controls[i].AddState(Tpdp1170PanelControlState.Create(1, 'ON')) ;
      end ;
  end { "constructor Tpdp1170Panel.Create" } ;


destructor Tpdp1170Panel.Destroy ;
  var i: integer ;
  begin
    for i := 0 to ControlCount-1 do
      Controls[i].Free ;
    inherited ;
  end ;


// wird von Anwendung aufgerufen, befor sie Switches auswerten will,
// bzw nachdem sie LED-Controls gesetzt hat.
procedure Tpdp1170Panel.SyncWithImplementor ; 
  var i: integer ; 
    ctrl: Tpdp1170PanelControl ; 
    switch_index: integer ; 
  begin 
    // der Implementor hat diese Event auf sich gelenkt.
    if not assigned(OnSyncSwitchesFromImplementor) then 
      raise Exception.Create('OnSyncSwitchesFromImplementor() not implemented!?') ; 
    if not assigned(OnSyncLedsToImplementor) then 
      raise Exception.Create('OnSyncLedsToImplementor() not implemented!?') ; 

    // Switches lesen
    OnSyncSwitchesFromImplementor(self) ; 

    //// LEDS in pdp1170panel updaten, die hardwaremässig durch Drehschalter definiert werden

    // ADRESS_SELECT
    ctrl := getControlByID(_Switch, pdp1170Sw_ADDRESS_SELECT, 0) ; assert(ctrl <> nil) ;
    switch_index := ctrl.implementor_active_state_index ; 
    // alle leds der gruppe pdp1170Led_ADDRESS_SELECT löschen, nur die eine setzen
    // leds über gleichen "index" finden!
    for i := 0 to ControlCount - 1 do begin
      ctrl := Controls[i] ; // ctrl ist jetzt die LED!
      if ctrl.group = pdp1170Led_ADDRESS_SELECT then
        if ctrl.index = switch_index then // Led[i] = Drehschalterstellung
          ctrl.implementor_active_state_index := 1 //LED ON
        else
          ctrl.implementor_active_state_index := 0 //LED OFF
    end;
    // nochmal, für DATA_SELECT.
    ctrl := getControlByID(_Switch, pdp1170Sw_DATA_SELECT, 0) ; assert(ctrl <> nil) ;
    switch_index := ctrl.implementor_active_state_index ;
    // index 0..7 auf led 0..3 abbilden
    switch_index := switch_index mod 4 ;
    // alle leds der gruppe pdp1170Led_DATA_SELECT löschen, nur die eine setzen
    // leds über gleichen "index" finden!
    for i := 0 to ControlCount - 1 do begin 
      ctrl := Controls[i] ; // ctrl ist jetzt die LED!
      if ctrl.group = pdp1170Led_DATA_SELECT then 
        if ctrl.index = switch_index then // Led[i] = Drehschalterstellung
          ctrl.implementor_active_state_index := 1 //LED ON
        else 
          ctrl.implementor_active_state_index := 0 //LED OFF
    end; 

    // LEDs setzen
    OnSyncLedsToImplementor(self) ; 

    // Löse die Change-Events aus
    for i := 0 to ControlCount - 1 do begin 
      ctrl := Controls[i] ; 
      if ctrl.active_state_index <> ctrl.implementor_active_state_index then begin 
        ctrl.active_state_index := ctrl.implementor_active_state_index ; 
        if assigned(OnControlStateChanged) then 
          OnControlStateChanged(ctrl) ; 
      end ; 
    end; 

  end{ "procedure Tpdp1170Panel.SyncWithImplementor" } ; 


end{ "unit pdp1170panelU" } . 
