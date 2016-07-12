unit FormPdp1170PanelU; 
{
  Benutzt das Console-Panel einer 11/70, um FormExecute
  zu steuert.
  Wenn ein echtes Panel (USB) eingesteckt ist, wird
  dynamisch darauf umgeschaltet. Das  on-screen-Panel
  zeugt dann nur noch den Zustand des physiaklsichen Panels an.
  Ist kein echtes Panel angeschlossen, kann das on-screen-Panelö benutzt werden.
}

interface 

uses 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, 
  FormChildU, 
  MemoryCellU, 
  AddressU, 
  FormExecuteU, 
  pdp1170PanelU, 
  pdp1170panelImplementorFrameU, 
  pdp1170panelImplementorPhysicalU ; 

type 
  TFormPdp1170Panel = class(TFormChild) 
      pdp1170panelImplementorFrame1: Tpdp1170PanelImplementorFrame; 
      Timer1: TTimer; 
      procedure FormShow(Sender: TObject); 
      procedure Timer1Timer(Sender: TObject); 
      procedure FormCreate(Sender: TObject); 
      procedure FormDestroy(Sender: TObject); 
    private 
      { Private-Deklarationen }

      // der zuletzt aktivierte Switch.
      // für Entscheiung, wann eine Adresse auto inc kriegt.
      // true für aufeinanderfolgende EXAM/DEPOSITs, reset durch LOAD ADDR
      LastAddrIncCtrl: Tpdp1170PanelControl ; 

      // die USB-Logik
      pdp1170panelImplementorPhysical: Tpdp1170PanelImplementorPhysical; 

      TheScreenPanel: Tpdp1170Panel ; // die Simulation
      ThePhysicalPanel: Tpdp1170Panel ; // das echte panel, über USB

      procedure ThePanelSwitchChanged(Sender: TObject) ; 

      procedure TheScreenPanelSwitchOrLEDChanged(Sender: TObject) ; 
      procedure ThePhysicalPanelSwitchChanged(Sender: TObject) ; 

      procedure UpdateDisplay ; 
//      procedure showAddress(address: TMemoryAddress) ;
      function getDataSwitches: dword ; 
//      procedure showDataAndAddrError(val: dword) ;

      function getNextAddress(oldaddr: TMemoryAddress): TMemoryAddress ; 

      // wenn von aussen eine MemoryCell geändert wird.
      // nur für Abfangen von Write auf DisplayRegsiter interessant
      procedure OnMemoryCellChange(Sender: TObject; memorycell: TMemoryCell) ; 


    public 
      { Public-Deklarationen }
      memorycellgroup: TMemoryCellgroup ; 
      TheMemorycell: TMemoryCell ; // die eine memorycell, über die Deposit/Examine läuft


      // Panel, das PDP11GUI tatsächlich steuert:
      // ThePhysicalPanel, wenn eingesteckt, sonst TheScreenPanel
      ThePanel: Tpdp1170Panel ; 

      DisplayRegisterMemoryCell: TMemoryCell ; // wird autoamtisch aktualisiert,
      // wenn jemand in PDP11GUI addr 1777570 = DisplayRegister setzt.
      // wird auch nach erkanntem Halt aus der CPU ausgelesen,
      // aber nur SimH kann es liefern

//      Execution_laststate: TExecuteState ; // letzter Run/Halt-State der FormExecute

      Haltswitch_active: boolean ; // letzte Stellung des HALT/ENABLE Switches

      // Regsiter des Panels:
      AddressRegisterVal: TMemoryAddress ; 
      DataRegisterVal: dword ; // MEMORYCELL_ILLEGAL, wenn ungütlige adresse
      DisplayRegisterVal: dword ; // wird bei Änderung von DisplayRegisterMemoryCell
      // und bei HALT gesetzt.

      AddressSelectState: Tpdp1170PanelControlState ; // Stand des ADDRESS SELECT Drehknopfes
      DataSelectState: Tpdp1170PanelControlState ; // Stand des DATA SELECT Drehknopfes


      procedure ConnectToMemoryCells(mcg:TMemoryCellgroup) ; 

      // wird von ausserhalb aufgerufen, wenn die Codeausführung startet
      procedure OnCpuStart(Sender: TObject) ; 
      // wird von ausserhalb aufgerufen, wenn die Codeausführung stoppt
      procedure OnCpuHalt(Sender: TObject) ; 
    end{ "TYPE TFormPdp1170Panel = class(TFormChild)" } ; 


implementation 

uses 
  AuxU, 
  OctalConst, 
  FormMainU ; 

{$R *.dfm}


procedure TFormPdp1170Panel.FormCreate(Sender: TObject); 
  var 
    ctrl: Tpdp1170PanelControl ; 
    state: Tpdp1170PanelControlState ; 
  begin 
    pdp1170panelImplementorPhysical := Tpdp1170PanelImplementorPhysical.Create ; 

    TheScreenPanel := pdp1170panelImplementorFrame1.pdp1170panel ; 
    ThePhysicalPanel := pdp1170panelImplementorPhysical.pdp1170panel ; 

    // Event-Handler für Änderungsereignisse
    TheScreenPanel.OnControlStateChanged := TheScreenPanelSwitchOrLEDChanged ; 
    ThePhysicalPanel.OnControlStateChanged := ThePhysicalPanelSwitchChanged ; 

    // Eingabe erstmal auf die Simulation lenken.
    ThePanel := TheScreenPanel ; 
    // Der Timer entdeckt dann, ob das physical panel angeschlossen ist


    // Drehschalter auf Standardeinstellungen. Falls physikal Panel,
    //wird das sofort überschrieben. Bei simuliertem Panel bleibt es gültig
    ctrl := ThePanel.getControlByID(_Switch, pdp1170Sw_ADDRESS_SELECT, 0) ; 
    assert(ctrl <> nil) ; 
    state := ctrl.getStateByName('CONS PHY') ; 
    assert(state <> nil) ; 
    ctrl.setActiveStateByValue(state.value); 

    ctrl := ThePanel.getControlByID(_Switch, pdp1170Sw_DATA_SELECT, 0) ; 
    assert(ctrl <> nil) ; 
    state := ctrl.getStateByName('DATA PATHS') ; 
    assert(state <> nil) ; 
    ctrl.setActiveStateByValue(state.value); 

    // anfangs adresse ist 0 ;
    AddressRegisterVal.mat := matPhysical22 ; 
    AddressRegisterVal.val := 0 ; 
    DataRegisterVal := 0 ; 
    DisplayRegisterVal := 0 ; 

    LastAddrIncCtrl := nil ; 

    TheMemorycell := nil ; 
    DisplayRegisterMemoryCell := nil ; 

    AddressSelectState := nil ; 
    DataSelectState := nil ; 

    Haltswitch_active := false ; 
//    Execution_laststate := esUnknown ;
  end{ "procedure TFormPdp1170Panel.FormCreate" } ; 


procedure TFormPdp1170Panel.FormDestroy(Sender: TObject); 
  begin 
    pdp1170panelImplementorPhysical.Free ; 
  end; 


procedure TFormPdp1170Panel.FormShow(Sender: TObject); 
  begin 
    // den Frame ganz anzeigen
    ClientHeight := pdp1170panelImplementorFrame1.Height ; 
    ClientWidth := pdp1170panelImplementorFrame1.Width ; 
    pdp1170panelImplementorFrame1.Top := 0 ; 
    pdp1170panelImplementorFrame1.Left := 0 ; 

    // Frame ist sichtbar, aber richtig wichtig ist sein pdp11panel!
    // Frame ist nur der "Implementor"!

    ThePanel.SyncWithImplementor ; 

  end{ "procedure TFormPdp1170Panel.FormShow" } ; 


procedure TFormPdp1170Panel.Timer1Timer(Sender: TObject); 
  var 
    i: integer ; 
    ctrl: Tpdp1170PanelControl ; 
    //curpcval: TMemoryAddress  ;
  begin 
    if ThePanel = nil then Exit ; // noch zu früh!
    if not Visible then Exit ; // disabled?


    // Check, ob ein echtes Panel angeschlossen ist
    // wenn ja: ScreenPanel auf Kopie des echten Panels schalten
    ThePhysicalPanel.SyncWithImplementor ; 

    if pdp1170panelImplementorPhysical.Connected then begin 
      for i:= 0 to ThePhysicalPanel.ControlCount - 1 do begin 
        ctrl := ThePhysicalPanel.Controls[i] ; 
//        if ctrl.ctltype = _Switch then
        // Das entsprechende Switch-Control im Frame anpassen
        // Lamptest wird aber nie vom physical panel geliefert
        if ctrl.group <> pdp1170Sw_LampTest then 
          TheScreenPanel.Controls[i].setActiveStateByValue(ctrl.getActiveState.value); 
      end; 
      TheScreenPanel.SyncWithImplementor ; 

      ThePanel := ThePhysicalPanel ; 
      Caption := setFormCaptionInfoField(Caption, 'physical panel is CONNECTED') ; 
    end { "if pdp1170panelImplementorPhysical.Connected" } else begin 
      ThePanel := TheScreenPanel ; 
      Caption := setFormCaptionInfoField(Caption, 'physical panel is NOT CONNECTED') ; 
    end; 


(* NEIN: Synvhron in FormExecute, nicht hier ASYNCHRON
    // erkenne, wenn Program stoppt (HALT instruction oder action
    // dann execution address auf die ADDR LED
    // (ich habe keine Angabe gefunden ,was bei HALT isntruction geschieht).
    if (Execution_laststate = esRunning)
            and (FormMain.FormExecute.TheState = esStopped) then begin
Execution_laststate := esStopped ; // verhidnere Rekurion, wenn das hier zu lange dauert
      // PC-adresse anzeigen, ist virtual
      curpcval.mat := matVirtual ;
      curpcval.val := FormMain.FormExecute.CurPc.pdp_value ;
      // Anzeige der Addr wird in loadedaddr gespeichert
      AddressRegisterVal := FormMain.PDP11Console.MMU.Virtual2PhysicalInstruction(curpcval) ;
//      showAddress(AddressRegisterVal) ;

      // Jetzt versuche, das DisplayRegister abzufragen
      // Klappt nur, wenn SimH connected ist!
      try
        TheMemorycell.addr.mat := matSpecialRegister ;
        TheMemorycell.addr.val := MEMORYCELL_SPECIALADDR_DISPLAYREG ;
        TheMemorycell.Examine ;
        DisplayRegisterVal := TheMemorycell.pdp_value ;

      except // alle Fehler unterdrücken .. wir sind in Timer, er käme immer wieder!

      end;

    end{ "if (Execution_laststate = esRunning) and (FormMain.FormExecute.TheState = esS..." } ;
    Execution_laststate := FormMain.FormExecute.TheState ;
  *)
    // LEDs und Keys mit Frame abgleichen
    UpdateDisplay ; 

    ThePanel.SyncWithImplementor ; 

  end{ "procedure TFormPdp1170Panel.Timer1Timer" } ; 


// braucht 2 cells der globalen group
// : 1 für Examine/Deposit, eins für Update des Displayregisters
procedure TFormPdp1170Panel.ConnectToMemoryCells(mcg:TMemoryCellgroup) ; 
  begin 
    assert(mcg.Count >= 2) ; 
    memorycellgroup := mcg ; 

    AddressRegisterVal.mat := mcg.mat ; 

    TheMemorycell := mcg.Cell(0) ; 
    TheMemorycell.addr := AddressRegisterVal ; // initialisierung

    DisplayRegisterMemoryCell := mcg.Cell(1) ; // zeigt auf das DisplayRegister
//     DisplayRegisterMemoryCell.addr.mat := matPhysical22 ;
    DisplayRegisterMemoryCell.addr.val := PhysicalIopageBaseAddr(memorycellgroup.mat) + _17570 ; 

    DisplayRegisterMemoryCell.pdp_value := MEMORYCELL_ILLEGALVAL ; 

    memorycellgroup.OnMemoryCellChange := OnMemoryCellChange ; 

    memorycellgroup.calcAddrRange ; 

  end{ "procedure TFormPdp1170Panel.ConnectToMemoryCells" } ; 


// KENREL/USER/SUPER, 16,18,22 etc setzen
procedure TFormPdp1170Panel.UpdateDisplay ; 

// wert auf Adress LEDs anzeigen
  procedure showAddress(address: TMemoryAddress) ; 
    var i: integer ; 
      ctrl: Tpdp1170PanelControl ; 
      val: dword ; 
    begin 
      val := address.val ; 
      if address.mat = matVirtual then 
        val := val and $ffff ; // nur 16 bit

      for i := 0 to 21 do begin 
        ctrl := ThePanel.getControlByID(_Led, pdp1170Led_ADDRESS, i) ; 
        assert(ctrl <> nil) ; 
        ctrl.setActiveStateByValue(val and 1); 
        val := val shr 1 ; 
      end; 
    end{ "procedure showAddress" } ; 



// wert auf DATA LEDs anzeigen
// wenn value = MEMORYCELL_ILLEGALVAL: ADR EROR LED AN
  procedure showDataAndAddrError(val: dword) ; 
    var i: integer ; 
      addrerrorled: Tpdp1170PanelControl ; 
      ctrl: Tpdp1170PanelControl ; 
    begin 
      addrerrorled := ThePanel.getControlByID(_Led, pdp1170Led_ADRS_ERR, 0) ; 
      if val = MEMORYCELL_ILLEGALVAL then begin 
        addrerrorled.setActiveStateByValue(1); 
        val := 0 ; 
      end else 
        addrerrorled.setActiveStateByValue(0); 

      for i := 0 to 15 do begin 
        ctrl := ThePanel.getControlByID(_Led, pdp1170Led_DATA, i) ; 
        assert(ctrl <> nil) ; 
        ctrl.setActiveStateByValue(val and 1); 
        val := val shr 1 ; 
      end; 
    end{ "procedure showDataAndAddrError" } ; 


  var 
    ctrl: Tpdp1170PanelControl ; 
    state: Tpdp1170PanelControlState ; 
    tmpAddr,emptyAddr: TMemoryAddress ; 
  begin { "procedure TFormPdp1170Panel.UpdateDisplay" } 
    ctrl := ThePanel.getControlByID(_Led, pdp1170Led_MASTER, 0) ; 
    ctrl.setActiveStateByValue(1); 

    ctrl := ThePanel.getControlByID(_Led, pdp1170Led_USER_SUPER_KERNEL, 0) ; 
    state := ctrl.getStateByName('KERNEL') ; 
    assert(state <> nil) ; 
    ctrl.setActiveStateByValue(state.value); 

    ctrl := ThePanel.getControlByID(_Led, pdp1170Led_ADDRESSING_16, 0) ; 
    ctrl.setActiveStateByValue(0); 
    ctrl := ThePanel.getControlByID(_Led, pdp1170Led_ADDRESSING_18, 0) ; 
    ctrl.setActiveStateByValue(0); 
    ctrl := ThePanel.getControlByID(_Led, pdp1170Led_ADDRESSING_22, 0) ; 
    ctrl.setActiveStateByValue(1) ; 

    ctrl := ThePanel.getControlByID(_Led, pdp1170Led_RUN, 0) ; 
    if FormMain.FormExecute.TheState = esRunning then 
      ctrl.setActiveStateByValue(1) 
    else ctrl.setActiveStateByValue(0) ; 

    // ADDRESS LEDs: nur Anzeige, wenn CONS PHY oder PROG PHY
    if (AddressSelectState <> nil) and (AddressSelectState.name = 'CONS PHY') then begin 
      showAddress(AddressRegisterVal) ; 
    end else if (AddressSelectState <> nil) and (AddressSelectState.name = 'PROG PHY') then begin 
      // PROG PHY: die Fomulierung ist
      // "Displays the 22-bit physical address generated by Memory Management for the
      // current Unibus or Memory cycle."
      // nach einem HALT müsste der aktuelle Befehl, also PC-2 sein ?!
      tmpAddr.mat := memorycellgroup.mat ; 
      tmpAddr.val := FormMain.FormExecute.CurPc.edit_value - 2 ; 
      showAddress(tmpAddr) ; 
    end else begin // clear ADDRESS LEDs
      emptyAddr.mat := memorycellgroup.mat ; 
      emptyAddr.val := 0 ; 
      showAddress(emptyAddr) ; 
    end; 

    // DATA LEDs: DATA PATHS = Daten, DISPLAY REGISTER
    if (DataSelectState <> nil) and (DataSelectState.name = 'DATA PATHS') then 
      showDataAndAddrError(DataRegisterVal) 
    else if (DataSelectState <> nil) and (DataSelectState.name = 'DISPLAY REGISTER') then 
      showDataAndAddrError(DisplayRegisterVal) 
    else 
      showDataAndAddrError(0) ; 

    // wenn CPU läuft:
    // konstant, bis auf Run-Mode aus FormExecute
  end{ "procedure TFormPdp1170Panel.UpdateDisplay" } ; 


// lies die 22 data switches
function TFormPdp1170Panel.getDataSwitches: dword ; 
  var i: integer ; 
    ctrl: Tpdp1170PanelControl ; 
  begin 
    result := 0 ; 
    for i := 0 to 21 do begin 
      ctrl := ThePanel.getControlByID(_Switch, pdp1170Sw_DATA, i) ; 
      assert(ctrl <> nil) ; 
      if ctrl.active_state_index > 0 then 
        result := result or (1 shl i) ; 
    end; 
  end; 




// implementiert die Auto-Increment-Funktion des Panels
// normaler addresse: address +2 ;
// im Registerspace R0..R7 aber +1, und zyklisch R7->R0
function TFormPdp1170Panel.getNextAddress(oldaddr: TMemoryAddress): TMemoryAddress ; 
  begin 
    result := oldaddr ; 
    if (result.val >= _17777700) and (result.val < _17777707) then 
      result.val := result.val + 1 
    else if result.val = _17777707 then 
      result.val := _17777700 // R7 -> R0
    else 
      result.val := result.val + 2 ; 
  end ; 


// Wenn ein Switch verändert wurde
// Verhalten wie in
// EK-KB11C-TM-001_1170procMan.pdf, Section III, pdf page 187
procedure TFormPdp1170Panel.ThePanelSwitchChanged(Sender: TObject) ; 

  var 
    ctrl: Tpdp1170PanelControl ; 
    state: Tpdp1170PanelControlState ; 
    typestr: string ; 
  begin 
    if Sender is Tpdp1170PanelControl then begin 
      ctrl := Sender as Tpdp1170PanelControl ; 
      case ctrl.ctltype of 
        _Led: typestr := 'LED' ; 
        _Switch: typestr := 'Switch' ; 
      end; 
      Log('screen panel  %s "%s" changed to idx=%d, val=%d = "%s"', 
              [typestr, ctrl.name, 
              ctrl.active_state_index, 
              ctrl.States[ctrl.active_state_index].value, 
              ctrl.States[ctrl.active_state_index].name]) ; 

      // wenn ctrl ein Switch ist: auf Active-Position reagieren
      if (ctrl.ctltype = _Switch)  then 
        case ctrl.group of 
          pdp1170Sw_LOAD_ADRS: 
            if ctrl.getActiveState.value > 0 then begin 
              // wenn activ: DATA-Switches nach address buffer
              // adresse virtual, wenn DATA_SELECT IN KERNEL*/USER*
              AddressRegisterVal.val := getDataSwitches ; 
//              showAddress(AddressRegisterVal) ;
              LastAddrIncCtrl := ctrl ; 
            end ; 
          pdp1170Sw_EXAM: 
            if ctrl.getActiveState.value > 0 then begin 
              // eigentlich: nur wenn ADRESS_SELECT Drehschalter auf DATA_PATH

              if LastAddrIncCtrl = ctrl then begin 
                // zweimal hintereinander togglen: Addresse auto inc

                AddressRegisterVal := getNextAddress(AddressRegisterVal) ; 

//                showAddress(AddressRegisterVal) ;
              end ; 
              TheMemorycell.addr := AddressRegisterVal ; 
              TheMemorycell.Examine ; // bei zugriffsfehler: pdp_value = ILLEGAL
              DataRegisterVal := TheMemorycell.pdp_value ; 
//              showDataAndAddrError(TheMemorycell.pdp_value) ;

              LastAddrIncCtrl := ctrl ; 
            end { "if ctrl.getActiveState.value > 0" } ; 
          pdp1170Sw_DEP: 
            if ctrl.getActiveState.value > 0 then begin 
              if LastAddrIncCtrl = ctrl then begin 
                // zweimal hintereinander togglen: Addresse auto inc
                AddressRegisterVal := getNextAddress(AddressRegisterVal) ; 
//                showAddress(AddressRegisterVal) ;
              end ; 

              TheMemorycell.addr := AddressRegisterVal ; 
              TheMemorycell.edit_value := getDataSwitches and $ffff ; 
              TheMemorycell.Deposit ; 
              // check: adresse legal?
              TheMemorycell.Examine ; // bei zugriffsfehler: pdp_value = ILLEGAL
              DataRegisterVal := TheMemorycell.pdp_value ; 
//              showDataAndAddrError(TheMemorycell.pdp_value) ;

              LastAddrIncCtrl := ctrl ; 
            end { "if ctrl.getActiveState.value > 0" } ; 
          pdp1170Sw_CONT: 
            if ctrl.getActiveState.value > 0 then begin 
              // CONT gedrückt. Funktion hängt von HALT/ENABLE ab:
              if Haltswitch_active then begin 
                // wenn HALT/ENABLE = HALT: single step
                // (eigentlich noch depending on SINST/BUS)
                FormMain.FormExecute.doSingleStep; 
//                Execution_laststate := esRunning ; // damit STOP in Timer bemerkt wird
              end else begin 
                // wenn HALT/ENABLE = ENABLE: continue ohne reset
                FormMain.FormExecute.doContinue; 
              end; 
            end ; 
          pdp1170Sw_ENABLE_HALT: begin 
            // switch to HALT: STOP program
            //FormExecute.StopButtonClick
            state := ctrl.getStateByName('HALT') ; 
            // haltswitch changed?
            if (ctrl.getActiveState = state) then begin 
              // HALT ist aktiv
              if not Haltswitch_active then // Switch wurde gerade auf HALT gesetzt:
                // stoppe Program, wenn es läuft.
                Haltswitch_active := true ; 
              if FormMain.FormExecute.TheState = esRunning then 
                FormMain.FormExecute.doHalt; 

            end else // ENABLE ist aktiv
              Haltswitch_active := false ; 


          end { "case ctrl.group of pdp1170Sw_ENABLE_HALT:" } ; 
          pdp1170Sw_SINST_SBUS_CYCLE: begin 
          end ; 
          pdp1170Sw_START: begin 
            if ctrl.getActiveState.value > 0 then begin 
              if Haltswitch_active and (FormMain.FormExecute.TheState <> esRunning) then begin 
                // wenn HALT/ENABLE = HALT: reset, nur wenn nicht ausführend
/// chaos virtual/physical!!!
                FormMain.FormExecute.StartPCEdit.text := Addr2OctalStr(AddressRegisterVal) ; 
                FormMain.FormExecute.doResetMachineAndSetPC; 
                // FormExecute wird jetzt auf die eingetastete Adresse resettet.
              end; 
              if not Haltswitch_active and (FormMain.FormExecute.TheState <> esRunning) then begin 
                // wenn HALT/ENABLE = ENABLE: Reset und Run ab addresse
/// chaos virtual/physical!!!
                FormMain.FormExecute.StartPCEdit.text := Addr2OctalStr(AddressRegisterVal) ; 
//                Application.ProcessMessages ; // set pc in EditChange-handler
//                FormMain.FormExecute.doResetCpuAndSetPC;
                Application.ProcessMessages ; 
                FormMain.FormExecute.doResetMachineAndSetPCandStart ; 
              end ; 
            end{ "if ctrl.getActiveState.value > 0" } ; 
          end{ "case ctrl.group of pdp1170Sw_START:" } ; 
          pdp1170Sw_ADDRESS_SELECT: begin 
            AddressSelectState := ctrl.getActiveState ; 
          end ; 
          // KERNEL-I, SUPER-I, USER-I, CONS PHY, PROG PHY
          pdp1170Sw_DATA_SELECT: begin 
            DataSelectState := ctrl.getActiveState ; 
          end ; 
//      pdp1170Sw_DATA // 22 Sw

        end { "case ctrl.group" } ; 
      ThePanel.SyncWithImplementor ; 
    end{ "if Sender is Tpdp1170PanelControl" } ; 
  end{ "procedure TFormPdp1170Panel.ThePanelSwitchChanged" } ; 


procedure TFormPdp1170Panel.TheScreenPanelSwitchOrLEDChanged(Sender: TObject) ; 
  begin 
    // nur wirksam, wenn physical Panel nicht eingesteckt
    if not pdp1170panelImplementorPhysical.Connected then 
      if (Sender is Tpdp1170PanelControl) then 
        if (Sender as Tpdp1170PanelControl).ctltype = _Switch then 
          ThePanelSwitchChanged(Sender); 
  end; 


procedure TFormPdp1170Panel.ThePhysicalPanelSwitchChanged(Sender: TObject) ; 
  begin 
    ThePanelSwitchChanged(Sender) ; 
  end; 


// wenn von aussen eine MemoryCell geändert wird.
// nur für Abfangen von Write auf DisplayRegister interessant
procedure TFormPdp1170Panel.OnMemoryCellChange(Sender: TObject; memorycell: TMemoryCell) ; 
  begin 
    if memorycell = DisplayRegisterMemoryCell then 
      DisplayRegisterVal := DisplayRegisterMemoryCell.pdp_value ; 
  end; 

// wird von ausserhalb aufgerufen, wenn die Codeausführung started
procedure TFormPdp1170Panel.OnCpuStart(Sender: TObject) ; 
  begin 
    if not Visible then Exit ; // SR nicht setzen, wenn Display unsichtbar
    // Jetzt versuche, das SwitchRegister zu setzen
    // Klappt nur, wenn SimH connected ist!
    try 
      TheMemorycell.addr.mat := matSpecialRegister ; 
      TheMemorycell.addr.val := MEMORYCELL_SPECIALADDR_SWITCHREG ; 
      TheMemorycell.edit_value := getDataSwitches and $ffff ; 
      TheMemorycell.Deposit ; 
    except // alle Fehler unterdrücken .. wir sind im Timer, er käme immer wieder!

    end; 
  end{ "procedure TFormPdp1170Panel.OnCpuStart" } ; 


// wird von ausserhalb aufgerufen, wenn die Codeausführung stoppt
procedure TFormPdp1170Panel.OnCpuHalt(Sender: TObject) ; 
//  var curpcval: TMemoryAddress  ;
  begin 
    if not Visible then Exit ; // DR nicht abfragen, wenn Display unsichtbar 

    (*
    Anzeige des Stopped PC wäre ein schönes feature ... ist aber nirgends dokumentiert!

    // PC-adresse anzeigen, ist virtual
    curpcval.mat := matVirtual ;
    curpcval.val := FormMain.FormExecute.CurPc.pdp_value ;
    // Anzeige der Addr wird in loadedaddr gespeichert
    AddressRegisterVal := FormMain.PDP11Console.MMU.Virtual2PhysicalInstruction(curpcval) ;
//      showAddress(AddressRegisterVal) ;
*)
    // Jetzt versuche, das DisplayRegister abzufragen
    // Klappt nur, wenn SimH connected ist!
    try 
      TheMemorycell.addr.mat := matSpecialRegister ; 
      TheMemorycell.addr.val := MEMORYCELL_SPECIALADDR_DISPLAYREG ; 
      TheMemorycell.Examine ; 
      DisplayRegisterVal := TheMemorycell.pdp_value ; 

    except // alle Fehler unterdrücken .. wir sind im Timer, er käme immer wieder!

    end; 

  end{ "procedure TFormPdp1170Panel.OnCpuHalt" } ; 



end{ "unit FormPdp1170PanelU" } . 
