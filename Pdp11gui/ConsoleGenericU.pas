unit ConsoleGenericU; 

{
  Abstrakte Basisklasse für die Ansteuerung verschiedener PDP-11-Consol-Terminals.
  Konkrete Consolen-Implementationen sind
  - PDP-11 M9312-Style
  - PDP-11/44-Style
  - PDP-11/23,73,93 ODT-Style
  - SimH

  Jede konkrete Console muss folgende Methoden implementieren:
  ClearState - Wissen über Status löschen
  Examine - Memory lesen
  Deposit - Memory schreiben
}

interface 

uses 
  Classes, Windows, Sysutils, 
  ExtCtrls, 
  FormTerminalU, 
  SerialIoHubU, 
  AddressU, 
  MemoryCellU, 
  Pdp11MMuU, 
  FormBusyU ; 



type 

  // mit welcher Art PDP-11 soll kommuniziert werden?
  TConsoleType = ( 
      consoleNone, 
      consoleSelftest11M9301, 
      consoleSelftest11M9312, 
      consoleSelftest1144, 
      consoleSelftest1144v340c, 
      consoleSelftest11ODT16, 
      consoleSelftest11ODT18, 
      consoleSelftest11ODT22,
      consoleSelftest11ODTK1630,
      consolePDP11M9301,
      consolePDP11M9312,
      consolePDP1144,
      consolePDP1144v340c,
      consolePDP11ODT16,
      consolePDP11ODT18,
      consolePDP11ODT22,
      consolePDP11ODTK1630, // robotron A6402 CPU K1630
      consoleSimH
    ) { "TYPE TConsoleType" } ; 

  TConsoleAnswerPhraseType = (phNone, 
      phPrompt, // Prompt erkannt
      phHalt, // CPU halt erkannt
      phExamine, // Anwort auf Examine, auch Unibus timeout
      phOtherLine // nicht-interpretierte Zeile
    ) ; 

// Record, der eine geparste Antwort enthält
// Union.
  TConsoleAnswerPhrase = class(TCollectionItem) 
    public 
      phrasetype:TConsoleAnswerPhraseType ; 
      rawtext: string ; // ungeparst, wie empfangen

      // wenn phHalt:
      haltaddr: TMemoryAddress ; 
      // wenn phExamine
      examineaddr: TMemoryAddress ; 
      examinevalue: dword ; 

      // wenn phOtherLine
      otherline: shortstring ; 

      function AsText: string ; 
    end{ "TYPE TConsoleAnswerPhrase = class(TCollectionItem)" } ; 


  // Scanner, umn den Output der PDP zu analysieren
  EConsoleScannerInputIncomplete = class(Exception) ; 
  // Wenn Parseversuche kein Ergebnis erzeugen. kein Fehler
  EConsoleScannerUnknownExpression = class(Exception) ; 

  TConsoleScanner = class(TObject) 
    public 
      CurInputLine: string ; // das parst und verkürzt er!
      nxtcharidx: integer ; // nächstes unverarbeites Zeichen aus CurInputLine
      CurSymTxt: string ; 
      CurSymType: integer ; // eines der symtype*

      marked_nxtcharidx: integer ; 
      marked_CurSymTxt: string ; 
      marked_CurSymType: integer ; 

      constructor Create ; virtual ; 
      procedure Clear ; virtual ; 
      procedure MoreInput(s:string) ; 
      procedure CleanupInput ; 
      procedure MarkParsePosition ; 
      procedure RestoreParsePosition ; 
      function NxtSym(raiseIncompleteOnEof: boolean = true): string ; virtual ; abstract ; 
    end{ "TYPE TConsoleScanner = class(TObject)" } ; 



type 

  // was kann eine konkrete Console alles?
  // verschiedene '11s haben da verschiedene Logiken

  // Für eine '11 hängen die Features ausserdem noch vom Schalter
  // "RunMode" ab

  // A console can "implement" an action also by popping up a message to
  // tell the user wich swithc he should operate.

  TConsoleFeatures = ( 
      // examine/deposit können sie alle!
      cfNonFatalHalt, // console läuft nach HALT weiter, Maschine bleibt nicht stehen (M9312!)
      cfNonFatalUNIBUStimeout, // console läuft nach UNIBUS timeout weiter
      cfSwitchEnableOrHalt, // console has a physical "Enable/Halt switch,
      // so featuresSet depends on "RunMode"
      // implemented commands
      cfActionResetMachine, // reset unabhängig von run, PC set
      cfActionResetMaschineAndStartCpu, // weiterlaufen mit Init
      cfActionContinueCpu, // weiterlaufen ohne Init
      cfActionHaltCpu, // Console kann Program stoppen
      cfActionSingleStep, 
//      cfMicroStep
      cfFlagResetCpuSetsPC // wenn das "Reset" den PC mit setzt
    ) { "TYPE TConsoleFeatures" } ; 

  TConsoleFeatureSet = set of TConsoleFeatures ; 

  // 11'70: Halt mode, or Run mode? Naming "ENABLE/HALT as on 11/70 panel
  TConsoleRunMode = (crmUnknown, crmRun, crmHalt) ; 


// wird ausgelöst, wenn die überwachte CPU anhält
  TConsoleCPUStopEvent = procedure(Sender: TObject ; pc: TMemoryAddress) of object ; 

  TConsoleGeneric = class(TObject) 
    private 
      CriticalSectionLevel: integer ; // > 0 wenn der User gerade kein Zuriff auf die
      // Console erlauben, weil eine geschlossene Sequenz abläuft
      MonitorTimer: TTimer ; 
      procedure MonitorTimerCallback(Sender: TObject) ; 

    protected 
      procedure BeginCriticalSection(location:string = 'location unknown') ; 
      procedure EndCriticalSection(location:string = 'location unknown') ; 
      // stelle sicher, das die Commandoprompt kommt.
      procedure CheckPrompt(errinfo: string) ; 
    public 
      AnswerLines: TCollection ; // of TConsoleAnswerPhrase

      CommandTimeoutMillis: dword ; // Zeit, bis ein ConsoelCmd benatwortet werden muss.
      // in SUbklasse Constructor definieren

      RcvScanner: TConsoleScanner ; // muss von subclass instanziiert werden
//      SerialRcvDataBuffer: string ;

      Connection: TSerialIoHub ; // Link auf COM/Telnet-Routinen, extern

      // CPU running or stopped by console switch? Influences "Features"!
      // Only meaningful if features cfSwitchEnableOrHalt
      RunMode: TConsoleRunMode ; 


      machine_name:string ; // name

      // wird signalisiert, wenn die Console einen Stop der PDP-11 entdeckt
      onExecutionStopPcVal: TMemoryAddress ; // PC value, wenn ein Stop der Simulation
      // erkannt wurde. Muss von abgeleiteteten Klassen erkannt und gesetzt werden.
      onExecutionStopDetected : boolean ; // true, wenn stop erkannt wurde.
      // Muss von abgeleiteteten Klassen erkannt und gesetzt werden.

      OnExecutionStop: TConsoleCPUStopEvent ; 

      // für die Umrechnung virtual->physical Addresse
      MMU: TPdp11Mmu ; 

      constructor Create ; 
      destructor Destroy ; override ; 

      procedure Init(aConnection: TSerialIoHub) ; virtual ; 

      function InCriticalSection: boolean ; 

      function getFeatures: TConsoleFeatureSet ; virtual ; abstract ; 

      function getPhysicalMemoryAddressType: TMemoryAddressType ; virtual ; abstract ; // 16, 18, 22

      // siehe M9312. default fuer andere Consoles: immer 0
      function getMonitorEntryAddress: TMemoryAddress ; virtual ; 


      procedure Resync ; virtual ; abstract ; 

      // Aus SerialRcvDataBuffer die Antworten extrahieren
      function DecodeNextAnswerPhrase: boolean ; virtual ; abstract ; 

      // low level io
      procedure OnSerialRcv(curdata: string) ; 

      // gib die letzte Antwort eines gewissen Typs zurück
      function GetLastAnswer(phrasetype: TConsoleAnswerPhraseType): TConsoleAnswerPhrase ; 
      function WaitForAnswer(phrasetype: TConsoleAnswerPhraseType; waitmillis: dword): TConsoleAnswerPhrase ; 
//      function ReadLineFromPDP(var buff:string): boolean ;
      procedure WriteToPDP(buff:string) ; 

//      procedure TerminalCharToPDP(c: char) ;
//      procedure TerminalPollPDP(var s:string) ;


      // damit können konkrete Consolen den Output der
      // PDP überwachen, zB um selbständigen Stop der Simulation zu erkennen
      procedure MonitorPdpOutput(curchar: char) ; virtual ; abstract ; 

      // name der angeschlossenen Console liefern
      function getName: string ; virtual ;abstract ; 
      function getTerminalSettings: TTerminalSettings ; virtual ;abstract ; 

      procedure ClearState ; virtual ; 
      procedure Deposit(addr: TMemoryAddress ; val: dword) ; overload ; virtual ;abstract ; 
      procedure Deposit(mcg: TMemoryCellGroup ; optimize:boolean ; abortable: boolean) ; overload ; virtual ; 
      function Examine(addr: TMemoryAddress): dword ; overload ; virtual ; abstract ; 
      procedure Examine(mcg: TMemoryCellGroup ; unknown_only: boolean ; abortable: boolean) ; overload ; virtual ; abstract ; 

      // Laufkontrolle. Siehe auch OnSimulationStop. PC ist virtuelle Adresse
      procedure ResetMachine(newpc_v: TMemoryAddress) ; virtual ; abstract ; // CPU+UNIBUS reset
      procedure ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ; virtual ; abstract ; // CPU starten, Reset vorher
//      function IsRunning: boolean ; virtual ; abstract ; // läuft die CPU (noch?)
      procedure ContinueCpu ; virtual ; abstract ; // CPU weiter laufen lassen, kein Reset
      procedure HaltCpu(var newpc_v: TMemoryAddress) ; virtual ; abstract ; // CPU anhalten, neuer PC zurück
      procedure SingleStep ; virtual ; abstract ; // einen Zyklus ausführen, neuer PC zurück


    end{ "TYPE TConsoleGeneric = class(TObject)" } ; 

implementation 

uses 
  Forms, 
  TypInfo, // GetEnumName
  AuxU, 
  FormMainU, 
  FormLogU, 
  FormNoConsolePromptU 
  ; 



function TConsoleAnswerPhrase.AsText: string ; 
  var s: string ; 
  begin 
    s := 'Console answered: ' ; 
    case phrasetype of 
      phNone: s := s + 'phNone' ; 
      phPrompt:  s := s + 'phPrompt' ; 
      phHalt:  s := s + Format('phHalt, haltaddr= %s', [Addr2OctalStr(haltaddr)]) ; 
      phExamine:  s := s + Format('phExamine, addr=%s, value=%s', 
                [Addr2OctalStr(examineaddr), Dword2OctalStr(examinevalue)]) ; 
      phOtherLine:  s := s + Format('phOtherLine "%s"', [otherline]) ; 
    end; 
    result := s ; 
  end{ "function TConsoleAnswerPhrase.AsText" } ; 



constructor TConsoleScanner.Create ; 
  begin 
    inherited ; 
    Clear ; 
  end; 

procedure TConsoleScanner.Clear ; 
  begin 
    CurInputLine := '' ; 
    nxtcharidx := 1 ; 
    // abgeleitete Klassen müssen selbst das erste Symbol mit "NxtSym" fetchen.
  end; 

// Add serial input to the parser.
procedure TConsoleScanner.MoreInput(s:string) ; 
  var i: integer ; 
  begin 
    // filter out NUL characters.
    // Some consoles produce fill NULs, they may appear everywhere.
    for i := 1 to length(s) do 
      if s[i] <> #0 then 
        CurInputLine := CurInputLine + s[i] ; 
  end; 

procedure TConsoleScanner.CleanupInput ; 
  begin 
    // gescannten Teil des Inputbuffers löschen
    CurInputLine := Copy(CurInputLine, nxtcharidx, maxint) ; 
    nxtcharidx := 1 ; 
  end; 

procedure TConsoleScanner.MarkParsePosition ; 
  begin 
    marked_nxtcharidx := nxtcharidx ; 
    marked_CurSymTxt := CurSymTxt ; 
    marked_CurSymType := CurSymType ; 
  end; 

procedure TConsoleScanner.RestoreParsePosition ; 
  begin 
    nxtcharidx := marked_nxtcharidx ; 
    CurSymTxt := marked_CurSymTxt ; 
    CurSymType :=marked_CurSymType ; 
  end; 


constructor TConsoleGeneric.Create ; 
  begin 
    inherited ; 
    AnswerLines:= TCollection.Create(TConsoleAnswerPhrase) ; 

    MMU := nil ; // muss von abgeleiteter Class passend zur Console erzeugt werden.
    Connection := nil ; 
    OnExecutionStop := nil ; 
    RunMode := crmUnknown ; 

    RcvScanner := nil ; 

    onExecutionStopPcVal.mat := matUnknown ; 
    onExecutionStopPcVal.val := MEMORYCELL_ILLEGALVAL ; 
    onExecutionStopDetected := false ; 

    MonitorTimer := TTimer.Create(nil) ; 
    MonitorTimer.Interval := 100 ; // 10x pro sekunde
    MonitorTimer.OnTimer := MonitorTimerCallback; 
    MonitorTimer.Enabled := true ; 
  end{ "constructor TConsoleGeneric.Create" } ; 


destructor TConsoleGeneric.Destroy ; 
  begin 
    MonitorTimer.Free ; 
    AnswerLines.Free ; 
    inherited ; 
  end; 


procedure TConsoleGeneric.Init(aConnection: TSerialIoHub) ; 
  begin 
    CriticalSectionLevel:= 0; 
    Connection := aConnection ; 
    onExecutionStopPcVal.mat := matUnknown ; 
    onExecutionStopPcVal.val := MEMORYCELL_ILLEGALVAL ; 
  end; 

procedure TConsoleGeneric.ClearState ; 
  begin 
    // macht hier im Moment nix
    onExecutionStopPcVal.val := MEMORYCELL_ILLEGALVAL ; 
  end; 



// Begin/Ende von zusammenhängendem  Console/IO markieren.
// der SimualtionStopEvent wird erst nach Ende
// einer CriticalSection ausgelöst.
procedure TConsoleGeneric.BeginCriticalSection(location:string) ; 
  begin 
    inc(CriticalSectionLevel) ; 
    LogStrCol(LogCol_CriticalSection, 'BeginCriticalSection from ' + location) ; 
  end; 


procedure TConsoleGeneric.EndCriticalSection(location:string) ; 
  begin 
    LogStrCol(LogCol_CriticalSection, 'EndCriticalSection from ' + location) ; 
    if CriticalSectionLevel > 0 then dec(CriticalSectionLevel) ; 
  end; 

function TConsoleGeneric.InCriticalSection: boolean ; 
  begin 
    result := CriticalSectionLevel > 0 ; 
  end; 


// String ausgeben. Anwendung muss \n selbst ergänzen!
// Ausgabe in Logfenster mit Farbe 2
procedure TConsoleGeneric.WriteToPDP(buff:string) ; 
  begin 
    Connection.DataFromConsole(buff) ; 
  end; 



procedure TConsoleGeneric.OnSerialRcv(curdata: string) ; 
  begin 
    RcvScanner.MoreInput(curdata); 
    LogStrCol(LogCol_DecodeNextAnswerPhrase,'TConsoleGeneric.OnSerialRcv> "%s"', [String2PrintableText(curdata, true)]); 
    while DecodeNextAnswerPhrase do 
      Application.ProcessMessages ; 
  end; 


// gib die letzte Antwort eines gewissen Typs zurück
function TConsoleGeneric.GetLastAnswer(phrasetype: TConsoleAnswerPhraseType): TConsoleAnswerPhrase ; 
  var i: integer ; 
    answerline: TConsoleAnswerPhrase ; 
  begin 
    result := nil ; 
    for i := AnswerLines.Count-1 downto 0 do begin 
      answerline := AnswerLines.Items[i] as TConsoleAnswerPhrase ; 
      LogStrCol(LogCol_DecodeNextAnswerPhrase, 
              'TConsoleGeneric.GetLastAnswer(): #%d = %s "%s"', [i , 
              GetEnumName(TypeInfo(TConsoleAnswerPhraseType), integer(answerline.phrasetype)), 
              answerline.rawtext]); 

      if answerline.phrasetype = phrasetype then begin 
        result := answerline ; 
        Exit ; 
      end; 
    end; 
  end { "function TConsoleGeneric.GetLastAnswer" } ; 

// warte eine Zeit, bis eine Antwort eines gewissen Typs vorkam
function TConsoleGeneric.WaitForAnswer(phrasetype: TConsoleAnswerPhraseType; waitmillis: dword): TConsoleAnswerPhrase ; 
  var starttime: dword ; 
  begin 
    // result := nil ;

    LogStrCol(LogCol_Other, 'WaitForAnswer(phrase=%s, millis=%d)', [
            GetEnumName(TypeInfo(TConsoleAnswerPhraseType), integer(phrasetype)),
            waitmillis]) ;
    starttime := getTickCount ;
    repeat 
      Application.ProcessMessages ; // Backgroundempfang
      result := GetLastAnswer(phrasetype) ; 
      if result = nil then 
        sleep(1) ; 
    until (getTickCount > starttime+waitmillis) or (result <> nil) ; 
    if result = nil then 
      LogStrCol(LogCol_Other, 'WaitForAnswer: result = nil') 
    else 
      LogStrCol(LogCol_Other, 'WaitForAnswer: found!') 
  end{ "function TConsoleGeneric.WaitForAnswer" } ; 


// stelle sicher, dass die Commandoprompt in AnswerLines[] vorkommt.
// warte sonst.
procedure TConsoleGeneric.CheckPrompt(errinfo: string) ; 
  var s: string ; 
  begin 
    if WaitForAnswer(phPrompt, CommandTimeoutMillis) = nil then begin 
      FormNoConsolePrompt.ShowModal ; 
      s := Format('No console prompt: %s!', [errinfo]) ; 
      Log(s) ; 
//      raise Exception.Create(s) ;
      Abort ; 
    end; 
  end; 

// Monitor-Funktion der Console aufrufen, wenn nicht gerade andere Sequenzen aktiv sind
procedure TConsoleGeneric.MonitorTimerCallback(Sender: TObject) ; 
  begin 
    if InCriticalSection then Exit ; 
    BeginCriticalSection('MonitorTimerCallback'); 
    try 
      LogStrCol(LogCol_MonitorTimerCallback, 'MonitorTimerCallback') ; 

      // wenn die abgeleitete console einen pc erkannt hat,
      // der Anwendung das jetzt signalisieren. Dann kann sie
      // im Handler schon wieder die PDP-11 benutzen
      if assigned(OnExecutionStop) and onExecutionStopDetected then begin 
//      if assigned(OnExecutionStop) and (onExecutionStopPcVal.val <> MEMORYCELL_ILLEGALVAL) then begin
        LogStrCol(LogCol_MonitorTimerCallback, 'call OnExecutionStop()') ; 
        OnExecutionStop(self, onExecutionStopPcVal) ; 
      end ; 
      onExecutionStopPcVal.val := MEMORYCELL_ILLEGALVAL ; 
      onExecutionStopDetected := false ; 
    finally 
      EndCriticalSection('MonitorTimerCallback'); 
    end{ "try" } ; 
  end{ "procedure TConsoleGeneric.MonitorTimerCallback" } ; 


procedure TConsoleGeneric.Deposit(mcg: TMemoryCellGroup ; optimize:boolean; abortable:boolean) ; 
  var 
    i: integer ; 
    mc: TMemoryCell ; 
  begin 
    BusyForm.Start('Depositing ...', mcg.Count, abortable) ; 
    try 
      for i := 0 to mcg.Count - 1 do begin 
        BusyForm.StepIt ; if BusyForm.Aborted then Break ; 
        mc := mcg.Cell(i) ; 
        if not optimize or (mc.pdp_value <> mc.edit_value) then begin 
          Deposit(mc.addr, mc.edit_value) ; 
          mc.pdp_value := mc.edit_value ; 
          // dieselbe Zelle in NachbarGroups aktualisieren,
          // dort werden die OnMemoryCellChange-Callbacks aufgerufen
          if mcg.Collection <> nil then // nil bei code window
            (mcg.Collection as TMemoryCellGroups).SyncMemoryCells(mc) ; 
        end; 
      end; 
    finally 
      BusyForm.Close ; 
    end{ "try" } ; 
  end{ "procedure TConsoleGeneric.Deposit" } ; 


// alle Consoles ausser M9312 markieren ein "not defined"
function TConsoleGeneric.getMonitorEntryAddress: TMemoryAddress ; 
  begin 
    result.mat := matPhysical16 ; 
    result.val := MEMORYCELL_ILLEGALVAL ; 
  end ; 



end{ "unit ConsoleGenericU" } . 
