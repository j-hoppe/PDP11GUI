unit ConsolePDP11M9312U; 
{
  Steuert über RS232 den console emulator auf dem
  M9312 boot terminator an. Wird zB in PDP-11/34 benutzt.
  Die Anwendung ruft nur Deposit(), Examine(), Start auf

  Der M9312 console emulator läuft aus BOOTROM auf der CPU
   und ist unglaublich schwach:
  - nur EXAM/DEPOSIT im 16 bit adressraum
  - nur START, kein Stop, oder single step.
  - kein Reset, oder Initialize BUS
  - läuft ein program auf HALT, kann die Maschine nur über front panel (CTRL-BOOT)
    neu gestartet werden.
  - BUS ERROR (nicht implementierte Adressen) führen ebenfalls zum HALT
   -> kein I/O page scan oder memory scan ist möglich
  - kein Zugriff auf die CPU Register R0..R7 1777700..1777707 !

  die 11/34 hat 18bit Adressen, der console emulator aber nur 16 bit.

}
interface 

uses 
  Classes, Windows, Sysutils, 
  JH_Utilities,
  FormSettingsU, 
  ConsoleGenericU, 
  SerialIoHubU, 
  FormTerminalU, 
  Pdp11MmuU, 
//  Pdp1144uCodeU,
  AddressU, 
  MemoryCellU,
  FormBusyU ;

const // Zeilenumbruch der PDP-Console
  CHAR_PDP11M9312_CR = #$d ; 
  CHAR_PDP11M9312_LF = #$a ; 
  PDP11M9312_CMD_TIMEOUT = 1000 ; // timeout für Antwort in Millisekunden. Long for slow telnet connections
  //PDP11M9312_PROMPT = '@' ;

type 

  // Scanner, um den Output der PDP zu analysieren
  // macht noch nix, nur CurInputLine wird von eigener Chaos-Logik hier benutzt
  TConsolePDP11M9312Scanner = class(TConsoleScanner) 
    public 
      constructor Create ; override; 
      function NxtSym(raiseIncompleteOnEof: boolean = true): string ; override ; 
      class function Symtype2text(symtype:integer): string ; 
    end; 


  TConsolePDP11M9312 = class(TConsoleGeneric) 
    private 
      deposit_lastaddr: TMemoryAddress ; // die gesetzte Adresse
      // der console emulator wird von CPU ausgeführt und ist nach HALT tot.
      // der disk image driver springt statt HALT an die den Einstiegspunkt zurück
      monitor_entryaddress: TMemoryAddress ; 
    public 
      Prompt: string ; // default '@', für M9301: '$'

      constructor Create(memorycellgroups: TMemoryCellGroups; // die MMU legt eigene memorycells an
              aMonitor_entryaddress: TMemoryAddress) ; 
      destructor Destroy ; override ; 

      // wenn serial: host='COM'
      procedure Init(aConnection: TSerialIoHub) ; override ; 
      procedure ClearState ; override ; 
      procedure Resync ; override ; 


      function getName: string ; override ; 
      function getTerminalSettings: TTerminalSettings ; override ; 

      function getFeatures: TConsoleFeatureSet ; override ; 
      function getPhysicalMemoryAddressType: TMemoryAddressType ; override ; 

      // überschreibe leere Function der Basisklasse
      function getMonitorEntryAddress: TMemoryAddress ; override ; 


      procedure Deposit(addr: TMemoryAddress ; val: dword) ; override ; 
      function Examine(addr: TMemoryAddress): dword ; overload ; override ; 
      procedure Examine(mcg: TMemoryCellgroup ; unknown_only: boolean ; abortable: boolean) ; overload ; override ;
      // ganze Liste optimiert

      function DecodeNextAnswerPhrase: boolean ; override ; 

      procedure ResetMachine(newpc_v: TMemoryAddress) ; override ; // Maschine reset
      procedure ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ; override ; // CPU starten.
      procedure ContinueCpu ; override ; 

//      function IsRunning: boolean ; virtual ; abstract ; // läuft die CPU (noch?)
      procedure HaltCpu(var newpc_v: TMemoryAddress) ;  override ; // CPU anhalten
      procedure SingleStep ;  override ; // einen Zyklus ausführen

    end{ "TYPE TConsolePDP11M9312 = class(TConsoleGeneric)" } ; 


implementation 


uses 
  Forms, 
  OctalConst, 
  AuxU, 
  FormLogU, 
  FormMainU ; 

const 
  // die 16 globalen Register R0..R7, R10..R17 werden kürzer addressiert
  global_register_base = _177700 ; // 16 bit
  global_register_blocksize = 16 ; 


const 
  // Scannt den output der PDP
  symtypeOctal = 0 ; 
  symtypeOpcode = 1 ; 
  symtypeOther = 2 ; 
  symtypeEoln = 3 ; 
  symtypeEof = 4 ; 

constructor TConsolePDP11M9312Scanner.Create ; 
  begin 
    inherited ; 
    NxtSym(false) ; // get first symbol, ist ein EOLN
  end; 


class function TConsolePDP11M9312Scanner.Symtype2text(symtype:integer): string ; 
  begin 
    result := '???' ; 
    case symtype of 
      symtypeOctal: result := 'Octal' ; 
      symtypeOpcode: result := 'Opcode' ; 
      symtypeOther: result := 'Other' ; 
      symtypeEoln: result := 'EOLN' ; 
      symtypeEof: result := 'EOF' ; 
    end; 
  end ; 


function TConsolePDP11M9312Scanner.NxtSym(raiseIncompleteOnEof: boolean = true): string ; 
  var 
    again: boolean ; 
    symlen: integer ; 
    s: string ; 
    i: integer ; 
  begin 
    // muss nur IDs und octalzahlen erkennen
    // ID kann sein: 1 Zeichen -> L, E D, S command
    // zwei Zeichne : boot command ("DD", "DL"
    // zwei Zeichen + Zahlen: boot command ("DD001").

    repeat 
      again := false ; 
      s := Copy(CurInputLine, nxtcharidx, maxint) ; 
      if s = '' then begin 
        result := 'EOF' ; 
        CurSymType := symtypeEof ; 
        // Exception an den Parser: Schluss machen
        if raiseIncompleteOnEof then 
          raise EConsoleScannerInputIncomplete.Create('EOF') ; 
        Exit ; 
      end ; 
      result := '' ; 
      i := 1 ; 
      case s[1] of 
        CHAR_PDP11M9312_CR: begin 
          symlen := 1 ; 
          again := true ; // die Carriage-Returns rausfiltern
        end; 
        '0'..'7': begin // octalzahl
          while (i <= length(s)) and isOctalDigit(s[i]) do inc(i) ; 
          if i > length(s) then // octalziffern bis Stringende: es kann noch mehr kommen
            raise EConsoleScannerInputIncomplete.Create('EConsoleScannerInputIncomplete 1') ; 
          CurSymType := symtypeOctal; // abgeschlossene Octalzahl
          symlen := i-1 ; 
        end ; 
        'A'..'Z': begin // start of command opcode
          // Letter,letter, ... number numebr
          i := 2 ; // process further chars
          // scan more letters
          while (i <= length(s)) and CharInSet(s[i], ['A'..'Z']) do inc(i) ; 
          // scan more numbers
          while (i <= length(s)) and isOctalDigit(s[i]) do inc(i) ; 

          if i > length(s) then // Zeichen bis Stringende: es kann noch mehr kommen
            raise EConsoleScannerInputIncomplete.Create('EConsoleScannerInputIncomplete 2') ; 
          CurSymType := symtypeOpcode; // abgeschlossenes Command
          symlen := i-1 ; 
        end{ "case s[1] of 'A'..'Z':" } ; 
        CHAR_PDP11M9312_LF: begin 
          symlen := 1 ; 
          CurSymType := symtypeEoln ; 
        end else begin // alle anderen Zeichen als String
          while (i <= length(s)) and not CharInSet(s[i], ['A'..'Z', '0'..'7', CHAR_PDP11M9312_CR, CHAR_PDP11M9312_LF]) do inc(i) ; 
          symlen := i-1 ; 
          CurSymType := symtypeOther ; 
        end ; 
      end{ "case s[1]" } ; 
      CurSymTxt := Copy(CurInputLine, nxtcharidx, symlen) ; 
      result := CurSymTxt ; 
      nxtcharidx := nxtcharidx + symlen ; 
    until not again ; 

    // Detailierte Ausgabe. Rest auf 32 Zeichen beschränken,
    // falls mal der SerialXfer-Zeichenstrom in den Parser gerät.
    Log('{Rcved Sym = (%s, "%s"), rest="%s"} ', [ 
            Symtype2text(CurSymType), CurSymTxt, 
            String2PrintableText(Copy(CurInputLine, nxtcharidx, 32), true)]) ;
  end{ "function TConsolePDP11M9312Scanner.NxtSym" } ; 


constructor TConsolePDP11M9312.Create(memorycellgroups: TMemoryCellGroups; 
        aMonitor_entryaddress: TMemoryAddress) ; 
  begin 
    inherited Create; 
    CommandTimeoutMillis := PDP11M9312_CMD_TIMEOUT ; 
    Prompt := '@' ; // default für M9312
    MMU := TPdp11MMU.Create(memorycellgroups) ; 
    RcvScanner := TConsolePDP11M9312Scanner.Create ; 
    monitor_entryaddress := aMonitor_entryaddress ; 
  end; 

destructor TConsolePDP11M9312.Destroy ; 
  begin 
    MMU.Free ; 
    RcvScanner.Free ; 
    inherited ; 
  end; 

function TConsolePDP11M9312.getName: string ; 
  begin 
    result := 'PDP-11 M9312 console' ; 
  end; 

// Terminal-Einstellungen für die PDP-11 M9312
function TConsolePDP11M9312.getTerminalSettings: TTerminalSettings ; 
  begin 
    result.Receive_CRisNewline := false ;  // CR wird als Füllzecihen eingesetzt
    result.Receive_LFisNewline := true ; //  LF macht Zeilenumbruch
    result.Backspace := #0 ; // M9312 console emulator löscht nie
    result.TabStop := 0 ; 
  end; 


function TConsolePDP11M9312.getFeatures: TConsoleFeatureSet ; 
  begin 
    // der M9312 console emulator kann nur Starten.
    // Nach HALT kann nur über das front panel wieder gestartet werden.
    // ich erstmal nicht!
    result := [ 
//            cfActionResetMachine, // reset unabhängig von run: INITIALIZE
            cfActionResetMaschineAndStartCpu // weiterlaufen mit Init: START
//            cfActionContinueCpu, // weiterlaufen ohne Init: GO bzw C
//            cfActionHaltCpu, // Console kann Program stoppen
//            cfActionSingleStep     // N matvi
//            cfMicroStep  // MICROSTEP
            ] ; 
    // kein cfFlagResetCpuSetsPC: "Reset" setzt den PC NICHT!

  end{ "function TConsolePDP11M9312.getFeatures" } ; 


function TConsolePDP11M9312.getPhysicalMemoryAddressType: TMemoryAddressType ; 
  begin 
    result := matPhysical16 ; // hat 16 Bit Adressen
  end; 


// return hauen und sicher stellen, dass antwort da ist.
procedure TConsolePDP11M9312.Resync ; 
  begin 
    try 
      BeginCriticalSection('Resync') ; // User sperren

//      examine_lastaddr := MEMORYCELL_ILLEGALVAL ; // ungültig, da 32 bit
      deposit_lastaddr.mat := matPhysical16 ; 
      deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; 

      ClearState ; 

      RcvScanner.Clear ; // unverarbeiteter Input weg
      Answerlines.Clear ; // erkannter Input weg
      // 2x Return hauen, es muss die Prompt "@" kommen
      // 1. Return wird verschluckt. Vorher register dump, der wird ignoriert
      // direkt nach Boot steht Prompt schon da
      if WaitForAnswer(phPrompt, CommandTimeoutMillis) = nil then begin 
        WriteToPDP(CHAR_CR) ; // probiere 1tes CR
        if WaitForAnswer(phPrompt, CommandTimeoutMillis) = nil then begin 
          // 2tes CR muss Anzeige bringen
          WriteToPDP(CHAR_CR) ; 
          CheckPrompt('Could not wake up PDP-11 M9312 console emulator') ; 
        end; 
      end; 
      Log('PDP-11 M9301/M9312 console emulator ready and prompting "%s"', [Prompt]) ;
    finally 
      EndCriticalSection('Resync') ; 
    end{ "try" } ; 
  end { "procedure TConsolePDP11M9312.Resync" } ; 


// COM initialisieren
procedure TConsolePDP11M9312.Init(aConnection: TSerialIoHub) ; 
  begin 
    inherited Init(aConnection) ; 

    Resync ; 
  end; 

// sagt der PDP-Console, dass sie nix mehr über den M9312 console emulator weiss
procedure TConsolePDP11M9312.ClearState ; 
  begin 
    inherited ; 
    deposit_lastaddr.mat := matPhysical16 ; 
    deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; 
    // erzwinge explizite Adressausgabe
  end; 


// Aus RcvScanner.CurInputLine die Antworten extrahieren
// und in die Colection AnswerLines schreiben
// erkennt im output von SimH die nächste Antwort phrase
// wird vom "onRcv"-Even aufgerufen - letztlich von SerialIoHub-PollTimer
function TConsolePDP11M9312.DecodeNextAnswerPhrase: boolean ; 

  var 
    curAnswerLine: TConsoleAnswerPhrase ; 

  procedure makePrompt ; 
    var haltAnswerline: TConsoleAnswerPhrase ; 
    begin 
      // Prompt:
      curAnswerLine := Answerlines.Add as TConsoleAnswerPhrase ; 
      curAnswerLine.phrasetype := phPrompt ; 

      // wenn vor der Prompt ein HALT kam, ist es jetzt die LETZTE Zeile
      // Achtung eben wurde gerade die Prompt eingefügt
      if Answerlines.Count > 1 then 
        haltAnswerline := Answerlines.Items[Answerlines.Count-2] as TConsoleAnswerPhrase 
      else haltAnswerline := nil ; 

      // Hier nicht das OnHalt event auslösen ... das will wahrscheinlich
      //Funktionen durchführen, (EXAMINE list), die wiederum vom background-empfang abhängig sind!
      // war die Phrase davor ein "phHalt", wird jetzt das OnHalt-Event ausgelöst
      if (haltAnswerline <> nil) and (haltAnswerline.phrasetype = phHalt) then begin 
        onExecutionStopPcVal := haltAnswerline.haltaddr ; 
        onExecutionStopDetected := true ; 

        // MonitorTimer kann jetzt OnExecutionStop auslösen
        assert(onExecutionStopPcVal.mat = matVirtual) ; 
      end else begin 
        onExecutionStopPcVal.val := MEMORYCELL_ILLEGALVAL ; 
        onExecutionStopDetected := false ; 
      end; 
    end{ "procedure makePrompt" } ; 

  // 4x Octalzahl. Die Registerausgabe gibt nur über den console emulator selber Auskunft
  // kommt nach reboot und nach viele Fehler.
  // wegen reboot wird es als halt interpretiert
  procedure makeHalt ; 
    begin 
      curAnswerLine := Answerlines.Add as TConsoleAnswerPhrase ; 
      curAnswerLine.phrasetype := phHalt ; 
      curAnswerLine.rawtext := '????' ; 
      curAnswerLine.haltaddr.mat := matVirtual ; 
      curAnswerLine.haltaddr.val := MEMORYCELL_ILLEGALVAL ; // PC ist unbekannt
      // halt -phrase löst noch nicht das OnExecutionStop-Event aus.
      // erst, wenn die nachfolgende cmd prompt erkannt wird
    end; 

// Es wurde E <addr> <val> gefunden: addrtext: octal addr
  procedure makeExamine(addrtxt, valtxt:string); 
    var 
      addr: TMemoryAddress ; 
      val: dword ; 
    begin 
      // ist es registername oder oktal?
      addr := OctalStr2Addr(addrtxt, matPhysical16) ; // exception
      assert(addr.val <> MEMORYCELL_ILLEGALVAL) ; 

      val := OctalStr2Dword(valtxt, 16) ; // exception?
      assert(val <> MEMORYCELL_ILLEGALVAL) ; 
//      if val <> MEMORYCELL_ILLEGALVAL then begin
      curAnswerLine := Answerlines.Add as TConsoleAnswerPhrase ; 
      curAnswerLine.phrasetype := phExamine ; 
      curAnswerLine.examineaddr := addr ; 
      curAnswerLine.examinevalue := val ; 
//      end ;
      // bei UNIBUS timeout gibts kein "?" oder sowas, der ganze console emualtor bleibt dann stehen!
//      else if valtxt = '?' then begin
//        // UNIBUS timeout ist gültige Antwort
//        CurAnswerline := Answerlines.Add as TConsoleAnswerPhrase ;
//        CurAnswerline.phrasetype := phExamine ;
//        CurAnswerline.examineaddr.mat := mat ;
//        CurAnswerline.examineaddr.val := MEMORYCELL_ILLEGALVAL ;  // adresse ist leider unbekannt
//        CurAnswerline.examinevalue := MEMORYCELL_ILLEGALVAL ; // gültiges Fehlersignal
//      end else
//        raise EConsoleScannerUnknownExpression.Create('EConsoleScannerIllegalExpression 3') ;
    end{ "procedure makeExamine" } ; 

  procedure makeOtherLine(s: string); 
    begin 
      curAnswerLine := Answerlines.Add as TConsoleAnswerPhrase ; 
      curAnswerLine.phrasetype := phOtherLine ; 
      curAnswerLine.rawtext := s ; 
      curAnswerLine.otherline := s ; 
    end; 


  var 
    //curline: string ;
    //i: integer ;
    //eoln: boolean ;
    s: string ; 
    //haltAnswerline: TConsoleAnswerPhrase ;
    addr_s, val_s: string ; 
  begin { "function TConsolePDP11M9312.DecodeNextAnswerPhrase" } 
    result := false ; 
    curAnswerLine := nil ; 

    with RcvScanner do begin 
      MarkParsePosition ; 
      try 
        // Eingabe-Ende vom letzten Scan löschen
        if CurSymType = symtypeEof then NxtSym ; // Incomplete, wenn nichts weiter da!

        // NextSym() erzeugt EConsoleScannerInputIncomplete, wenn Ende des Inputs
        // NextSym(false) tut das nicht
        LogStrCol(LogCol_DecodeNextAnswerPhrase, 'CurSym.Type=%s, .Txt="%s"', [
                TConsolePDP11M9312Scanner.Symtype2text(CurSymType), String2PrintableText(CurSymTxt, true)] );
        if CurSymType = symtypeEoln then begin 
          // Eingabezeile ist komplett: auswerten
          // erkennen:
          // 1) octal octal octal octal -> HALT
          // 2) "E " <adr> val>             phExam
          // andere opcodes: ignorieren

          // EOLN: alle zu ignorierenden Phrasen ausdrücklich hier eintragen!
          NxtSym ; 
          if CurSymTxt = Prompt then begin // <CR>@
            makePrompt ; 
            NxtSym(false) ; // input processed. keine "incomplete" exception, wenn abschlissende EOLN nicht da
            // ???? ODT: das '@' stehen lassen!
          end else if CurSymType = symtypeOctal then begin // <CR><octal>: HALT?
            // 4x <octal> hintereinander gilt als HALT
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymTxt <> ' ' then 
              raise EConsoleScannerUnknownExpression.Create('Space after 1st octal const expected for HALT'); 
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymType <> symtypeOctal then 
              raise EConsoleScannerUnknownExpression.Create('only 1 octal const, 4 expected for HALT'); 
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymTxt <> ' ' then 
              raise EConsoleScannerUnknownExpression.Create('Space after 2nd octal const expected for HALT'); 
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymType <> symtypeOctal then 
              raise EConsoleScannerUnknownExpression.Create('only 2 octal const, 4 expected for HALT'); 
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymTxt <> ' ' then 
              raise EConsoleScannerUnknownExpression.Create('Space after 3rd octal const expected for HALT'); 
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymType <> symtypeOctal then 
              raise EConsoleScannerUnknownExpression.Create('only 3 octal const, 4 expected for HALT'); 
            if length(CurSymTxt) <> 6 then 
              raise EConsoleScannerInputIncomplete.Create('4th octal const not yet complete') ; 
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymTxt <> ' ' then 
              raise EConsoleScannerUnknownExpression.Create('Space after 4th octal const expected for HALT'); 
            makeHalt ; 
            NxtSym ; // input processed
            // alle octal constanten sind jetzt weggelesen: ClenupInput läuft,
            // wenn keine Exception oder EConsoleScannerUnknownExpression kommt.
          end { "if CurSymType = symtypeOctal" } 
        end { "if CurSymType = symtypeEoln" } else // E <addr> val wid ach erkannt, wenn kein @ davor kommt
          if (CurSymType = symtypeOpcode) and (CurSymTxt = 'E') then begin // <CR><opcode>:
            // "opcode" ist letter, letter, number number,
            // also "E", "S" "D" , oder ein bootcode: "DL001"
            // examine: "E <octal-addr> <octal-val>"
            NxtSym ; 
            if CurSymTxt <> ' ' then 
              raise EConsoleScannerUnknownExpression.Create('Space after "E" expected for EXAMINE'); 
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymType <> symtypeOctal then 
              raise EConsoleScannerUnknownExpression.Create('octal addr expected after "E"'); 
            addr_s := CurSymTxt ; // merken
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymTxt <> ' ' then 
              raise EConsoleScannerUnknownExpression.Create('Space after <addr> expected for EXAMINE'); 
            NxtSym ; // raises "Incomplete" on Eof
            if CurSymType <> symtypeOctal then 
              raise EConsoleScannerUnknownExpression.Create('octal val expected after "E"'); 
            val_s := CurSymTxt ; // merken
            makeExamine(addr_s, val_s); 
            NxtSym ; // input processed
          end { "if (CurSymType = symtypeOpcode) and (CurSymTxt = 'E')" } else begin 
            // ? alles bis nächstes EOL ist unverstandene "OTHERLINE"
            s := CurSymTxt ; 
            NxtSym ; // raises "Incomplete" on Eof
            while CurSymType <> symtypeEoln do begin 
              s := s + CurSymTxt ; 
              NxtSym ;  // raises "Incomplete" on Eof
            end; // abort mit exception auch bei EOF
            makeOtherLine(s); 
            // kein NxtSym: EOLN stehen lassen
          end ; 
        // Verarbeiteten String abknipsen. NxtSym bleibt gültig
        CleanupInput ; 

        if curAnswerLine <> nil then begin // eine der make*() ist gelaufen.
          Log(curAnswerLine.AsText);
          result := true ;        // true, wenn phrase erkannt
        end; 

      except 
        on E: EConsoleScannerUnknownExpression do begin 
          // semantischer Fehler: den ganzen Ausdruck ignorieren
          CleanupInput ; 
          // JETZT nochmal versuchen!!!
          result := true ; 
          // oder: nochmal ab nächstem Symbol probieren?
          // RestoreParsePosition ;
          // NxtSym
        end; 
        on E: EConsoleScannerInputIncomplete do begin 
          // NxtSym ist auf ein unvollständiges Symbol gestossen,
          // oder ein NxtSym konnte nichts mehr liefern
          // nix zurückgeben, diese Funktion SPÄTER nochmal aufrufen, mit
          // mehr input.
          RestoreParsePosition ; 
          result := false ; 
        end; 
      end{ "try" } ; 
    end { "with RcvScanner" } ; 
(*

    // arbeite auf SerialRcDataBuffer
    // verarbeiter Pufferstart bis EOLN.
    i := 1 ;

    // leading EOLN  weg
    while (i <= length(RcvScanner.CurInputLine)) and CharInSet(RcvScanner.CurInputLine[i], [CHAR_PDP_CR, CHAR_PDP_LF])  do begin
      inc(i) ;
    end;
    RcvScanner.CurInputLine := Copy(RcvScanner.CurInputLine, i, maxint) ;

    // Scanne bis Zeilenende oder Prompt
    i := 1 ;
    curline := '' ;
    while (i <= length(RcvScanner.CurInputLine))
            and (curline <> PDP_PROMPT)
            and not CharInSet(RcvScanner.CurInputLine[i], [CHAR_PDP_CR, CHAR_PDP_LF])  do begin
      curline := curline + RcvScanner.CurInputLine[i] ;
      inc(i) ;
    end ;
    eoln := (i <= length(RcvScanner.CurInputLine)) and CharInSet(RcvScanner.CurInputLine[i], [CHAR_PDP_CR, CHAR_PDP_LF]) ; // vorzeitiger Stop, weil eoln getroffen

    // in curline steht jetzt die erste unverarbeitete Ausgabezeile
    if curline = PDP_PROMPT then begin
      // wenn vor der Prompt ein HALT kam, ist es jetzt die VORLETZTE Zeile. Point to it.
      if Answerlines.Count > 1 then
        haltanswerline := Answerlines.Items[Answerlines.Count-2] as  TConsoleAnswerPhrase
      else haltanswerline := nil ;

      curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
      curanswerline.phrasetype := phPrompt ;
      curanswerline.rawtext := curline ;

      // Hier nicht das OnHalt event auslösen ... das will wahrscheinlich
      // Funktionen durchführen, (EXAMINE list), die wiederum vom background-empfang abhängig sind!
      // war die Phrase davor ein "phHalt", wird jetzt das OnHalt-Event ausgelöst
      if (haltanswerline <> nil) and (haltanswerline.phrasetype = phHalt) then begin
        onExecutionStopPcVal := haltanswerline.haltaddr ;
        LogStrCol(LogCol_DecodeNextAnswerPhrase,  'prompt detected, onExecutionStopPcVal := haltanswerline.haltaddr') ;
        // MonitorTimer kann jetzt OnExecutionStop auslösen
      end else
        onExecutionStopPcVal.val := MEMORYCELL_ILLEGALVAL ;

    end { "if curline = PDP_PROMPT" } ;
    if (curanswerline = nil) and eoln then begin
      // Analyse der letzten vollständigen Zeile.
      // erkenne, ob es ein CPU-stop ist. Das heisst beim M9312 console emulator:
      // Wurde nach HALT der consolue emulator neu gebootetet?
      // -> dann gibt es den Register dump xxxxxx xxxxxxx xxxxxxx xxxxx
      // Den gibt allerdings auch so ständig!
      //
      // wichtig: phHalt muss die VORLETZTE Zeile werden, damit das folgende Prompt
      // dem phHalt zugeordnet wird.
      // die LETZTE Zeile wird das Examine!
      i := pos('17777707', curline) ;
      if i  = 1 then begin // muss am zeilenanfang stehen
        s := Copy(curline, i+9, 7) ;

        curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
        curanswerline.phrasetype := phHalt ;
        curanswerline.rawtext := curline ;
        curanswerline.haltaddr := OctalStr2Addr(s, matVirtual) ;

        curanswerline.Log ;
        // Trick: curanswerline auf nil ... dann wird es nochmal
        // geparst und als Examine_Ergebnis intepretiert
        curanswerline := nil ;
      end { "if i = 1" } ;
    end{ "if (curanswerline = nil) and eoln" } ;

    if (curanswerline = nil) and eoln then begin
      // Examine-answer format:
      //  <addr> <val>
      //  >>>
      curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
      curanswerline.phrasetype := phExamine ;
      curanswerline.rawtext := curline ;

      try // bei Formatfehler: curanswerline wieder löschen und := nil
        i := pos('?20 TRAN ERR', curline) ;
        if i > 0 then begin
          // UNIBUS timeout ist gültige Antwort
          curanswerline.examineaddr.mat := matPhysical16 ;
          curanswerline.examineaddr.val := MEMORYCELL_ILLEGALVAL ;  // adresse ist leider unbekannt
          curanswerline.examinevalue := MEMORYCELL_ILLEGALVAL ; // gültiges Fehlersignal
        end else begin
          s := ExtractWord(1, curline, [' ', #9]) ; // val extrahieren
          if s = ''  then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error
          curanswerline.examineaddr := OctalStr2Addr(s, matPhysical16) ; // exception
          if curanswerline.examineaddr.val = MEMORYCELL_ILLEGALVAL then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

          s := ExtractWord(2, curline, [' ', #9]) ; // val extrahieren
          curanswerline.examinevalue := OctalStr2Dword(s, 16) ; // exception?
          if curanswerline.examinevalue = MEMORYCELL_ILLEGALVAL then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

          s := ExtractWord(3, curline, [' ', #9]) ; // val extrahieren
          if s <> '' then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error
        end { "if i > 0 ... ELSE" } ;
      except
        curanswerline.Free ;
        curanswerline := nil ; // probiere nächsten Typ
      end{ "try" } ;

      if (curanswerline = nil) and eoln then begin
        curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
        curanswerline.phrasetype := phOtherLine ;
        curanswerline.rawtext := curline ;
        curanswerline.otherline := curline ;
      end;
    end{ "if (curanswerline = nil) and eoln" } ;

    if curanswerline <> nil then begin
      //curline ist verarbeitet, entferne es aus RcvScanner.CurInputLine
      RcvScanner.CurInputLine := Copy(RcvScanner.CurInputLine, length(curline)+1, maxint) ;
      curanswerline.Log ;
    end ; //else Log('invalid curAnswerPhrase, RcvScanner.CurInputLine="%s"', [RcvScanner.CurInputLine]) ;

    // true, wenn phrase erkannt
    result := curanswerline <> nil ;
  *)
  end{ "function TConsolePDP11M9312.DecodeNextAnswerPhrase" } ; 




procedure TConsolePDP11M9312.Deposit(addr: TMemoryAddress ; val: dword) ; 

  var s: string ; 
  begin 
    // VB: console ist bereit
    try 
      BeginCriticalSection('Deposit') ; // User sperren

      // PDP11/44 console kann nur physical: translate
      // DA MEIST CODE GESCHRIEBEN WIRD, IMMER INSTRUCTION MAP / ISPACE !
      assert(MMU.getPhysicalAddressType = matPhysical16) ; 
      if addr.mat = matVirtual then 
        addr := MMU.Virtual2PhysicalInstruction(addr) ; 
      assert(addr.mat = matPhysical16) ; 

      // die "globalen Register" R0..R7 können nicht geschrieben werden.
      if (addr.val >= global_register_base) and (addr.val < global_register_base + global_register_blocksize) then begin 
        // erfolglos!
        deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; 
      end else begin 
        if (deposit_lastaddr.val = MEMORYCELL_ILLEGALVAL) or (addr.val <> deposit_lastaddr.val+2) then begin 
          // Adresse für Deposit mus explizit geladen werden.
          Answerlines.Clear ; 
          WriteToPDP(Format('L %s' + CHAR_PDP11M9312_CR, [Dword2OctalStr(addr.val)])) ; 
          CheckPrompt('LOAD ADRESS failed, no prompt') ; 
          deposit_lastaddr := addr ; 
        end; 

        // Optimierung: bei aufsteigenden deposits braucht nur noch Data angegeben werden.
        s := Format('D %s' + CHAR_PDP11M9312_CR, [Dword2OctalStr(val)]) ; 
        Answerlines.Clear ; 
        WriteToPDP(s) ; 
        CheckPrompt('DEPOSIT failed, no prompt') ; 
        deposit_lastaddr := addr ; 
      end{ "if (addr.val >= global_register_base) and (addr.val < global_register_base + ...... ELSE" } ; 
    finally 
      EndCriticalSection('Deposit') ; 
    end { "try" } ; 
  end{ "procedure TConsolePDP11M9312.Deposit" } ; 




// alle edit_values setzen
function TConsolePDP11M9312.Examine(addr: TMemoryAddress): dword ; 
  var s: string ; 
    answerline: TConsoleAnswerPhrase ; 
  begin 
    result := 0 ; 
    // VB: console ist bereit

    try 
      BeginCriticalSection('Examine') ; // User sperren

      if addr.mat = matSpecialRegister then begin 
        // nur MEMORYCELL_SPECIALADDR_DISPLAYREG definerit, udn das ist nicht abfragtbar
        result := MEMORYCELL_ILLEGALVAL ; // kann aus 11/44 nicht abgefragt werden
        Exit ; 
      end ; 

      // PDP11/44 console kann nur physical: translate
      assert(MMU.getPhysicalAddressType = matPhysical16) ; 
      if addr.mat = matVirtual then 
        addr := MMU.Virtual2PhysicalData(addr) ; 
      assert(addr.mat = matPhysical16) ; 
      assert(addr.val <> MEMORYCELL_ILLEGALVAL) ; 

      // die "globalen Register" sind nicht zugreifbar, do noting
      if (addr.val >= global_register_base) and (addr.val < global_register_base + global_register_blocksize) then begin 
        result := MEMORYCELL_ILLEGALVAL ; 
        // s := Format('E/G %s' + CHAR_PDP11M9312_CR, [Dword2OctalStr(addr.val - global_register_base)]) ;
      end else begin 
        Answerlines.Clear ; 
        s := Format('L %s' + CHAR_PDP11M9312_CR, [Dword2OctalStr(addr.val)]) ; 
        deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; 
        WriteToPDP(s) ; // Adressregister setzen
        CheckPrompt('LOAD ADRESS failed, no prompt') ; 

        Answerlines.Clear ; // erkannter Input weg
        WriteToPDP('E ') ; // bewirkt Ausgabe von "<addr> <val>"
        // warte, bis Antwort auf Examine kam

        answerline := WaitForAnswer(phExamine, PDP11M9312_CMD_TIMEOUT) ; 
        if answerline = nil then 
          result := MEMORYCELL_ILLEGALVAL // keine Antwort, oder ? Fehler
        else if (answerline.examinevalue <> MEMORYCELL_ILLEGALVAL) and (answerline.examineaddr.val <> addr.val) then 
          // wenn ergebnis kommt, muss die gelieferte Adresse auch stimmen!
          raise Exception.CreateFmt('EXAMINE failure: request for addr %s, answer is %s!', 
                  [Dword2OctalStr(addr.val), answerline.rawtext]) 
        else result := answerline.examinevalue ; 
        CheckPrompt('EXAMINE failed, no prompt') ; 
      end { "if (addr.val >= global_register_base) and (addr.val < global_register_base + ...... ELSE" } ; 
    finally 
      EndCriticalSection('Examine') ; 
    end { "try" } ; 
  end{ "function TConsolePDP11M9312.Examine" } ; 


/////////////////////////////////////////////////
// Examine(list):
// Kernfunktion von PDP11GUI: schnell viele Memoryadressen auslesen
// optimiert eine ganze Memorylist auslesen
// Bricht bei 11M9312 leider ab, wenn BUS ERROR kommt, da dann der ganze console emulatr
// stehen bleibt.

procedure TConsolePDP11M9312.Examine(mcg: TMemoryCellgroup ; unknown_only: boolean ; abortable: boolean) ;

// Examine-Commandos für eine Liste ausgeben. addr_inc: 1 für globale register, sonst 2
// nur solche listmembers beachten, die tag = 0 haben (= noch nicht abgefragt sind)
// result: true, wenn alle cells abgefragt wurden
// false: neuer Aufruf ist nötig, mindestens die erste Zelle wurde abgefragt
// UNIBUSTIMEOUTS: abbruch, mindestens die 1. Zelle ist gesetzt (mit INVALID)
  function examineAddrList(list: TList ; addr_inc: integer ): boolean ; 
    const 
      max_block_len = 100 ; 
    var 
      blockstart, blockend: integer ; // start .. end-1 ist ein Block
      i: integer ; 
      mc: TMemoryCell ; 
      s_addr: string ; 
      answerline: TConsoleAnswerPhrase ; 
      block_failure: boolean ; // Blockabfrage ist durcheinander, UNIBUS TIMEOUT: restart nötig
      // min. die erste Adresse hat jetzt tag = 1
    begin 
      // Zusicherung: pro Aufruf wird immer mindestens eine weitere
      // Adresse mit tag = 1  markiert!
      result := false ; 

      // Default: alle ungelesenen Adressen ungültig setzen
      // Bei Zugriff auf unlesbare GPR R0..R7 kann dann einfach abgebrochen werdem.
      for i := 0 to list.Count - 1 do 
        with TMemoryCell(list[i]) do 
          if tag = 0 then 
            pdp_value := MEMORYCELL_ILLEGALVAL ; 

      // finde alle sequentiellen Bereiche. addressen physical vergleichen ... addr.tmpval!
      // finde erste, nicht abgefragte adresse
      blockstart := -1 ; 
      for i := 0 to list.Count - 1 do begin 
        if TMemoryCell(list[i]).tag = 0 then begin 
          blockstart := i ; 
          break ; 
        end; 
      end; 
      if blockstart = -1 then begin 
        result := true ; // alle memorycells wurden einmal abgefragt, oder list empty
        Exit ; 
      end; 

      block_failure := false ; 
      while not block_failure and (blockstart < list.Count) do begin 
        // finde einen block, in dem die Adressen um addr_inc aufsteigen
        // block darf aber nicht länger als max_block_len werden,
        // längere blöcke werden unterbrochen.
        blockend := blockstart+1  ; 
        while (blockend < list.Count) 
                and (TMemoryCell(list[blockend-1]).tag = 0) 
                and (TMemoryCell(list[blockend-1]).addr.tmpval + addr_inc = TMemoryCell(list[blockend]).addr.tmpval) 
                // blocklänge begrenzen
                and ((blockend-blockstart) < max_block_len) 
                do 
          inc(blockend) ; 

        //Log('!!!1 blockstart=%d, blockend=%d', [blockstart,blockend]) ;


        // Addresse vor jedem Block neu laden.
        // Sie könnte zwar zwischen zwei Blöcken gemerkt werden,
        // aber der User könnte sie ja über das Terminalwindow verstellen.

        // Die GPR R0..R7 sind nicht abfragbar: INVALID_VALUE stehen lassen
        if addr_inc = 1 then begin // KEIN register space
          result := true ; // ignore GPR
        end else if addr_inc = 2 then begin // KEIN register space

          Answerlines.Clear ; 

          s_addr := Dword2OctalStr(TMemoryCell(list[blockstart]).addr.tmpval,0) ; 

          // LOAD ADRESS
          WriteToPDP(Format('L %s', [s_addr]) + CHAR_PDP11M9312_CR) ; 
          CheckPrompt('LOAD ADRESS failed, no prompt') ; 

          deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; 

          // Adresse incrementiert jetzt bei folgenden EXAMINE immer um 2

          for i := blockstart to blockend-1 do begin 
            Answerlines.Clear ; 
            mc := TMemoryCell(list[i]) ; 
            Application.ProcessMessages ; // background receive
            WriteToPDP('E ') ; 
            // warte, bis Antwort auf Examine kam
            answerline := WaitForAnswer(phExamine, PDP11M9312_CMD_TIMEOUT) ; 
            if answerline = nil then begin 
              block_failure := true ; 
              break ; 
            end else begin 
              if  (answerline.examinevalue <> MEMORYCELL_ILLEGALVAL) and (answerline.examineaddr.val <> mc.addr.val) then 
                // wenn Ergebnis kommt, muss die gelieferte Adresse auch stimmen!
                raise Exception.CreateFmt('EXAMINE failure: request for addr %s, answer is %s!', 
                        [Dword2OctalStr(mc.addr.val), answerline.rawtext]) 
              else begin 
                mc.tag := 1 ; // wert ist jetzt abgefragt
                mc.pdp_value := answerline.examinevalue ; 
              end; 
            end; 
            CheckPrompt('EXAMINE failed, no prompt') ; 
          end{ "for i" } ; 
        end{ "if addr_inc = 2" } ; 

        // nächster Block
        blockstart := blockend ; 
      end { "while not block_failure and (blockstart < list.Count)" } ; 
    end{ "function examineAddrList" } ; 

  var i: integer ; 
    mc: TMemoryCell ; 
    listmem: TList ; // sortierte Liste mit Memoryadressen (inkrement 2) ;
    listcpureg: TList ; // sortierte Liste mit CPU-Regsiteradressen (inkrement 2) ;
  begin { "procedure TConsolePDP11M9312.Examine" }
    // a) memorycells sortiert auslesen
    // b) Memory/globalregister Bereiche unterscheiden, zusammenhängende
    //    Adressbereiche unterscheiden
    // c) sequentiell aufeinanderfolgende Adressen
    //    kombiniert mit pdp-11/44 "E/N" auslesen

    listmem := TList.Create ; 
    listcpureg := TList.Create ; 
    try 

      // list kann virtual sein ... mc's nach physical umrechnen
      // an hier ist .tmpval 16 bit!

      BeginCriticalSection('Examine') ; // User sperren
      BusyForm.Start('Examining ...', mcg.Count, abortable);
      for i := 0 to mcg.Count - 1 do begin
      BusyForm.StepIt ; if BusyForm.Aborted then Break ;
        mc := mcg.Cell(i) ;
        mc.tag := 0 ; // markiere als "noch keine antwort"
        if not unknown_only or (mc.pdp_value = MEMORYCELL_ILLEGALVAL) then begin 
          // "tmpval" aller memorycell.addr wird die physical addr
          // nur auszulesende Adressen in die Listen

          // PDP11/44 console kann nur physical: translate
          assert(MMU.getPhysicalAddressType = matPhysical16) ; 
          if mc.addr.mat = matVirtual then 
            mc.addr.tmpval := MMU.Virtual2PhysicalData(mc.addr).val 
          else 
            mc.addr.tmpval := mc.addr.val ; 

          if (mc.addr.tmpval >= global_register_base) and (mc.addr.tmpval < global_register_base + global_register_blocksize) then 
            listcpureg.Add(mc) 
          else listmem.Add(mc) ; 
        end{ "if not unknown_only or (mc.pdp_value = MEMORYCELL_ILLEGALVAL)" } ; 
      end{ "for i" } ; 

      listmem.Sort(MemoryCellSortCompare) ; // aufsteigend sortieren
      listcpureg.Sort(MemoryCellSortCompare) ; 

      // alle die analysieren, die tag = 0 haben
      // Abfrage kann mittendrin abbrechen, wenn UNIBUS timeout
      while not examineAddrList(listmem, 2) do ; // abfrage, bis alle Zellen mal dran waren
      while not examineAddrList(listcpureg, 1) do ; 

    finally 
      listmem.Free ; 
      listcpureg.Free ;
      BusyForm.Close ;
      EndCriticalSection('Examine') ; 
    end{ "try" } ; 
  end { "procedure TConsolePDP11M9312.Examine" } ; 


// Check, ob s mit prompt endet
// BSp: s = '$d$asim>', prompt = 'sim>' : true!
//  function CheckForPrompt(s:string ; prompt: string): boolean ;
//  begin
//    s := copy(s, length(s)-length(prompt), maxint) ;
//    result := (s = prompt) ;
//  end;


procedure TConsolePDP11M9312.ResetMachine(newpc_v: TMemoryAddress) ; // Maschine CPU reset
  begin 
    raise Exception.Create('TConsolePDP11M9312.ResetMachineAndStartCpu() not supported by PDP-11 M9312 console emulator') ; 
  end; 



// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP11M9312.ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ; // CPU starten.
  var s: string ; 
  begin 
    // START. Program läuft bis HALT.
    // console emulator nur durch "reboot" wieder aktivierbar

    // keine Prompt, CPU läuft jetzt
    try 
      BeginCriticalSection('ResetCpuAndStart') ; 
      assert(newpc_v.mat = matVirtual) ; 

      Answerlines.Clear ; 
      s := Format('L %s'+ CHAR_PDP11M9312_CR, [Dword2OctalStr(newpc_v.val, 16)]) ; 
      WriteToPDP(s) ; 
      CheckPrompt('LOAD ADRESS failed, no prompt') ; 
      deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; 

      Answerlines.Clear ; 
      s := Format('L %s'+ CHAR_PDP11M9312_CR, [Dword2OctalStr(newpc_v.val, 16)]) ; 
      WriteToPDP('S' + CHAR_PDP11M9312_CR) ; 
      //
    finally 
      EndCriticalSection('ResetCpuAndStart') ; ; 
    end{ "try" } ; 
  end{ "procedure TConsolePDP11M9312.ResetMachineAndStartCpu" } ; 


procedure TConsolePDP11M9312.ContinueCpu ; 
  begin 
    raise Exception.Create('TConsolePDP11M9312.ContinueCpu() not supported by PDP-11 M9312 console emulator') ; 
  end; 



//      function IsRunning: boolean ; virtual ; abstract ; // läuft die CPU (noch?)
// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP11M9312.HaltCpu(var newpc_v: TMemoryAddress) ; // CPU anhalten
  begin 
    raise Exception.Create('TConsolePDP11M9312.HaltCpu() not supported by PDP-11 M9312 console emulator') ; 
  end; 


// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP11M9312.SingleStep ; // einen Zyklus ausführen
  begin 
    raise Exception.Create('TConsolePDP11M9312.SingleStep() not supported by PDP-11 M9312 console emulator') ; 
  end; 

function TConsolePDP11M9312.getMonitorEntryAddress: TMemoryAddress ; 
  begin 
    result := monitor_entryaddress ; 
  end ; 


end{ "unit ConsolePDP11M9312U" } . 


