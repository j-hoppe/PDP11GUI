unit ConsolePDP1144U;
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
  Steuert über RS232 die PDP-11/44 Console an
  Die Anwendung ruft nur Deposit(), Examine()

  Der Zugriff auf die CPU-R0..R7 wird in die besonderen Examines
  "E/g 0..7" umgesetzt. Deposit genauso.
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
  CHAR_PDP_CR = #$d ;
  CHAR_PDP_LF = #$a ;
  CHAR_CONTROL_C = #$3 ;
  CHAR_CONTROL_P = #$10 ;
  PDP_CMD_TIMEOUT = 1000 ; // timeout für Antwort in Millisekunden. Long for slow telnet connections
  PDP_PROMPT = '>>>' ;

type

  // Scanner, umn den Output der PDP zu analysieren
  // macht noch nix, nur CurInputLine wird von eigener Chaos-Logik hier benutzt
  TConsolePDP1144Scanner = class(TConsoleScanner)
    public
      constructor Create ; override;
      function NxtSym(raiseIncompleteOnEof: boolean = true): string ; override ;
    end;




  TConsolePDP1144 = class(TConsoleGeneric)
    private
      deposit_lastaddr: TMemoryAddress ; //
    public
      isV340c: boolean ; // false: "clasic console". True: neue "V3.40C" variante

      constructor Create(memorycellgroups: TMemoryCellGroups) ; // die MMU legt eigene mmeorycells an
      destructor Destroy ; override ;

      // wenn serial: host='COM'
      procedure Init(aConnection: TSerialIoHub) ; override ;
      procedure ClearState ; override ;
      procedure Resync ; override ;


      function getName: string ; override ;
      function getTerminalSettings: TTerminalSettings ; override ;

      function getFeatures: TConsoleFeatureSet ; override ;
      function getPhysicalMemoryAddressType: TMemoryAddressType ; override ;

      procedure Deposit(addr: TMemoryAddress ; val: dword) ; override ;
      function Examine(addr: TMemoryAddress): dword ; overload ; override ;
      procedure Examine(mcg: TMemoryCellgroup ; unknown_only: boolean ; abortable:boolean) ; overload ; override ;
      // ganze Liste optimiert

      function DecodeNextAnswerPhrase: boolean ; override ;

      procedure ResetMachine(newpc_v: TMemoryAddress) ; override ; // Maschine reset
      procedure ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ; override ; // CPU starten.
      procedure ContinueCpu ; override ;

//      function IsRunning: boolean ; virtual ; abstract ; // läuft die CPU (noch?)
      procedure HaltCpu(var newpc_v: TMemoryAddress) ;  override ; // CPU anhalten
      procedure SingleStep ;  override ; // einen Zyklus ausführen

    end{ "TYPE TConsolePDP1144 = class(TConsoleGeneric)" } ;


implementation


uses
  Forms,
  OctalConst,
  AuxU,
  FormLogU,
  FormMainU ;

const
  // die 16 globalen Register R0..R7, R10..R17 werden kürzer addressiert
  global_register_base = _17777700 ;
  global_register_blocksize = 16 ;


constructor TConsolePDP1144Scanner.Create ;
  begin
    inherited ;
  end;


function TConsolePDP1144Scanner.NxtSym(raiseIncompleteOnEof: boolean = true): string ;
  begin
    raise Exception.Create('not implemented') ;
    result := '' ;
  end;


constructor TConsolePDP1144.Create(memorycellgroups: TMemoryCellGroups) ;
  begin
    inherited Create;
    CommandTimeoutMillis := PDP_CMD_TIMEOUT ;
    MMU := TPdp11MMU.Create(memorycellgroups) ;
    RcvScanner := TConsolePDP1144Scanner.Create ;
    isV340c := false ; // default
  end;

destructor TConsolePDP1144.Destroy ;
  begin
    MMU.Free ;
    RcvScanner.Free ;
    inherited ;
  end;

function TConsolePDP1144.getName: string ;
  begin
    result := 'PDP-11/44 console' ;
  end;

// Terminal-Einstellungen für die PDP-11/44
function TConsolePDP1144.getTerminalSettings: TTerminalSettings ;
  begin
    result.Receive_CRisNewline := true ;
    result.Receive_LFisNewline := false ; // ignoriere LF
    result.Backspace := #0 ; // PDP-11/44 hat das teletype-Backspace und löscht nie
    result.TabStop := 0 ;
  end;


function TConsolePDP1144.getFeatures: TConsoleFeatureSet ;
  begin
    // die 11/44 hat zwar einen HALT/CONTINUE switch, aber den beachte
    // ich erstmal nicht!
    result := [
            cfNonFatalHalt,
            cfNonFatalUNIBUStimeout,
            cfActionResetMachine, // reset unabhängig von run: INITIALIZE
            cfActionResetMaschineAndStartCpu, // weiterlaufen mit Init: START
            cfActionContinueCpu, // weiterlaufen ohne Init: GO bzw C
            cfActionHaltCpu, // Console kann Program stoppen
            cfActionSingleStep     // N matvi
//            cfMicroStep  // MICROSTEP
            ] ;
    // kein cfFlagResetCpuSetsPC: "Reset" setzt den PC NICHT!

  end{ "function TConsolePDP1144.getFeatures" } ;


function TConsolePDP1144.getPhysicalMemoryAddressType: TMemoryAddressType ;
  begin
    result := matPhysical22 ; // hat 22 Bit Adressen
  end;


// return hauen und sicher stellen, dass antwort da ist.
procedure TConsolePDP1144.Resync ;
  begin
    try
      BeginCriticalSection('Resync') ; // User sperren

//      examine_lastaddr := MEMORYCELL_ILLEGALVAL ; // ungültig, da 32 bit
      deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ;

      RcvScanner.Clear ; // unverarbeiteter Input weg
      Answerlines.Clear ; // erkannter Input weg
      // ^C hauen, es muss die Prompt ">>>" kommen
      WriteToPDP(CHAR_CONTROL_C) ;
      CheckPrompt('Could not wake up PDP-11/44') ;
      Log('PDP-11/44 ready and prompting ">>>"') ;
    finally
      EndCriticalSection('Resync') ;
    end{ "try" } ;
  end { "procedure TConsolePDP1144.Resync" } ;


// COM initialisieren
procedure TConsolePDP1144.Init(aConnection: TSerialIoHub) ;
  begin
    inherited Init(aConnection) ;

    Resync ;
  end;

// sagt der PDP-Console, dass sie nix mehr über die PDP-11/44 weiss
procedure TConsolePDP1144.ClearState ;
  begin
    inherited ;
//    examine_lastaddr := MEMORYCELL_ILLEGALVAL ; // erzwinge explizite Adressausgabe
    deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; // erzwinge explizite Adressausgabe
  end;


// Aus RcvScanner.CurInputLine die Antworten extrahieren
// und in die Colection AnswerLines schreiben
// erkennt im output von SimH die nächste Antwort phrase
// wird vom "onRcv"-Even aufgerufen - letztlich von SerialIoHub-PollTimer
function TConsolePDP1144.DecodeNextAnswerPhrase: boolean ;
  var
    curline: string ;
    i: integer ;
    eoln: boolean ;
    s: string ;
    addr_offset:dword ;
    haltanswerline, curanswerline: TConsoleAnswerPhrase ;
  begin
    result := false ;

    // arbeite auf SerialRcDataBuffer
    // verarbeiter Pufferstart bis EOLN.
    i := 1 ;
//      Log2Buffer('DecodeNextAnswerPhrase() Enter: RcvScanner.CurInputLine after = %s', [RcvScanner.CurInputLine]) ;

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

    curanswerline := nil ;
    // in curline steht jetzt die erste unverarbeitete Ausgabezeile
    if curline = PDP_PROMPT then begin
      // wenn vor der Prompt ein HALT kam, ist es jetzt die VORLETZTE Zeile
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
        onExecutionStopDetected := true ;
        LogStrCol(LogCol_DecodeNextAnswerPhrase,  'prompt detected, onExecutionStopPcVal := haltanswerline.haltaddr') ;
        // MonitorTimer kann jetzt OnExecutionStop auslösen
      end else begin
        onExecutionStopPcVal.val := MEMORYCELL_ILLEGALVAL ;
        onExecutionStopDetected := false ;
      end;

    end { "if curline = PDP_PROMPT" } ;
    if (curanswerline = nil) and eoln then begin
      // Analyse der letzten vollständigen Zeile
      // erkenne, ob es ein CPU-stop ist. PDP-11/44 gibt
      // bei Single step und Console-Halt gibt es nur den PC aus:
      ///////////////////
      // std: 17777707 <pc>
      // v340.c: Halted at <pc>
      ///////////////////
      //
      // -> Ausgabe des PC gilt als HALT.
      // Das kommt auch bei "E/G 7" == "EXAMINE PC" vor,
      // also muss das EXAMINE-PC als Halt und Examine-Ergebnis interpetiert werden.
      // wichtig: phHalt muss die VORLETZTE Zeile werden, damit das folgende Prompt
      // dem phHalt zugeordnet wird.
      // die LETZTE Zeile wird das Examine!
      s := '' ;
      if isV340c then begin
        i := pos('Halted at', curline) ;
        if i > 0 then
          s := Copy(curline, i+9, 7) ;
      end else begin
        i := pos('17777707', curline) ;
        if i = 1 then // muss am zeilenanfang stehen
          s := Copy(curline, i+9, 7) ;
      end;
      if s <> '' then begin

        curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
        curanswerline.phrasetype := phHalt ;
        curanswerline.rawtext := curline ;
        curanswerline.haltaddr := OctalStr2Addr(s, matVirtual) ;

        Log(curanswerline.AsText) ;
        // Trick: curanswerline auf nil ... dann wird es nochmal
        // geparst und als Examine_Ergebnis intepretiert
        curanswerline := nil ;
      end ;
    end{ "if (curanswerline = nil) and eoln" } ;

    if (curanswerline = nil) and eoln then begin
      // Examine-answer format:
      //  <addr> <val>
      //  >>>
      curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
      curanswerline.phrasetype := phExamine ;
      curanswerline.rawtext := curline ;

      try // bei Formatfehler: curanswerline wieder löschen und := nil
        if isV340c then begin
          // EXAMINE  on v3.40c console
          if (pos('?Bus timeout error?', curline) > 0) then begin
            // style v3.40
            // UNIBUS timeout ist gültige Antwort
            curanswerline.examineaddr.mat := matPhysical22 ;
            curanswerline.examineaddr.val := MEMORYCELL_ILLEGALVAL ;  // adresse ist leider unbekannt
            curanswerline.examinevalue := MEMORYCELL_ILLEGALVAL ; // gültiges Fehlersignal
          end else begin
            /// v340c:"    P   12345670 123456"
            ///   oder"    G   01 123456"  //R1
            s := ExtractWord(1, curline, [' ', #9]) ; // P or G ?
            if s = 'P' then addr_offset := 0
            else if s = 'G' then addr_offset := _17777700
            else
              raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

            s := ExtractWord(2, curline, [' ', #9]) ; // skip "P". addr extrahieren
            if s = ''  then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error
            curanswerline.examineaddr := OctalStr2Addr(s, matPhysical22) ; // exception
            curanswerline.examineaddr .val := curanswerline.examineaddr .val + addr_offset ;
            if curanswerline.examineaddr.val = MEMORYCELL_ILLEGALVAL then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

            s := ExtractWord(3, curline, [' ', #9]) ; // val extrahieren
            curanswerline.examinevalue := OctalStr2Dword(s, 16) ; // exception?
            if curanswerline.examinevalue = MEMORYCELL_ILLEGALVAL then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

            s := ExtractWord(4, curline, [' ', #9]) ; // test: more word in line?
            if s <> '' then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error
            // END V340c
          end { "if (pos('?Bus timeout error?', curline) > 0) ... ELSE" } ;
        end { "if isV340c" } else begin
          // EXAMINE on std console
          if (pos('?20 TRAN ERR', curline) > 0) then begin
            // UNIBUS timeout ist gültige Antwort
            curanswerline.examineaddr.mat := matPhysical22 ;
            curanswerline.examineaddr.val := MEMORYCELL_ILLEGALVAL ;  // adresse ist leider unbekannt
            curanswerline.examinevalue := MEMORYCELL_ILLEGALVAL ; // gültiges Fehlersignal
          end else begin
            // std: "00000000 222222"
            s := ExtractWord(1, curline, [' ', #9]) ; // val extrahieren
            if s = ''  then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error
            curanswerline.examineaddr := OctalStr2Addr(s, matPhysical22) ; // exception
            if curanswerline.examineaddr.val = MEMORYCELL_ILLEGALVAL then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

            s := ExtractWord(2, curline, [' ', #9]) ; // val extrahieren
            curanswerline.examinevalue := OctalStr2Dword(s, 16) ; // exception?
            if curanswerline.examinevalue = MEMORYCELL_ILLEGALVAL then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

            s := ExtractWord(3, curline, [' ', #9]) ; // val extrahieren
            if s <> '' then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

          end { "if (pos('?20 TRAN ERR', curline) > 0) ... ELSE" } ;
        end{ "if isV340c ... ELSE" } ;
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
      Log(curanswerline.AsText) ;
    end ; //else Log('invalid curAnswerPhrase, RcvScanner.CurInputLine="%s"', [RcvScanner.CurInputLine]) ;
//     Log2Buffer('DecodeNextAnswerPhrase() exit: RcvScanner.CurInputLine after = %s', [RcvScanner.CurInputLine]) ;

    // true, wenn phrase erkannt
    result := curanswerline <> nil ;

  end{ "function TConsolePDP1144.DecodeNextAnswerPhrase" } ;




procedure TConsolePDP1144.Deposit(addr: TMemoryAddress ; val: dword) ;

  var s: string ;
  begin
    // VB: console ist bereit
    try
      BeginCriticalSection('Deposit') ; // User sperren

      // PDP11/44 console kann nur physical: translate
      // DA MEIST CODE GESCHRIEBEN WIRD, IMMER INSTRUCTION MAP / ISPACE !
      assert(MMU.getPhysicalAddressType = matPhysical22) ;
      if addr.mat = matVirtual then
        addr := MMU.Virtual2PhysicalInstruction(addr) ;
      assert(addr.mat = matPhysical22) ;

      // die "globalen Register" mit "D/G i" adressieren
      if (addr.val >= global_register_base) and (addr.val < global_register_base + global_register_blocksize) then begin
        s := Format('D/G %s %s' + CHAR_PDP_CR,
                [Dword2OctalStr(addr.val - global_register_base), Dword2OctalStr(val)]) ;
        deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ;
      end else begin
        // Optimierung: bei aufsteigenden deposits kann statt addr ein "+" angegeben werden.
        if (deposit_lastaddr.val <> MEMORYCELL_ILLEGALVAL) and (addr.val = deposit_lastaddr.val+2) then
          s := Format('D + %s' + CHAR_PDP_CR, [Dword2OctalStr(val)])
        else
          s := Format('D %s %s'+CHAR_PDP_CR, [Dword2OctalStr(addr.val), Dword2OctalStr(val)]) ;
        deposit_lastaddr := addr ;
      end ;

      Answerlines.Clear ;

      WriteToPDP(s) ;
      CheckPrompt('DEPOSIT failed, no prompt') ;
    finally
      EndCriticalSection('Deposit') ;
    end { "try" } ;
  end{ "procedure TConsolePDP1144.Deposit" } ;




// alle edit_values setzen
function TConsolePDP1144.Examine(addr: TMemoryAddress): dword ;
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
      assert(MMU.getPhysicalAddressType = matPhysical22) ;
      if addr.mat = matVirtual then
        addr := MMU.Virtual2PhysicalData(addr) ;
      assert(addr.mat = matPhysical22) ;
      assert(addr.val <> MEMORYCELL_ILLEGALVAL) ;

      // die "globalen Register" mit "E/G i" adressieren
      if (addr.val >= global_register_base) and (addr.val < global_register_base + global_register_blocksize) then begin
        s := Format('E/G %s' + CHAR_PDP_CR, [Dword2OctalStr(addr.val - global_register_base)]) ;
//        examine_lastaddr := MEMORYCELL_ILLEGALVAL ;
      end else begin

        // Optimierung: bei aufsteigenden examines muss addr nicht immer neu angegeben werden.

//        if (examine_lastaddr <> MEMORYCELL_ILLEGALVAL) and (addr = examine_lastaddr+2) then
//          s := 'E' + CHAR_PDP_CR
//        else
        s := Format('E %s' + CHAR_PDP_CR, [Dword2OctalStr(addr.val)]) ;
//        examine_lastaddr := addr ;
      end ;

      Answerlines.Clear ;
      WriteToPDP(s) ;
      // warte, bis Antwort auf Examine kam
      answerline := WaitForAnswer(phExamine, PDP_CMD_TIMEOUT) ;
      if answerline = nil then
        result := MEMORYCELL_ILLEGALVAL // keine Antwort, oder ? Fehler
      else if (answerline.examinevalue <> MEMORYCELL_ILLEGALVAL) and (answerline.examineaddr.val <> addr.val) then
        // wenn ergebnis kommt, muss die gelieferte Adresse auch stimmen!
        raise Exception.CreateFmt('EXAMINE failure: request for addr %s, answer is %s!',
                [Dword2OctalStr(addr.val), answerline.rawtext])
      else begin
        Log('Single-examine: processing answer "'+(answerline.AsText)+'"') ;
        result := answerline.examinevalue ;
      end;
      CheckPrompt('EXAMINE failed, no prompt') ;
    finally
      EndCriticalSection('Examine') ;
    end { "try" } ;
  end{ "function TConsolePDP1144.Examine" } ;


/////////////////////////////////////////////////
// Examine(list):
// Kernfunktion von PDP11GUI: schnell viele Memoryadressen auslesen
// optimiert eine ganze Memorylist auslesen
// nicht abbrechen, wenn ?20 TRAN ERR kommt
// Antwort '?20 TRAN ERR' ist gültig, schnell behandeln

// Beispiel:
// >>>E/N:10 17772370
// 17772370 156735
// 17772372 156735
// 17772374 156735
// 17772376 156735
// ?20 TRAN ERR
// >>>
//  oder
// >>>E/N:10 17772400
// ?20 TRAN ERR
// >>>

// Die Addresse der "?20" muss aus dem Vorgänger geschlossen werden.


procedure TConsolePDP1144.Examine(mcg: TMemoryCellgroup ; unknown_only: boolean ; abortable: boolean) ;

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
      i, j: integer ;
      mc: TMemoryCell ;
      cmd, s_addr: string ;
      starttime: dword ;
      answerline: TConsoleAnswerPhrase ;
      found: boolean ;
      next_expected_addr: dword ;
      timeout: boolean ;
      ready: boolean ; // false: das frage/antwort-Spiel abbrechen
      block_failure: boolean ; // Blockabfrage ist durcheinander, UNIBUS TIMEOUT: restart nötig
      // min. die erste Adresse hat jetzt tag = 1
    begin { "function examineAddrList" }
      // Zusicherung: pro Aufruf wird immer mindestens eine weitere
      // Adresse mit tag = 1  markiert!
      result := false ;

      // finde alle sequentiellen Bereiche. addressen physical vergleichen ... addr.tmpval!
      // finde erste, nicht abgefragte adresse
      blockstart := -1 ;
      for i := 0 to list.Count - 1 do
        if TMemoryCell(list[i]).tag = 0 then begin
          blockstart := i ;
          break ;
        end;
      if blockstart = -1 then begin
        result := true ; // alle memorycells wurden einmal abgefragt, oder list empty
        Exit ;
      end;

      BusyForm.Start('Examining ...', list.Count, abortable) ;
      try
        block_failure := false ;
        while not BusyForm.Aborted and not block_failure and (blockstart < list.Count) do begin
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

          if addr_inc = 1 then begin // register space, "/G" - Option
            cmd := 'E/G' ;
            s_addr := Dword2OctalStr(TMemoryCell(list[blockstart]).addr.tmpval - global_register_base,0)
          end else begin
            cmd := 'E' ;
            s_addr := Dword2OctalStr(TMemoryCell(list[blockstart]).addr.tmpval,0) ;
          end;

          if blockend - blockstart > 1 then
            cmd := cmd + Format('/N:%s %s', [Dword2OctalStr(blockend - blockstart, 0), s_addr])
          else
            cmd := cmd + Format(' %s', [s_addr]) ; // länge 1, kein "/N:"
          Answerlines.Clear ;
          WriteToPDP(cmd + CHAR_PDP_CR) ;
//Log('!!!2 cmd=%s', [cmd]) ;

          // warten, bis
          // - alle adressen abgefragt wurden
          // - oder ILLEGAL VAL kam. Dann gabs an der adresse
          //   (letzteadresse+inc) ein UNIBUS TIMEOUT und multi-read brach ab
          // - timeout
          starttime := GetTickCount ; // Timeout reset
          ready := false ;
          // UNIBUS timeouts werden next_expected_addr zugeordnet
          next_expected_addr := TMemoryCell(list[blockstart]).addr.val ;
          repeat
            Application.ProcessMessages ; // background receive
            // kein hidden ProcessMessages in for(Answerlines): new receive!
            BusyForm.StepIt(Answerlines.Count) ;
            // alle Antworten analysieren
            for i := 0 to Answerlines.Count-1 do begin
              answerline := Answerlines.Items[i] as TConsoleAnswerPhrase ;
              if answerline.phrasetype = phExamine then begin
                Log('List-examine: processing answer "'+(answerline.AsText)+'"') ;
                starttime := GetTickCount ; // Timeout reset
                // gültiges address/wert paar, oder TIMEOUT
                if answerline.examinevalue = MEMORYCELL_ILLEGALVAL then begin
                  // die aktuelle Adresse verursachte einen Fehler,
                  // sie ist aber unbekannt!
                  answerline.examineaddr.val := next_expected_addr ;
                  block_failure := true ;
                end ;
                // finde memorycell über adresse
                found := false ;
                for j := blockstart to blockend-1 do begin
                  mc := TMemoryCell(list[j]) ;
                  if mc.addr.val = answerline.examineaddr.val then begin
                    mc.pdp_value := answerline.examinevalue ;
//Log('!!!3 mc[%s].tag = 1', [Dword2OctalStr(mc.addr.tmpval)]) ;
                    mc.tag := 1 ; // wert ist jetzt abgefragt
                    found := true ;
                    // calc next addr for "?20 TRAN ERR"
                    next_expected_addr := answerline.examineaddr.val + addr_inc ;
                  end;
                end;
                if not found then Log('NO Memorycell set by this answer!');

              end{ "if answerline.phrasetype = phExamine" } ;
            end{ "for i" } ;
            // alle antworten der pdp-11/44 sind jetzt abgearbeitet
            Answerlines.Clear ;

            // check, ob alle adressen abgefragt wurden
            ready := true ;
            for i := blockstart to blockend-1 do
              if TMemoryCell(list[i]).tag = 0 then
                ready := false ;
//Log('!!!4 ready=%d', [ord(ready)]) ;
            timeout := (starttime + PDP_CMD_TIMEOUT < GetTickCount) ;
          until timeout or ready or block_failure;
          if timeout then begin // keine Exception, sonst wird die ganze Useraktion abgebrochen
            Log('EXAMINE list failure: timeout waiting for list addr %s!',
                    [Dword2OctalStr(next_expected_addr, 22)]) ;
            result := true ; // keine weiteren versuche, Endlos-schleife!
          end;

          // nächster Block
          blockstart := blockend ;
        end { "while not BusyForm.Aborted and not block_failure and (blockstart < list.Count)" } ;
        if BusyForm.Aborted then result := true ; // do not retry
      finally
        BusyForm.Close ;
      end{ "try" } ;
    end{ "function examineAddrList" } ;

  var i: integer ;
    mc: TMemoryCell ;
    listmem: TList ; // sortierte Liste mit Memoryadressen (inkrement 2) ;
    listcpureg: TList ; // sortierte Liste mit CPU-Regsiteradressen (inkrement 2) ;
  begin { "procedure TConsolePDP1144.Examine" }
    // a) memorycells sortiert auslesen
    // b) Memory/globalregister Bereiche unterscheiden, zusammenhängende
    //    Adressbereiche unterscheiden
    // c) sequentiell aufeinanderfolgende Adressen
    //    kombiniert mit pdp-11/44 "E/N" auslesen

    listmem := TList.Create ;
    listcpureg := TList.Create ;
    try

      // list kann virtual sein ... mc's nach physical umrechnen
      // an hier ist .tmpval 22 bit!

      BeginCriticalSection('Examine') ; // User sperren
      for i := 0 to mcg.Count - 1 do begin
        mc := mcg.Cell(i) ;
        mc.tag := 0 ; // markiere als "noch keine antwort"
        if not unknown_only or (mc.pdp_value = MEMORYCELL_ILLEGALVAL) then begin
          // "tmpval" aller memorycell.addr wird die physical addr
          // nur auszulesende Adressen in die Listen

          // PDP11/44 console kann nur physical: translate
          assert(MMU.getPhysicalAddressType = matPhysical22) ;
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
      // Abfrage kann mittendrin abbrechen, wenn UNIBUS timeout = ?20 TRAN ERR
      while not examineAddrList(listmem, 2) do ; // abfrage, bis alle Zellen mal dran waren
      while not examineAddrList(listcpureg, 1) do ;

    finally
      listmem.Free ;
      listcpureg.Free ;
      EndCriticalSection('Examine') ;
    end{ "try" } ;
  end { "procedure TConsolePDP1144.Examine" } ;


// Check, ob s mit prompt endet
// BSp: s = '$d$asim>', prompt = 'sim>' : true!
//  function CheckForPrompt(s:string ; prompt: string): boolean ;
//  begin
//    s := copy(s, length(s)-length(prompt), maxint) ;
//    result := (s = prompt) ;
//  end;


procedure TConsolePDP1144.ResetMachine(newpc_v: TMemoryAddress) ; // Maschine CPU reset
  begin
    try
      BeginCriticalSection('ResetCpu') ;

      // Der PC wird nicht gesetzt => kein cfFlagResetCpuSetsPC

      // INITIALIZE
      // Ist UNIBUS reset -> alle Devices reset?
      Answerlines.Clear ;
      WriteToPDP('I'+ CHAR_PDP_CR) ;
      CheckPrompt('Reset failed, no prompt') ;
    finally
      EndCriticalSection('ResetCpu') ;
    end{ "try" } ;
  end{ "procedure TConsolePDP1144.ResetMachine" } ;



// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP1144.ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ; // CPU starten.
  var s: string ;
  begin
    try
      BeginCriticalSection('ResetCpuAndStart') ;
      assert(newpc_v.mat = matVirtual) ;
      // START
      // Ist mit UNIBUS reset -> alle Devices reset?
      s  := Format('S %s'+ CHAR_PDP_CR, [Dword2OctalStr(newpc_v.val, 16)]) ;
      Answerlines.Clear ;
      WriteToPDP(s) ;
      // keine Prompt, CPU läuft jetzt
    finally
      EndCriticalSection('ResetCpuAndStart') ; ;
    end;
  end{ "procedure TConsolePDP1144.ResetMachineAndStartCpu" } ;


procedure TConsolePDP1144.ContinueCpu ;
  begin
    try
      BeginCriticalSection('ContinueCpu') ;

      Answerlines.Clear ;
      WriteToPDP('C'+ CHAR_PDP_CR) ;
      // keine Prompt, CPU läuft jetzt

    finally
      EndCriticalSection('ContinueCpu') ;
    end;

  end{ "procedure TConsolePDP1144.ContinueCpu" } ;



//      function IsRunning: boolean ; virtual ; abstract ; // läuft die CPU (noch?)
// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP1144.HaltCpu(var newpc_v: TMemoryAddress) ; // CPU anhalten
  var answerline: TConsoleAnswerPhrase ;
  begin
    try
      BeginCriticalSection('HaltCpu') ;

      Answerlines.Clear ;

      // ^P  -> Terminal in Consolemode
      WriteToPDP(CHAR_CONTROL_P) ;// ^P
      sleep(100) ;
      // Kein Result, wenn CPU is already halted
      // HALT
      WriteToPDP('H'+ CHAR_PDP_CR) ;
      // Result: 17777707 <pc>
      answerline := WaitForAnswer(phHalt,PDP_CMD_TIMEOUT) ;
      if answerline = nil then raise Exception.Create('Stopping CPU failed, no answer.') ;

      CheckPrompt('Stopping CPU failed, no prompt') ;
      // jetzt wurde der Output mit ReadFromPdp() gelesen,
      // und MonitorPdpOutput() hat den CpuStop-Event erkannt
      newpc_v := onExecutionStopPcVal ;
    finally
      EndCriticalSection('HaltCpu') ; ;
    end{ "try" } ;
  end{ "procedure TConsolePDP1144.HaltCpu" } ;


// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP1144.SingleStep ; // einen Zyklus ausführen
  var answerline: TConsoleAnswerPhrase ;
    //pcaddr: TMemoryAddress ;
  begin
    try
      BeginCriticalSection('SingleStep') ;
      // SINGLE-INSTRUCTION-STEP

//      assert(newpc_v.mat = matVirtual) ;

      // PC setzen
      // Im PDP-11/44 Single step-Cmd kann er nicht nochmal angegeben werden.
//      pcaddr.mat := matPhysical22 ;
//      pcaddr.val := _17777707 ;
//      Deposit(pcaddr, newpc_v.val) ;

      Answerlines.Clear ;
      WriteToPDP('N 1'+ CHAR_PDP_CR) ;
      // Result: 17777707 <pc>

      // Es wird ein Stop erkannt
      answerline := WaitForAnswer(phHalt,PDP_CMD_TIMEOUT) ;
      if answerline = nil then raise Exception.Create('Single Step failed, no answer.') ;

      CheckPrompt('Single Step failed, no prompt') ;
      // ProcessNextAnswerPhrase() hat den CpuStop-Event erkannt
//      newpc_v := onExecutionStopPcVal ;
    finally
      EndCriticalSection('SingleStep') ;
    end{ "try" } ;
  end{ "procedure TConsolePDP1144.SingleStep" } ;



end{ "unit ConsolePDP1144U" } .


