unit ConsolePDP11SimHU;
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
  Steuert über Telnet den Emulatir SimH an
  Die Anwendung ruft nur Deposit(), Examine()

    Der Zugriff auf die CPU-R0..R7 wird in die besonderen Examines
   "E R0..R7" umgesetzt. Deposit genauso.
   SimH sagt sonst "illegal address space", es mapt die Register
   NICHt in den Adressraum

   SimH hat immer 22 Bit physical addresses, auch wenn er kleine PDP-11 emuliert!

}


interface

uses
  Classes, Windows, SysUtils,
  ConsoleGenericU,
  FormSettingsU,
  FormTerminalU,
  SerialIoHubU,
  AddressU,
  MemoryCellU,
  Pdp11MmuU,
  FormBusyU
  ;


const
  CHAR_SIMH_CR = #$d ;
  CHAR_SIMH_LF = #$a ;
  CHAR_SIMH_HALT = #5 ; // ^E
  SIMH_CMD_TIMEOUT = 1000 ; // telnet verbindung ist lahm: warte lange!
  SIMH_PROMPT = 'sim> ' ;

type

  // Scanner, umn den Output der PDP zu analysieren
  // macht noch nix, nur CurInputLine wird von eigener Chaos-Logik hier benutzt
  TConsoleSimHScanner = class(TConsoleScanner)
    public
      constructor Create ; override;
      function NxtSym(raiseIncompleteOnEof: boolean = true): string ; override ;
    end;



  TConsolePDP11SimH = class(TConsoleGeneric)
    private

      examine_lastaddr: TMemoryAddress ; //
      deposit_lastaddr: TMemoryAddress ; //

    public
      constructor Create(memorycellgroups: TMemoryCellGroups) ;// die MMU legt eigene mmeorycells an
      destructor Destroy ; override ;

      procedure Init(aConnection: TSerialIoHub) ; override ;

      procedure ClearState ; override ;
      procedure Resync ; override ;

      function getName: string ; override ;
      function getTerminalSettings: TTerminalSettings ; override ;

      function getFeatures: TConsoleFeatureSet ; override ;
      function getPhysicalMemoryAddressType: TMemoryAddressType ; override ;

      procedure Deposit(addr: TMemoryAddress ; val: dword) ; overload ; override ;
      procedure Deposit(mcg: TMemoryCellGroup ; optimize:boolean; abortable:boolean) ; overload ; override ;
      function Examine(addr: TMemoryAddress): dword ; overload ;override ;
      procedure Examine(mcg: TMemoryCellGroup ; unknown_only: boolean; abortable:boolean) ;overload ;override ;

      function DecodeNextAnswerPhrase: boolean ; override ;
      procedure ResetMachine(newpc_v: TMemoryAddress) ; override ; // Maschine reset
      procedure ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ; override ; // CPU starten.
//      function IsRunning: boolean ; virtual ; abstract ; // läuft die CPU (noch?)
      procedure ContinueCpu ;  override ; // CPU anhalten
      procedure HaltCpu(var newpc_v: TMemoryAddress) ;  override ; // CPU anhalten
      procedure SingleStep ;  override ; // einen Zyklus ausführen


    end{ "TYPE TConsolePDP11SimH = class(TConsoleGeneric)" } ;


implementation


uses
  Forms,
  JH_Utilities,
  OctalConst,
  AuxU,
  FormMainU ;


constructor TConsoleSimHScanner.Create ;
  begin
    inherited ;
  end;

function TConsoleSimHScanner.NxtSym(raiseIncompleteOnEof: boolean = true): string ;
  begin
    raise Exception.Create('not implemented') ;
    result := '' ;
  end;




// ermittelt den symbolischen SimH-Registernamen zu einer 22bit-UNIBUS-Adresse
// '', wenn keiner gefunden
// Problem: die CPU-Register addressiert SimH nicht über UNIBUS-Adressen!

// diese Liste hier ist Model-abhängig .. eigentlich sollte sie aus
// der .ini-Datei mit geladen werden!
//function addr2regname(physicaladdrval:dword): string ;
function addr2regname(addr: TMemoryAddress; use_tmpval: boolean = false): string ;
  var val: dword ;
  begin
    if use_tmpval then
      val := addr.tmpval
    else val := addr.val ;
    result := '' ;
    case addr.mat of
      matPhysical22:
        case val of
          _17777700: result := 'R0' ; // 17777700
          _17777701: result := 'R1' ;
          _17777702: result := 'R2' ;
          _17777703: result := 'R3' ;
          _17777704: result := 'R4' ;
          _17777705: result := 'R5' ;
          _17777706: result := 'SP' ;
          _17777707: result := 'PC' ;

          _17777710.._17777717: result := '?' ; // marker: das nicht abfragen, ist SimH unbekannt!


          _17777766: result := 'CPUERR' ;
          _17777772: result := 'PIRQ' ;
          _17777776: result := 'PSW' ;
//        $7fffc0 + 6: result := 'KSP' ; // kernel stack pointer
//        $7fffc0 + 6: result := 'SSP' ; // supervisor stack pointer
//        $7fffc0 + 6: result := 'USP' ; // user stack pointer
        end { "case val" } ;
      matVirtual: ;
      matSpecialRegister:
        case val of
          // auf 'e -d dr' wird mit 'DR: xxxx' geantwortet
          MEMORYCELL_SPECIALADDR_DISPLAYREG:
            result := 'DR' ;
          MEMORYCELL_SPECIALADDR_SWITCHREG:
            result := 'SR' ;
        end ;
    end{ "case addr.mat" } ;
  end{ "function addr2regname" } ;


function regname2addr(regname:string): TMemoryAddress ;
  begin
    result.mat := matPhysical22 ;
    result.val := MEMORYCELL_ILLEGALVAL ;
    if regname = 'R0' then result.val := _17777700 ;
    if regname = 'R1' then result.val := _17777701 ;
    if regname = 'R2' then result.val := _17777702 ;
    if regname = 'R3' then result.val := _17777703 ;
    if regname = 'R4' then result.val := _17777704 ;
    if regname = 'R5' then result.val := _17777705 ;
    if regname = 'SP' then result.val := _17777706 ;
    if regname = 'PC' then result.val := _17777707 ;

    if regname = 'CPUERR' then result.val := _17777766 ;
    if regname = 'PIRQ' then result.val := _17777772 ;
    if regname = 'PSW' then result.val := _17777776 ;

    if regname = 'DR' then begin
      result.mat := matVirtual ;
      result.val := MEMORYCELL_SPECIALADDR_DISPLAYREG ;
    end;
    if regname = 'SR' then begin
      result.mat := matVirtual ;
      result.val := MEMORYCELL_SPECIALADDR_SWITCHREG ;
    end;

  end{ "function regname2addr" } ;



constructor TConsolePDP11SimH.Create(memorycellgroups: TMemoryCellGroups) ;
  begin
    inherited Create ;
    CommandTimeoutMillis := SIMH_CMD_TIMEOUT ;
    MMU := TPdp11MMu.Create(memorycellgroups) ;
    RcvScanner := TConsoleSimHScanner.Create ;
  end;

destructor TConsolePDP11SimH.Destroy ;
  begin
    RcvScanner.Free ;
    inherited ;
  end;


function TConsolePDP11SimH.getName: string ;
  begin
    result := 'SimH PDP-11 console' ;
  end;

// Terminal-Einstellungen für SimH
function TConsolePDP11SimH.getTerminalSettings: TTerminalSettings ;
  begin
//    result.Receive_CRisCRLF := false ; // Unser "Enter" gibt CR
//    result.Receive_LFisCRLF := false ; // Ausgabe bricht Zeilen mit LF um
    result.Receive_CRisNewline := true ; // Unser "Enter" gibt CR
    result.Receive_LFisNewline := true ; // Telnet bricht Zeilen mit LF um
    result.Backspace := #8 ; //
    result.TabStop := 8 ;
  end;

function TConsolePDP11SimH.getFeatures: TConsoleFeatureSet ;
  begin
    result := [
            cfNonFatalHalt,
            cfNonFatalUNIBUStimeout,
            cfActionResetMachine, // reset unabhängig von run: RESET ALL
            cfActionResetMaschineAndStartCpu, // weiterlaufen mit Init: RUN.
            cfActionContinueCpu, // weiterlaufen ohne Init: GO bzw C
            cfActionHaltCpu, // Console kann Program stoppen
            cfActionSingleStep
            // cfMicroStep
            ] ;
    // kein cfFlagResetCpuSetsPC: "Reset" setzt den PC NICHT!
  end{ "function TConsolePDP11SimH.getFeatures" } ;


function TConsolePDP11SimH.getPhysicalMemoryAddressType: TMemoryAddressType ;
  begin
    // SimH hat immer 22 Bit physical addresses, auch wenn er kleine PDP-11 emuliert!
    result := matPhysical22 ;
  end;


// Aus SerialRcvDataBuffer die Antworten extrahieren
// und in die Colection AnswerLines schreiben
// erkennt im output von SimH die nächste Antwort phrase
// wird vom "onRcv"-Even aufgerufen - letztlich von SerialIoHub-PollTimer
function TConsolePDP11SimH.DecodeNextAnswerPhrase: boolean ;
  var
    curline: string ;
    i: integer ;
    eoln: boolean ;
    s: string ;
    haltanswerline, curanswerline: TConsoleAnswerPhrase ;
  begin
    result := false ;

    // arbeite auf SerialRcDataBuffer
    // verarbeiter Pufferstart bis EOLN.
    i := 1 ;

    // leading EOLN  weg
    while (i <= length(RcvScanner.CurInputLine)) and CharInSet(RcvScanner.CurInputLine[i], [CHAR_SIMH_CR, CHAR_SIMH_LF])  do begin
      inc(i) ;
    end;
    RcvScanner.CurInputLine := Copy(RcvScanner.CurInputLine, i, maxint) ;

    // Scanne bis Zeilenende oder Prompt
    i := 1 ;
    curline := '' ;
    while (i <= length(RcvScanner.CurInputLine))
            and (curline <> SIMH_PROMPT)
            and not CharInSet(RcvScanner.CurInputLine[i],[CHAR_SIMH_CR, CHAR_SIMH_LF])  do begin
      curline := curline + RcvScanner.CurInputLine[i] ;
      inc(i) ;
    end ;
    eoln := (i <= length(RcvScanner.CurInputLine)) and CharInSet(RcvScanner.CurInputLine[i], [CHAR_SIMH_CR, CHAR_SIMH_LF]) ; // vorzeitiger Stop, weil eoln getroffen


    curanswerline := nil ;
    // in curline steht jetzt die erste unverarbeitete Ausgabezeile von SimH
    if curline = SIMH_PROMPT then begin
      // merke die Zeile vor der Prompt
      if Answerlines.Count > 0 then
        haltanswerline := Answerlines.Items[Answerlines.Count-1] as  TConsoleAnswerPhrase
      else haltanswerline := nil ;

      curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
      curanswerline.phrasetype := phPrompt ;
      curanswerline.rawtext := curline ;

      // Hier nicht as OnHalt event auslösen ... das will wahrscheinlich
      //Funktionen durchführen, (EXAMINE list), die wiederum vom background-empfang abhängig sind!
      // war die Phrase davor ein "phHalt", wird jetzt das OnHalt-Event ausgelöst
      if (haltanswerline <> nil) and (haltanswerline.phrasetype = phHalt) then begin
        onExecutionStopPcVal := haltanswerline.haltaddr ;
        onExecutionStopDetected := true ;
        // MonitorTimer kann jetzt OnExecutionStop auslösen
      end else begin
        onExecutionStopPcVal.val := MEMORYCELL_ILLEGALVAL ;
        onExecutionStopDetected := false ;
      end;
    end { "if curline = SIMH_PROMPT" } ;

    if (curanswerline = nil) and eoln then begin
      // Analyse der letzten vollständigen Zeile
      if (pos('SIMULATION STOPPED', Uppercase(curline)) > 0)
              or (pos('HALT', Uppercase(curline)) > 0)
              or (pos('STEP EXPIRED', Uppercase(curline)) > 0)
              or (pos('TRAP', Uppercase(curline)) > 0)
              or (pos('BREAKPOINT', Uppercase(curline)) > 0)
        then begin

          // erkenne, ob es ein CPU-stop ist
          //   Simulation stopped, PC: 002502 (MOV (SP)+,177776)
          // oder
          //  HALT instruction, PC: 000114 (SWAB (R0)+)
          // oder
          //  Step expired, PC: 000006 (SWAB -(R0))
          // Es gibt den virtuellen PC aus

          i := pos('PC:', curline) ;
          if i > 0 then begin // else: unknown error!
            s := Trim(Copy(curline, i+3, 7)) ;
            curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
            curanswerline.phrasetype := phHalt ;
            curanswerline.rawtext := curline ;
            curanswerline.haltaddr := OctalStr2Addr(s, matVirtual) ;
            // halt -phrase löst noch nicht das OnExecutionStop-Event aus.
            // erst, wenn die cmd prompt erkannt wird
          end ;
        end{ "if (pos('SIMULATION STOPPED', Uppercase(curline)) > 0) or (pos('HALT', Upperc..." } ;
      if (curanswerline = nil) and eoln then begin
        // examineanswer: Form "addr: value", addr auch PC, RO, ...
        curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
        curanswerline.phrasetype := phExamine ;
        curanswerline.rawtext := curline ;
        try // bei Formatfehler: curanswerline wieder löschen und := nil
          i := pos('Address space exceeded', curline) ;
          if i > 0 then begin
            // UNIBUS timeout ist gültige Antwort
            curanswerline.examineaddr.mat := matPhysical22 ;
            curanswerline.examineaddr.val := MEMORYCELL_ILLEGALVAL ;  // adresse ist leider unbekannt
            curanswerline.examinevalue := MEMORYCELL_ILLEGALVAL ; // gültiges Fehlersignal
          end else begin

            // es kommt <addr>:  <val> zurück
            s := ExtractWord(1, curline, [' ', #9]) ; // val extrahieren
            if (s = '') or (s[length(s)] <> ':') then raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

            s := Copy(s, 1, length(s)-1) ;
            curanswerline.examineaddr := regname2addr(s) ;
            // ist es registername oder oktal?
            if curanswerline.examineaddr.val = MEMORYCELL_ILLEGALVAL then
              curanswerline.examineaddr := OctalStr2Addr(s, matPhysical22) ; // exception
            if curanswerline.examineaddr.val = MEMORYCELL_ILLEGALVAL then
              raise EConsoleScannerUnknownExpression.Create('no examine answer') ; //no error

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
      end { "if (curanswerline = nil) and eoln" } ;

      if (curanswerline = nil) and eoln then begin
        curanswerline := Answerlines.Add as TConsoleAnswerPhrase ;
        curanswerline.phrasetype := phOtherLine ;
        curanswerline.rawtext := curline ;
        curanswerline.otherline := curline ;
      end;
    end{ "if (curanswerline = nil) and eoln" } ;

    if curanswerline <> nil then begin
      //curline ist verarbeitet, entferne es aus SerialRcvDataBuffer
      RcvScanner.CurInputLine := Copy(RcvScanner.CurInputLine, length(curline)+1, maxint) ;
      Log(curanswerline.AsText) ;
    end ; //else Log('invalid curAnswerPhrase, SerialRcvDataBuffer="%s"', [SerialRcvDataBuffer]) ;

    // true, wenn phrase erkannt
    result := curanswerline <> nil ;

  end{ "function TConsolePDP11SimH.DecodeNextAnswerPhrase" } ;



procedure TConsolePDP11SimH.Resync ;
  begin
    try
      BeginCriticalSection ; // User sperren

      examine_lastaddr.val := MEMORYCELL_ILLEGALVAL ; // ungültig, da 32 bit
      deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ;

      // Irgendwas eingeben, es muss die Prompt "sim>" kommen.
      // Ein einzelnes "RETURN" wiederholt das letzte Kommando ...
      // unvorhersehbare Folgen!
      RcvScanner.Clear ; // unverarbeiteter Input weg
      Answerlines.Clear ; // erkannter Input weg

      // nur NACH diesem Befehel kann mit EXAMINE auf den iospace zugegriffen
      // werden ????
      WriteToPDP('sh cpu iospace'+ CHAR_SIMH_CR) ;
      CheckPrompt('Could not wake up SimH');

      // receive polling schnell stellen, versuche, 38400 baud zu erreichen

      // poll-arg = simulated PDP-11 instruction between polls
      // execution speed is almost unknown ...
      // if executing at 5 MHZ, baudrate =38400
      // char rate = 3840 , timing = 5000000/3840 = 1300

      WriteToPDP('set throttle 5M'+ CHAR_SIMH_CR) ; // 5MIPS
      CheckPrompt('"set throttle" failed');
      WriteToPDP('deposit tti time 1300'+ CHAR_SIMH_CR) ;
      CheckPrompt('"deposit tti time" failed');

      // die letzten 100 Befehle speichern
      WriteToPDP('SET CPU HISTORY=100'+ CHAR_SIMH_CR) ;
      CheckPrompt('"deposit tti time" failed');

      // Quatsch eingeben, damit ein einzelnes Return nix wiederholt.
//      WriteToPDP('Hello, SimH!'+ CHAR_SIMH_CR) ;
//      CheckPrompt('Could not wake up SimH');
      Log('SimH ready and prompting "sim>"') ;
    finally
      OutputDebugString('Resync ends') ;
      EndCriticalSection ;
    end { "try" } ;
  end { "procedure TConsolePDP11SimH.Resync" } ;

// Telnet initialisieren
// return hauen und sicher stellen, dass Antwort da ist.

procedure TConsolePDP11SimH.Init(aConnection: TSerialIoHub) ;
  var i: integer ;
  begin
    try
      BeginCriticalSection ; // User sperren
      inherited Init(aConnection) ;
      // 1. Sek warten, die Startausgabe weg lesen!
      for i := 1 to 100 do begin
        Application.ProcessMessages ;  // Background Empfang
        sleep(10) ;
//aConnection.Physical_Poll(nil);
      end;

      Resync ;
    finally
      OutputDebugString('Init ends') ;
      EndCriticalSection ;
    end { "try" } ;
  end{ "procedure TConsolePDP11SimH.Init" } ;



procedure TConsolePDP11SimH.Deposit(addr: TMemoryAddress ; val: dword) ;
  var s, regname: string ;
  begin
    // VB: console ist bereit
    try
      BeginCriticalSection ; // User sperren

      regname := '' ;
      if addr.mat = matSpecialRegister then begin
        regname := addr2regname(addr) ;
      end else begin
        // addr kann auch Virtual sein! SimH kann das! bis auf weiters aber
        // nicht benutzen, bis verhalten klar!
        assert(MMU.getPhysicalAddressType = matPhysical22) ;
        if addr.mat = matVirtual then
          addr := MMU.Virtual2PhysicalData(addr) ;
        assert(addr.mat = matPhysical22) ;
        assert(addr.val <> MEMORYCELL_ILLEGALVAL) ;

        regname := addr2regname(addr) ;
      end;
      if regname <> '' then begin // es ist ein CPU-Register
        s := Format('D %s %s'+CHAR_SIMH_CR, [regname, Dword2OctalStr(val)]) ;
        deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ;
      end else begin
        s := Format('D %s %s'+CHAR_SIMH_CR, [Dword2OctalStr(addr.val), Dword2OctalStr(val)]) ;
        deposit_lastaddr := addr ;
      end;

      Answerlines.Clear ;
      WriteToPDP(s) ;
      CheckPrompt('DEPOSIT failed, no prompt') ;
    finally
      OutputDebugString('Deposit ends') ;
      EndCriticalSection ;
    end { "try" } ;
  end{ "procedure TConsolePDP11SimH.Deposit" } ;


procedure TConsolePDP11SimH.Deposit(mcg: TMemoryCellGroup ; optimize:boolean; abortable:boolean) ;
  var
    i: integer ;
    mc: TMemoryCell ;
    s, regname: string ;
    addr: TMemoryAddress ;
    tmpdir: string ;
    foutname: string ; // temporärer file
    fout: System.Text ;
  begin
    // simh muss auf der lokalen Machine laufen, sonst gibts keinen Zugriff auf den
    // script-file
    if not Connection.isLocalTelnet then begin
      inherited Deposit(mcg, optimize, abortable) ;
      Exit ;
    end ;


    if not GetEnv('TEMP', tmpdir) then
      raise Exception.Create('Environment variable TEMP not set!') ;
    foutname := GetUniqueFilename(tmpdir, 'pdp11gui_deposit', 'sim') ;
    try
      try
        Log('TConsolePDP11SimH.Deposit(): writing file %s', [foutname]) ;
        AssignFile(fout, foutname) ; Rewrite(fout) ;
        for i := 0 to mcg.Count - 1 do begin
          mc := mcg.Cell(i) ;
          if not optimize or (mc.pdp_value <> mc.edit_value) then begin
            addr := mc.addr ;
            // derselbe Code wie in "Deposit()
            regname := '' ;
            if addr.mat = matSpecialRegister then begin
              regname := addr2regname(addr) ;
            end else begin
              // addr kann auch Virtual sein! SimH kann das! bis auf weiters aber
              // nicht benutzen, bis verhalten klar!
              assert(MMU.getPhysicalAddressType = matPhysical22) ;
              if addr.mat = matVirtual then
                addr := MMU.Virtual2PhysicalData(addr) ;
              assert(addr.mat = matPhysical22) ;
              assert(addr.val <> MEMORYCELL_ILLEGALVAL) ;

              regname := addr2regname(addr) ;
            end;
            if regname <> '' then begin // es ist ein CPU-Register
              s := Format('D %s %s', [regname, Dword2OctalStr(mc.edit_value)]) ;
            end else begin
              s := Format('D %s %s', [Dword2OctalStr(addr.val), Dword2OctalStr(mc.edit_value)]) ;
            end;
            writeln(fout, s) ;
          end { "if not optimize or (mc.pdp_value <> mc.edit_value)" } ;
        end { "for i" } ;
      finally
        // file schliessen
        try CloseFile(fout) ;except ;end ;
      end { "try" } ;


      // execeute "DO" command
      s := Format('DO "%s"'+CHAR_SIMH_CR, [foutname]) ;
      Answerlines.Clear ;
      WriteToPDP(s) ;
      CheckPrompt('DEPOSIT-DO failed, no prompt') ;

    finally
      // file wieder löschen
      try Erase(fout) ;except ;end ;
    end { "try" } ;

    // sync: same loop as above
    for i := 0 to mcg.Count - 1 do begin
      mc := mcg.Cell(i) ;
      if not optimize or (mc.pdp_value <> mc.edit_value) then begin
        mc.pdp_value := mc.edit_value ;
        // dieselbe Zelle in NachbarGroups aktualisieren,
        // dort werden die OnMemoryCellChange-Callbacks aufgerufen
        if mcg.Collection <> nil then // nil bei code window
          (mcg.Collection as TMemoryCellGroups).SyncMemoryCells(mc) ;
      end;
    end;
  end{ "procedure TConsolePDP11SimH.Deposit" } ;


// alle edit_values setzen
function TConsolePDP11SimH.Examine(addr: TMemoryAddress): dword ;
  var s, regname: string ;
    answerline: TConsoleAnswerPhrase ;
  begin
    // VB: console ist bereit
    try
      BeginCriticalSection ; // User sperren

      regname := '' ;

      if addr.mat = matSpecialRegister then begin
        regname := addr2regname(addr) ;
      end else begin
        assert(MMU.getPhysicalAddressType = matPhysical22) ;
        assert(addr.mat = matPhysical22) ; // TEST, BIS MMU angeschlossen ist!
        if addr.mat = matVirtual then
          addr := MMU.Virtual2PhysicalData(addr) ;
        assert(addr.mat = matPhysical22) ;
        assert(addr.val <> MEMORYCELL_ILLEGALVAL) ;

        regname := addr2regname(addr) ;
      end;

      if regname <> '' then begin // es ist ein CPU-Register
        s := Format('E %s'+ CHAR_SIMH_CR, [regname]) ;
        examine_lastaddr.val := MEMORYCELL_ILLEGALVAL ;
      end else begin
        s := Format('E %s' + CHAR_SIMH_CR, [Dword2OctalStr(addr.val)]) ;
        examine_lastaddr := addr ;
      end;

      Answerlines.Clear ;
      WriteToPDP(s) ;
      // Antwort format:
      //  <addr> <val>
      //  sim>
      answerline := WaitForAnswer(phExamine, SIMH_CMD_TIMEOUT) ;

      if answerline = nil then
        result := MEMORYCELL_ILLEGALVAL // keine Antwort, oder ? Fehler
      else if (answerline.examinevalue <> MEMORYCELL_ILLEGALVAL)
              and (answerline.examineaddr.val <> addr.val) then
        raise Exception.CreateFmt('EXAMINE failure: request "%s", answer is "%s!"',
                [s, answerline.rawtext])
      else result := answerline.examinevalue ;

      CheckPrompt('EXAMINE failed, no prompt') ;
    finally
      OutputDebugString('Examine ends') ;
      EndCriticalSection ;
    end { "try" } ;
  end{ "function TConsolePDP11SimH.Examine" } ;


// eine ganze Memorylist auslesen.
// SimH kann E addr,addr,addr,... auswerten.
procedure TConsolePDP11SimH.Examine(mcg: TMemoryCellGroup ; unknown_only: boolean; abortable:boolean) ;

// Examine-Commandos für eine Liste ausgeben. addr_inc: 1 für globale register, sonst 2
// nur solche listmembers beachten, die tag = 0 haben (= noch nicht abgefragt sind)
// result: true, wenn alle cells abgefragt wurden
// register werden an bekanntem Registername erkannt
// false: neuer Aufruf ist nötig, mindestens die erste Zelle wurde abgefragt
// UNIBUSTIMEOUTS: abbruch, mindestens die 1. Zelle ist gesetzt (mit INVALID)

  function examineAddrList(list: TList ; addr_inc: integer): boolean ;

  // true, wenn list[idx] nicht in von-bis Beriech mit aufgenommen werden darf
    function is_SpecialAddrname(idx: integer): boolean ;
      begin
        result := addr2regname(TMemoryCell(list[idx]).addr, {tmpval!}true) <> '' ;
      end ;

    const
      max_block_len = 100 ;
    var
      blockstart, blockend: integer ; // start .. end-1 ist ein Block
      blockstart1: integer ; // Beginn eines von-bis Bereichs
      i, j: integer ;
      s: string ;
      mc: TMemoryCell ;
      cmd, sep: string ;
      starttime: dword ;
      answerline: TConsoleAnswerPhrase ;
      next_expected_addr: TMemoryAddress ;
      block_failure: boolean ; // Blockabfrage ist durcheinander, address error: restart nötig
      ready: boolean ; // false: das frage/antwort-Spiel abbrechen
      found: boolean ;
      //lastaddr: TMemoryAddress ;
      timeout: boolean ;
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

      // finde alle sequentiellen Bereiche.
      // addressen physical vergleichen ... addr.tmpval!
      // es wird nur ein Command für die ganze Liste ausgegeben:
      // E 0-100,230,234,r0,pc,1000-1006
      // Oder:
      // E R0,R1,R2,R3,R4
      BusyForm.Start('Examining ...', list.Count, abortable) ;

      try
        block_failure := false ;
        while not BusyForm.Aborted and not block_failure and (blockstart < list.Count) do begin

          // finde einen block, in dem die Adressen um 2 aufsteigen
          // und keine Register dabei sind
          // block darf aber nicht länger als max_block_len werden,
          // längere blöcke werden unterbrochen.
          // Register, die '?' heissen, sind immer unbekannt.
          cmd := 'E' ;
          sep := ' ' ;
          blockend := blockstart ;  // blockend immer nächster Index NICHT im Bereich

          // Kommaliste aus von-bis bereichen bilden
          repeat
            blockstart1 := blockend ; // start des nächsten von-bis-Bereichs
            blockend := blockstart1+1 ;

            // (A) einzelnen von-bis bereich bilden
//          if is_SpecialAddrname(blockend) then
//            inc(blockend)
//          else
            while (blockend < list.Count)
                    and (TMemoryCell(list[blockend-1]).tag = 0)
                    and (TMemoryCell(list[blockend-1]).addr.tmpval + 2 = TMemoryCell(list[blockend]).addr.tmpval)
                    and not is_SpecialAddrname(blockend)
                    // blocklänge begrenzen
                    and ((blockend-blockstart) < max_block_len)
                    do
              inc(blockend) ;

            // cmd für von-bis Bereich rendern
            if blockend - blockstart1 > 1 then begin
              cmd := cmd + sep + Format('%s-%s', [
                      Dword2OctalStr(TMemoryCell(list[blockstart1]).addr.tmpval, 0),
                      Dword2OctalStr(TMemoryCell(list[blockend-1]).addr.tmpval, 0)
                      ]) ;
            end else begin // einzelne Adresse
              s := addr2regname(TMemoryCell(list[blockstart1]).addr, {tmpval!}true) ;
              if s = '' then
                cmd := cmd + sep + Dword2OctalStr(TMemoryCell(list[blockstart1]).addr.tmpval, 0)
              else
                cmd := cmd + sep + s ;
            end ;
            sep := ',' ;

            // Wie (A), nur negativ und ohne Test auf Unterbechung im Adressbereich
          until (blockend >= list.Count)
                  or ((blockend-blockstart) >= max_block_len) ;

          Answerlines.Clear ; // Empfangsbuffer leeren
          WriteToPDP(cmd + CHAR_SIMH_CR) ;
          // Antwort format:
          //  <addr>: <val>
          //  <addr>: <val>
          //  ...
          //  sim>
          // Ignoriere <addr>

          // warten, bis
          // - alle adressen abgefragt wurden
          // - oder ILLEGAL VAL kam. Dann gabs an der adresse
          //   (letzteadresse+inc) ein UNIBUS TIMEOUT und multi-read brach ab
          // - timeout
          starttime := GetTickCount ; // Timeout reset
          ready := false ;
          // lastaddr ist die letzte gute adresse,
          // UNIBUS timeouts werden lastaddr + inc zugeordnet
          next_expected_addr := TMemoryCell(list[blockstart]).addr ;

          repeat
            Application.ProcessMessages ; // background receive
                BusyForm.StepIt(Answerlines.Count) ;  // es sollten nur Examine-Aanswers kommen
                // no hidden ProcessMessages while scanning Answerlines: inhibit new recoeve
            // alle antworten analysieren
            for i := 0 to Answerlines.Count-1 do begin
              answerline := Answerlines.Items[i] as TConsoleAnswerPhrase ;
              if answerline.phrasetype = phExamine then begin
                Log('list examine: processing answer');
                Log(answerline.AsText);
                starttime := GetTickCount ; // Timeout reset
                // gültiges address/wert paar, oder TIMEOUT
                if answerline.examinevalue = MEMORYCELL_ILLEGALVAL then begin
                  // die aktuelle Adresse verursachte einen Fehler,
                  // sie ist aber unbekannt!
                  answerline.examineaddr.val := next_expected_addr.tmpval ;
                  block_failure := true ;
                end ;
                // finde memorycell über adresse
                found := false ;
                for j := blockstart to blockend-1 do begin
                  mc := TMemoryCell(list[j]) ;
                  if mc.addr.tmpval = answerline.examineaddr.val then begin
                    next_expected_addr := answerline.examineaddr ; // antworten kommen strikt aufsteigend
                    next_expected_addr.tmpval := answerline.examineaddr.tmpval + addr_inc ; // antworten kommen strikt aufsteigend
                    mc.tag := 1 ; // wert ist jetzt abgefragt
//Log('!!!3 mc[%s].tag = 1', [Dword2OctalStr(mc.addr.tmpval)]) ;
                    mc.pdp_value := answerline.examinevalue ;
                    found := true ;
                  end;
                end;
                if not found then Log('NO Memorycell set by this answer!');

              end{ "if answerline.phrasetype = phExamine" } ;
            end{ "for i" } ;
            // alle antworten von SimH sind jetzt abgearbeitet
            Answerlines.Clear ;

            // check, ob alle adressen abgefragt wurden
            ready := true ;
            for i := blockstart to blockend-1 do
              if TMemoryCell(list[i]).tag = 0 then
                ready := false ;
//Log('!!!4 ready=%d', [ord(ready)]) ;
            timeout := (starttime + SIMH_CMD_TIMEOUT < GetTickCount) ;
          until timeout or ready or block_failure;
          if timeout then begin // keine Exception, sonst wird die ganze Useraktion abgebrochen
            Log('EXAMINE list failure: timeout waiting for list addr %s!',
                    [Dword2OctalStr(next_expected_addr.tmpval, 22)]) ;
            result := true ; // keine weiteren versuche, Endlos-schleife!
          end;
          // nächster Block für neues "E" command
          blockstart := blockend ;
        end { "while not BusyForm.Aborted and not block_failure and (blockstart < list.Count)" } ;
        if BusyForm.Aborted then result := true ; // do not retry
      finally
        BusyForm.Close ;
      end { "try" } ;
    end{ "function examineAddrList" } ;


  var i: integer ;
    mc: TMemoryCell ;
    listmem: TList ; // sortierte Liste mit Memoryadressen (inkrement 2) ;
    listcpureg: TList ; // sortierte Liste mit CPU-Regsiteradressen (inkrement 2) ;
  begin { "procedure TConsolePDP11SimH.Examine" }
    // a) memorycells sortiert auslesen
    // b) Memory/globalregister Bereiche unterscheiden, zusammenhängende
    //    Adressbereiche unterscheiden
    // c) sequentiell aufeinanderfolgende Adressen auslesen

    listmem := TList.Create ;
    listcpureg := TList.Create ;
    try
      // list kann virtual sein ... mc's nach physical umrechnen
      // an hier ist .tmpval 22 bit!
      BeginCriticalSection ; // User sperren
      for i := 0 to mcg.Count - 1 do begin
        mc := mcg.Cell(i) ;
        mc.tag := 0 ; // markiere als "noch keine antwort"
        if not unknown_only or (mc.pdp_value = MEMORYCELL_ILLEGALVAL) then begin
          // "tmpval" aller memorycell.addr wird die physical addr
          // nur auszulesende Adressen in die Listen
          // Register, die '?' heissen, sind von SimH nicht abfragbar
          assert(MMU.getPhysicalAddressType = matPhysical22) ;
          if mc.addr.mat = matVirtual then
            mc.addr.tmpval := MMU.Virtual2PhysicalData(mc.addr).val
          else
            mc.addr.tmpval :=  mc.addr.val ;

          if addr2regname(mc.addr, {tmpval!}true) <> '?' then
            if addr2regname(mc.addr, {tmpval!}true) <> '' then
              listcpureg.Add(mc)
            else listmem.Add(mc) ;
        end{ "if not unknown_only or (mc.pdp_value = MEMORYCELL_ILLEGALVAL)" } ;
      end{ "for i" } ;

      listmem.Sort(MemoryCellSortCompare) ; // aufsteigend sortieren
      listcpureg.Sort(MemoryCellSortCompare) ;

      // alle die analysieren, die tag = 0 haben
      // Abfrage kann mittendrin abbrechen, wenn address failure
      while not examineAddrList(listmem, 2) do ;
      while not examineAddrList(listcpureg, 1) do ;

    finally
      listmem.Free ;
      listcpureg.Free ;
      EndCriticalSection('Examine') ;
    end{ "try" } ;

  end { "procedure TConsolePDP11SimH.Examine" } ;


// sagt der TPDP-Console, dass sie nix mehr über SimH weiss
procedure TConsolePDP11SimH.ClearState ;
  begin
    inherited ;
    examine_lastaddr.val := MEMORYCELL_ILLEGALVAL ; // erzwinge explizite Adressausgabe
    deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; // erzwinge explizite Adressausgabe
  end;


procedure TConsolePDP11SimH.ResetMachine ; // Maschine CPU reset
  begin
    try
      BeginCriticalSection ; // User sperren
      Answerlines.Clear ;
      // Der PC wird nicht gesetzt => kein cfFlagResetCpuSetsPC
      WriteToPDP('reset all'+ CHAR_SIMH_CR) ;
      CheckPrompt('Reset failed, no prompt') ;
    finally
      EndCriticalSection ;
    end;
  end;




// PC ist virtuelle 16 bit Adresse
// Run mit Reset
procedure TConsolePDP11SimH.ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ;  // CPU starten.
  var s: string ;
  begin
    try
      BeginCriticalSection ; // User sperren

      assert(newpc_v.mat = matVirtual) ;

      Answerlines.Clear ;
      WriteToPDP('reset cpu'+ CHAR_SIMH_CR) ;
      CheckPrompt('Reset failed, no prompt') ;


      // 'go'
      Answerlines.Clear ;
      s  := Format('go %s'+ CHAR_SIMH_CR, [Dword2OctalStr(newpc_v.val, 16)]) ;
      WriteToPDP(s) ;
      // keine Prompt, CPU läuft jetzt
    finally
      EndCriticalSection ;
    end{ "try" } ;
  end{ "procedure TConsolePDP11SimH.ResetMachineAndStartCpu" } ;


procedure TConsolePDP11SimH.ContinueCpu ; // Maschine CPU reset
  begin
    Answerlines.Clear ;
    WriteToPDP('cont'+ CHAR_SIMH_CR) ;
//      CheckPrompt('Continue failed, no prompt') ;

  end;

// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP11SimH.HaltCpu(var newpc_v: TMemoryAddress) ; // CPU anhalten
  var answerline: TConsoleAnswerPhrase ;
  begin
    try
      BeginCriticalSection ; // User sperren
      Answerlines.Clear ;

      // ^E hauen. Dann muss die CPU ihren PC auspucken und anhalten
      WriteToPDP(CHAR_SIMH_HALT) ; // ^E

      answerline := WaitForAnswer(phHalt, SIMH_CMD_TIMEOUT) ;
      WriteToPDP(CHAR_SIMH_CR) ; // Störungen beseitigen
      // if answerline = nil then begin
      //   WriteToPDP(CHAR_SIMH_CR) ;
      //   answerline := WaitForAnswer(phHalt, SIMH_CMD_TIMEOUT) ;
      //  end;
      if answerline = nil then
        raise Exception.Create('Stopping CPU failed, no answer') ;

      CheckPrompt('Stopping CPU failed, no prompt') ;
      // jetzt wurde der Output mit ReadFromPdp() gelesen,
      // und MonitorPdpOutput() hat den CpuStop-Event erkannt
      newpc_v := onExecutionStopPcVal ;
    finally
      EndCriticalSection ;
    end{ "try" } ;
  end{ "procedure TConsolePDP11SimH.HaltCpu" } ;


// PC ist virtuelle 16 bit Adresse
// einen Zyklus ausführen, danach CPU-Stop-Event auslösen
procedure TConsolePDP11SimH.SingleStep;
  var answerline: TConsoleAnswerPhrase ;
    pcaddr: TMemoryAddress ;
  begin
    try
      BeginCriticalSection ; // User sperren

//        assert(newpc_v.mat = matVirtual) ;

      // vor Singlestep den PC setzen.
      // Im SimH-Step-Cmd kann er nicht nochmal angegeben werden.
      pcaddr.mat := matPhysical22 ;
      pcaddr.val := _17777707 ;
//        Deposit(pcaddr, newpc_v.val) ;


      Answerlines.Clear ;
      WriteToPDP('STEP 1'+ CHAR_SIMH_CR) ;
      answerline := WaitForAnswer(phHalt, SIMH_CMD_TIMEOUT) ;
      if answerline = nil then raise Exception.Create('Single Step failed, no answer') ;

      CheckPrompt('Single Step failed, no prompt') ;
      // jetzt wurde der Output mit ReadFromPdp() gelesen,
      // und MonitorPdpOutput() hat den CpuStop-Event erkannt
//        newpc_v := onExecutionStopPcVal ;
    finally
      EndCriticalSection ;
    end{ "try" } ;
  end{ "procedure TConsolePDP11SimH.SingleStep" } ;



end{ "unit ConsolePDP11SimHU" } .



