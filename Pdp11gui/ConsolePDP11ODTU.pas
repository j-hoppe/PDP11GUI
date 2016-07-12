unit ConsolePDP11ODTU;
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
  Steuert über RS232 die PDP-11 mit ODT Console an
  Die Anwendung ruft nur Deposit(), Examine()

  Steuert 16, 18 und 22 Bit ODT consolen an.
  Steuerung durch ThePhysicalAddressWidth
  Das muss von Aufrufer nach .Create durch Abfragen von
  getPhysicalAddressWidth gesetzt werden!

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
  AddressU,
  MemoryCellU,
  FormBusyU,
  FormLogU  ;

const
  // Zeilenumbruch der PDP-Console
  CHAR_ODT_CR = #$d ;
  CHAR_ODT_LF = #$a ;
  ODT_CMD_TIMEOUT = 1000 ; // timeout für Antwort in Millisekunden

type

  TConsolePDP11ODTScanner = class(TConsoleScanner)
    public
      constructor Create ; override ;
      function NxtSym(raiseIncompleteOnEof: boolean = true): string ; override ;
      class function Symtype2text(symtype:integer): string ;
    end;



  TConsolePDP11ODT = class(TConsoleGeneric)
    private
      mat: TMemoryAddressType ; // ist die console 18 oder 22?
      //     physicalAddressWidth: integer ; // merker
      function addr2regname(addr: TMemoryAddress): string ;
      function regname2addr(regname:string): TMemoryAddress ;
      function physicaladdr2text(addr: TMemoryAddress): string ;
    public
      // Rüdiger Kurt 2016: hat PDP-11 Nachbau, der als ODT prompt nicht "@", sondern "@ " zeigt.
      GobbleExtraSpaceAfterPrompt: boolean ;
      isK1630: boolean ; // Robotron 11/23 clone A6402, CPU K1630

      constructor Create(memorycellgroups: TMemoryCellGroups; mat: TMemoryAddressType) ;
      destructor Destroy ; override ;

      // wenn serial: host='COM'
      procedure Init(aConnection: TSerialIoHub) ; override ;
      procedure ClearState ; override ;
      procedure Resync ; override ;


      function getName: string ; override ;
      function getTerminalSettings: TTerminalSettings ; override ;

      function getFeatures: TConsoleFeatureSet ; override ;
      function getPhysicalMemoryAddressType: TMemoryAddressType ; override ;

      procedure CheckAddrOpen(errinfo: string) ;

      procedure Deposit(addr: TMemoryAddress ; val: dword) ; override ;
      function Examine(addr: TMemoryAddress): dword ; overload ; override ;
      procedure Examine(mcg: TMemoryCellgroup ; unknown_only: boolean; abortable:boolean) ; overload ; override ;
      // ganze Liste optimiert


      function DecodeNextAnswerPhrase: boolean ; override ;

      procedure ResetMachine(newpc_v: TMemoryAddress) ; override ; // Maschine reset
      procedure ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ; override ; // CPU starten.
//      function IsRunning: boolean ; virtual ; abstract ; // läuft die CPU (noch?)
      procedure ContinueCpu ; override ; // Maschine CPU reset
      procedure SingleStep; override ;

    end{ "TYPE TConsolePDP11ODT = class(TConsoleGeneric)" } ;


implementation


uses
  TypInfo, // GetEnumName
  Forms,
  OctalConst,
  AuxU,
  FormMainU,
  FormNoConsolePromptU ;

const
  // Scannt den output der PDP
  symtypeOctal = 0 ;
  symtypeRegister = 1 ;
  symtypeOther = 2 ;
  symtypeEoln = 3 ;
  symtypeEof = 4 ;

constructor TConsolePDP11ODTScanner.Create ;
  begin
    inherited ;
    NxtSym(false) ; // get first symbol, ist ein EOLN
  end;


class function TConsolePDP11ODTScanner.Symtype2text(symtype:integer): string ;
  begin
    result := '???' ;
    case symtype of
      symtypeOctal: result := 'Octal' ;
      symtypeRegister: result := 'Register' ;
      symtypeOther: result := 'Other' ;
      symtypeEoln: result := 'EOLN' ;
      symtypeEof: result := 'EOF' ;
    end;
  end ;


// ist nicht klar, ob ein Symbol durch mehr input noch
// verlängert werden kann, wird EConsoleScannerIncompleteSymbol
// geworfen.
function TConsolePDP11ODTScanner.NxtSym(raiseIncompleteOnEof: boolean = true):string ;
  var
    again: boolean ;
    symlen: integer ;
    s: string ;
    i: integer ;
  begin
    repeat
      again := false ;
      s := Copy(CurInputLine, nxtcharidx, maxint) ;
      if s = '' then begin
        result := 'EOF' ;
        CurSymType := symtypeEof ;
        // Exception an den Parser: schluss machen
        if raiseIncompleteOnEof then
          raise EConsoleScannerInputIncomplete.Create('EOF') ;
        Exit ;
      end ;
      result := '' ;
      i := 1 ;
      case s[1] of
        CHAR_ODT_LF: begin
          symlen := 1 ;
          again := true ; // die Linefeeds rausfiltern
        end;
        '0'..'7': begin // octalzahl
          while (i <= length(s)) and isOctalDigit(s[i]) do inc(i) ;
          if i > length(s) then // octalziffern bis Stringende: es kann noch mehr kommen
            raise EConsoleScannerInputIncomplete.Create('EConsoleScannerInputIncomplete 1') ;
          CurSymType := symtypeOctal; // abgeschlossene Octalzahl
          symlen := i-1 ;
        end ;
        'r', 'R', '$':   begin // check for R0..R7,$0..$7, RS, $S
          if length(s) < 2 then
            raise EConsoleScannerInputIncomplete.Create('EConsoleScannerInputIncomplete 2')
          else begin
            if CharInSet(s[2], ['0'..'7','S','s']) then begin
              symlen := 2;
              CurSymType := symtypeRegister ;
            end else begin
              symlen := 1 ;
              CurSymType := symtypeOther ;
            end ;
          end;
        end{ "case s[1] of 'r', 'R', '$':" } ;
        CHAR_ODT_CR: begin
          symlen := 1 ;
          CurSymType := symtypeEoln ;
        end else begin
          symlen := 1 ;
          CurSymType := symtypeOther ;
        end ;
      end{ "case s[1]" } ;
      CurSymTxt := Copy(CurInputLine, nxtcharidx, symlen) ;
      result := CurSymTxt ;
      nxtcharidx := nxtcharidx + symlen ;
    until not again ;

    // Detailierte Augsbe. rest auf 32 Zeichen beschärnaken,
    // falls mal der SerialXfer-Zeichenstrom i nden Parser gerät.
    Log('{Rcved Sym = (%s, "%s"), rest="%s"} ', [
            Symtype2text(CurSymType), CurSymTxt,
            Copy(CurInputLine, nxtcharidx, 32)]) ;

  end{ "function TConsolePDP11ODTScanner.NxtSym" } ;



constructor TConsolePDP11ODT.Create(memorycellgroups: TMemoryCellGroups; mat: TMemoryAddressType ) ;
  begin
    inherited Create;
    self.mat := mat ;
    RcvScanner := TConsolePDP11ODTScanner.Create ;
    CommandTimeoutMillis := ODT_CMD_TIMEOUT ;
    MMU := TPdp11MMU.Create(memorycellgroups) ;

    // DEC ODT doesn't need this, but its harmless.
    GobbleExtraSpaceAfterPrompt := true ;
    isK1630 := false ; // if true, needs also GobbleExtraSpaceAfterPrompt !
  end;

destructor TConsolePDP11ODT.Destroy ;
  begin
    MMU.Free ;
    RcvScanner.Free ;
    inherited ;
  end;

// ermittelt den symbolischen ODT-Registernamen zu einer 22bit-UNIBUS-Adresse
// '', wenn keiner gefunden

// diese Liste hier ist Model-abhängig .. eigentlich sollte sie aus
// der .ini-Datei mit geladen werden!
function TConsolePDP11ODT.addr2regname(addr: TMemoryAddress): string ;
  begin
    result := '' ;
    assert(addr.mat = mat) ;
    // code für 16, 18 und 22 bit Adressen
    if addr.val = (PhysicalIopageBaseAddr(mat) + _17700) then result := 'R0'
    else if addr.val = (PhysicalIopageBaseAddr(mat) + _17701) then result := 'R1'
    else if addr.val = (PhysicalIopageBaseAddr(mat) + _17702) then result := 'R2'
    else if addr.val = (PhysicalIopageBaseAddr(mat) + _17703) then result := 'R3'
    else if addr.val = (PhysicalIopageBaseAddr(mat) + _17704) then result := 'R4'
    else if addr.val = (PhysicalIopageBaseAddr(mat) + _17705) then result := 'R5'
    else if addr.val = (PhysicalIopageBaseAddr(mat) + _17706) then result := 'R6'
    else if addr.val = (PhysicalIopageBaseAddr(mat) + _17707) then result := 'R7'
    else if addr.val = (PhysicalIopageBaseAddr(mat) + _17776) then result := 'RS' ;

  end{ "function TConsolePDP11ODT.addr2regname" } ;


function TConsolePDP11ODT.regname2addr(regname:string): TMemoryAddress ;
  begin
    regname := Uppercase(regname) ;
    result.mat := self.mat ;
    result.val := MEMORYCELL_ILLEGALVAL ;
    // code für 18 und 22 bit Adressen
    if regname = 'R0' then result.val := PhysicalIopageBaseAddr(mat) + _17700 ;
    if regname = 'R1' then result.val := PhysicalIopageBaseAddr(mat) + _17701 ;
    if regname = 'R2' then result.val := PhysicalIopageBaseAddr(mat) + _17702 ;
    if regname = 'R3' then result.val := PhysicalIopageBaseAddr(mat) + _17703 ;
    if regname = 'R4' then result.val := PhysicalIopageBaseAddr(mat) + _17704 ;
    if regname = 'R5' then result.val := PhysicalIopageBaseAddr(mat) + _17705 ;
    if regname = 'R6' then result.val := PhysicalIopageBaseAddr(mat) + _17706 ;
    if regname = 'R7' then result.val := PhysicalIopageBaseAddr(mat) + _17707 ;
    if regname = 'RS' then result.val := PhysicalIopageBaseAddr(mat) + _17776 ;
    if regname = '$0' then result.val := PhysicalIopageBaseAddr(mat) + _17700 ;
    if regname = '$1' then result.val := PhysicalIopageBaseAddr(mat) + _17701 ;
    if regname = '$2' then result.val := PhysicalIopageBaseAddr(mat) + _17702 ;
    if regname = '$3' then result.val := PhysicalIopageBaseAddr(mat) + _17703 ;
    if regname = '$4' then result.val := PhysicalIopageBaseAddr(mat) + _17704 ;
    if regname = '$5' then result.val := PhysicalIopageBaseAddr(mat) + _17705 ;
    if regname = '$6' then result.val := PhysicalIopageBaseAddr(mat) + _17706 ;
    if regname = '$7' then result.val := PhysicalIopageBaseAddr(mat) + _17707 ;
    if regname = '$S' then result.val := PhysicalIopageBaseAddr(mat) + _17776 ;
  end{ "function TConsolePDP11ODT.regname2addr" } ;

// convert an address into textual ODT representation
function TConsolePDP11ODT.physicaladdr2text(addr: TMemoryAddress):string ;
  var s: string;
  begin
    s := addr2regname(addr) ; // CPU-Register?
    if s = '' then begin
      s := Dword2OctalStr(addr.val,0) ; // nein: adresse ohne leading 0's
      if isK1630 then
        s := s + 'A' ; // suffix for 18 bit physical addresses
    end ;
    result := s ;
  end;



function TConsolePDP11ODT.getName: string ;
  begin
    result := 'PDP-11 ODT console' ;
  end;

// Terminal-Einstellungen für die PDP-11/ODT
function TConsolePDP11ODT.getTerminalSettings: TTerminalSettings ;
  begin
    // ODT sendet selber CR,LF
    result.Receive_CRisNewline := false ; // ignore CR
    //result.Receive_CRisCRLF := true ;
    result.Receive_LFisNewline := true ; // Line end on LF
    result.Backspace := #0 ; // PDP-11/ODT löscht nie
    result.TabStop := 0 ;
  end;


function TConsolePDP11ODT.getFeatures: TConsoleFeatureSet ;
  begin
    result := [
            cfNonFatalHalt,
            cfNonFatalUNIBUStimeout,
            cfSwitchEnableOrHalt, // der Switch auf der machine bewirkt features
            cfFlagResetCpuSetsPC  // "Reset" setzt den PC!
            ] ;
    if runmode = crmHalt then
      result := result + [
              cfActionResetMachine, // "nnnG" ist reset
              cfActionSingleStep // "P" ist single step
              ]
    else
      result := result + [
              // cfReset, // reset unabhängig von run: INITIALIZE
              cfActionResetMaschineAndStartCpu, // "nnnG" ist reset und start
              cfActionContinueCpu  // "P" ist Continue
              ] ;
    // kein : "Reset" setzt den PC NICHT!
  end{ "function TConsolePDP11ODT.getFeatures" } ;


function TConsolePDP11ODT.getPhysicalMemoryAddressType: TMemoryAddressType ;
  begin
    // wurde im Constructor übergeben
    result := mat ;
  end;


// stelle sicher, das die Examine-Answer "addr/val " in AnswerLines[] vorkommt.
// warte sonst.
procedure TConsolePDP11ODT.CheckAddrOpen(errinfo: string) ;
  var s: string ;
  begin
    if WaitForAnswer(phExamine, CommandTimeoutMillis) = nil then begin
      FormNoConsolePrompt.ShowModal ;
      s := Format('No console prompt: %s!', [errinfo]) ;
      Log(s) ;
      Abort ;
//      raise Exception.Create(s) ;
    end;
  end;


// return hauen und sicher stellen, dass antwort da ist.
procedure TConsolePDP11ODT.Resync ;
  begin
    try
      BeginCriticalSection ; // User sperren

      RcvScanner.Clear ; // unverarbeiteter Input weg
      Answerlines.Clear ; // erkannter Input weg
      // Return hauen, es muss die Prompt "@" kommen
      // Bei LSI-11/03: Vielleicht erst ein "?", dann ein "@"
      WriteToPDP(CHAR_ODT_CR) ;
      CheckPrompt('Could not wake up PDP-11 ODT') ;
      Log('PDP-11 ODT ready and prompting "@"') ;
    finally
      EndCriticalSection ;
    end{ "try" } ;
  end { "procedure TConsolePDP11ODT.Resync" } ;


// COM initialisieren
procedure TConsolePDP11ODT.Init(aConnection: TSerialIoHub) ;
  begin
    inherited Init(aConnection) ;

    Resync ;
  end;

// sagt der PDP-Console, dass sie nix mehr über die PDP-11/44 weiss
procedure TConsolePDP11ODT.ClearState ;
  begin
    inherited ;
//    examine_lastaddr := MEMORYCELL_ILLEGALVAL ; // erzwinge explizite Adressausgabe
//    deposit_lastaddr.val := MEMORYCELL_ILLEGALVAL ; // erzwinge explizite Adressausgabe
  end;


// Aus SerialRcvDataBuffer die Antworten extrahieren
// und in die Colection AnswerLines schreiben
// wird vom "onRcv"-Even aufgerufen - letztlich von SerialIoHub-PollTimer
function TConsolePDP11ODT.DecodeNextAnswerPhrase: boolean ;
(*
  type TParse_SymType = (symtypeIncomplete, symtypeOctal,
        symtypeRegister, symtypeOther, symtypeEoln, symtypeEof) ;
  const SymTypeTxt: array[TParse_SymType] of string =
    ('Incomplete', 'Octal', 'Register', 'Other', 'EOLN', 'EOF') ;
  var
    parse_nxtcharidx: integer ; // nächstes unverarbeites Zeichen aus SerialRcvDatabuffer
    CurSymTxt: string ;
    CurSymType: TParse_SymType ;

    // gibt aus dem SerialRcvDatabuffer das nächste Smybol zurück
  procedure Parse_Init ;
    begin
      parse_nxtcharidx := 1 ;
    end ;

  function Parse_NxtSym:string ;
    var symlen: integer ;
      s: string ;
      i: integer ;
    begin
      s := Copy(CurInputLine, nxtcharidx, maxint) ;
      if s = '' then begin
        result := 'EOF' ;
        CurSymType := symtypeEof ;
        Exit ;
      end ;
      result := '' ;
      i := 1 ;
      case s[1] of
        '0'..'7': begin // octalzahl
          while (i <= length(s)) and CharInSet(s[i], ['0'..'7']) do inc(i) ;
          if i > length(s) then CurSymType := symtypeIncomplete // octalziffern bis Stringende: es kann noch mehr kommen
          else CurSymType := symtypeOctal; // abgeschlossene Octalzahl
          symlen := i-1 ;
        end ;
        'r', 'R', '$':   begin // check for R0..R7,$0..$7, RS, $S
          if length(s) < 2 then
            CurSymType := symtypeIncomplete
          else begin
            if CharInSet(s[2], ['0'..'7','S','s']) then begin
              symlen := 2;
              CurSymType := symtypeRegister ;
            end else begin
              symlen := 1 ;
              CurSymType := symtypeOther ;
            end ;
          end;
        end{ "case s[1] of 'r', 'R', '$':" } ;
        CHAR_OCR_CR, CHAR_ODT_LF: begin
          symlen := 1 ;
          CurSymType := symtypeEoln ;
        end else begin
          symlen := 1 ;
          CurSymType := symtypeOther ;
        end ;
      end{ "case s[1]" } ;
      CurSymTxt := Copy(SerialRcvDatabuffer, parse_nxtcharidx, symlen) ;
      result := CurSymTxt ;
      parse_nxtcharidx := parse_nxtcharidx + symlen ;

      // Detailierte Augsbe
      Log('{Rcved Sym = (%s, "%s"), rest="%s"} ', [
              SymTypeTxt[CurSymType], CurSymTxt, Copy(SerialRcvDatabuffer, parse_nxtcharidx, maxint)]) ;

    end{ "function Parse_NxtSym" } ;
  *)
{ String patterns:          Remaining text after processing
<EOLN>@                phPrompt   -> @
<EOLN>addr<EOLN>       phHalt     -> EOLN
@addr/val<space>       phExamine  -> ""
<EOLN>addr/val<space>  phExamine  -> ""
else: scan until eoln  phOther    ->EOLN
}
  var
    // Globals für die parser
    CurAnswerline: TConsoleAnswerPhrase ;

  procedure makePrompt ;
    var haltAnswerline: TConsoleAnswerPhrase ;
    begin
      // Prompt:
      CurAnswerline := Answerlines.Add as TConsoleAnswerPhrase ;
      CurAnswerline.phrasetype := phPrompt ;

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

// einzelne octal Adresse
  procedure makeHalt(addrtxt:string) ;
    var val: dword ;
    begin
      val := OctalStr2Dword(addrtxt, 0) ;
      assert(val <> MEMORYCELL_ILLEGALVAL) ;
      // es ist nicht etwa ein einzelner register-Identifier:
      CurAnswerline := Answerlines.Add as TConsoleAnswerPhrase ;
      CurAnswerline.phrasetype := phHalt ;
      CurAnswerline.rawtext := addrtxt ;
      CurAnswerline.haltaddr := OctalStr2Addr(addrtxt, matVirtual) ;
      // halt -phrase löst noch nicht das OnExecutionStop-Event aus.
      // erst, wenn die nachfolgende cmd prompt erkannt wird
    end;

// Es wurde <addr>/<val> gefunden: addrtext: octal addr or register name
  procedure makeExamine(addrtxt, valtxt:string);
    var
      addr: TMemoryAddress ;
      val: dword ;
    begin
      // "addr/value<space>"
      // ODER
      //  "addr/?"
      // Aus Registername oder octalstring eine adresse bilden
      addr := regname2addr(addrtxt) ;
      // ist es registername oder oktal?
      if addr.val = MEMORYCELL_ILLEGALVAL then
        addr := OctalStr2Addr(addrtxt, mat) ; // exception
      assert(addr.val <> MEMORYCELL_ILLEGALVAL) ;
      // addr vor "/" ist gültig!
      val := OctalStr2Dword(valtxt, 16) ; // exception?
      if val <> MEMORYCELL_ILLEGALVAL then begin
        CurAnswerline := Answerlines.Add as TConsoleAnswerPhrase ;
        CurAnswerline.phrasetype := phExamine ;
        CurAnswerline.examineaddr := addr ;
        CurAnswerline.examinevalue := val ;
      end else if valtxt = '?' then begin
        // UNIBUS timeout ist gültige Antwort
        CurAnswerline := Answerlines.Add as TConsoleAnswerPhrase ;
        CurAnswerline.phrasetype := phExamine ;
        CurAnswerline.examineaddr.mat := mat ;
        CurAnswerline.examineaddr.val := MEMORYCELL_ILLEGALVAL ;  // adresse ist leider unbekannt
        CurAnswerline.examinevalue := MEMORYCELL_ILLEGALVAL ; // gültiges Fehlersignal
      end else
        raise EConsoleScannerUnknownExpression.Create('EConsoleScannerIllegalExpression 3') ;
    end{ "procedure makeExamine" } ;

  procedure makeOtherLine(s: string);
    begin
      CurAnswerline := Answerlines.Add as TConsoleAnswerPhrase ;
      CurAnswerline.phrasetype := phOtherLine ;
//        CurAnswerline.examineaddr := MEMORYCELL_ILLEGALVAL ;
//        CurAnswerline.examinevalue := MEMORYCELL_ILLEGALVAL ;
      CurAnswerline.rawtext := s ;
      CurAnswerline.otherline := s ;
    end;


  var
    s: string ;
    is_regname: boolean ;
  begin { "function TConsolePDP11ODT.DecodeNextAnswerPhrase" }
    CurAnswerline := nil ;
    result := false ;

    with RcvScanner do begin
      MarkParsePosition ;
      try
        // Eingabe-Ende vom letzten Scan löschen
        if CurSymType = symtypeEof then NxtSym ; // Incomplete, wenn nichts weiter da!

        // NextSym() erzeugt EConsoleScannerInputIncomplete, wenn Ende des Inputs
        // NextSym(false) tut das nicht
        LogStrCol(LogCol_DecodeNextAnswerPhrase, 'CurSym.Type=%s, .Txt="%s"', [
                TConsolePDP11ODTScanner.Symtype2text(CurSymType), String2PrintableText(CurSymTxt, true)] );
        if CurSymType = symtypeEoln then begin

// Wann können unverarbeitet Zeichen aus dem Empfangspuffer rausgeworfen werden?
// Wenn sie durch Zusatzempfang von noch mehr Zeichen nicht mehr in eine
// gültige Phrase gewandelt werden können.
// NB: "Rauswerfen" heisst: "makeOtherLine()
//
// a) wenn zwei CR in einer Zeile sind:


          // EOLN: alle zu ignorierenden Phrasen ausdrücklich hier eintragen!
          NxtSym ;
          while GobbleExtraSpaceAfterPrompt and (CurSymTxt = ' ') do
            // ignore whitespace after CR/LF: Rüdiger Kurth, K1630
            NxtSym ;

          if CurSymTxt = '@' then begin
            makePrompt ;
            // das '@' stehen lassen!
          end else if CurSymType = symtypeEoln then begin
            // PDP11/23 M8186: liefert manchmal eine Extra-Leerzeile
            makeOtherLine(''); // One CR is already eaten up
          end else if CurSymTxt = '?' then begin
            // PDP11/23 M8186: liefert nach Ausgabe von <CR> einfach ein "?" zurück
            NxtSym ; // eat up
            makeOtherLine('?');
          end else if CurSymType in [symtypeOctal, symtypeRegister] then begin // <EOLN><addr<regname>
            s := CurSymTxt ; // merke octal address/regname
            is_regname := (CurSymType = symtypeRegister) ;
            NxtSym ; //
            if isK1630 and (CurSymTxt = 'A') then
              NxtSym ; // K1630 gibt physical adressen als "<addr>A" aus.

            if not is_regname and (CurSymType = symtypeEoln) then begin // <EOLN><addr><EOLN>
              // addresse auf einzelner Zeile: Halt
              makeHalt(s) ;
              // das EOLN stehen lassen!
            end else if CurSymTxt = '/' then begin // <EOLN><addr|regname>"/"
              // nach EOLN: <addr>=halt oder <addr>/<val> = examine
              NxtSym ;
              if (CurSymType= symtypeOctal) or (CurSymTxt = '?' ) then
                makeExamine(s, CurSymTxt)
              else raise EConsoleScannerUnknownExpression.Create('no value after "<EOLN><addr>/" ') ;
              NxtSym ; // Examine-Ergebnis weg
            end else
              raise EConsoleScannerUnknownExpression.Create('<EOLN><addr> not proper terminated') ;
          end { "if CurSymType in [symtypeOctal, symtypeRegister]" } ;
        end { "if CurSymType = symtypeEoln" } else if CurSymTxt = '@' then begin
          NxtSym ;
          // Rüdiger Kurths ODT: extra space nach @
          if GobbleExtraSpaceAfterPrompt and (CurSymTxt = ' ') then
            NxtSym ;

          if not (CurSymType in [symtypeOctal, symtypeRegister]) then
            raise EConsoleScannerUnknownExpression.Create('no addr after @') ;

          s := CurSymTxt ; // merke address-Ausdruck (octal or regname)
          NxtSym ;
          if isK1630 and (CurSymTxt = 'A') then
            NxtSym ; // K1630 gibt physical adressen als "<addr>A" aus.
          if CurSymTxt <> '/' then
            raise EConsoleScannerUnknownExpression.Create('no "/" after "@<addr>"') ;
          // nach EOLN: <addr>=halt oder <addr>/<val> = examine
          NxtSym ;
          // Rüdiger Kurths ODT: extra space nach /
          if GobbleExtraSpaceAfterPrompt and (CurSymTxt = ' ') then
            NxtSym ;
          if (CurSymType= symtypeOctal) or (CurSymTxt = '?' ) then begin
            makeExamine(s, CurSymTxt) ;
            NxtSym ; // Examine-Ergebnis weg
          end ;
        end { "if CurSymTxt = '@'" } else begin // ? alles bis nächstes EOL ist unverstandene "OTHERLINE"
          s := CurSymTxt ;
          NxtSym ;
          while CurSymType <> symtypeEoln do begin
            s := s + CurSymTxt ;
            NxtSym ;
          end; // abort mit exception auch bei EOF
          makeOtherLine(s);
        end ;
        // Verarbeiteten String abknipsen
        CleanupInput ;

        if CurAnswerline <> nil then
          Log(CurAnswerline.AsText) ;

        // true, wenn phrase erkannt
        result := CurAnswerline <> nil ;

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
          // mehr input
          RestoreParsePosition ;
          result := false ;
        end;
      end{ "try" } ;

    end { "with RcvScanner" } ;
  end { "function TConsolePDP11ODT.DecodeNextAnswerPhrase" } ;



procedure TConsolePDP11ODT.Deposit(addr: TMemoryAddress ; val: dword) ;

  var s: string ;
  begin
    // VB: console ist bereit
    // Deposit hier zweiteilig:
    // 1. drucke "addr/" aus
    // 2. ODT gibt aktuellen Wert aus
    // 3. Decoder erkennt "Examine"
    // 4. gib neuen Weret und CR aus
    // gib neuen wert aus
    try
      BeginCriticalSection ; // User sperren

      // PDP11 ODT console kann nur physical: translate
      // DA MEIST CODE GESCHRIEBEN WIRD, IMMER INSTRUCTION MAP / ISPACE !
      assert(MMU.getPhysicalAddressType = mat) ;
      if addr.mat = matVirtual then
        addr := MMU.Virtual2PhysicalInstruction(addr) ;
      assert(addr.mat = mat) ;

      Answerlines.Clear ;

      s := physicaladdr2text(addr) ;
      WriteToPDP(s+'/') ;

      CheckAddrOpen('DEPOSIT failed, addr not opened') ;

      WriteToPDP(Dword2OctalStr(val,0)+ CHAR_ODT_CR) ;

      CheckPrompt('DEPOSIT failed, no prompt') ;
    finally
      EndCriticalSection ;
    end { "try" } ;
  end{ "procedure TConsolePDP11ODT.Deposit" } ;




// alle edit_values setzen
function TConsolePDP11ODT.Examine(addr: TMemoryAddress): dword ;
  var s: string ;
    answerline: TConsoleAnswerPhrase ;
  begin
    result := 0 ;
    // VB: console ist bereit

    try
      BeginCriticalSection ; // User sperren

      if addr.mat = matSpecialRegister then begin
        // nur MEMORYCELL_SPECIALADDR_DISPLAYREG definiert, und das ist nicht abfragbar
        result := MEMORYCELL_ILLEGALVAL ; // kann aus ODT nicht abgefragt werden
        Exit ;
      end ;

      // PDP11/44 console kann nur physical: translate
      assert(MMU.getPhysicalAddressType = mat) ;
      if addr.mat = matVirtual then
        addr := MMU.Virtual2PhysicalData(addr) ;
      assert(addr.mat = mat) ;
      assert(addr.val <> MEMORYCELL_ILLEGALVAL) ;

      Answerlines.Clear ;

      s := physicaladdr2text(addr) ;
      WriteToPDP(s+'/') ;

      answerline := WaitForAnswer(phExamine, ODT_CMD_TIMEOUT) ;
      if answerline = nil then
        result := MEMORYCELL_ILLEGALVAL // keine Antwort, oder ? Fehler
      else if (answerline.examinevalue <> MEMORYCELL_ILLEGALVAL) and (answerline.examineaddr.val <> addr.val) then
        raise Exception.CreateFmt('EXAMINE failure: request for addr %s, answer is "%s"',
                [Dword2OctalStr(addr.val), answerline.rawtext])
      else result := answerline.examinevalue ;

      // wenn "/?" Antwort: Prompt kam schon
      if (answerline <> nil) and (answerline.examinevalue <> MEMORYCELL_ILLEGALVAL) then
        WriteToPDP(CHAR_ODT_CR) ;

      CheckPrompt('EXAMINE failed, no prompt') ;
    finally
      EndCriticalSection ;
    end { "try" } ;
  end{ "function TConsolePDP11ODT.Examine" } ;


// optimiert eine ganze Memorylist auslesen
procedure TConsolePDP11ODT.Examine(mcg: TMemoryCellgroup ; unknown_only: boolean; abortable:boolean) ;
  var
    i: integer ;
    mc: TMemoryCell ;
  begin
    try
      BeginCriticalSection ; // User sperren
      BusyForm.Start('Examining ...', mcg.Count, abortable);
      for i := 0 to mcg.Count - 1 do begin
        BusyForm.StepIt ; if BusyForm.Aborted then Break ;
        mc := mcg.Cell(i) ;
        if not unknown_only or (mc.pdp_value = MEMORYCELL_ILLEGALVAL) then
          mc.pdp_value := Examine(mc.addr) ;
      end;
    finally
      BusyForm.Close ;
      EndCriticalSection ;
    end{ "try" } ;
  end { "procedure TConsolePDP11ODT.Examine" } ;


procedure TConsolePDP11ODT.ResetMachine(newpc_v: TMemoryAddress) ; // Maschine CPU reset
  var s: string ;
  begin
    if runmode <> crmHalt then
      raise Exception.Create('RESET in RUN mode not possible') ;
    try
      BeginCriticalSection ;
      assert(newpc_v.mat = matVirtual) ;

      Answerlines.Clear ;

      // START wirkt wie Reset. Der PC wird gesetzt => cfFlagResetCpuSetsPC
      s  := Format('%sG', [Dword2OctalStr(newpc_v.val, 0)]) ;
      WriteToPDP(s) ;

      CheckPrompt('RESET failed, no prompt') ;
    finally
      EndCriticalSection ;
    end{ "try" } ;
  end{ "procedure TConsolePDP11ODT.ResetMachine" } ;


procedure TConsolePDP11ODT.SingleStep ; // Maschine CPU reset
  begin
    if runmode <> crmHalt then
      raise Exception.Create('SINGLE STEP in RUN mode not possible') ;
    try
      BeginCriticalSection ;

      Answerlines.Clear ;
      WriteToPDP('P') ;
      CheckPrompt('SINGLE STEP failed, no prompt') ;
    finally
      EndCriticalSection ;
    end;
  end{ "procedure TConsolePDP11ODT.SingleStep" } ;


// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP11ODT.ResetMachineAndStartCpu(newpc_v: TMemoryAddress) ; // CPU starten.
  var s: string ;
  begin
    if runmode <> crmRun then
      raise Exception.Create('START in HALT mode not possible') ;
    try
      BeginCriticalSection ;
      assert(newpc_v.mat = matVirtual) ;

      Answerlines.Clear ;

      // START
      // Ist mit QBUS reset -> alle Devices reset?
      // K1630: <adr>AG ?
      s  := Format('%sG', [Dword2OctalStr(newpc_v.val, 0)]) ;
      WriteToPDP(s) ;
      // keine Prompt, CPU läuft jetzt
    finally
      EndCriticalSection ;
    end{ "try" } ;
  end{ "procedure TConsolePDP11ODT.ResetMachineAndStartCpu" } ;


// PC ist virtuelle 16 bit Adresse
procedure TConsolePDP11ODT.ContinueCpu ; // CPU starten.
  begin
    if runmode <> crmRun then
      raise Exception.Create('CONTINUE in HALT mode not possible') ;
    try
      BeginCriticalSection ;

      Answerlines.Clear ;
      WriteToPDP('P') ;
      // keine Prompt, CPU läuft jetzt
    finally
      EndCriticalSection ;
    end;
  end{ "procedure TConsolePDP11ODT.ContinueCpu" } ;





end{ "unit ConsolePDP11ODTU" } .


