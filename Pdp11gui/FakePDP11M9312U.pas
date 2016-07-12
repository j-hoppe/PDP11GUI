unit FakePDP11M9312U;
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
  Simuliert eine rudimentäre PDP-11 mit M9312/M9301 console emulator.

  Beschreibung des console emulators mit M9301 ROMS:
  "PDP-11-34 system user's manual (Jul 1977, EK-11034-UG-001).pdf"
  Chapter 2.1.3 "console emulator", pages 2-3

  !!! Hier implementiert: M9312 ROMS !!!
  "M9312 bootstrap-terminator module technical manual (Mar 1981, EK-M9312-TM-003).pdf"
   pages 3-1 ff
   Insbesondere Fehlerbehandlung auf p3-6,7

Unterschiede:
"@" prompt  statt "$"
Andere Bedeutung der 4 Register:
nicht R0, R4, R6, R7
sondern:
  000000 174300 165212 <loaded addr>


Der M9312 console emulator wird durch CPU aus boot ROM ausgeführt
und ist unglaublich schwach:

- nur "load adress", EXAM, DEPOSIT, START
- nur virtueller 16bit Adressraum, aber 76xxxxx Adresse werden nach 16xxxx gemappt
- nur gerade Adressen, R0..R7 = 177700-07 nicht zugreifbar, HALT!
- Ausgabe von R0,R4,R6(SP) und R7(PC) bei illegalen Befehlen
@X
000000 173400 165212 165212
  Aber Register enthalten content des console emulators!!!

- HALT bei BUS ERRORs = nicht existierenden Adressen,

- nur START möglich, kein HALT, RESET oder INIT
- bei HALT nur restart über reboot auf front panel
- meldet sich nicht bei HALT, nur bei REBOOT über front panel



Sonderfunktion:
Bei einem simulierten HALT wird der Text ausgegben:
"[Now press <ESC> to simulate a <Ctrl>-<BOOT> on KY11-LB programmer's console]"
Ein ESC führt dann ein RESET durch.

Der echte console-treiber "ConsolePDP11M9312" muss ein timeout der Ausgabe erkennen und
dann eine Warnmeldung ausgeben:



globals:
  last_addr letzte offene adresse, string



}

interface

uses
  Windows, Classes, SysUtils,
  JH_Utilities,
  FakePDP11GenericU,
  AddressU,
  MemoryCellU ;


type

  TFakePDP11M9312State = (
      pdp11M9312stateInit,
      pdp11M9312statePrompt,   // Input als Antwort auf Prompt
      pdp11M9312stateHALTed // console emulator ist auf HALT gelaufen
    ) ;
{
      pdp11M9312stateEnterLocationAddr, // user hat nach @ was getippt
      pdp11M9312stateOpen,  // user hat / getippt, wert von addr wurde angezeigt
      pdp11M9312stateEnterLocationContents // user tippt nach val neuen val ein
    ) ;
 }

  TFakePDP11M9312 = class(TFakePDP11Generic)
    private
      // Adresse, wie mit "L" geladen
      LoadedAddress: TMemoryAddress ; // letzte geöffnete Addresse
      LastLoadedAddressUse: char ; // grund, warum LastLoadedAdress
      // benutzt wurde. 2x E oder 2x "D" bewirkt AutoInc

      procedure print(s:string) ;

      // Consol-Commandos verarbeiten
      procedure doPrompt(with_registers:boolean=false) ; // Eingabe reset
      procedure doInputError(with_registers:boolean) ;
      procedure doConsoleEmulatorErrorHALT(msg:string) ;
      function isLoadedAddrValid: boolean ;
      procedure doIncLoadedAddress(curuse: char) ;

      procedure doLoadAddress(addrexpr:string) ;
      procedure doEXAM ;
      procedure doDEPOSIT(valexpr: string) ;
      procedure doSTART ;

      function doCommand(cmd: string ; check_only: boolean): boolean ;
      function isBootCommand(s: string) : boolean ;

    public

      TheState: TFakePDP11M9312State ;

      Prompt: string ; // '@' for M9312, '$' for M9301

      constructor Create ;

      procedure PowerOn ; override ;
      procedure Reset ; override ;

      // Interface zur seriellen Console
      function SerialReadByte(var b: byte) : boolean ; override ;
      function SerialWriteByte(b: byte) : boolean ; override ;

      // So tun, als ob eine laufende CPU auf ein HALT gerannt waere
      procedure doHalt ; override ;

    end{ "TYPE TFakePDP11M9312 = class(TFakePDP11Generic)" } ;


implementation

uses
  OctalConst,
  AuxU
  ;


constructor TFakePDP11M9312.Create ;
  begin
    inherited Create(matPhysical16) ;

    Prompt := '@' ; // default: M9312

  end;

// Speicher löschen
procedure TFakePDP11M9312.PowerOn ;
  var i: integer ;
  begin
    for i := 0 to PhysicalMemorySize - 1 do
      Mem[i] := 0 ;

    SerialInBuff := '' ;
    SerialOutBuff := '' ;
    Reset ;
  end;


// = wie Reboot, ohne Speicher initialisierung
procedure TFakePDP11M9312.Reset ;
  begin
    TheState := pdp11M9312stateInit ;

    SerialInBuff := '' ;

    LoadedAddress.mat := matPhysical16 ;
    LoadedAddress.val := _165212 ; //  tatsächlich, das ist der Startwert!
    // und der Wert nach jedem Fehler!
    LastLoadedAddressUse := 'X' ; // invalid

    doPrompt(true) ;
  end;



// Interface zur serialle Console
function TFakePDP11M9312.SerialReadByte(var b: byte) : boolean ;
  begin
    if SerialOutBuff = '' then
      result := false // buffer leer: nix zu lesen da!
    else begin
      // ältestes Zeichen zurückgeben
      b := byte(SerialOutBuff[1]) ;
      SerialOutBuff := Copy(SerialOutBuff, 2, maxint) ;
      result := true ;
    end;
  end;

procedure TFakePDP11M9312.print(s:string) ;
  begin
    SerialOutBuff := SerialOutBuff + s ;
  end;


// with_registers: prompt mit den vier registern
procedure TFakePDP11M9312.doInputError(with_registers:boolean) ;
  begin
    SerialInBuff := '' ;
    LastLoadedAddressUse := 'X' ; // invalid

    doPrompt(with_registers) ; // alten addr anzeigen

    LoadedAddress.val := _165212 ; //  tatsächlich, das ist der nach normalem Fehler!

//    Reset ;
  end;


// wenn ein laufendes Programm auf HALT rennt.
procedure TFakePDP11M9312.doHalt ;
//  var pc: dword ;
  begin
//    pc := getMem(ProgramCounterAddr) ;
//    print(CHAR_CR + CHAR_LF + Dword2OctalStr(pc, 16)) ;
//    doPrompt ;
    print(CHAR_CR+CHAR_LF) ;
    print(Format('[Started program halted. Reboot by pressing ESC]', [0])) ;
    TheState := pdp11M9312stateHALTed ;
  end;


// Der console emulator ist selber auf HALT gelaufen.
// auf echter PDP-11 muss er über console panel neu gebootet werden.
procedure TFakePDP11M9312.doConsoleEmulatorErrorHALT(msg:string) ;
  begin
    print(CHAR_CR+CHAR_LF) ;
    print(Format('[Emulator error "%s": HALT. Reboot by pressing ESC]', [msg])) ;
    TheState := pdp11M9312stateHALTed ;
    // im HALTed state wird nur ESC als Eingabe akzeptiert
  end;


// Prompt "@" drucken, ggf mit vollem Register-output
// keine Funktion, wenn die CPU läuft.
procedure TFakePDP11M9312.doPrompt(with_registers:boolean=false) ;
  const
    // Diese Sequenz wird vor Registeroutput und vor Q ausgegeben:
    // 1xLF, 13xCR
    LONG_NEWLINE_SEQ = CHAR_LF+CHAR_CR+CHAR_CR+CHAR_CR+CHAR_CR+CHAR_CR+CHAR_CR+CHAR_CR
            +CHAR_CR+CHAR_CR+CHAR_CR+CHAR_CR+CHAR_CR+CHAR_CR ;
    //var
    //regaddr: TMemoryAddress ; // letzte geöffnete Addresse
    //regval: dword ;
  begin
    SerialInBuff := '' ;

    if TheState = pdp11M9312stateHALTed then
      Exit ; // keine Ausgabe, wenn console emulator steht

    if isRunning then
      Exit ;


    if with_registers then begin
      print(LONG_NEWLINE_SEQ) ;

      // M9312: 000000 173400 165212 <loadedaddress>
      // Hier nicht implementiert: nach PowerON wird einmal
      // "000000 000000 000000 000000" ausgegeben
      print('000000 173400 165212 ') ;
      print(Dword2OctalStr(LoadedAddress.val, 16) + ' ') ;

      (*
      // M9301: print R0,R4,R6,R7
      // mit leading Space!
      regaddr.mat := matPhysical16 ;


      regaddr.val := PhysicalIopageBaseAddr + _17700 + 0 ; // R0
      regval := getMem(regaddr) ;
      print(Dword2OctalStr(regval, 16) + ' ') ;

      regaddr.val := PhysicalIopageBaseAddr + _17700 + 4 ; // R4
      regval := getMem(regaddr) ;
      print(Dword2OctalStr(regval, 16) + ' ') ;

      regaddr.val := PhysicalIopageBaseAddr + _17700 + 6 ; // R6
      regval := getMem(regaddr) ;
      print(Dword2OctalStr(regval, 16) + ' ') ;

      regaddr.val := PhysicalIopageBaseAddr + _17700 + 7 ; // R7
      regval := getMem(regaddr) ;
      print(Dword2OctalStr(regval, 16) + ' ') ;
      *)

    end { "if with_registers" } ;
    print(LONG_NEWLINE_SEQ + Prompt) ;
    TheState := pdp11M9312statePrompt ;
  end{ "procedure TFakePDP11M9312.doPrompt" } ;


// Syntaktisch gutes "L xxxx" wurde eingetippt
procedure TFakePDP11M9312.doLoadAddress(addrexpr:string) ;
  var
    i: integer ;
    addr: TMemoryAddress ;
  begin
    // addrexpr ist octalstring, numerisch aber ungeprüft
    // nur die rechten 6 Zahlen verwenden
    i := length(addrexpr) ;
    addrexpr := Copy(addrexpr, i-5, maxint) ;
    try
      // 18 bit addressen auf 16 bit adressen abrunden
      addr := OctalStr2Addr(addrexpr, matPhysical18) ;
      addr.val := addr.val and $ffff ;

      LoadedAddress.val := addr.val ;
      LastLoadedAddressUse := 'L' ; // vor nächstem Exam/Deposit nicht erhöhen
    except
      doInputError({with_registers=}false) ; // keine Registeranzeige bei falscher octalzahl
    end;
  end{ "procedure TFakePDP11M9312.doLoadAddress" } ;


function TFakePDP11M9312.isLoadedAddrValid: boolean ;
  var val: dword ;
  begin
    result := true ;
    // EXAM/DEPOSIT/START:
    // loaded addr darf nicht ungerade sein,
    // muss daten enthalten,
    // darf nicht im GPR-Space liegen

    // Prüfung, ob Adresse verwendbar ist
    if odd(LoadedAddress.val) then begin
      result := false ;
      doConsoleEmulatorErrorHALT('odd address') ;
    end;

    if (LoadedAddress.val >= _177700) and (LoadedAddress.val < _177700+7) then begin
      result := false ;
      doConsoleEmulatorErrorHALT('GPR space') ; // HALT wegen error
    end ;

    // Ein Zugriff, als Prüfung auf Buserror->HALT
    try
      val := getMem(LoadedAddress) ;
    except
      result := false ;
      doConsoleEmulatorErrorHALT('BUS error') ; // HALT wegen error
    end;
  end { "function TFakePDP11M9312.isLoadedAddrValid" } ;


// ggf die Adresse um zwei erhöhen, mit rollaround von
// 177776 nach 0
procedure TFakePDP11M9312.doIncLoadedAddress(curuse: char) ;
  begin
    // Adresse ist ggf. frisch gesetzt, dann kein Increment
    if curuse = LastLoadedAddressUse then begin
      // auf LoadedAdress wird zum zweite Mal per Examin oder Deposit hintereinander
      // zugegriffen. : Increment!
      if LoadedAddress.val = _177776 then
        LoadedAddress.val := 0
      else LoadedAddress.val := LoadedAddress.val + 2 ;
    end;
    // bei nächstem EXAM/DEPOSIT die Adresse erhöhen
    LastLoadedAddressUse := curuse ;
  end ;


// syntaktisch guts EXAMINE cmd wurde eingegeben
// "E " ist schon angezeigt
procedure TFakePDP11M9312.doEXAM ;
  const
    memdefect = false; // simulate defective memory
    memdefect_addr = _20000 ;
    memdefect_bit = _1000 ; // bit 10
  var
    val: dword ;
    tmpaddr: TMemoryAddress ;
  begin
    doIncLoadedAddress('E') ;

    if not isLoadedAddrValid then
      Exit ; // Fehler schon ausgegeben

    try
      tmpaddr := LoadedAddress;
      //tmpaddr.val := tmpaddr.val and not 8 ; // A3 damaged!
      val := getMem(tmpaddr) ;
      if memdefect and (tmpaddr.val = memdefect_addr) then begin
        val := val or memdefect_bit ; // bit always set
        //  val := val and not memdefect_bit ; // bit always 0
      end;

    except
      raise Exception.Create('Programfehler in TFakePDP11M9312.doEXAM: getMem checked in isLoadedAddrValid') ;
    end;
    // adresse und
    print(Format('%s %s ', [Addr2OctalStr(LoadedAddress), Dword2OctalStr(val, 16)])) ;
    // Es wird jetzt angezeigt:
    // "@E 000010 177777 "
  end{ "procedure TFakePDP11M9312.doEXAM" } ;



// Syntaktisch gutes "D xxxx" wurde eingetippt
procedure TFakePDP11M9312.doDEPOSIT(valexpr: string) ;
  var
    i: integer ;
    val: dword ;
  begin
    doIncLoadedAddress('D') ;

    if not isLoadedAddrValid then
      Exit ; // Fehler schon ausgegeben

    // valexpr ist octalstring, numerisch aber ungeprüft
    // nur die rechten 6 Zahlen verwenden
    i := length(valexpr) ;
    valexpr := Copy(valexpr, i-5, maxint) ;
    try
      // 18 bit addressen auf 16 bit adressen abrunden
      val := OctalStr2Dword(valexpr, 18) ;
      val := val and $ffff ;
    except
      doInputError(false) ; // keine Registerausgabe bei falscher octalzahl
    end;

    // setzen
    try
      setMem(LoadedAddress, val) ;
    except
      raise Exception.Create('Programfehler in TFakePDP11M9312.doDEPOSIT: getMem checked in isLoadedAddrValid') ;
    end;
  end{ "procedure TFakePDP11M9312.doDEPOSIT" } ;



// setze PC und starte Pseudo run, der nach Zufallszeit an Zufallsadresse endet
procedure TFakePDP11M9312.doSTART ;
  var
    val: dword ;
  begin
    if not isLoadedAddrValid then
      doConsoleEmulatorErrorHALT('Start to invalid address') ;

    RunToHalt(LoadedAddress.val) ;
  end;


// Kommandoeingabe prüfen, und ggf ausführne
// result: true= OK
// cmd wie "L 1234" oder "E " oder "D 7654" oder "S"
function TFakePDP11M9312.doCommand(cmd:string ; check_only: boolean): boolean ;
  var opcode: string ;
    args: string ;
  begin
    result := false ;
    opcode := Copy(cmd, 1 ,2) ; // das erste oder die ersten beiden Zeichen sind opcode
    args := trim(Copy(cmd, 3, maxint)) ;
    if (opcode = 'L ') then begin
      if args = '' then args := '0' ; // Argumente: leerer octalstring gilt wie "0"
      if not check_only then
        doLoadAddress(args);
      result := true ;
    end else if (opcode = 'D ') then begin
      if args = '' then args := '0' ; // Argumente: leerer octalstring gilt wie "0"
      if not check_only then
        doDEPOSIT(args);
      result := true ;
    end else if  (opcode = 'E ') then begin
      if not check_only then
        doEXAM;
      result := true ;
    end else if (opcode = 'S') then begin
      if not check_only then
        doSTART ;
      result := true ;
    end;
  end{ "function TFakePDP11M9312.doCommand" } ;


// checken, ob "s" ein gültiges boot command ist
// Format: zwei chars für device,
// dann Zahlenstring
function TFakePDP11M9312.isBootCommand(s: string) : boolean ;
  var
    devicecode: string ;
    devicenumber: string ;
  begin
    result := false ; //

    devicecode := Copy(s, 1, 2) ;
    devicenumber:= Copy(s, 3, maxint) ;
    if devicenumber = '' then devicenumber := '0' ;

    if devicecode <> UpperCase(devicecode) then
      Exit ; // nur uppercase erlaubt

    if length(devicecode) <> 2 then
      Exit ; // immer 2stellig

    // Liste wie in "M9312 bootstrap-terminator module technical manual (Mar 1981, EK-M9312-TM-003).pdf"
    // page 3-4. ist aber abh. von ROM Bestückung
    if (devicecode <> 'DL')
            and (devicecode <> 'DX')
            and (devicecode <> 'DK')
            and (devicecode <> 'DT')
            and (devicecode <> 'DY')
            and (devicecode <> 'DM')
            and (devicecode <> 'MT')
            and (devicecode <> 'MM')
            and (devicecode <> 'CT')
            and (devicecode <> 'PR')
            and (devicecode <> 'TT')
            and (devicecode <> 'DP')
            and (devicecode <> 'DB')
            and (devicecode <> 'DS')
            and (devicecode <> 'MS')
            and (devicecode <> 'DD') then
      Exit ; // ungültiges device

    try
      OctalStr2Dword(devicenumber, 16) ;
    except
      Exit ; // ungültige nummer
    end;
    result := true ;
  end{ "function TFakePDP11M9312.isBootCommand" } ;


// Ein Eingabezeichen in die Zustandsmachine eingeben
function TFakePDP11M9312.SerialWriteByte(b: byte) : boolean ;
  var
    c: char ;
    valid_command: boolean ;
  begin
    // Wenn die fiktive CPU aktiv ist, gibts keine Console
    if isRunning then
      Exit ;

    c := char(b and $7f) ; // 7Bit
    result := true ; // write klappt immer

    if TheState = pdp11M9312stateHALTed then begin
      // Sonderbedingung: nur ESC als CTRL-BOTT ist erlaubt
      if c = #$1b then begin
        Reset ;
      end;
      Exit ;
    end;

    // Alle anderen States

    { Eingabe an String anhängen. Danach prüfen, ob Eingabe fertig

      Eingabeprozessor, wie im ROM implmenetiert:
      Zeilenende nur mit ^M = #13 = CR

      RUBOUT = $7f: Zeile löschen

      1. Eingabezeichen nach Prompt: wird immer geechot + gespiechert,
         löst nie Fehler aus.
      2. Eingabezeichen nach prompt: löst verarbeitung aus
         Wenn nicht SPACE: Registerdump+prompt
         (auch ^J = LF)
         Wenn illegal opcode: Regsiterdump+prompt

         ACHTUNG: 2stellige Bootcodes hier beachten!!!!
         "DL" -> nicht ungültiges DEPOSIT, sodnern Boot RL0
         "DL0", "DL01", "DL000" ist erlaubt
         bei illegalem Bootcode "DC": prompt
          (mit Registerdump, wenn noch ein ROM-Sockel leer ist!!!

         siehe proecedure "isBootCommand"

                E

      weitere Zeichen: nur Oktal 0..7 erlaubt.
      illegal char = sofort neue Prompt, aber kein Registerdump?
     }

    // RUBOUT löscht die Eingabe
    if c = #$7f then begin
      SerialInBuff := '' ;
      doPrompt ;
    end else if c = CHAR_CR then begin
      // Befehlszeile ausführen

      // das 1. CR wird einfach geechot. Das 2te führt dann zur Ausführung
      if SerialInBuff = '' then begin
        SerialInBuff := c ;
      end else if doCommand(SerialInBuff, {check=}false) then begin
        doPrompt(false) ;
      end else if isBootCommand(SerialInBuff) then begin
        // Bootcommandos wie Programstart: HALT nach einiger zeit
        doSTART ;
        doPrompt(false) ;
      end else begin
        // wenn nur 1 Zeichen + CR getippt wird, wird das unten nicht abgefangen!
        doInputError({with_registers=}true) ;
      end;
    end { "if c = CHAR_CR" }
    else if (c = ' ') and (SerialInBuff = 'E') then begin
      // Ausnahme: 'E ' bewirkt Examin, ohne dass CR nötig ist
      SerialInBuff := SerialInBuff + c ;
      print(c) ;
      doEXAM ;
      doPrompt(false) ;
    end else
      case length(SerialInBuff) of
        0: begin
          // 1. Zeichen immer in den Buffer
          SerialInBuff := SerialInBuff + c ;
          print(c) ;
        end;
        1: begin
          // 2. Zeichen in den Buffer
          // prüfen, ob opcode oder bootcode gültig
          SerialInBuff := SerialInBuff + c ;
          print(c) ;
          valid_command:= false ;
          // kann das noch ein gültiger Befehl werden?
          if doCommand(SerialInBuff, {check=}true) then
            valid_command := true
          else if isBootCommand(SerialInBuff) then
            valid_command := true ;
          if not valid_command then begin
            doInputError({with_regsiters=}true) ; // leert SerialInBuff
          end;
        end{ "case length(SerialInBuff) of 1:" } ;
        else begin // octalzeichen anfügen
          SerialInBuff := SerialInBuff + c ;
          print(c) ;
          if not CharInSet(c, ['0'..'7']) then
            doInputError({with_regsiters=}false) ; // ohne registeranzeige
        end ;
      end{ "case length(SerialInBuff)" } ;
  end{ "function TFakePDP11M9312.SerialWriteByte" } ;


end{ "unit FakePDP11M9312U" } .
