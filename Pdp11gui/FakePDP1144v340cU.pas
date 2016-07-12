unit FakePDP1144v340cU;
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
  Simuliert eine rudimentäre PDP-11/44 console.
  Es ist die undokumentierte Firmware V 3.40C

  Sie dient nur zum Test der GUI

  - Deposit, , mit /n, /g
  - Examine, /n, /g
  - ^C

}
interface

uses
  Windows, Classes, SysUtils,
  JH_Utilities,
  AddressU,
  FakePDP11GenericU ;

type
  TFakePDP1144v340C = class(TFakePDP11Generic)
    private
      // Zustandsvariable
      last_examine_addr : TMemoryAddress ;
      last_deposit_addr : TMemoryAddress ;
      last_deposit_display_memaddr: dword ;

      // letzter Fehlerstring
      last_error: string ;

      rubout_active : boolean ; // für besonders Echo bei Löschen mit RUBOUT
      rubout_echochar: char ; // zecihen,das weiderholt wird, wenn man zu oft rubout drückt

      // Consol-Commandos verarbeiten
      procedure doPrompt ; // Eingabe reset

      procedure doDeposit(opcode_args: TStringList; s_addr, s_val: string) ;
      procedure doExamine(opcode_args: TStringList; s_addr:string) ;
      procedure doInit ;
      procedure doStart(opcode_args: TStringList; s_addr:string) ;

    public
      constructor Create ;

      procedure PowerOn ; override ;
      procedure Reset ; override ;

      // Interface zur serialle Console
      function SerialReadByte(var b: byte) : boolean ; override ;
      function SerialWriteByte(b: byte) : boolean ; override ;
    protected
      // So tun, als ob eine laufende CPU auf ein HALT gerannt waere
      procedure setMem(addr: TMemoryAddress ; val: word) ; override ;
      function getMem(addr: TMemoryAddress): dword ; override ;

      procedure doHalt ; override ;


    end{ "TYPE TFakePDP1144v340C = class(TFakePDP11Generic)" } ;


implementation

uses
  Forms,
  OctalConst,
  AuxU,
  MemoryCellU
  ;

const

  CHAR_BS = #$08 ; // ^H

  // die 16 globalen Register R0..R7, R10..R17 werden kürzer addressiert
  global_register_base = _17777700 ;
  global_register_blocksize = 16 ;


constructor TFakePDP1144v340C.Create ;
  begin
    inherited Create(matPhysical22);
  end;

// Speicher löschen
procedure TFakePDP1144v340C.PowerOn ;
  var i: integer ;
  begin
    for i := 0 to PhysicalMemorySize - 1 do
      mem[i] := 0 ;

    SerialInBuff := '' ;
    SerialOutBuff := #0 + #0 + #0 + #0 + #0 + CHAR_CR + CHAR_LF
            +'(Console V3.40C)' + CHAR_CR + CHAR_LF + CHAR_CR + CHAR_LF
            + '(Program)' ;
    Reset ;
  end;

procedure TFakePDP1144v340C.Reset ;
  begin
    setMem(ProgramCounterAddr, _165714) ;

    SerialInBuff := '' ;
    SerialOutBuff := SerialOutBuff + CHAR_CR + CHAR_LF + CHAR_CR + CHAR_LF + '(Console)'
                + CHAR_CR + CHAR_LF ;
//    last_examine_addr := OctalStr2Dword('17777707', ThePhysicalAddressWidth) ;
//    doPrompt ;
    doHalt ;
  end;

procedure TFakePDP1144v340C.setMem(addr: TMemoryAddress ; val: word) ;
  begin
    try
      inherited setMem(addr, val) ;
    except
      last_error := '?Bus timeout error?' ;
      // kein test auf gerade Adressen
    end ;
  end;


function TFakePDP1144v340C.getMem(addr: TMemoryAddress): dword;
  begin
    result := 0 ;
    try
      result := inherited getMem(addr) ;
    except
      last_error :='?Bus timeout error?' ;
      // kein test auf gerade Adressen                                                 '
    end;
  end;




// Interface zur serielle Console
function TFakePDP1144v340C.SerialReadByte(var b: byte) : boolean ;
  begin
    if last_error <> '' then begin
      SerialOutBuff := CHAR_CR + CHAR_LF
              + last_error ;
      doPrompt ;
    end;
    last_error := '' ;
    if SerialOutBuff = '' then
      result := false // buffer leer: nix zu lesen da!
    else begin
      // ältestes Zeichen zurückgeben
      b := byte(SerialOutBuff[1]) ;
      SerialOutBuff := Copy(SerialOutBuff, 2, maxint) ;
      result := true ;
    end;
  end{ "function TFakePDP1144v340C.SerialReadByte" } ;


function TFakePDP1144v340C.SerialWriteByte(b: byte) : boolean ;
  var
    c: char ;
    opcode:string ;
    parm1: string ;
    parm2: string ;
    i, n: integer ;
    opcode_args: TStringList ;
  begin
    c := char(b) ;

    result := true ; // write klappt immer

    {
      TODO:
      Unterscheiden zwischen
      a) Console/Program mode für serial I/O
      b) CPU halted / running
    }

    if isRunning then begin
        // Program mode.
        // cpu is running : only key accepted is ^P
      if c = CHAR_CONTROL_P then begin// Control-P
         Halt ;
        SerialOutBuff := SerialOutBuff
        + CHAR_CR + CHAR_LF
        + '(Console)'
        + CHAR_CR + CHAR_LF
        + '^P'
        + CHAR_CR + CHAR_LF ;
        doPrompt ;
      end ;
    end else begin
    // Console mode
    if c <> CHAR_BS then begin
      if rubout_active then // signal: end des rubouts
        SerialOutBuff := SerialOutBuff + '\' ;
      rubout_active := false ;
    end;

    if c = CHAR_CONTROL_C then begin// Control-C
//    doBreak ;
      SerialOutBuff := SerialOutBuff + '^C' + CHAR_CR + CHAR_LF ;
      doPrompt ;
    end else if c = CHAR_CR then begin
      // eingetippte Zeile auswerten
      //versteht:
      // D addr val
      // D + val
      // D/G i val
      // E addr
      // E
      // E/G i
      opcode_args := TStringList.Create ;
      try
        opcode := UpperCase(ExtractWord(1, SerialInBuff, [' '])) ;
        parm1 := UpperCase(ExtractWord(2, SerialInBuff, [' '])) ;
        parm2 := UpperCase(ExtractWord(3, SerialInBuff, [' '])) ;

        // opcode kann mehrere modifier haben: "E/G/N:66"

        n := WordCount(opcode, ['/']) ;
        for i := 2 to n do
          opcode_args.Add(ExtractWord(i, opcode, ['/'])) ;
        opcode := ExtractWord(1, opcode, ['/']) ;
        if opcode = 'D' then
          doDeposit(opcode_args, parm1, parm2)
        else if opcode = 'E' then
          doExamine(opcode_args, parm1)
        else if opcode = 'S' then
          doStart(opcode_args, parm1)
        else if opcode = 'I' then
          doInit
        else if opcode = 'H' then begin
         if not isRunning then
          last_error := '?Already halted' ;
        end else if opcode = '' then
          // only CR: new prompt
        else begin
          last_error := '?What?' ;
        end;

      finally
        opcode_args.Free ;
      end{ "try" } ;
      if not isRunning then
         doPrompt ;
    end { "if c = CHAR_CR" } else if c = CHAR_BS then begin
      // die RUBOUT-Logik ist etwas hohl
      n := length(SerialInBuff) ;
      if not rubout_active then begin
        // Rubout beginnt
        if n = 0 then
          rubout_echochar := #0 ; // rubout beginnt auf leerem string: nix echo
        SerialOutBuff := SerialOutBuff + '\' ;
        rubout_active := true ;
      end;
      if n = 0 then begin // wiederhole uenndlich letzes gelöschtes Zeichen
        if rubout_echochar <> #0 then
          SerialOutBuff := SerialOutBuff + rubout_echochar ;
      end else begin
        // Es ist noch was da zum Löschen:
        // dann: letzes zeichen aus in_buff_echo
// Seltsam: wenn es nix mehr zu löschen gibt, wird trotzdem mit
// dem letzten buchstaben geechot, obwohl er schon weg ist.
        rubout_echochar := SerialInBuff[n] ;
        SerialOutBuff := SerialOutBuff + rubout_echochar ;
        SerialInBuff := Copy(SerialInBuff, 1, n-1) ;
      end;
    end { "if c = CHAR_BS" } else begin
      SerialInBuff := SerialInBuff + c ;
      // Zeichen echo
      if c< #32 then // echo ^code
        SerialOutBuff := SerialOutBuff + Controlcode2Mnemonic(c)
      else
        SerialOutBuff := SerialOutBuff + c ;
    end;
    end;
  end{ "function TFakePDP1144v340C.SerialWriteByte" } ;


procedure TFakePDP1144v340C.doPrompt ;
  begin
    if last_error <> '' then begin
      SerialOutBuff := SerialOutBuff
              + CHAR_CR + CHAR_LF + last_error + CHAR_CR + CHAR_LF;
      last_error := '' ;
    end;

    SerialOutBuff := SerialOutBuff + CHAR_CR + CHAR_LF + '>>>' ;
    rubout_active := false ;
    SerialInBuff := '' ;
  end;


// args: 'G' -> s_addr = oktale Nummer eines globalen Registers R0..R17
// 'N:<count>' -> die nächsten <count> Adressen mit demselben Wert füllen
procedure TFakePDP1144v340C.doDeposit(opcode_args: TStringList; s_addr, s_val: string) ;
  var i, n: integer ;
    s: string ;
    addr: TMemoryAddress ;
    addr_inc: integer ;
    display_memaddr: dword ;
    val: word ;
  begin
    addr.mat := mat;
    addr.val := 0 ;
    val := 0 ;
    last_error := '' ;
    n := 1 ;
    addr_inc := 2 ; // Adressen in 2er-Sprüngen, ausser bei 'G'

    // addr erstmal normal interpretieren. addr kann '+' oder addr sein
    if s_addr = '+'  then begin
      addr.val := last_deposit_addr.val + 2 ;
      display_memaddr := last_deposit_display_memaddr + 2 ;
    end
    else
      try
        addr := OctalStr2Addr(s_addr, matPhysical22) ;
      except
        last_error := '?Context?' // zahlenformat
      end;

    for i := 0 to opcode_args.Count - 1 do begin
      if UpperCase(opcode_args[i]) = 'G'  then begin
          addr_inc := 1 ;
          display_memaddr := addr.val ;
          addr.val := global_register_base + addr.val ;
      end;
      if UpperCase(opcode_args[i][1]) = 'N'  then begin
        s := ExtractWord(2, opcode_args[i], [':']) ;
        try
          n := OctalStr2Dword(s, 16) ;
        except
          last_error := '?Context?' ;
        end;
      end;
    end{ "for i" } ;

    try
      val := OctalStr2Dword(s_val, 16) ;
    except
      last_error := '?Context?'
    end;
    // Wertebereich by AND
    addr.val := addr.val and _17777777 ;

    // n Speicherzellen setzen
    if last_error = '' then
      for i := 0 to n-1 do begin
        setMem(addr, val) ;
        last_deposit_addr := addr ;
        last_deposit_display_memaddr := display_memaddr ;
        addr.val := addr.val + addr_inc ;
        display_memaddr := display_memaddr + addr_inc ;
      end;

  end{ "procedure TFakePDP1144v340C.doDeposit" } ;


// args: 'G' -> s_addr = oktale Nummer eines globalen Registers R0..R17
// args: 'N:<count>' liefere die nächsten <count> Adressen, kombinierbar mit 'G'
procedure TFakePDP1144v340C.doExamine(opcode_args: TStringList; s_addr: string) ;
  var i, n: integer ;
    s: string ;
    addr: TMemoryAddress ;
    addr_inc: integer ;
    display_memprefix: char ;
    display_memaddr: dword ;
    val: word ;
  begin
    addr.mat := mat;
    addr.val := 0 ;
    last_error := '' ;
    n := 1 ;
    addr_inc := 2 ; // Adressen in 2er-Sprüngen, ausser bei 'G'

    // Addr erstmal normal interpretieren. s_addr kann leer sein , dann auto inc
    if (s_addr = '') then
      addr.val := last_examine_addr.val + 2
    else
      try
        addr := OctalStr2Addr(s_addr, mat) ;
      except
        last_error := '?What?' ;
      end;

    display_memprefix := 'P' ; // default: physical mem
    display_memaddr := addr.val ;
    for i := 0 to opcode_args.Count - 1 do begin
      if UpperCase(opcode_args[i]) = 'G'  then begin
        display_memprefix := 'G' ;
        addr_inc := 1 ;
        display_memaddr := addr.val ; // show param secified at /g
        addr.val := global_register_base + addr.val ;
      end;
      if UpperCase(opcode_args[i][1]) = 'N'  then begin
        s := ExtractWord(2, opcode_args[i], [':']) ;
        try
          n := OctalStr2Dword(s, 16) ;
        except
          last_error := '?Context?' ;
        end;
      end;
    end{ "for i" } ;

    addr.val := addr.val and _17777777 ; // mask to 22 bit

    if last_error = '' then begin
      for i := 0 to n-1 do begin
        SerialOutBuff := SerialOutBuff
                + CHAR_CR + CHAR_LF
                +'  '+display_memprefix+ '  '
                + Dword2OctalStr(display_memaddr,22) ;
        if (display_memprefix = 'G') and (display_memaddr > 32) then
          last_error := '?Too big' ; // illegal cpu register
        if last_error = '' then begin
          val := getMem(addr) ;
          last_examine_addr := addr ;
        end;
        if last_error = '' then begin
          // Echo: '  P <addr> <val>
          // oder '  P <addr>' \n?Bus timeout error?'
          SerialOutBuff := SerialOutBuff + '  ' + Dword2OctalStr(val, 16) ;
        end else
          Break ; // end loop
        addr.val := addr.val + addr_inc ;
        display_memaddr  := display_memaddr + addr_inc ;
      end{ "for i" } ;
      SerialOutBuff := SerialOutBuff + CHAR_CR + CHAR_LF ;
    end { "if last_error = ''" } ;
  end{ "procedure TFakePDP1144v340C.doExamine" } ;


// nur I
procedure TFakePDP1144v340C.doInit ;
  var starttime: dword ;
  begin
    // warte 1 sek
    starttime := getTickCount ;
    repeat
      Application.ProcessMessages ; // Backgroundempfang
      sleep(1) ;
    until getTickCount > starttime+1000 ;
//  doPrompt ;
  end;


// Aufruf: 'S addr
procedure TFakePDP1144v340C.doStart(opcode_args: TStringList; s_addr: string) ;
  var
    val: dword ;
  begin
    if s_addr <> '' then begin
      // nur die letzten 6 Ziffern beachten
      s_addr := Copy(s_addr, length(s_addr)-5, maxint) ;
      val := OctalStr2Dword(s_addr, 0) ;
      if val = MEMORYCELL_ILLEGALVAL then
        raise EFakePDP11Error.Create('') ;
    SerialOutBuff := SerialOutBuff
            + CHAR_CR + CHAR_LF
            + CHAR_CR + CHAR_LF
            + '(Program)'
            + CHAR_CR + CHAR_LF ;

      RunToHalt(val) ;
    end;
  end;


procedure TFakePDP1144v340C.doHalt ;
  var pcval: dword ;
  begin
    pcval := getMem(ProgramCounterAddr) ;
    // if program runs on HALT
    SerialOutBuff := SerialOutBuff
            + CHAR_CR + CHAR_LF
            + '(Console)'
            + CHAR_CR + CHAR_LF
            + '  Halted at ' + Dword2OctalStr(pcval, 16)
            + CHAR_CR + CHAR_LF ;
    doPrompt ;
  end;



end{ "unit FakePDP1144v340cU" } .
