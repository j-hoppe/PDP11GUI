unit BlinkenlightInstructionsU;
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

{ erzeugt aus einer memorycellgroup
  eine Anweisung, wie der Speicher über eine DEC blinkenlight console zu
  füllen ist
}

interface

uses
  Windows,
  Classes,
  SysUtils,
  AuxU,
  MemoryCellU,
  AddressU
  ;

type
  TBlinkenlightInstructions = class(TObject)
    private
      procedure puts(fmt: string ; args: array of const) ;
      procedure putln(s: string) ; overload ;
      procedure putln(fmt: string ; args: array of const) ; overload ;
      procedure _Generate(mcg: TMemoryCellGroup ;
              memorycontentttext: TStrings;
              show_programcontrol: boolean ;
              startaddr: dword
              ) ;

    public
      OutputLines: TStringlist ; // Ergebnis

      constructor Create ;
      destructor Destroy ;
      procedure Generate(mcg: TMemoryCellGroup) ; overload ;// ohne Startprogramm
      procedure Generate(mcg: TMemoryCellGroup ; listing: TStrings ; startaddr: dword) ; overload ;// mit Startprogramm
    end{ "TYPE TBlinkenlightInstructions = class(TObject)" } ;

implementation

uses
  FormAboutU ;


{ TBlinkenlightInstructions }

constructor TBlinkenlightInstructions.Create;
  begin
    inherited ;
    OutputLines := TStringlist.Create ;
  end;

destructor TBlinkenlightInstructions.Destroy;
  begin
    OutputLines.Free ;
    inherited ;
  end;



procedure TBlinkenlightInstructions.puts(fmt: string ; args: array of const) ;
  var n: integer ;
  begin
    n := OutputLines.Count - 1 ; // idx of last line
    if n < 0 then begin
      // create first line
      OutputLines.Add('') ;
      n := 0 ;
    end ;
    OutputLines[n] := OutputLines[n] + Format(fmt, args) ;
  end;

procedure TBlinkenlightInstructions.putln(s: string) ;
  begin
    OutputLines.Add(s) ;
  end;

procedure TBlinkenlightInstructions.putln(fmt: string ; args: array of const) ;
  begin
    putln(Format(fmt, args)) ;
  end;


function getBit(val:dword ; bitnr: integer): boolean ;
  begin
    result := ((val shr bitnr) and 1) = 1 ;
  end ;

function Bit2UpDown(bit: boolean): string ;
  begin
    if bit then result :='UP'
    else result := 'DOWN' ;
  end;

function Octal2Switches(value: dword ; switchcount: integer): string ;
  var
    searchbitval: boolean ; // zeige liste von 0en, oder von 1en an?
    searchbitcount: integer ; // soviele Bits der gewählten Sorte sind in "value"
    i : integer ;
    i0: integer ; // block start
    i1: integer ; // block end
    s1: string ;
  begin
    result := '' ;
    // String wie "SWITCHES(S) 15-13,10-9,4 UP, all others DOWN'
    // Oder wie "SWITCHE(S) 12,2 DOWN, all others UP'

    searchbitval := true ;
    searchbitcount := 0 ; // zaehlt erstmal 1en
    for i := switchcount-1 downto 0  do
      if getBit(value, i) then inc(searchbitcount) ;
      // Die Ukkehrdarstellung der DOWN switches ist eher verwirrend, disable!
      (*
    if searchbitcount > (switchcount div 2) then begin
      // zuviele 1en, zeige lieber die 0en an
      searchbitval := false ;
      searchbitcount := switchcount - searchbitcount ;
    end;
     *)
    s1 := '' ; // nur den range: "14,12,8-7"
    i := switchcount-1 ;
    while i >= 0 do begin

      // skip Bereich aus nicht zu suchenden Bits
      while (i >= 0) and (getBit(value, i) <> searchbitval) do
        dec(i) ;
      i0 := i ; // start des Bereichs mit zu suchenden bits gefunden.
      while (i >= 0) and (getBit(value, i) = searchbitval) do
        dec(i) ;

      // i1 jetzt NACH dem bereich
      i1 := i ; // wenn i0 = i1: leerer Bereich. i1 may be -1
      if i1 = i0-1 then begin // range ist 1 bit gross
        if s1 <> '' then s1 := s1 +',' ;
        s1 :=  s1 + Format('%d', [i0]) ;
      end else if i1 < i0-1 then begin // range ist mehrere bits gross
        if s1 <> '' then s1 := s1 +',' ;
        s1 :=  s1 + Format('%d-%d', [i0, i1+1]) ;
      end;
    end{ "while i >= 0" } ;
    if searchbitcount = 0 then
      result := Format('all switches %s', [Bit2UpDown(not searchbitval)])
    else if searchbitcount = 1 then
      result := Format('all switches %s, then switch %s %s', [
              Bit2UpDown(not searchbitval), s1, Bit2UpDown(searchbitval)])
    else
      result := Format('all switches %s, then switches %s %s', [
              Bit2UpDown(not searchbitval), s1, Bit2UpDown(searchbitval)])

  end{ "function Octal2Switches" } ;


// ConsoleLEDPicture(): anzeige der LEDs wie auf der PDP-11 console
//   linecode=0:  ' _ ___ __ __ ___ __ __ ___ __ __ ___ __ __ ___ __ __ _ '
//   linecode=1:  '[ O | O  O  O |       O |         |         |         ]'
//   linecode=2:  '[___|_________|_________|_O__O__O_|_O__O__O_|____O____]'
//   linecode=3:  ' 15  14 13 12  11 10  9   8  7  6   5  4  3   2  1  0  '
function ConsoleLEDPicture(linecode: integer ; value: dword ; ledcount: integer): string ;
  const
    //                                   1         2         3         4         5
    //                          123456789012345678901234567890123456789012345678901234

    led_picture_hline: string = ' _ ___ __ __ ___ __ __ ___ __ __ ___ __ __ ___ __ __ _ ' ;
    led_picture_addr : string = '[ + | +  +  + | +  +  + | +  +  + | +  +  + | +  +  + ]' ;
    led_picture_data : string = '[_+_|_+__+__+_|_+__+__+_|_+__+__+_|_+__+__+_|_+__+__+_]' ;
    led_picture_bitnr: string = ' 15  14 13 12  11 10  9   8  7  6   5  4  3   2  1  0  ' ;
    led_picture_ledpos: array[0..15] of integer = (
      53,50,47, 43,40,37, 33,30,27, 23,20,17, 13,10,7, 3
    ) ;
  var
    i: integer ;
    s: string ;
    c: char ;
  begin { "function ConsoleLEDPicture" }
    case linecode of
      0: s := led_picture_hline ;
      1: s := led_picture_addr ;
      2: s := led_picture_data ;
      3: s := led_picture_bitnr ;
      else raise Exception.Create('ConsoleLEDImage(): linecount must be 1..3') ;
    end;

    if linecode in [1,2] then
      // die LED Zustände an die richtigen Stellen im picture string schreiben
      // (da, wo die '+' stehen).
      for i := 0 to ledcount - 1 do begin
        if getBit(value, i) then
          c := 'O'// burning LED
        else c := '.' ;  // dark LED
        s[led_picture_ledpos[i]] := c ;
      end;
    result := s ;
  end{ "function ConsoleLEDPicture" } ;


// erzeuge Anweisung
// mcg: der Memoryinhalt
// memorycontentttext: wenn nicht nil: Text, der anzeigt, was einegegne werden soll
//    wenn nil: Anzeige von 'mcg'
// show_programcontrol: Anweisungen, wie man das Programmstartet/stopt/Singlestept
procedure TBlinkenlightInstructions._Generate(mcg: TMemoryCellGroup ;
        memorycontentttext: TStrings;
        show_programcontrol: boolean ;
        startaddr: dword) ;
  var
    // line: string ;
    wordcount: dword ;
    lastaddr, curaddr: dword ;
    val: dword ;
    DEPOSIT_sequence_first_step, DEPOSIT_sequence_last_step: integer ;
    i: integer ;
    step: integer ;
    //s: string ;
  begin
    assert(mcg.mat > matAnyPhysical) ;

    OutputLines.Clear ;

    wordcount := mcg.Count;

    putln('*** Instructions, how to fill a PDP-11 memory over blinkenlight console') ;
    putln('*** Generated by PDP11GUI %s, %s', [FormAbout.VersionStr, DateTimeToStr(Now)]) ;
    putln('') ;

    if wordcount <= 0 then begin
      raise Exception.Create('No memory cells to dump!') ;
    end ;

    putln('*************** Memory content to dump **********************') ;
    if memorycontentttext = nil then begin // keine anders formatierte Anzeige
      putln('  address  data') ;
      putln('  ------   ----') ;
      //       777777   17777
      for i := 0 to wordcount - 1 do begin
        curaddr := mcg.Cell(i).addr.val ;
        val :=  mcg.Cell(i).edit_value ;
        if val <> MEMORYCELL_ILLEGALVAL then begin
          putln('  %6s   %s', [Dword2OctalStr(curaddr, 16), Dword2OctalStr(val, 16)]) ;
        end;
      end;
    end else begin // eigene Anzeige
      for i := 0 to memorycontentttext.Count - 1 do
        putln(memorycontentttext[i]) ;
    end;

    putln('*************************************************************') ;



    step := 0 ;
    putln('') ;
    putln('Phase 1 - Preparation') ;
    putln('---------------------') ;
    putln('          - Set all switches to normal position.') ;
    putln('          - Set HALT/RUN switch to HALT position.') ;
    putln('          - Power ON.') ;
    putln('          - Perform a lamp test.') ;
    putln('          - Machines like PDP-11/70: Set upper rotary switch to "CONS PHY"') ;
    putln('            and lower rotary switch to "DATA PATH".') ;
    inc(step) ;

    lastaddr :=  MEMORYCELL_ILLEGALVAL ;
    DEPOSIT_sequence_first_step := step ;
    putln('') ;
    putln('Phase 2 - Enter data') ;
    putln('--------------------') ;
    for i := 0 to wordcount - 1 do begin
      curaddr := mcg.Cell(i).addr.val ;
      val := mcg.Cell(i).edit_value ;
      if val <> MEMORYCELL_ILLEGALVAL then begin

        if (lastaddr = MEMORYCELL_ILLEGALVAL) or (curaddr <> lastaddr+2) then begin
          // new start address
          putln('Step 2.%d   ', [step]) ;
          putln('          To do: Initialize current address to %s', [Dword2OctalStr(curaddr, 16)]) ;
          putln('            How: Set %s. Then press LOAD ADRS.', [Octal2Switches(curaddr,16)]) ;
          putln('          Check: LED display =          %s', [ConsoleLEDPicture(0, curaddr,16)]) ;
          putln('                                ADDRESS %s', [ConsoleLEDPicture(2, curaddr,16)]) ;
          putln('                                        %s', [ConsoleLEDPicture(3, 0, 0)]) ;
          inc(step) ;
        end ;

        putln('Step 2.%d   ', [step]) ;
        putln('          To do: Fill memory at address %s with value %s', [
                Dword2OctalStr(curaddr, 16), Dword2OctalStr(val, 16)]) ;
        putln('            How: Set %s. Then raise DEP.', [Octal2Switches(val,16)]) ;
        putln('          Check: LED display =          %s', [ConsoleLEDPicture(0, curaddr,16)]) ;
        putln('                                ADDRESS %s', [ConsoleLEDPicture(1, curaddr,16)]) ;
        putln('                                   DATA %s', [ConsoleLEDPicture(2, val,16)]) ;
        putln('                                        %s', [ConsoleLEDPicture(3, 0, 0)]) ;
        lastaddr := curaddr ;
        inc(step) ;
      end{ "if val <> MEMORYCELL_ILLEGALVAL" } ;
    end { "for i" } ;
    DEPOSIT_sequence_last_step := step-1 ;

    putln('') ;
    putln('Phase 3 - Verify data') ;
    putln('---------------------') ;
    putln('Step 3.%d - 3.%d', [DEPOSIT_sequence_first_step, DEPOSIT_sequence_last_step]) ;
    putln('          To do: Read the memory again and verify content.') ;
    putln('            How: Repeat steps 2.%d. - 2.%d., but press the EXAM switch instead of raising DEP.', [
            DEPOSIT_sequence_first_step, DEPOSIT_sequence_last_step]) ;
    putln('          Check: The LED patterns must be look like shown above.') ;
    inc(step) ;


    if show_programcontrol then begin

//    putln('') ;
//    putln('*** Program start: If the above was a program you like to start,') ;
//    putln('*** and the start address is %s, then proceed:', [Dword2OctalStr(startaddr, 16)]) ;

      putln('') ;
      putln('Phase 4 - Program control') ;
      putln('-------------------------') ;
      putln('Step 4.1  ') ;
      putln('          To do: Set program start address to %s', [Dword2OctalStr(startaddr, 16)]) ;
      putln('            How: Set %s. Then press LOAD ADRS.', [Octal2Switches(startaddr,16)]) ;
      putln('          Check: LED display =          %s', [ConsoleLEDPicture(0, startaddr,16)]) ;
      putln('                                ADDRESS %s', [ConsoleLEDPicture(2, startaddr,16)]) ;
      putln('                                        %s', [ConsoleLEDPicture(3, 0, 0)]) ;

      putln('Step 4.2   ') ;
      putln('          To do: Start program with arguments.') ;
      putln('                 Enter data over switches, Reset UNIBUS and start CPU.') ;
      putln('            How: Set ENABLE/HALT switch to the ENABLE position.') ;
      putln('                 Set the DATA switches as desired.') ;
      putln('                 Depress and release the START switch.') ;
      putln('          Check: RUN LED goes ON.') ;
      putln('                 If program stops by running onto HALT, RUN LED is OFF again.') ;

      putln('Step 4.3   ') ;
      putln('          To do: HALT, single step and continue CPU') ;
      putln('            How: To halt a running program, set ENABLE/HALT switch to the HALT position.') ;
      putln('          Check: Run LED goes OFF.') ;
      putln('            How: To single step, leave ENABLE/HALT in HALT position,') ;
      putln('                 and press CONT for each program step.') ;
      putln('            How: To start the program again, set ENABLE/HALT switch to ENABLE.') ;
      putln('          Check: Run LED goes ON.') ;
    end{ "if show_programcontrol" } ;
  end{ "procedure TBlinkenlightInstructions._Generate" } ;


// Aufruf OHNE Anleitung zum Programmstart
procedure TBlinkenlightInstructions.Generate(mcg: TMemoryCellGroup);
  begin
    _Generate(mcg, nil, false, 0)  ;
  end;

// Aufruf mit Listingfile und MIT Anleitung zum Programmstart
procedure TBlinkenlightInstructions.Generate(mcg: TMemoryCellGroup ; listing: TStrings ; startaddr: dword);
  begin
    _Generate(mcg, listing, true, startaddr)  ;
  end;

end{ "unit BlinkenlightInstructionsU" } .
