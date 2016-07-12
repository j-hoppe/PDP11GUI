unit Pdp1144MicroCodeU;
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
  Klasse, die den Mirkocode der PDP-11/44 versteht
  - kann die Mikroprogramme aus den gescannten Listings aus
    EY-C3012-RB-001 PDP-1144 Processor Maintenance Supplementary Listings (microcode) Apr-81
   interpretieren.
 Ein uCode Listing besteht aus mehreren Pages diesen Formats:
; 44OUTU.MCR [160,1311] Micro-2.1 1B(41)        14:3:34 14-Sep-1979         COMBINED 1144 AND FLOATING POINT FIELD DEFS Page 22
; 44FLWU,FP  [160,1311J

U 0461, 0000,2042,0001,4140,0140,3033,4000,0422,017     ;1062   461:    2-I:    R0_R0+1,J/1-A
                                                        ;1063
U 0732, 0043,6005,0104,0140,3740,3033,4000,0422,017     ;1064   732:    2-J:    DATO,UDATA,J/2-L
                                                        ;1065
 Die Felder sind:
 - "U"
 - addr - Adresse des mikroSteps
    "0732"
 - nextaddr - Adresse des  nächsten mikroSteps (wenn kein Branch)
    wird im Infofeld mit "J/<nextaddrlabel>" wieder erwähnt
    "0043,"
 - Mehr bits
    "6005,0104,0140,3740,3033,4000,0422,017"

 - Zeilennummer

}

interface

uses
  Windows, Classes,
  SysUtils,
  JH_Utilities,
  Contnrs,
  OctalConst
  ;

type

// ein Bitfeld in einem  µ-Intruction bitvector
  TPDP1144MicroInstructionFieldDef = record
      tag:integer; // für verknüpfung mit bitfieldEnumeration
      name:string;
      def: integer; // default wert
      lsb: integer ; // Offset des lsb im 14-bit Vector
      len: integer ;
    end ;

  // beschreibt einen Wert dere Bits in einem Bitfled
  TPDP1144MicroInstructionFieldEnumDef = record
      tag: integer ; // referenz auf BitField
      val:integer ; // wert des Bitfelds. decimal geschreiben, aber octal!
      txt: string ; // Klartext
    end;


const
  PDP1144MicroInstructionFields_Count = 37 ;
  PDP1144MicroInstructionFieldDefs: array[0..PDP1144MicroInstructionFields_Count-1]
          of TPDP1144MicroInstructionFieldDef = (
    // KD11Z
    (tag: 1 ; name:'NEXT MICROWORD ADDRESS' ; def: -1 ; lsb:93 ; len:10),
    (tag: 2 ; name:'AMUX CONTROL'           ; def:  1 ; lsb:91 ; len:2),
    (tag: 3 ; name:'MISC CONTROL'           ; def:  0 ; lsb:88 ; len:3),
    (tag: 4 ; name:'LOAD BA'                ; def:  0 ; lsb:87 ; len:1),
    (tag: 5 ; name:'CYCLE'                  ; def:  1 ; lsb:86 ; len:1),
    (tag: 6 ; name:'ALU/BLEG CONTROL'       ; def:  5 ; lsb:81 ; len:5),
    (tag: 7 ; name:'AUX CONTROL'            ; def:  0 ; lsb:80 ; len:1),
    (tag: 8 ; name:'B,BX,OVX,DBE CONTROL'   ; def:  0 ; lsb:76 ; len:4),
    (tag: 9 ; name:'DATA TRAN'              ; def:  0 ; lsb:75 ; len:1),
    (tag:10 ; name:'ENAB MAINT'             ; def:  0 ; lsb:74 ; len:1),
    (tag:11 ; name:'SSMUX CONTROL'          ; def:  0 ; lsb:72 ; len:2),
    (tag:12 ; name:'UNIBUS CONTROL'         ; def:  0 ; lsb:70 ; len:2),
    (tag:13 ; name:'SCRATCH PAD DST SELECT' ; def:  0 ; lsb:68 ; len:2),
    (tag:14 ; name:'BUT ENABLE'             ; def:  0 ; lsb:64 ; len:4),
    (tag:15 ; name:'SRC REG OR 1'           ; def:  1 ; lsb:63 ; len:1),
    (tag:16 ; name:'PREVIOUS MODE'          ; def:  1 ; lsb:62 ; len:1),
    (tag:17 ; name:'BUT SERV'               ; def:  0 ; lsb:61 ; len:1),
    (tag:18 ; name:'FORCE KERNEL'           ; def:  0 ; lsb:60 ; len:1),
    (tag:19 ; name:'SRI CONTROL'            ; def:  0 ; lsb:58 ; len:2),
    (tag:20 ; name:'I/D SPACE'              ; def:  0 ; lsb:57 ; len:1),
    // Bit 56 not used
    (tag:21 ; name:'ROM SCRATCH PAD ADDRESS'; def: 15 ; lsb:52 ; len:4),
    (tag:22 ; name:'SCRATCH PAD SRC SELECT' ; def:  3 ; lsb:50 ; len:2),
    (tag:23 ; name:'S0/S1 CONSTANT CONTROL' ; def:  0 ; lsb:48 ; len:2),
    // FP11
    (tag:51 ; name:'FCTL'                   ; def:  3 ; lsb:42 ; len:6),
    (tag:52 ; name:'ECTL'                   ; def:  3 ; lsb:36 ; len:6),
    (tag:53 ; name:'ECIN'                   ; def:  0 ; lsb:35 ; len:1),
    (tag:54 ; name:'BSEL'                   ; def:  3 ; lsb:33 ; len:2),
    (tag:55 ; name:'ASEL'                   ; def:  1 ; lsb:32 ; len:1),
    (tag:56 ; name:'TOUT'                   ; def:  0 ; lsb:31 ; len:1),
    (tag:57 ; name:'DCTL'                   ; def:  0 ; lsb:27 ; len:4),
    (tag:58 ; name:'XTRA'                   ; def:  0 ; lsb:26 ; len:1),
    (tag:59 ; name:'BUT'                    ; def:  0 ; lsb:20 ; len:6),
    (tag:60 ; name:'CONST'                  ; def:  8 ; lsb:14 ; len:6),
    (tag:61 ; name:'MISC'                   ; def:  9 ; lsb:10 ; len:4),
    (tag:62 ; name:'AROM'                   ; def:  0 ; lsb: 7 ; len:3),
    (tag:63 ; name:'BROM'                   ; def:  0 ; lsb: 4 ; len:3),
    (tag:64 ; name:'SECTOR'                 ; def: 15 ; lsb: 0 ; len:4)
  ) { "constPDP1144MicroInstructionFieldDefs: array[0..PDP1144MicroInstructionFields_Coun..." } ;

  // wird ein Feldwert hier nicht defineirt, wird er numerisch angezeigt
  PDP1144MicroInstructionFieldEnumDef_Count = 130 ;
  PDP1144MicroInstructionFieldEnumDefs: array[0..PDP1144MicroInstructionFieldEnumDef_Count-1]
          of TPDP1144MicroInstructionFieldEnumDef = (
    // amux control
    ( tag: 2 ; val:   0 ; txt:'PSW' ),
    ( tag: 2 ; val:   1 ; txt:'ALU' ),
    ( tag: 2 ; val:   2 ; txt:'VECT' ),
    ( tag: 2 ; val:   3 ; txt:'UBUS' ),
    // misc control
    ( tag: 3 ; val:   0 ; txt:'NOP' ),
    ( tag: 3 ; val:   1 ; txt:'LOAD IR' ),
    ( tag: 3 ; val:   2 ; txt:'LOAD PSW' ),
    ( tag: 3 ; val:   3 ; txt:'LOAD CC' ),
    ( tag: 3 ; val:   4 ; txt:'BUT DEST' ),
    ( tag: 3 ; val:   5 ; txt:'ENAB STOV' ),
    ( tag: 3 ; val:   6 ; txt:'LOAD COUNT' ),
    ( tag: 3 ; val:   7 ; txt:'CLK COUNT' ),
    // load ba
    ( tag: 4 ; val:   0 ; txt:'' ),
    ( tag: 4 ; val:   1 ; txt:'BA' ),
    // cycle
    ( tag: 5 ; val:   0 ; txt:'LONG CYCLE' ),
    ( tag: 5 ; val:   1 ; txt:'SHORT CYCLE' ),
    // alu and bleg control
    ( tag: 6 ; val:   0 ; txt:'ZERO' ),
    ( tag: 6 ; val:   1 ; txt:'~A' ),
    ( tag: 6 ; val:   2 ; txt:'A PLUS 1' ),
    ( tag: 6 ; val:   3 ; txt:'A MINUS 1' ),
    ( tag: 6 ; val:   4 ; txt:'A MINUS B' ),
    ( tag: 6 ; val:   5 ; txt:'A' ),
    ( tag: 6 ; val:   6 ; txt:'B' ),
    ( tag: 6 ; val:   7 ; txt:'MINUS ONE' ),
    ( tag: 6 ; val: _10 ; txt:'A PLUS B' ),
    ( tag: 6 ; val: _11 ; txt:'A • B' ), // and ?
    ( tag: 6 ; val: _12 ; txt:'~A • B' ),
    ( tag: 6 ; val: _13 ; txt:'A + B' ),  // or
    ( tag: 6 ; val: _14 ; txt:'A XOR B' ),
    ( tag: 6 ; val: _15 ; txt:'A • ~B' ),
    ( tag: 6 ; val: _16 ; txt:'A • ~BX' ),
    ( tag: 6 ; val: _17 ; txt:'A • BX' ),
    ( tag: 6 ; val: _20 ; txt:'A PLUS B PLUS 1' ),
    ( tag: 6 ; val: _21 ; txt:'A PLUS BX' ),
    ( tag: 6 ; val: _22 ; txt:'A MINUS BX' ),
    ( tag: 6 ; val: _23 ; txt:'A PLUS BX PLUS 1' ),
    ( tag: 6 ; val: _24 ; txt:'A PLUS 2' ),
    ( tag: 6 ; val: _25 ; txt:'A MINUS 2' ),
    ( tag: 6 ; val: _26 ; txt:'A PLUS A' ),
    ( tag: 6 ; val: _27 ; txt:'BX' ),
    ( tag: 6 ; val: _30 ; txt:'~B' ),
    ( tag: 6 ; val: _31 ; txt:'~BX' ),
    ( tag: 6 ; val: _32 ; txt:'A PLUS A PLUS 1' ),
    // aux ctrl
    ( tag: 7 ; val:   0 ; txt:'' ),
    ( tag: 7 ; val:   1 ; txt:'AUX' ),
    // B,BX,OVX,DBE CONTROL
    ( tag: 8 ; val:   0 ; txt:'HOLD' ),
    ( tag: 8 ; val:   1 ; txt:'LOAD B' ),
    ( tag: 8 ; val:   2 ; txt:'LOAD BX' ),
    ( tag: 8 ; val:   3 ; txt:'SHF LFT(BX-0), LOAD B' ),
    ( tag: 8 ; val:   4 ; txt:'SHF LFT(BX-COUT), LOAD B' ),
    ( tag: 8 ; val:   5 ; txt:'SHF LFT(BX-1), LOAD B' ),
    ( tag: 8 ; val:   6 ; txt:'SHF LFT(B-0)' ),
    ( tag: 8 ; val:   7 ; txt:'SHF LFT(B-0), LOAD BX' ),
    ( tag: 8 ; val: _10 ; txt:'SHF LFT(B-BX15)' ),
    ( tag: 8 ; val: _11 ; txt:'SHF LFT(BX-0)' ),
    ( tag: 8 ; val: _12 ; txt:'SHF LFT(BX-1)' ),
    ( tag: 8 ; val: _13 ; txt:'SHF LFT(BX-OVX)' ),
    ( tag: 8 ; val: _14 ; txt:'SHF LFT(BX-COUT)' ),
    ( tag: 8 ; val: _15 ; txt:'SHF LFT(B-BX-0)' ),
    ( tag: 8 ; val: _16 ; txt:'SHF RT(B15-B-BX)' ),
    ( tag: 8 ; val: _17 ; txt:'ENAB DBE' ),
    // data tran
    ( tag: 9 ; val:   0 ; txt:'' ),
    ( tag: 9 ; val:   1 ; txt:'TRAN' ),
    // enab maint
    ( tag:10  ; val:  0 ; txt:'' ),
    ( tag:10  ; val:  1 ; txt:'MAINT' ),
    // ssmux control
    ( tag:11 ; val:   0 ; txt:'STRT' ),
    ( tag:11 ; val:   1 ; txt:'SEX' ),
    ( tag:11 ; val:   2 ; txt:'SWAB' ),
    ( tag:11 ; val:   3 ; txt:'EXTRNL' ),
    // unibus control
    ( tag:12 ; val:   0 ; txt:'DATI' ),
    ( tag:12 ; val:   1 ; txt:'DATIP' ),
    ( tag:12 ; val:   2 ; txt:'DATO' ),
    ( tag:12 ; val:   3 ; txt:'DATOB' ),
    // scratch pad dst select
    ( tag:13 ; val:   0 ; txt:'RBA' ),
    ( tag:13 ; val:   1 ; txt:'RS' ),
    ( tag:13 ; val:   2 ; txt:'RD' ),
    ( tag:13 ; val:   3 ; txt:'ROM' ),
    // but enable
    ( tag:14 ; val:   0 ; txt:'NOP' ),
    ( tag:14 ; val:   1 ; txt:'N BIT' ),
    ( tag:14 ; val:   2 ; txt:'Z BIT' ),
    ( tag:14 ; val:   3 ; txt:'C05' ),
    ( tag:14 ; val:   4 ; txt:'BOOT' ),
    ( tag:14 ; val:   5 ; txt:'BX00' ),
    ( tag:14 ; val:   6 ; txt:'BX01' ),
    ( tag:14 ; val:   7 ; txt:'COUT' ),
    ( tag:14 ; val: _10 ; txt:'NO SERV' ),
    ( tag:14 ; val: _11 ; txt:'N BIT, Z BIT' ),
    ( tag:14 ; val: _12 ; txt:'BX00, N BIT' ),
    ( tag:14 ; val: _13 ; txt:'C05, BX01, BX00' ),
    ( tag:14 ; val: _14 ; txt:'C05, BX01, BX00' ),
    ( tag:14 ; val: _15 ; txt:'ALL' ),
    ( tag:14 ; val: _16 ; txt:'BX00, C05' ),
    // src reg or 1
    ( tag:15 ; val:   0 ; txt:'RS + 1' ),
    ( tag:15 ; val:   1 ; txt:'' ),
    // previous mode
    ( tag:16 ; val:   0 ; txt:'PREV MODE' ),
    ( tag:16 ; val:   1 ; txt:'' ),
    // but serv
    ( tag:17 ; val:   0 ; txt:'' ),
    ( tag:17 ; val:   1 ; txt:'SERV' ),
    // force kernel
    ( tag:18 ; val:   0 ; txt:'' ),
    ( tag:18 ; val:   1 ; txt:'FORCE KERNEL' ),
    // sri control
    ( tag:19 ; val:   0 ; txt:'NOP' ),
    ( tag:19 ; val:   1 ; txt:'SRI LOW' ),
    ( tag:19 ; val:   2 ; txt:'SRI HI' ),
    ( tag:19 ; val:   3 ; txt:'ZERO SRI' ),
    // i/d space
    ( tag:20 ; val:   0 ; txt:'I SPACE' ),
    ( tag:20 ; val:   1 ; txt:'D SPACE' ),
    // rom scratch pad address
    ( tag:21 ; val:   0 ; txt:'R0' ),
    ( tag:21 ; val:   1 ; txt:'R1' ),
    ( tag:21 ; val:   2 ; txt:'R2' ),
    ( tag:21 ; val:   3 ; txt:'R3' ),
    ( tag:21 ; val:   4 ; txt:'R4' ),
    ( tag:21 ; val:   5 ; txt:'R5' ),
    ( tag:21 ; val:   6 ; txt:'R6(SP)' ),
    ( tag:21 ; val:   7 ; txt:'R7(PC)' ),
    ( tag:21 ; val: _10 ; txt:'R10' ),
    ( tag:21 ; val: _11 ; txt:'R11' ),
    ( tag:21 ; val: _12 ; txt:'R12' ),
    ( tag:21 ; val: _13 ; txt:'R13' ),
    ( tag:21 ; val: _14 ; txt:'R14' ),
    ( tag:21 ; val: _15 ; txt:'R15' ),
    ( tag:21 ; val: _16 ; txt:'R16' ),
    ( tag:21 ; val: _17 ; txt:'R17' ),
    // scratch pad src sel
    ( tag:22 ; val:   0 ; txt:'RBA' ),
    ( tag:22 ; val:   1 ; txt:'RD' ),
    ( tag:22 ; val:   2 ; txt:'RS' ),
    ( tag:22 ; val:   3 ; txt:'ROM' ),
    // constant control
    ( tag:23 ; val:   0 ; txt:'K0' ),
    ( tag:23 ; val:   1 ; txt:'K16' ),
    ( tag:23 ; val:   2 ; txt:'K26' ),
    ( tag:23 ; val:   3 ; txt:'K366' )
  ) { "constPDP1144MicroInstructionFieldEnumDefs: array[0..PDP1144MicroInstructionFieldEn..." } ;



type

  // Info ein bitfeld
  TPDP1144MicroInstructionField = record
      name: string ;
      value: dword ; // numerischer Wert
      text: string ; // Anzeigewert
    end;

  TPDP1144MicroInstruction = class(TObject)
    public
      // roh aus listingfile
      raw_code: array [0..8] of word ;
      // Werte in den Bitfeldern

      fields: array[0..PDP1144MicroInstructionFields_Count-1] of TPDP1144MicroInstructionField ;

      filename: string ; // listingfile
      fileline: string ; // originale Zeile

      linenr: integer ; //  ;1065

      // interpretiert
      addr: word ; // muss = raw_code[0] sein
      nextaddr: word ; // aus raw_code[1]

      symbolictag: string ; // symbolische adresse, wie "2-J"
      sortabletag: string ; // andere Darstellung für richtige sortierreihenfolge
      opcodes: TStringList ; // die Kommaliste, "DATO,UDATA,J/2-L"

      constructor Create ;
      destructor Destroy ; override ;

      procedure Parse(aFilename, aLine:string) ; // Eine Zeile aus dem Listingfile einlesen.
      procedure BuildFields ;

    end{ "TYPE TPDP1144MicroInstruction = class(TObject)" } ;



  TPDP1144MicroCode = class(TObjectList) // of TPDP1144uInstruction
    public
      listingfilenamepattern: string ; // die Listingfiles, die eingelesen wurden
      constructor Create ;
      destructor Destroy ; override ;

      function Instruction(idx: integer): TPDP1144MicroInstruction ;


      procedure LoadListingPages(filenamepattern:string) ;
      procedure Cleanup ;
      procedure Verify ;

      procedure Test ;

      function InstructionByAddr(aAddr:word): TPDP1144MicroInstruction ;
      function InstructionByTag(aTag:string): TPDP1144MicroInstruction ;
      function InstructionByLineNr(linenr: integer): TPDP1144MicroInstruction ;
    end{ "TYPE TPDP1144MicroCode = class(TObjectList)" } ;

// nach addr sortieren
function TPDP1144MicroInstruction_ListSortCompare_Addr(Item1, Item2: Pointer): integer;
// intelligent nach tag
function TPDP1144MicroInstruction_ListSortCompare_Tag(Item1, Item2: Pointer): integer;
// nach Zeilennr im Listing
function TPDP1144MicroInstruction_ListSortCompare_LineNr(Item1, Item2: Pointer): integer;

implementation

uses
  AuxU,
  FormMainU ;


// bei Formatfehler Exception!
procedure Error(info:string) ;
  begin
    raise Exception.Create(info) ;
  end;

procedure ErrorFmt(fmt:string; args: array of const) ;
  begin
    Error(Format(fmt, args)) ;
  end;




constructor TPDP1144MicroInstruction.Create ;
  begin
    inherited ;
    opcodes := TStringList.Create ;
  end;


destructor TPDP1144MicroInstruction.Destroy ;
  begin
    opcodes.Free ;
    inherited ;
  end;

// eine Zeile analysieren
procedure TPDP1144MicroInstruction.Parse(aFilename, aLine:string) ; // Eine Zeile aus dem Listingfile einlesen.

  var
    s: string ;
    val: word ;
    sl: TStringList ;
    i: integer ;
  begin
    sl := TStringList.Create ;
    try
      filename :=  aFilename ;
      fileline := aLine ;
      // Patternr:
      //U 0461, 0000,2042,0001,4140,0140,3033,4000,0422,017     ;1062   461:    2-I:    R0_R0+1,J/1-A
      // Word indexes:
      //1 2     3                                               4       5       6       7
      if Length(aLine) < 80 then Error('line len < 80') ;
      if aLine[1] <> 'U' then Error('first char must be "U"') ;
      s := ExtractWord(2, aLine, [' ', #9, ',']) ;
      val := 0 ;
      try    val := OctalStr2Dword(s, 16) ;
      except ErrorFmt('µWord address "%s" not an octal number', [s]) ;
      end ;
      addr := val ;

      sl.CommaText := ExtractWord(3, aLine, [' ', #9]) ;
      if sl.Count <> 9 then ErrorFmt('Binary data "%s": 9 code words expected', [sl.CommaText]) ;

      // die 9 words sind 8..0 numeriert
      for i := 0 to sl.Count - 1 do
        try     raw_code[i] := OctalStr2Dword(sl[8-i], 16) ;
        except  ErrorFmt('code field #%d "%s" is not an octal number', [8-i, sl[8-i]]) ;
        end;

      // ;linenr
      s := ExtractWord(4, aLine, [' ', #9]) ;
      if (Length(s) < 2) or (s[1] <> ';') then ErrorFmt('Line number "%s" must come after ";"', [s]) ;
      s := Copy(s, 2, maxint) ;
      try    linenr := StrToInt(s) ;
      except ErrorFmt('Line number "%s" is not a decimal number', [s]) ;
      end;

      // die step address nochmal mit ':' "732:"
      s := ExtractWord(5, aLine, [' ', #9]) ;
      if (Length(s) < 2) or (s[Length(s)] <> ':')  then ErrorFmt('µWord address in comment must look like "nnn:"', [s]) ;
      s := Copy(s, 1, Length(s)-1) ;
      try    val := OctalStr2Dword(s, 16) ;
      except ErrorFmt('µWord address "%s" in comment is not an octal number', [s]) ;
      end;
      if val <> addr then
        ErrorFmt('µWord address "%s" in comment differs from step addr %s',
                [Dword2OctalStr(val, 16),Dword2OctalStr(addr, 16)]) ;

      // das symbolic tag: "2-I"
      s := ExtractWord(6, aLine, [' ', #9]) ;
      if (Length(s) < 2) or (s[Length(s)] <> ':')  then ErrorFmt('µWord symbolic tag "%s" must end with ":"', [s]) ;
      symbolictag := Copy(s, 1, Length(s)-1) ;
      // hat das tag die Form [FP]<number>-<letters>?  ("page-block")
      // erzeuge sortierbare Darstellung:
      // - number immer 2 stellig
      // wenn kein prefiox "FP": setze "AA" ein (CPU vor PPP)
      // Ausnahme: "FP-33AA" kommt wirklich vor!
      if symbolictag = 'FP-33AA' then
        sortabletag := 'FP33-AA'
      else begin
        s := ExtractWord(1, symbolictag, ['-']) ;
        if (Length(s) > 2) and (s[1]='F') and (s[2]='P') then begin
          sortabletag := 'FP' ;
          s := Copy(s, 3, maxint) ;
        end else
          sortabletag := 'AA' ;
        try    i := StrToInt(s) ;
          sortabletag := sortabletag + Format('%0.2d',[i]) ;
        except ErrorFmt('µWord symbolic tag "%s" has not the form "[FP]<number>-<letters>"', [symbolictag]) ;
        end;
        s := ExtractWord(2, symbolictag, ['-']) ;
        if s = '' then ErrorFmt('µWord symbolic tag "%s" has not the form "[FP]<number>-<letters>"', [symbolictag]) ;
        for i := 1 to Length(s) do
          if not CharInSet(s[i], ['A'..'Z']) then ErrorFmt('µWord symbolic tag "%s" has not the form "[FP]<number>-<letters>"', [symbolictag]) ;
        sortabletag := sortabletag + '-' + s ;
      end { "if symbolictag = 'FP-33AA' ... ELSE" } ;

      // alle folgenden Zeilen gehören zu den Opcode
      s := ExtractWord(7, aLine, [' ', #9]) ;
      i := 8 ;
      while ExtractWord(i, aLine, [' ', #9]) <> '' do begin
        s := s + ' ' + ExtractWord(i, aLine, [' ', #9]) ;
        inc(i) ;
      end ;

      if s = '' then Error('opcode list missing') ;
      // an den Kommas trennen. "Commatext" geht nicht, weil es auch an Spaces trennt
      for i := 1 to WordCount(s, [',']) do
        opcodes.Add(ExtractWord(i, s, [','])) ;

    finally
      sl.Free ;
    end { "try" } ;

  end{ "procedure TPDP1144MicroInstruction.Parse" } ;


{ die Bitfelder der µInstruction aus raw_code[] bilden.
 Die Felder für KD11-Z CPU und FP11 sind zusammen 104 Bits lang
Lage der Microword bits in den Listing words
Listing         µInstruction      µInstruction
Words          KD11-Z+FPP         KD11-Z only
w[8] <10:0> = <103:93>            0:11
w[7] <11:0> = <92:81>             12:23
w[6] <11:0> = <80:69>             24:35
w[5] <11:0> = <68:57>             36:47
w[4] <11:0> = <56:45>             48:55  (4 LSB für FPP)
w[3] <11:0> = <44:33>
w[2] <11:0> = <32:21>
w[1] <11:0> = <20:9>
w[0] <8:0>  = <8:0>
}
procedure TPDP1144MicroInstruction.BuildFields ;
  const
    wordidx2bitpos: array[0..8] of integer // listing word pos nach vector bits
            = ( 0, 9, 21, 33, 45, 57, 69, 81, 93) ;
    wordidx2bitlen: array[0..8] of integer // valid bits in listing words
            = ( 9, 12, 12, 12, 12, 12, 12, 12, 12) ;
    bitlen2mask: array[0..12] of integer
            = ( 0, 1, 3, 7, $0f, $1f, $3f, $7f, $ff, $1ff, $3ff, $7ff, $fff ) ;
  var
    i, j: integer ;
    fieldlsbpos: integer ; // bitaddr des lsb
    fieldbitlen: integer ;
    fieldtag: integer ;
    vector, tmp: dword ; // bitvektor während der Montage
    enumdef: TPDP1144MicroInstructionFieldEnumDef ;
  begin { "procedure TPDP1144MicroInstruction.BuildFields" }
    for i := 0 to PDP1144MicroInstructionFields_Count-1 do begin
      fieldlsbpos := PDP1144MicroInstructionFieldDefs[i].lsb ;
      fieldbitlen := PDP1144MicroInstructionFieldDefs[i].len ;
      fieldtag := PDP1144MicroInstructionFieldDefs[i].tag ;
      // 1) joine die benötigten raw_code
      j := 0 ;
      while fieldlsbpos >= (wordidx2bitpos[j] + wordidx2bitlen[j] ) do inc(j) ;
      // bitfield[i] beginnt in raw_code [j]
      // word[j], word[j+1] joinen ... das reicht für alle Feldlängen
      vector := (raw_code[j] and bitlen2mask[wordidx2bitlen[j]]) ;
      if j < 8 then begin
        tmp := (raw_code[j+1] and bitlen2mask[wordidx2bitlen[j+1]]) ;
        tmp := tmp shl wordidx2bitlen[j] ;  // links neben vorheriges feld setzen
        vector := vector or tmp ;
      end;
      // Vector beginnt an word Grenze
      vector := vector shr (fieldlsbpos - wordidx2bitpos[j]) ; //
      // LSB des vector ist jetzt bit 'lsbpos'

      // überflüssige MSB ausmaskieren
      vector := vector and bitlen2mask[fieldbitlen] ;

      // Fertig!
      fields[i].value := vector ;
      fields[i].name := PDP1144MicroInstructionFieldDefs[i].name ;
      // Default: "?"
      fields[i].text := '?'; //Dword2OctalStr(fields[i].value,fieldbitlen) ;
      // Enumeration text für den value da? Suche nach tag des feldes und value.
      for j := 0 to PDP1144MicroInstructionFieldEnumDef_Count-1 do begin
        enumdef := PDP1144MicroInstructionFieldEnumDefs[j] ;
        if (enumdef.tag = fieldtag) and (enumdef.val = fields[i].value) then begin
          fields[i].text := enumdef.txt ;
          break ; // fertig
        end;
      end;

    end{ "for i" } ;
  end{ "procedure TPDP1144MicroInstruction.BuildFields" } ;


// nach addr sortieren
function TPDP1144MicroInstruction_ListSortCompare_Addr(Item1, Item2: Pointer): integer;
  begin
    result := TPDP1144MicroInstruction(Item1).addr - TPDP1144MicroInstruction(Item2).addr ;
  end;


// nach symbolic tag sortieren. die haben die Form
// <prefix><number>-<postfix>
// Bsp: J2-AA < J20-Z
// dabei am "-" trennen
function TPDP1144MicroInstruction_ListSortCompare_Tag(Item1, Item2: Pointer): integer;
  var s1, s2: string ;
  begin
    s1 := TPDP1144MicroInstruction(Item1).sortabletag ;
    s2 := TPDP1144MicroInstruction(Item2).sortabletag ;
    result := CompareStr(s1, s2) ;
  end;


function TPDP1144MicroInstruction_ListSortCompare_LineNr(Item1, Item2: Pointer): integer;
  begin
    result := TPDP1144MicroInstruction(Item1).linenr - TPDP1144MicroInstruction(Item2).linenr ;
  end;




constructor TPDP1144MicroCode.Create ;
  begin
    inherited ;
    OwnsObjects := true ; // verwaltet die TPDP1144uInstruction selbst
  end;

destructor TPDP1144MicroCode.Destroy ;
  begin
    inherited ;
  end;


function TPDP1144MicroCode.Instruction(idx: integer): TPDP1144MicroInstruction ;
  begin
    result := Items[idx] as TPDP1144MicroInstruction ;
  end;


function TPDP1144MicroCode.InstructionByAddr(aAddr:word): TPDP1144MicroInstruction ;
  var i: integer ;
  begin
    for i := 0 to Count - 1 do begin
      result := TPDP1144MicroInstruction(Items[i]) ;
      if result.addr = aAddr then Exit ; // found
    end;
    result := nil ; // not found
  end ;


function TPDP1144MicroCode.InstructionByTag(aTag:string): TPDP1144MicroInstruction ;
  var i: integer ;
  begin
    for i := 0 to Count - 1 do begin
      result := TPDP1144MicroInstruction(Items[i]) ;
      if result.symbolictag = aTag then Exit ; // found
    end;
    result := nil ; // not found
  end;


function TPDP1144MicroCode.InstructionByLineNr(linenr: integer): TPDP1144MicroInstruction ;
  var i: integer ;
  begin
    for i := 0 to Count - 1 do begin
      result := TPDP1144MicroInstruction(Items[i]) ;
      if result.linenr = linenr then Exit ; // found
    end;
    result := nil ; // not found
  end;


procedure TPDP1144MicroCode.LoadListingPages(filenamepattern:string) ;

// Regeln zum gewinnen von Code-Zeilen
// 1) die ersten 2 zeilen in jedem File ignorieren (header)
// 2)   ignoriere Leerzeilen
// 3) Wenn Zeile[1..2] = 'U ': Code Zeile
// 4) Wenn keine Linennumber ";n" in 58,59
//    append Zeile an vorherige Zeile, wenn das eine Codezeile war.
// 5) wenn linennumber da, aber kein code: ignoriere Zeile
// flags: has_code, has_lineno

// Das hier erkennen:
//U 0461, 0000,2042,0001,4140,0140,3033,4000,0422,017     ;1062   461:    2-I:    R0_R0+1,J/1-A
//add this to precious line
//                                                        ; just another comment
  procedure LoadListingPage(filename:string) ;
    var
      line, nextline: string ; // nächste, unverabreitete Zeile
      i: integer ;
      f: System.text ;
      has_code, has_linenr: boolean ; // see comment
      s: string ;
      microInst: TPDP1144MicroInstruction ;
      lastlinenr: integer ;
    begin
      filename := ExpandUncFileName(filename) ;
      Log('TPDP1144MicroCode.LoadListingPages(): reading file %s', [filename]) ;

      AssignFile(f, filename) ;
      try
        Reset(f) ;
//        readln(f, nextline) ; readln(f, nextline) ; // die ersten beiden Zeilen ignorieren
        line := '' ;
        lastlinenr := -1 ;
        repeat
          if Eof(f) then
            nextline := '<EOF>'  // marker: keine Continuation, eject
          else begin
            readln(f, nextline) ;

            if (nextline = '') or (nextline[1] = ';') then
              Continue ; // skip line

            for i := 1 to Length(nextline) do
              if not CharInSet(nextline[i], [' ', 'A'..'Z','0'..'9',
                      '!', '.', ':',';',',','_','-','+','*','/', '=', '(', ')',
                      '\', '&']) then
                raise Exception.CreateFmt('Illegales Zeichen "%s" = #%d:'  + #13
                        +'file="%s"'+#13
                        +'line="%s"'+#13, [nextline[i], ord(nextline[i]),filename,nextline]) ;
          end { "if Eof(f) ... ELSE" } ;

// Check: nur [A-Z, 0-9,
          // leerzeile, oder reine Kommentarzeile?
          s := trim(nextline) ;
          if (s <> '') and (s[1] = ';') then nextline := '' ;
//        if pos('U 0506', nextline) > 0 then
//          has_code := false ; // break heere

          has_code := (Copy(nextline, 1, 2) = 'U ') ;
          has_linenr := true ;
          // reiner kommentar, oder Continuation?
          if Length(nextline) < 60 then has_linenr := false
//        else if nextline[57] <> ';' then has_linenr := false
          else if nextline[57] = ' ' then has_linenr := false
          else if not CharInSet(nextline[58], ['0'..'9']) then has_linenr := false ;

          if (has_code and has_linenr) or (nextline = '<EOF>') then begin
            // nächste Zeile ist leer, oder hat selber Code:
            // Codezeile ist fertig
            if line <> '' then begin // wenn nextline 1. Zeile: noch nix da
              microInst := TPDP1144MicroInstruction.Create ;
              try
                microInst.Parse(filename, line) ;
                microInst.BuildFields ;
                microInst.nextaddr := microInst.fields[0].value ; // eval next microword address

                if (lastlinenr >= 0) and (microInst.linenr < lastlinenr) then
                  ErrorFmt('line numbers not in sequence: last=%d, now=%d', [lastlinenr, microInst.linenr]) ;
                lastlinenr := microInst.linenr ;

                Add(microInst) ; // rein in die Liste
              except on e: Exception do begin
                  microInst.Free ;
                  raise Exception.CreateFmt(
                          'Error while parsing µCode: file="%s"'
                          + #13 + 'Line="%s"'
                          + #13 + 'Error="%s"',
                          [filename, line, e.Message]) ;
                end;
              end{ "try" } ;
            end{ "if line <> ''" } ;

            line := nextline ;
          end { "if (has_code and has_linenr) or (nextline = '<EOF>')" } else
            if not has_linenr and not has_code and (nextline <> '') then
              // Continuation montieren
              line := line + nextline ;
        until nextline = '<EOF>' ;
      finally
        CloseFile(f) ;
      end{ "try" } ;
    end { "procedure LoadListingPage" } ;

  var
    srec: TSearchRec ;
    res:  integer ;
    filepath:string ;
  begin { "procedure TPDP1144MicroCode.LoadListingPages" }
    self.listingfilenamepattern := filenamepattern ;
    Clear ;
    try
      filepath:= ExtractFilePath(filenamepattern) ;
      res := FindFirst(filenamepattern, 0, srec) ;
      while res = 0 do begin
        LoadListingPage(filepath + '\' + srec.name) ;
        res := FindNext(srec) ;
      end;
    finally
      FindClose(srec) ;
    end;
    Cleanup ;
    Verify ;
    // Test ;
  end{ "procedure TPDP1144MicroCode.LoadListingPages" } ;


// sortieren, notation ändern
procedure TPDP1144MicroCode.Cleanup ;
  var
    i, j, k: integer ;
    s, s1: string ;
  begin
    // in opcodes den Zuweisungsoperator '_' in ':=' wandeln
    for i := 0 to Count-1 do
      with TPDP1144MicroInstruction(Items[i]) do
        for j := 0 to opcodes.Count - 1 do begin
          s := opcodes[j] ;
          s1 := '' ;
          for k := 1 to Length(s) do
            if s[k] = '_' then
              s1 := s1 + ':='
            else s1 := s1 + s[k] ;
          opcodes[j]  := s1 ;
        end;
  end{ "procedure TPDP1144MicroCode.Cleanup" } ;


// logische tests
procedure TPDP1144MicroCode.Verify ;
  var
    i: integer ;
    s0, s1: string ;
    microInst, nextMicroInst : TPDP1144MicroInstruction ;
  begin
    // Alle Adressen unique?
    Sort(TPDP1144MicroInstruction_ListSortCompare_Addr) ;

    for i := 0 to Count - 2 do begin
      s0 := Dword2OctalStr(TPDP1144MicroInstruction(Items[i]).addr, 16) ;
      s1 := Dword2OctalStr(TPDP1144MicroInstruction(Items[i+1]).addr, 16) ;
      if s0 = s1 then
        raise Exception.CreateFmt('Duplicate addr "%s":'+#13
                + 'file1="%s"' + #13
                + 'line1="%s"' + #13
                + 'file2="%s"' + #13
                + 'line2="%s"', [s0,
                TPDP1144MicroInstruction(Items[i]).filename,
                TPDP1144MicroInstruction(Items[i]).fileline,
                TPDP1144MicroInstruction(Items[i+1]).filename,
                TPDP1144MicroInstruction(Items[i+1]).fileline
                ]) ;
    end{ "for i" } ;

    // Alle symbolic tags unique?
    Sort(TPDP1144MicroInstruction_ListSortCompare_Tag) ;

    for i := 0 to Count - 2 do begin
      s0 := TPDP1144MicroInstruction(Items[i]).symbolictag ;
      s1 := TPDP1144MicroInstruction(Items[i+1]).symbolictag ;
      if s0 = s1 then
        raise Exception.CreateFmt('Duplicate symbolic tag "%s":'+#13
                + 'file1="%s"' + #13
                + 'line1="%s"' + #13
                + 'file2="%s"' + #13
                + 'line2="%s"', [s0,
                TPDP1144MicroInstruction(Items[i]).filename,
                TPDP1144MicroInstruction(Items[i]).fileline,
                TPDP1144MicroInstruction(Items[i+1]).filename,
                TPDP1144MicroInstruction(Items[i+1]).fileline
                ]) ;
    end{ "for i" } ;


    // Für alle µWords:
    // Der Sprung zur nächsten Adresse muss in den Opcodes stehen
    for i := 0 to Count - 1 do begin
      microInst := TPDP1144MicroInstruction(Items[i]) ;
      // finde Folge-uInstruction über die numerische Adresse
//if microInst.nextaddr = 582 then
//idx := -1 ; // break here
      nextMicroInst := InstructionByAddr(microInst.nextaddr) ;
      if nextMicroInst = nil then
        raise Exception.CreateFmt('next µWord "%s" not found:'+#13
                + 'file="%s"' + #13
                + 'line="%s"', [
                Dword2OctalStr(microInst.nextaddr,12),
                microInst.filename, microInst.fileline]) ;

      s0 := Format('J/%s', [nextMicroInst.symbolictag]) ;
      if microInst.opcodes.IndexOf(s0) < 0 then
        raise Exception.CreateFmt('Jump to next addr "%s" not found in opcodes' + #13
                + 'file="%s"' + #13
                + 'line="%s"', [s0, microInst.filename, microInst.fileline]) ;
    end{ "for i" } ;
  end { "procedure TPDP1144MicroCode.Verify" } ;


procedure TPDP1144MicroCode.Test ;
  var
    // Indexe: [word, bit, 0/1]
    raw_bit_hist: array[0..8,0..15,0..1] of integer ;
    i0, i1, i2, i3: integer ;
    microInst: TPDP1144MicroInstruction ;
    w: word ;
    b: integer ;
    usedbits:array[0..8] of word ;
    s: string ;
  begin
    // welche bits in rawcode habe wie oft 0 und wie oft 1
    for i1 := 0 to 8 do
      for i2 := 0 to 15 do
        for i3 := 0 to 1 do
          raw_bit_hist [i1,i2,i3] := 0 ;

    // 0en und 1en zählen
    for i0 := 0 to Count-1 do begin
      microInst := TPDP1144MicroInstruction(Items[i0]) ;
      for i1 := 0 to 8 do begin
        w := microInst.raw_code[i1] ;
        for i2 := 0 to 15 do begin
          b := (w shr i2) and 1 ;
          raw_bit_hist[i1,i2,b] := raw_bit_hist [i1,i2,b] + 1 ;
        end;
      end;
    end;
    // welche bits sind immer konstant?
    for i1 := 0 to 8 do
      usedbits[i1] := 0 ;

    for i1 := 0 to 8 do
      for i2 := 0 to 15 do begin
        // bit "used", wenn 0en und 1en vorkommen
        if (raw_bit_hist[i1,i2,0] > 0) and (raw_bit_hist[i1,i2,1] > 0) then
          usedbits[i1] := usedbits[i1] or (1 shl i2) ;  // bit setzen, wenn "benutzt"
      end;
    s := '' ;
    for i1 := 0 to 8 do begin
      if s <> '' then s := s +',' ;
      s := s + Dword2OctalStr(usedbits[i1], 12) ;
    end;
    Log('Used microcode bits: %s', [s]) ;

  end{ "procedure TPDP1144MicroCode.Test" } ;


end{ "unit Pdp1144MicroCodeU" } .
