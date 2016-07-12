unit AuxU;
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

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  Forms,
  JvExGrids, JvStringGrid,
  ShellApi,
  JH_Utilities
  ;


const
  CHAR_CONTROL_C = #$3 ;
  CHAR_CONTROL_P = #$10 ;
  CHAR_CR = #$d ; // ^M
  CHAR_LF = #$a ; // ^J
  CHAR_ESC = #$1b ;


  ///// Einheitliche Farben //////
  // Markierung Program Counter in Source
  ColorCodeExecutionPositionBkGnd = TColor($9999CC) ; // wie in Delphi: rosa/braun
  ColorCodeExecutionPositionText = clBlack ;
  // Markierung Fehler in Source
  ColorCodeErrorBkGnd = clRed ;
  ColorCodeErrorText = clWhite ;

  // reaod-only Grid-Zellen
  ColorGridCellReadOnlyBkGnd = TColor($E0E0E0) ; // grauer Hintergrund
  ColorGridCellReadOnlyText = clBlack ;
  // geänderte Grid-Zellen
  ColorGridCellChangedBkGnd = TColor($80FFFF) ; // helles gelb
  ColorGridCellChangedText = clBlack ;


type

  TArrayOfWord = array of word ;

  EOctalConvertError = class(Exception) ;


  TRestZeit = class(TObject)
    private
      startValue: integer ;
      endValue: integer ;
      startTickCount: dword ;
      restTicks: dword ; // berechnete Restzeit
    public
      valid: boolean ;
      constructor Create ;
      procedure setStart(aStartValue, aEndValue: integer) ;
      procedure setCur(curvalue: integer) ;
      function getRestZeit: string ; // hh:mm:ss
    end;

var
  test0: integer ;

function isOctalDigit(c: char): boolean ; inline ;

function OctalStr2Dword(s:string ; bitwidth: integer = 0): dword ;
function Dword2OctalStr(val: dword ; fixbitwidth: integer = 0): string ;

function String2ID(caption: string): string ; // einheitliche Schreibweise

function StripQuotes(s: string): string ;

// Ändert in einer Caption der Form "Fenster title - irgend was"
// den Teil hinter dem "-"
function setFormCaptionInfoField(orgCaption:string ; newInfo: string):string ;
function getFormCaptionFixedField(aCaption:string ):string ;


// speichert die colwidths als Kommaliste im key "<formname>.ColWidhts"
procedure FormGridSaveColWidths(formname:string ; grid: TJvStringGrid) ;
// lädt ColWidths für ein grid aus dem Key "<formname>.ColWidhts"
procedure FormGridLoadColWidths(formname:string ; grid: TJvStringGrid) ;

function Character2PrintableText(c:char; showcontrolchars: boolean): string ; inline ;
function String2PrintableText(s:string; showcontrolchars: boolean): string ;
function Character2Text_Full(c:char; numeric: boolean): string ;
function Controlcode2Mnemonic(c: char) : string ;

procedure OpenTextFileInNotepad(fname:string) ;


procedure WaitAndProcessMessages(ms:dword) ;

procedure BreakHere ;



implementation

uses
  RegistryU,
  MemoryCellU ;


function isOctalDigit(c: char): boolean ;
  begin
    result := (c >= '0') and (c <= '7') ;
  end;


// Octale Zahl nach Wert wandeln.
// wenn wert nicht in bitwidth passt: MEMORYCELL_ILLEGALVAL
function OctalStr2Dword(s:string ; bitwidth: integer = 0): dword ;
  var
    i: integer ;
  begin
    s := Trim(s) ;
    if s = '' then
      raise EOctalConvertError.Create('Empty string can not be converted to octal!') ;
    if s = '?' then begin
      result := MEMORYCELL_ILLEGALVAL ;
      Exit ;
    end;

    result := 0 ;
    s := Trim(s) ;
    for i := 1 to length(s) do begin
      if not isOctalDigit(s[i]) then
        raise EOctalConvertError.CreateFmt('String "%s" can not be converted to octal!', [s]) ;
      result := (result shl 3) or dword(ord(s[i]) - ord('0')) ;
    end;
    if bitwidth > 0 then
      if result >= (1 shl bitwidth) then
        result := MEMORYCELL_ILLEGALVAL ;
  end{ "function OctalStr2Dword" } ;


// ein Wert in eine octalzahl wandeln
// wenn MEMORYCELL_ILLEGALVAL: "?"
// wenn fixbitwidth gesetzt ist: Ausgabe auf eine Breite bringen,
// die die angegebene Bitzahl halten kann
// BSp: val = 0x14, bitwidth =  0 ->  "24"
//      val = 0x14, bitwidth = 16 ->  "000024"
function Dword2OctalStr(val: dword; fixbitwidth: integer = 0): string ;
  var
    digit: integer ;
    fixdigitwidth: integer ;
  begin
    result := '' ;
    if val = MEMORYCELL_ILLEGALVAL then begin
      result := '?' ;
      Exit ;
    end;

    fixdigitwidth := (fixbitwidth+2) div 3 ; // 1,2,3->1, 4,5,6-> 2, ....

    while val > 0 do begin
      digit := val and 7 ; // Ergebnis von rechts nach links aufbauen
      val := val shr 3 ;
      result := char(digit + ord('0')) + result ;
    end ;
    if result = '' then
      result := '0' ;

    // Soll-länge erzeugen
    while length(result) < fixdigitwidth do
      result := '0' + result ;
  end{ "function Dword2OctalStr" } ;


function String2ID(caption: string): string ; // einheitliche Schreibweise
  var i: integer ;
  begin
    result := '' ;
    caption := AnsiUppercase(caption) ;
    for i := 1 to length(caption) do
      case caption[i] of
        'A'..'Z', '0'..'9', '_':
          result := result + caption[i] ;
        'µ':
          result := result + 'MIKRO' ;
      end;
  end;


// umgebende ".."  wegnehmen
function StripQuotes(s: string): string ;
  var n: integer ;
  begin
    n := length(s) ;
    if n < 2 then
      result := s
    else if (s[1] = '"') and (s[1] = s[n]) then
      result := Copy(s, 2, n-2)
    else result := s ;
  end;

function setFormCaptionInfoField(orgCaption:string ; newInfo: string):string ;
  var savedcaption: string ;
  begin
    // alles vor dem " - " ist die originale Caption
    savedcaption := getFormCaptionFixedField(orgCaption) ;
    if Trim(newInfo) = '' then
      result := savedcaption
    else
      result := Format('%s - %s', [savedcaption, newInfo]) ;
  end;

function getFormCaptionFixedField(aCaption:string ):string ;
  var i: integer ;
  begin
    // Annahme: alles vor dem " - " ist die originale Caption, dahinter kommen info Angaben
    // Also: "Macro Source - test.mac" => "Macro Source"
    // "PDP-11/44 µCode" => "PDP-11/44 µCode"
    i := pos(' - ', aCaption) ;
    if i > 0 then
      result := Trim(Copy(aCaption, 1, i-1))
    else result := Trim(aCaption) ;
  end;

// speichert die colwidths als Kommaliste im key "<formname>.ColWidhts"
procedure FormGridSaveColWidths(formname:string ; grid: TJvStringGrid) ;
  var i: integer ;
    sl: TStringList ;
  begin
    sl := TStringList.Create ;
    try
      with grid do
        for i := 0 to ColCount - 1 do
          sl.Add(IntToStr(ColWidths[i])) ;
      TheRegistry.Save(formname + '.ColWidths', sl.Commatext) ;
    finally
      sl.Free ;
    end;
  end{ "procedure FormGridSaveColWidths" } ;


// lädt ColWidths für ein grid aus dem Key "<formname>.ColWidhts"
procedure FormGridLoadColWidths(formname:string ; grid: TJvStringGrid) ;
  var i: integer ;
    sl: TStringList ;
  begin
    sl := TStringList.Create ;
    try
      sl.Commatext := TheRegistry.Load(formname + '.ColWidths', '') ;
      with grid do
        for i := 0 to ColCount - 1 do
          if i < sl.Count then
            ColWidths[i] := StrToInt(sl[i]) ;
    finally
      sl.Free ;
    end;
  end{ "procedure FormGridLoadColWidths" } ;


// Zeichen als sichtbaren string: "\xhh" für Steuerzeichen
function Character2PrintableText(c:char; showcontrolchars: boolean): string ; inline ;
  begin
    result := '' ;
    if (c >= #$20) and (c <= #$7E) then
      result := c  // always output ASCII printable characters
    else
      if showcontrolchars then
        case c of
          #0: result := '<NUL>' ;
          #7: result := '<BEL>' ;
          #8: result := '<BS>' ;
          #9: result := '<HT>' ;
          #$a: result := '<LF>' ;
          #$d: result := '<CR>' ;
          #$1b: result := '<ESC>' ;
          #$7f: result := '<DEL>' ;
          else
            result := Format('<\x%0.2x>', [ord(c)])
        end
  end{ "function Character2Text" } ;


// Zeichen als sichtbaren string: "\xhh" für Steuerzeichen
function String2PrintableText(s:string; showcontrolchars: boolean): string ;
var i: integer ;
begin
result := '' ;
      for i := 1 to length(s) do
      result := result + Character2PrintableText(s[i], showcontrolchars) ;
end;


  // like  CharVisible(), but #$20 is <spc>,
// and <\x> is always given
// 'a' -> 'a'
// ' ' -> '<SP>'
// ^C -> '<^C>'
// wenn 'numeric': append ' <\xhex>
// 'a' -> 'a <\x61>'
// ' ' -> '<SP> <\x20>'
// ^C -> '<^C> <\x03>'
function Character2Text_Full(c:char; numeric: boolean): string ;
  begin
    case c of
      #0: result := '<NUL>' ;
      #7: result := '<BEL>' ;
      #8: result := '<BS>' ;
      #9: result := '<HT>' ; // horizontal tab
      #$a: result := '<LF>' ;
      #$d: result := '<CR>' ;
      #$1b: result := '<ESC>' ;
      #$20: result := '<SP>' ; // space
      #$7f: result := '<DEL>' ;
      else
        if c <= #$20 then
          result := '<^' + char( ord('A') + ord(c) - 1) + '>'
        else
          result := c // printable char > #$20
    end { "case c" } ;
    if numeric then
      result := result + ' ' + Format('<\x%0.2x>', [ord(c)]) ;
  end{ "function Character2Text_Full" } ;


function Controlcode2Mnemonic(c: char) : string ;
begin
        if c <= #$20 then
          result := '^' + char( ord('A') + ord(c) - 1)
        else
          result := c // printable char > #$20
end;


constructor TRestZeit.Create ;
  begin
    inherited ;
    valid := false ;
  end;

procedure TRestZeit.setStart(aStartValue, aEndValue: integer) ;
  begin
    startValue := aStartValue ;
    endValue := aEndValue ;
    startTickCount := GetTickCount ;
    valid := true ;
  end ;

// aktuellen Wert setzen, danach funktioniert getRestZeit()
procedure TRestZeit.setCur(curvalue: integer) ;
  var
    // curtickcount: dword ;
    curticks: dword ; // seit start vergangene Ticks
    totalticks: dword ; // berechnete Gesamtzeit
    anteil: double ;
  begin
    if not valid then Exit ;
    curticks := GetTickCount - startTickCount ;

    // curValue < startValue: kann vorkommen, wenn ein Wunsch-Block
    // bagerundent wird, um auf den Track-Anfang zu kommen.
    if curvalue < startValue then
      startValue := curvalue ;  // start korrigieren

    // wieviel % sind vorbei?
    anteil := (curvalue - startValue) / (endValue - startValue) ;
    if anteil = 0 then
      restTicks := 0 // es hat noch nicht angefangen: gesamtzeit kann nicht berechnet werden
    else if anteil > 1 then
      restTicks := 1000 // dauert länger als erwartet: immer 1 sek anzeigen
    else begin
      totalticks := round(curticks / anteil) ;
      restTicks := totalticks - curticks ;
    end;
  end{ "procedure TRestZeit.setCur" } ;


function TRestZeit.getRestZeit: string ; // hh:mm:ss
  var
    h, m, s: integer ;
  begin
    result := '' ;
    if not valid then Exit ;

    s := restTicks div 1000 ;
    h := s div 3600 ;
    s := s mod 3600 ;
    m := s div 60 ;
    s := s mod 60 ;
    if (h > 0) then begin
      m := 10 * (m div 10) ;  // Minuten auf 10er runden
      result := Format('%d h %0.2d m', [h, m])
//    end else if h > 0 then
//      result := Format('%d h %0.2d m', [h, m]) // 1h x m
    end else if m > 0 then
      result := Format('%d m', [m]) // nur minuten
    else
      result := Format('%d s', [s]) ; // nur sekunden
  end{ "function TRestZeit.getRestZeit" } ;

procedure OpenTextFileInNotepad(fname:string) ;
  var
//    programs_path: string ;
    windows_path: string ;
    search_paths: string ;
    notepad_path: string ;
    buff1, buff2: array[0..1023] of char ;
  begin
    if not FileExists(fname) then
      raise Exception.CreateFmt('File to display "%s" does not exist', [fname]) ;

//    if not getEnv('PROGRAMFILES', programs_path) then
//      raise Exception.Create('System folder for programs in "%PROGRAMFILES%" not found') ;
    if not getEnv('WINDIR', windows_path) then
      raise Exception.Create('System folder for Windows in "%WINDIR%" not found') ;
//      search_paths := Format('"%s";"%s"', [windows_path,programs_path]) ;
    search_paths := Format('"%s"', [windows_path]) ;

    notepad_path := FileSearchRecursive('notepad.exe', search_paths) ;
    if notepad_path = '' then
      raise Exception.CreateFmt('file"notepad.exe" not found in folder %s', [search_paths]) ;
    ShellExecute(Application.Handle, 'open',
            strpcopy(buff1, notepad_path),
            strpcopy(buff2, fname),
            nil, SW_SHOWNORMAL) ;
  end{ "procedure OpenTextFileInNotepad" } ;

procedure WaitAndProcessMessages(ms:dword) ;
  var starticks: dword ;
  begin
    starticks := GetTickCount ;
    while GetTickCount < (starticks + ms) do
      Application.ProcessMessages;
  end;


  // dummy, soemtimes needed to have a line for break points
  procedure BreakHere ;
begin
end;


initialization
  test0 := 0 ;

end{ "unit AuxU" } .
