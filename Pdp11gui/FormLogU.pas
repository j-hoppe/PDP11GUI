unit FormLogU;
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  TypInfo, // GetEnumName
  Dialogs, StdCtrls, Menus,
  FormChildU ;

var
{$ifdef DEBUG}
  Connection_LogIoStream: boolean = false ; // ReadByte/WriteByte in Logform anzeigen
//  Connection_LogIoStream: boolean = true ; // ReadByte/WriteByte in Logform anzeigen
{$else}
Connection_LogIoStream: boolean = false ; // in release build nie loggen
{$endif}


const
  Connection_LogIoStream_Filename =  'pdp11gui.ser.log' ; // in TMP directory


// Definition einer Spalte:
// Index, Title, Breite
type
  TLogColumnIndex = (
      // Spalten der Logausgabe, in dieser Reihenfolge
      LogCol_Other,
      LogCol_PhysicalWriteByte,
      LogCol_PhysicalReadByte,
      LogCol_DecodeNextAnswerPhrase,
      LogCol_CriticalSection,
      LogCol_MonitorTimerCallback
    ) ;

type
  TFormLog = class(TFormChild)
      LogMemo: TMemo;
      PopupMenu1: TPopupMenu;
      Clear1: TMenuItem;
      procedure Clear1Click(Sender: TObject);
      procedure LogMemoMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
    private
      { Private-Deklarationen }
      LogFileLineCount: Integer ;
      LogFile: System.Text ;
      function getLogline(s:string): string ;

    public
      { Public-Deklarationen }
      procedure Clear ;
      procedure Log(s:string) ; overload ;
      procedure Log(fmt:string; args: array of const) ; overload ;
      procedure Log(lines:TStrings) ; overload ;

      procedure LogStrCol(aColidx: TLogColumnIndex ; logstr: string) ;

      procedure Log2Buffer(s:string) ;

    end{ "TYPE TFormLog = class(TFormChild)" } ;


    // global, for easy acces while debugging
    var LogBuffer: TStringList ;

implementation

{$R *.dfm}

uses
  JH_Utilities,
  AuxU , SerialIoHubU;


procedure TFormLog.Clear ;
  begin
    LogMemo.Clear ;
  end;

function TFormLog.getLogline(s:string): string ;
  var datestr: string ;
    i: Integer ;
begin
//    if UseLog then begin
    result := String2PrintableText(s, true) ;

    // Für Log Form: anderer Zeitstempel
    DateTimeToString(datestr, 'ddd hh:nn:ss.zzz', Now) ;
    result := Format('[%d: %s] %s', [LogMemo.Lines.Count+1, datestr, result]) ;
end;

procedure TFormLog.Log(s:string) ;
  begin
    LogStrCol(LogCol_Other, String2PrintableText(s, true)) ; // ohne Zeitstempel

    LogMemo.Lines.Add(getLogline(s)) ;
//      writeln(Logfile, s_out) ;
//      Flush(Logfile) ;
//    end ;
  end { "procedure TFormLog.Log" } ;

  procedure TFormLog.Log(lines:TStrings) ;
  var i: integer ;
  begin
  for i := 0 to lines.Count-1 do
    Log(lines[i]) ;
  end;


  // write into Logbuffer, high speed for Heisen-bugs
  procedure  TFormLog.Log2Buffer(s:string) ;
  begin
  LogBuffer.Add(getLogline(s)) ;
  end;


// String in Column "colidx" loggen
// Wenn String = '$HEADERS': Überschriftenzeile fabrizieren
procedure TFormLog.LogStrCol(aColidx: TLogColumnIndex ; logstr: string) ;

  const
    columwidth = 20 ;
    LogFileTitleIntervall = 20 ; // sooft die Spaltenuberschriften im Logfile wiederholen

  function gettabseppos(colidx:TLogColumnIndex):Integer ;
    begin
      // Strings starten mit 1, an i+1 steht der '|', start der Zelle an i+2
      result := (ord(colidx) * columwidth)+ 1 ;
    end;

  // leere Zeile, mit Spaltenspearatoren und viel Platz zurückgeben
  procedure init_putstr(var buff:string) ;
    var i: Integer ;
      colidx: TLogColumnIndex ;
    begin
      buff := '' ;
      for i := 0 to 20 do
        buff := buff + '                                                          ' ;
      for colidx := low(TLogColumnIndex) to high(TLogColumnIndex) do
        buff[gettabseppos(colidx)] := '|' ;
    end ;

  // String in Zelle
  procedure putstr(var buff: string ; colidx:TLogColumnIndex; s: string) ;
    var j: Integer ;
    begin
      for j := 1 to length(s) do
        buff[1 + ord(colidx)*columwidth + j] := s[j] ;
    end;

  var i: Integer ;
    colidx: TLogColumnIndex ;
    linebuff: string ;
    s: string ;
    datestr: string ;
    lineinfostr:string ;
  begin { "procedure TFormLog.LogStrCol" }
    if not Connection_LogIoStream then Exit ;

    // Zeileninfo: hier mit GetTickCount
    DateTimeToString(datestr, 'hh:nn:ss', Now) ;
    lineinfostr := Format('[%4d: %s = %7.0n]', [LogFileLineCount, datestr, 1.0*GetTickCount]) ;

    // erstmal 1K Zeile leer füllen
    init_putstr(linebuff) ;
    // Titelzeile ausgeben?
    if (LogFileLineCount mod LogFileTitleIntervall) = 0 then begin
      for colidx := low(TLogColumnIndex) to high(TLogColumnIndex) do begin
        s := GetEnumName(TypeInfo(TLogColumnIndex), Integer(colidx)) ;
        s := Copy(s, 1+length('LogCol_'), maxint) ; // immer gleiches Prefix weg
        s := Copy(s, 1, columwidth-1) ; // Auf Platz verkürzen
        putstr(linebuff, colidx, s) ;
      end ;
      linebuff := Trim(linebuff) ;
      // Links keine lineinfo, dafür gleich langer Leerstring
      s := lineinfostr ;
      for i := 1 to length(s) do s[i] := '-' ;
      writeln(LogFile, s + ' ' + linebuff) ;
    end{ "if (LogFileLineCount mod LogFileTitleIntervall) = 0" } ;

    // nur den einen String in die Zelle schreiben
    init_putstr(linebuff) ;
    putstr(linebuff, aColidx, logstr) ;
    linebuff := Trim(linebuff) ;
    writeln(LogFile, lineinfostr + ' ' + linebuff) ;
    inc(LogFileLineCount) ;
  end{ "procedure TFormLog.LogStrCol" } ;


procedure TFormLog.Clear1Click(Sender: TObject);
  begin
    Clear ;
  end;


procedure TFormLog.FormCreate(Sender: TObject);
  var tmpdir: string ;
  begin
    LogFileLineCount := 0 ;
    if Connection_LogIoStream then begin
      try
        if not GetEnv('TEMP', tmpdir) then
          tmpdir := 'C:\' ; // panic, does  not work on win7/vista
        // Ungültig unter Vista !!!
        AssignFile(LogFile, tmpdir + '\' + Connection_LogIoStream_Filename) ;
        Rewrite(LogFile) ;
      except
        // kein Logging, wenn file nicht auf geht!
        Connection_LogIoStream := false ;
      end ;
//      LogStrCol(LogCol_Other, '$HEADERS') ;
    end { "if Connection_LogIoStream" } ;
  end{ "procedure TFormLog.FormCreate" } ;



procedure TFormLog.FormDestroy(Sender: TObject);
  begin
    if Connection_LogIoStream then
      CloseFile(LogFile) ;
  end;

procedure TFormLog.Log(fmt:string; args: array of const) ;
  begin
    Log(Format(fmt,args)) ;
  end ;



procedure TFormLog.LogMemoMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
  var p: TPoint ;
  begin
    if Button = mbRight then begin
      p.X := X ; p.Y := Y ;
      p := LogMemo.ClientToscreen(p) ;
      PopupMenu1.PopUp(p.X, p.Y) ;
    end;
  end;


  initialization
      LogBuffer:= TStringList.Create ;


end{ "unit FormLogU" } .
