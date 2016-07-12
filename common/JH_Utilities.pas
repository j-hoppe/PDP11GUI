unit JH_Utilities ;
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



{$WARN SYMBOL_PLATFORM OFF}
interface

uses
  {$ifdef VER240}
  WinApi.Windows, WinApi.ShellAPI,
  System.Win.Registry,
  WinApi.SHFolder,
  WinApi.WinSock,
  UITypes,        //XE3
  {$else}
  Windows, ShellAPI,
  Registry,
  SHFolder,
  WinSock,
  {$endif}
  SysUtils, Classes,
  Controls,
  StdCtrls, Buttons, Forms, Mask,
  Menus,
  Graphics,
  ComCtrls, ExtCtrls,
  ShlObj ;

type
  CharSet = set of AnsiChar ;

  TCreateDirMode = (cdmNever, cdmConfirm, cdmAlways) ;


  TJH_Registry = class(TRegistry)
      fRegistryKey: string ; // Schlüssel für alle Einstellungen
      fWasFound: boolean ; // false: letztes Load musste Defaultwert nehmen
      fReadOnly: boolean ;

      fSaveTimer: TTimer ;
      fChanged: boolean ;
      fFilename: string ; // dahinein wird periodisch gesichert

      procedure autoSaveToFile(Sender: TObject) ;
    public
      // Verhindert die Funktion von allen "Save()"
      // Grund: RegistryLoad() eines Controls loest (gewollt!) dessen OnChange-Handler
      // aus, in dem natuerlich wieder "RegsitrySave()" steht.
      // Also wird waehrend des Ladens die Registry schon wieder veraendert.
      Loading: boolean ;

      constructor Create ;
      destructor Destroy ; override ;


      procedure SaveToFile ;
      procedure LoadFromFile ;

      procedure Save(parmname:string ; parmval:string) ; overload ;
      function Load(parmname:string; defaultval: string = '') : string ; overload ;

      procedure Save(parmname:string; parmval: integer) ; overload ;
      function  Load(parmname:string; defaultval: integer = 0) : integer ; overload ;

      procedure Save(parmname:string; parmval: boolean) ; overload ;
      function Load(parmname:string; defaultval: boolean): boolean ; overload ;

      procedure Save(parmname:string ; parmval:double) ; overload ;
      function Load(parmname:string; defaultval: double = 0) : double ; overload ;

      procedure Save(cb: TComboBox) ; overload ;
      procedure Load(cb: TComboBox; defaultval: string = '') ; overload ;

      procedure Save(lb: TListBox) ; overload ;
      procedure Load(lb: TListBox; defaultval: string = '') ; overload ;

      procedure Save(cb: TCheckBox) ; overload ;
      procedure Load(cb: TCheckBox; defaultval: boolean = false) ; overload ;

      procedure Save(rb: TRadioButton) ; overload ;
      procedure Load(rb: TRadioButton; defaultval: boolean = false) ; overload ;

      procedure Save(ed: TEdit) ; overload ;
      procedure Load(ed: TEdit; defaultval: string = '') ; overload ;

      procedure Save(mi: TMenuItem) ; overload ;
      procedure Load(mi: TMenuItem; defaultval: boolean = false) ; overload ;

      procedure Save(ed: TMaskEdit) ; overload ;
      procedure Load(ed: TMaskEdit; defaultval: string = '') ; overload ;

      procedure Save(pagecontrol: TPageControl) ; overload ;
      procedure Load(pagecontrol: TPageControl) ; overload ;

      procedure Save(form: TForm) ; overload ;
      procedure Load(form: TForm{ left, top, width, height: integer; state: TWindowState}) ; overload ;

      procedure Save(button: TSpeedButton) ; overload ;
      procedure Load(button: TSpeedButton; defaultval: boolean = false) ; overload ;

      procedure Save(panel: TPanel) ; overload ;
      procedure Load(panel: TPanel) ; overload ;

      property Key: string read fRegistryKey write fRegistryKey ;
      property wasFound: boolean read fWasFound ; // false: letztes Load musste Defaultwert nehmen

      property Filename: string read fFilename write fFilename ; // wenn gesetzt, wird periodisch rein gesichert

      procedure Save(name: string ; sl: TStrings) ; overload ;
      procedure Load(name: string ; sl: TStrings) ; overload ;

      procedure Save(name: string ; strstr: TStringStream) ; overload ;
      procedure Load(name: string ; strstr: TStringStream) ; overload ;

      property ReadOnly:boolean read fReadOnly write fReadOnly ;

    end { "TYPE TJH_Registry = class(TRegistry)" } ;

function WordCount(s : string; WordDelims: CharSet) : word ;
function WordPosition(n : word; s : string; WordDelims : CharSet) : word;
function ExtractWord(n : word; s : string; WordDelims: CharSet) : string ;

function min(a, b: integer): integer ; overload ;
function min(a, b: double): double ; overload ;
function max(a, b: integer): integer ; overload ;
function max(a, b: double): double ; overload ;

function detab(s : string ; tabcnt : integer) : string ;
{ -- TAB's im String expandieren. Tabs alle 'tabcnt' Positionen }
function entab(ins : string ; tabcnt : integer) : string ;
{ -- Spaces im String zu TAB's komprimieren. ('tabcnt'er Positionen)
   VB : String darf keine TAB's enthalten }


// eindeutigen Path des controls aus Namen seinerr parent frames und froms verknüpfen
function getControlNamePath(ctl: TControl) : string ;

function GetEnv(varname: string ; var varval: string):boolean ;
{ Abfrage einer Environment-Variablen. True, wenn OK }

function GetLastErrorText:string ;
{ setzt Fehlermeldung von GetLastError in Text um }

// wie FileSearch(), aber auch in Unterverzeichnisse
function FileSearchRecursive(const name: string; const DirList: string): string;

function TempFilename : string ;
{ erzeugt einen guten Namen für einen temporären File }

function CorrectNumber(s:string):string ;
{ aus . und , jeweils das gültige Dezimaltrennzeichen machen }

function TryStrToDword(s: string ; var d: dword): boolean ;

function CorrectPath(p:string):string ;
{ verschiedene Korrekturen an dir/file-pathes }

procedure AssertPath(p:string; reason:string; createdirmode: TCreateDirMode) ;

function GetLongFileName( sShortName : string ) : string; // wandelt 8.3 mit ~ in lang

function OpenForWrite(var fout: System.text; fname:string;
        allow_skip:boolean;
        var create_file_always: boolean; // File ohne Rueckfrage ueberschreiben, auch wenn Archivebit gesetzt
        create_dir_mode: TCreateDirMode
        ) : boolean ;
{ Datei zum Schreiben öffnen.
  Wenn es sie schon gibt, und ihr Archivflag gesetzt ist:
  rückfragen, ob sie überschrieben werden soll.
  Wenn nein: false.
}
procedure CloseAfterWrite(var fout: System.text; fname:string) ;
{ Datei nach Schreiben schliessen.
  Danach das Archivbit löschen.
}

function IsBinaryFile(fname:string): boolean ;
{ true, wenn die Datei Zeichen aus #0..#31 enthält (ohne $9 $d $a $1a) }

function StrToDword(s:string):dword ;

function StrCText2Bin(s:string):string ;
{ ersetzt \x-Codes durch Einzelzeichen <32. Ergaenzt #0  }

function StrCBin2Text(s:string):string ;
{ ersetzt Einzelzeichen < 32 durch \x Sequenzen. Entfernt die letzte \0 }

function Dword2OctalStr(val: dword; fixbitwidth: integer = 0): string ;
function OctalStr2Dword(s:string): dword ;

function PatternMatch(line,pattern : string) : boolean ;
{ true, wenn 'pattern' in 'line' vorkommt.
  1.  * in pattern wird ausgewertet,
  2.  Mehrfachspaces werden wie 1 Space behandelt,
  3.  nicht case-sensitiv
}

function Replace (var s: string; s_find, s_replace : string; ignorecase: boolean = false) : boolean ;
{ ---- ersetzt in 's_org' den Substring 's_find' durch 's_replace'.
       ersetzt das erste Auftreten von 's_find', Gross/Kleinschreibung
       muss stimmen.
}

{ -- Trailing Spaces abschneiden }
function RTrim(s: string): string ;

function RTrimChars(s: string; cs: charset): string ;
function LTrimChars(s: string; cs: charset): string ;
function TrimChars(s: string; cs: charset): string ;

{-Given a set of word delimiters, return words into stringlist }
procedure ExtractWords(sl: TStringList; s : string; WordDelims : charset);

// Löscht rekursiv alle Registrykeys ab aPath
procedure DeleteRegKey(aRoot : HKey; aPath : string);


// bewegt einen ganzen key samt subkeys
// wie TRegsitry.Movekey, aber rekursiv.
procedure MoveRegKey(aRoot : HKey; aOldPath, aNewPath:string ; delete: boolean) ;
procedure OptimizeAnyGridColWidths(aGrid: TControl);

function StartProcess(var procinfo: TProcessInformation ; cmd, args, dir_exec: string ;
        CmdShow: integer; priority_class:dword = NORMAL_PRIORITY_CLASS): boolean ;
// -1 = nicht gestartet, sonst exitcode
function RunProcess(cmd, args, dir_exec: string ;
        CmdShow: integer; priority_class:dword = NORMAL_PRIORITY_CLASS): Cardinal ;


{ erzeugt einen nicht vorhandenen Datenamen nach dem Muster
  dir\prefix[_lfdnr].ext }
function GetUniqueFilename(dir: string; prefix, ext: string): string ;

function FileSetReadOnly(const Filename: string; ReadOnly: boolean): boolean;

function DirectoryExists(const Directory: string): boolean;
function IsDirectoryWriteable(const AName: string): boolean;
function GetFolder(Root:integer;Caption:string):string;

function IdenticFiles(fname1, fname2: string): boolean ;
// true, wenn beide Dateien den gleichen Inhalt haben.


// macht "AbsolutePath" relativ zu "BasePath", falls möglich
// Danach gilt:   Expand(BasePath + \ + result == AbsolutePath
// Bsp: Basepath = "C:\Tools\programme\MyProg"
//      AbsolutePath = "C:\Tools\Data\MyProg"
// -> Result = "..\..\Data\MyProg"
// Denn : C:\Tools\programme\MyProg \ ..\..\Data\MyProg == C:\Tools\Data\MyProg
function GetRelativePath(BasePath, AbsolutePath: string): string ;
// setzt "BasePath" vor "RelativePath", wenn das ein relativer Pfad ist
function GetAbsolutePath(BasePath, RelativePath: string): string ;

function String2Filename(s:string):string ;


function FindFilenameInText(workpath: string ; line: string): string ;
function FindIntegerInText(line: string): integer ;

procedure StringToFont(sFont : string; Font : TFont );
function FontToString( Font : TFont) : string;

function RGB2TColor(R, G, b: integer): integer;
procedure TColor2RGB(Color: TColor; var R, G, b: integer);
function GetColorDist(color1, color2: TColor): integer ;
function AssertDifferentColor(variablecolor, referencecolor: TColor): TColor ;

// Exception mit Stackdump versehen.
// Voraussetzungen: [x] project/Insert JCL debug data
procedure ReRaiseExceptionWithStackDump(e:Exception) ;


// Thread.Synchronize() laufen lassen
procedure SyncPoint ;

function SetForegroundWindowEx(hWndWindow: THandle): boolean ;

function SHGetFolderPath(csidl: integer):string ;

function GetIPAddress(const HostName: AnsiString): AnsiString;


procedure OutputDebugString(s: string) ; overload ;
procedure OutputDebugString(fmt: string; args: array of const) ; overload ;
// zum markieren von Breakpoints ;
procedure break_here() ;

// CharInSet nach delphi 2007 in sysutils
{$ifdef VER180}
function CharInSet(c: char ; cs: charset): boolean ;
{$endif}

procedure SortCollection(aCollection: TCollection ; comparefunc: TListSortCompare) ;


implementation

uses
  JclDebug,
  Dialogs,
  Grids,
  //WinSock,
  DB,
  DBGrids // use modified dbgrids.pas,
          // because of error "range index out of bounds"
          // See Change in procedure TCustomDBGrid.UpdateActive;
  ;

// CharInSet nach delphi 2007 in sysutils
{$ifdef VER180}
function CharInSet(c: char ; cs: charset): boolean ;
  begin
    result := c in cs ;
  end;
{$endif}

function WordCount(s : string; WordDelims : CharSet) : word;
{-Given a set of word delimiters, return number of words in S}
  var
    Count : word;
    i : word;
    SLen : word;
  begin
    SLen := length(s) ;
    Count := 0;
    i := 1;

    while i <= SLen do begin
      {skip over delimiters}
      while (i <= SLen) and CharInSet(s[i], WordDelims) do
        inc(i);

      {if we're not beyond end of S, we're at the start of a word}
      if i <= SLen then
        inc(Count);

      {find the end of the current word}
      while (i <= SLen) and not CharInSet(s[i], WordDelims) do
        inc(i);
    end{ while } ;

    WordCount := Count;
  end{ "function WordCount" } ;

function WordPosition(n : word; s : string; WordDelims : CharSet) : word;
{-Given a set of word delimiters, return start position of N'th word in S}
  var
    Count : word;
    i : word;
    SLen : word ;
  begin
    SLen := length(s) ;
    Count := 0;
    i := 1;
    WordPosition := 0;

    while (i <= SLen) and (Count <> n) do begin
      {skip over delimiters}
      while (i <= SLen) and CharInSet(s[i], WordDelims) do
        inc(i);

      {if we're not beyond end of S, we're at the start of a word}
      if i <= SLen then
        inc(Count);

      {if not finished, find the end of the current word}
      if Count <> n then
        while (i <= SLen) and not CharInSet(s[i], WordDelims) do
          inc(i)
      else
        WordPosition := i;
    end{ while } ;
  end{ "function WordPosition" } ;


function ExtractWord(n : word; s : string; WordDelims : CharSet) : string;
{-Given a set of word delimiters, return the N'th word in S}
  var
    i : word;
    Len : word;
    SLen : word ;
  begin
    SLen := length(s) ;
    Len := 0;
    SetLength(result, 1023) ; { intialisieren! auch für lange SubStrings !! Aschu 10.11.02}
    i := WordPosition(n, s, WordDelims);
    if i <> 0 then
      {find the end of the current word}
      while (i <= SLen) and not CharInSet(s[i], WordDelims) do begin
        {add the I'th character to result}
        inc(Len);
        result[Len] := s[i] ;
        inc(i);
      end;
    SetLength(result, Len);
  end{ "function ExtractWord" } ;




function min(a, b: integer): integer ; overload ;
  begin
    if a < b then result := a else result := b ;
  end;

function min(a, b: double): double ; overload ;
  begin
    if a < b then result := a  else result := b ;
  end;

function max(a, b: integer): integer ; overload ;
  begin
    if a > b then result := a else result := b ;
  end;

function max(a, b: double): double ; overload ;
  begin
    if a > b then result := a  else result := b ;
  end;


{ -- TAB's im String expandieren. Tabs alle 'tabcnt' Positionen }
function detab(s : string ; tabcnt : integer) : string ;
  var
    S1 : string ;
    Ch : char ;
    i, j, n : integer ;
  begin
    S1 := '' ;
    for i := 1 to length(s) do begin
        Ch := s[i] ;
        if Ch = #9 then begin
            // s1 := concat(s1, ' ') ; { -- min. 1 Space }
            n := length(S1) mod tabcnt ;
            for j := 1 to tabcnt - n do
              S1 := concat(S1, ' ') ;
          end  else S1 := concat(S1, Ch) ;
      end { for i } ;
    detab := S1 ;
  end { "function detab" } ;


function entab(ins : string ; tabcnt : integer) : string ;
{ -- Spaces im String zu TAB's komprimieren. (8er Positionen)
   VB : String darf keine TAB's enthalten }
  var
    outs : string ;
    beg, i, tabstop : integer ;
  begin
    tabstop := tabcnt ;
    outs := '' ;
    while tabstop <= length(ins) do begin
        { -- Bereich (incl) tabstop-7..tabstop kodieren }
        beg := tabstop - tabcnt ;
        { i := letztes Zeichen <> Space im Intervall }
        i := tabcnt ; while (i >= 1) and (ins[beg + i] = ' ') do i := i - 1 ;
        outs := concat(outs, Copy(ins, beg+1, i) ) ;
        { -- am Ende des Intervalls: Space oder TAB anhaengen }
        if i < tabcnt
          then if i = (tabcnt - 1)
              then outs := concat(outs, ' ')
              else outs := concat(outs, #9) ;
        tabstop := tabstop + tabcnt ;
      end { while } ;
    { -- Rest des Strings nach letzter 8er Position anhaengen }
    tabstop := tabstop - 8 ;
    outs := concat(outs, Copy(ins, tabstop+1, 999) ) ;
    { -- Spaces und TABs am Zeilenden abschneiden }
    i := length(outs) ;
    while (i > 0) and ( (outs[i] = ' ') or (outs[i] = #9) ) do
      i := i - 1 ;
    outs := Copy(outs, 1, i) ;
    entab := outs ;
  end { "function entab" } ;



// eindeutigen Path des controls aus Namen seinerr parent frames und froms verknüpfen
function getControlNamePath(ctl: TControl) : string ;
  begin
    result := ctl.name ;
    while (ctl <> nil) and (ctl.parent <> ctl) do begin
      ctl := ctl.parent ; // kette hoch steigen
      if (ctl is TFrame) or (ctl is TForm) then
        result := ctl.name + '.' + result ;
    end;
  end;


function GetLastErrorText: string ;
{ setzt Fehlermeldungen in Text um }
  var
    err: integer ;
    buff: array[0..512] of char {pchar } ;
  begin
    err := GetLastError ;
    FormatMessage(
            {FORMAT_MESSAGE_ALLOCATE_BUFFER OR} FORMAT_MESSAGE_FROM_SYSTEM,
            nil,
            err,
            0, { Default language }
            buff,
            512,
            nil
            );

    result := strpas(buff) ;
  end { "function GetLastErrorText" } ;

function GetEnv(varname: string ; var varval: string):boolean ;
{ Abfrage einer Environment-Variablen. True, wenn OK }
  var
    envval_buff: array[0..10000] of char ;
  begin
    varval := '' ;
    if {$ifdef VER240}WinApi.{$endif}Windows.GetEnvironmentVariable(pchar(varname), envval_buff, sizeof(envval_buff)) = 0
      then result := false
      else begin
        varval := strpas(envval_buff) ;
        result := true ;
      end ;
  end ;

// wie FileSearch(), aber auch in Unterverzeichnisse
function FileSearchRecursive(const name: string; const DirList: string): string;

// suche nach file in einem directory
  function FileSearchInDir(fname:string ; dirname: string): string;
    var searchrec: TSearchRec ;
      res: integer ;
      curfullpath: string ;

    begin
      result := '' ;
      fname := AnsiUpperCase(fname) ;

      // The attributes for c:\windows etc. are quite strange, so don't test them.
      // Just try every directory as file, and every file as sub directory.

      // 1.) search file in directory
      res := FindFirst(dirname+'\*.*', faAnyFile, searchrec) ;
      try
        while (res = 0) and (result = '') do begin
          curfullpath := dirname + '\' + searchrec.name ;

          if AnsiUpperCase(searchrec.name) = fname then
            result := curfullpath ; // found!
          FindNext(searchrec) ;
        end;
      finally
        FindClose(searchrec) ;
      end;

      if result <> '' then
        Exit ; // found!

      // 2.) search in sub dirs
      res := FindFirst(dirname+'\*.*', faAnyFile, searchrec) ;
      try
        while (res = 0) and (result = '') do begin
          curfullpath := dirname + '\' + searchrec.name ;
          if (searchrec.name <> '.') and (searchrec.name <> '..') then
            result := FileSearchInDir(fname, curfullpath) ; // found in subdir?
          FindNext(searchrec) ;
        end;
      finally
        FindClose(searchrec) ;
      end;
    end{ "function FileSearchInDir" } ;

  var dirnames: TStringList ;
    i: integer ;
  begin { "function FileSearchRecursive" }
    result := '' ;
    dirnames:= TStringList.Create ;
    try
      // analyse ";"-separated list of directories
      dirnames.Delimiter := ';' ;
      dirnames.QuoteChar := '"' ;
      dirnames.DelimitedText := DirList ;
      for i := 0 to dirnames.Count - 1 do begin
        result := FileSearchInDir(name, dirnames[i]) ;
        if result <> '' then Break ; // found!
      end;
    finally
      dirnames.Free ;
    end;
  end{ "function FileSearchRecursive" } ;


function TempFilename : string ;
{ erzeugt einen guten Namen für einen temporären File }
  var
    tempnamebuffer: array[0..255] of char ;
    temppathbuffer: array[0..80] of char ;
  begin
    { Win32-Funktion }
    GetTempPath(80,temppathbuffer) ;
    GetTempFileName(temppathbuffer, 'scr', 0, tempnamebuffer) ;
    result := strpas(tempnamebuffer) ;
  end ;

function CorrectNumber(s:string):string ;
{ aus . und , jeweils das gültige Dezimaltrennzeichen machen }
  var i: integer ;
  begin
    for i := 1 to length(s) do
{$ifdef  VER240}// xe3
      if CharInSet(s[i], ['.',',']) then s[i] := FormatSettings.DecimalSeparator ;
{$else}
    if CharInSet(s[i], ['.',',']) then s[i] := DecimalSeparator ;
{$endif}
    result := s ;
  end ;


function CorrectPath(p:string):string ;
{ verschiedene Korrekturen an dir/file-pathes }
  var
    n: integer ;
    i, j : integer ;
    fertig: boolean ;
  begin
    n := length(p) ;

    { 1. "\" am Ende von Verzeichnissnamen abschneiden }
    if n > 2  then
      if (p[n] = '\') and (p[n-1] <> ':') then
        p := Copy(p, 1, n-1) ;

    { 2. Einträge der Form "\xxx\.." entfernen, wiederholt }
    fertig := false ;
    while not fertig do begin
      i := pos('\..', p) ;
      if i = 0 then fertig := true
      else begin
        j := i-1 ; { j = Pos des \ }
        while (j > 0) and (p[j] <> '\') do dec(j) ;
        if p[j] <> '\' then fertig := true
        else begin
          { sequenz \xxx\.. rausschneiden }
          p := Copy(p,1,j-1) + Copy(p, i+3,999) ;
          { root \ wieder dranmachen }
          if length(p) < 3 then p := p+'\' ;
        end ;
      end ;
    end { "while not fertig" } ;
    result := p ;
  end { "function CorrectPath" } ;


procedure AssertPath(p:string; reason:string; createdirmode: TCreateDirMode) ;
  var
    i: integer ;
    isroot: boolean ;  { true, wenn pfad NUR drive or hostname ist}
    parentlevel: string ;
  begin
    { Vorbestandteile checken }

    isroot := false ;
    { Form "d:" erkennen }
    if (length(p) = 2) and (p[2] = ':') then isroot := true ;
    { Form "d:\" erkennen }
    if (length(p) = 3) and (p[2] = ':') then isroot := true ;
    { Form "\\name.name" erkennen }
    if (length(p) > 3) and (p[1]='\') and (p[2] = '\') and ( pos('\', Copy(p,3,999)) = 0 )
      then isroot := true ;
    { Stoppen bei x:\ oder \\name.... }
    if not isroot then begin { stoppen bei "D:\" }
      parentlevel := CorrectPath(ExtractFilePath(p)) ;
      if length(parentlevel) < length(p) then
        AssertPath(parentlevel,reason, createdirmode) ;

      i := FileGetAttr(p) ;
      if i = -1 then begin {gips nich }
        if (createdirmode = cdmAlways)
                or ( (createdirmode = cdmConfirm) and (MessageDlg('Directory "'+p+'" does not exist'+#13
                +'('+reason+').'#13
                +'Create it?',
                mtConfirmation, mbYesNoCancel, 0) = mrYes))
          then try
              MkDir(p) ;
            except
              raise Exception.CreateFmt('Can not create directory "%s"!', [p]) ;
            end ;
        {            then mkdirCreateDirectory(pchar(p),nil) ; }
        { Check, obs jetzt da ist. }
        if FileGetAttr(p) = -1 then
          raise Exception.CreateFmt('Directory "%s" does not exist!', [p]) ;
      end { "if i = -1" } else
        if (i and faDirectory) = 0 then
          raise Exception.CreateFmt('"%s" exists, but is no directory!', [p]) ;
    end { "if not isroot" } ;
  end { "procedure AssertPath" } ;



// from Internet, but corrected.
function GetLongFileName( sShortName : string ) : string;

  function __GetLongName(sShortName : string; var bError : boolean ) : string;
    var
      bAddSlash : boolean;
      searchrec : TSearchRec;
      nStrLen   : integer;
    begin
      bError    := false;
      result    := sShortName;
      nStrLen   := length( sShortName );
      bAddSlash := false;

      if '\' = sShortName[ nStrLen ] then begin // \ weg
        bAddSlash := true;
        SetLength( sShortName, nStrLen - 1 );
        dec(nStrLen) ;
      end;

      if ( nStrLen - length( ExtractFileDrive(sShortName ) ) ) > 0 then begin
        if 0 = FindFirst(sShortName, faAnyFile, searchrec ) then begin
          result := ExtractFilePath(sShortName ) + searchrec.name;
          if bAddSlash then
            result := result + '\';
        end else // handle errors...
          bError := true;
        FindClose( searchrec );
      end;
    end{ "function __GetLongName" } ;

  var
    s      : string;
    p      : integer;
    bError : boolean;
  begin { "function GetLongFileName" }
    result := sShortName;

    s := '';  // verzeichnisweise wandeln, s = ergebnis
    p := pos( '\', sShortName );
    while  p > 0 do begin
      s := __GetLongName( s + Copy( sShortName, 1, p ), bError );
      delete( sShortName, 1, p );
      p := pos( '\', sShortName );

      if bError then Exit;
    end;

    if '' <> sShortName then begin
      s := __GetLongName(s + sShortName, bError );
      if bError then Exit;
    end;
    result := s;
  end{ "function GetLongFileName" } ;


function OpenForWrite(var fout: System.text; fname:string;
        allow_skip: boolean; var create_file_always: boolean; create_dir_mode: TCreateDirMode) : boolean ;
{ Datei zum Schreiben öffnen.
  Wenn es sie schon gibt, und ihr Archivflag gesetzt ist:
  rückfragen, ob sie überschrieben werden soll.
  Wenn nein: false.
  Sonst:
        Datei öffnen.

  create_file_always: true: keine Check des archivebits.
  wird hier gesetzt, wenn "NoToall" geantwortet wird.
}
  begin
    result := true ;
    if not create_file_always then
      if FileExists(fname) and  ( (FileGetAttr(fname) and faArchive) <> 0) then begin
        if not allow_skip then begin
          case MessageDlg(
                  Format('Target file "%s" already exists'+#13+'and has probably been modified after creation by SCRIPTOR.'+
                  #13+'Keep existing file?'+
                  #13+'Press "No" to overwrite existing file.',[fname]),
                  mtConfirmation, [mbYes,mbNo,mbCancel], 0 )
                  of
            mrNo: ;
            mrNoToAll: create_file_always := true;
            else raise Exception.CreateFmt('Target file "%s" already exists',[fname]) ;
          end ;
        end else begin
          case MessageDlg(
                  Format('Target file "%s" already exists'+#13+'and has probably been modified after creation by SCRIPTOR.'+
                  #13+'Keep existing file?'+
                  #13+'Press "No" to overwrite existing file.',[fname]),
                  mtConfirmation, [mbYes,mbNo,mbNoToAll,mbCancel], 0
                  ) of
            mrYes:    result := false ;
            mrNo:     result := true ;
            mrNoToAll:     begin
              create_file_always := true ;
              result := true ;
            end ;
            mrCancel: result := false ;
            else raise Exception.CreateFmt('Target file "%s" already exists',[fname]) ;
          end ;
        end { "if not allow_skip ... ELSE" } ;
      end { "if FileExists(fname) and ( (FileGetAttr(fname) and faArchive) <> 0)" } ;

    if result = true then begin
      { -- ggf Verzeichnis erst anlegen }
      AssertPath(ExtractFilePath(fname),Format('Opening file "%s" for write',[fname]),create_dir_mode) ;
      try AssignFile(fout, fname) ;
      except raise Exception.CreateFmt('Illegal target file name "%s"',[fname]) ;
      end ;

      try rewrite(fout) ;
      except raise Exception.CreateFmt('Target file "%s" could not be opened',[fname]) ;
      end ;
    end ;
  end { "function OpenForWrite" } ;

procedure CloseAfterWrite(var fout: System.text; fname:string) ;
{ Datei nach Schreiben schliessen.
  Danach das Archivbit löschen.
}
  var
    res: integer ;
  begin
    CloseFile(fout) ;

    res := FileSetAttr(fname, 0) ;
    if res <> 0 then raise Exception.CreateFmt('Cannot reset archive bit of target file "%s"', [fname]) ;
  end ;

function IsBinaryFile(fname:string): boolean ;
{ true, wenn die Datei Zeichen aus #0..#31 enthält (ohne $9 $d $a $1a).
  Nur die ersten 1000. Byte untersuchen }
  const
    checksize = 1000 ; // maxsoviele Zeichen untersuchen
    binchars: set of Byte = [
            $00,$01,$02,$03,$04,$05,$06,$07,$08,        $0b,$0c,    $0e,$0f,
            $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,    $1b,$1c,$1d,$1e,$1f
            ] ;
  var
    fs: TFileStream ;
    buffer: array[0..checksize] of Byte ;
    i, n: integer ;
  begin
    result := false ;
    fs := nil ;
    try
      fs := TFileStream.Create(fname, fmOpenRead) ;
      // SourceInsight lockt Files so, dass es hier eine Exception gibt.
      n := fs.read(buffer, checksize) ;
      for i := 0 to n-1 do
        // nicht alle Binzeichen testen, dafür sehr schnell
        if (buffer[i] <= 8) or ((buffer[i] >= $0e) and (buffer[i] <= $19)) then begin
          result := true ;
          Break ;
        end ;
    finally
      if fs <> nil then
        fs.Free ;
    end { "try" } ;
  end { "function IsBinaryFile" } ;


function TryStrToDword(s: string ; var d: dword): boolean ;
  var i: int64 ;
  begin
    result := TryStrToInt64(s, i) ;
    if result then
      if (i < 0) or (i > $100000000) then
        result := false ;
    if result then
      d := i and $ffffffff ;
  end ;

// kann auch '$ffffffff' ohne Überlauf wandeln
function StrToDword(s:string): dword ;
  var base: dword ;
    i: integer ;
  begin
    s := Uppercase(trim(s)) ;
    result := 0 ;
    base := 10 ;
    if (s <> '') and (s[1] = '$') then begin
      base := 16 ;
      s := Copy(s, 2, maxint) ;
    end;
    if s = '' then raise EConvertError.Create('empty string') ;
    for i := 1 to length(s) do begin
      case s[i] of
        '0'..'9':
          result := result * base + ord(s[i]) - ord('0') ;
        'A'..'F':
          result := result * base + ord(s[i]) - ord('A') + 10 ;
        else raise EConvertError.CreateFmt('illegal digit "%s"', [s[i]]) ;
      end;
    end;
  end{ "function StrToDword" } ;


function StrCText2Bin(s: string):string ;
{ ersetzt \x-Codes durch Einzelzeichen < 32. Ergaenzt \0 }
  var
    i: integer ;
    outs: string ;
    tmps: string ;
  begin
    i := 1 ;
    outs := '' ;
    while i <= length(s) do begin
      if s[i] = '\' then begin
        inc(i) ;
        case s[i] of
          'b'  : outs := outs + #8 ;
          'f'  : outs := outs + #12 ;
          'n'  : outs := outs + #10 ;
          'r'  : outs := outs + #13 ;
          't'  : outs := outs + #9 ;
          '\'  : outs := outs + '\' ;
          '''' : outs := outs + '''' ;
          '"'  : outs := outs + '"' ;
          '0'  : outs := outs + #0;
          'x'  : begin { \xdd = hexangabe }
            inc(i) ; tmps := s[i] ;
            inc(i) ; tmps := tmps + s[i] ;
            outs := outs + char(StrToInt('$'+tmps)) ;
          end ;
          else outs := outs + s[i] ;
        end { "case s[i]" } ;
      end { "if s[i] = '\'" } else outs := outs + s[i] ;
      inc(i) ;
    end { "while i <= length(s)" } ;
    result := outs + #0 ;
  end { "function StrCText2Bin" } ;


function StrCBin2Text(s:string):string ;
{ ersetzt Einzelzeichen < 32 durch \x Sequenzen.
 Entfern die abschliessende \0 }
  var
    i : integer ;
    outs: string ;
  begin
    outs := '' ;
    for i := 1 to length(s) do
      case s[i] of
        #0  : if i < length(s) then { die letzte \0 entfernen }
            outs := outs + '\0' ;
        #8  : outs := outs + '\b' ;
        #9  : outs := outs + '\t' ;
        #10 : outs := outs + '\n' ;
        #12 : outs := outs + '\f' ;
        #13 : outs := outs + '\r' ;
        '\' : outs := outs + '\\' ;
        '"' : outs := outs + '\"' ;
        '''' : outs := outs + '\''' ;
        else if s[i] < ' '
            then outs := outs + Format('\%0.2x',[ord(s[i])])
            else outs := outs + s[i] ;
      end { "case s[i]" } ;
    result := outs ;
  end { "function StrCBin2Text" } ;

function Dword2OctalStr(val: dword; fixbitwidth: integer = 0): string ;
  var
    digit: integer ;
    fixdigitwidth: integer ;
  begin
    result := '' ;

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

// Octale Zahl nach Wert wandeln.
// wenn wert nicht in bitwidth passt: MEMORYCELL_ILLEGALVAL
function OctalStr2Dword(s:string): dword ;
  var
    i: integer ;
  begin
    s := trim(s) ;
    if s = '' then
      raise EConvertError.Create('Empty string can not be converted to octal!') ;
    result := 0 ;
    s := trim(s) ;
    for i := 1 to length(s) do begin
      if not CharInSet(s[i], ['0'..'7']) then
        raise EConvertError.CreateFmt('String "%s" can not be converted to octal!', [s]) ;
      result := (result shl 3) or dword(ord(s[i]) - ord('0')) ;
    end;
  end{ "function OctalStr2Dword" } ;


function ReduceWhiteSpace(s: string):string ;
{ Fuehrende, nachfolgende und Mehrfache-whitespace ausfiltern.
  NB: Jede Sequenz von white-space-chars wird durch ein einzelnes #32 ersetzt. }
  var
    s_new: string ;
    i: integer ;
  begin
    s_new := '' ;
    { whitespace durch space ersetzen. }
    for i := 1 to length(s) do
      if CharInSet(s[i], [#13, #9]) then s[i] := ' ' ;

    { Mehrfachspaces ersetzen }
    for i := 1 to length(s) do
      if s[i] = ' ' then
        begin if (i > 1) and (s[i-1] <> ' ') then s_new := s_new + ' ' end
      else s_new := s_new + s[i] ;

    result := trim(s_new) ;
  end { "function ReduceWhiteSpace" } ;

function Old_PatternMatch(line,pattern : string) : boolean ;
{ true, wenn 'pattern' in 'line' vorkommt.
  1.  * in pattern wird ausgewertet,
  2.  Mehrfachspaces werden wie 1 Space behandelt,
  3.  nicht case-sensitiv

  -- langsam
}
  var
    i: integer ;
    cur_found_pos: integer ;
    partial_pattern: string ;
    found: boolean ;
  begin
    line := Uppercase(ReduceWhiteSpace(line)) ;
    pattern := Uppercase(ReduceWhiteSpace(pattern)) ;
    if pattern = ''   then result := true { leeres Muster: ist über all drin }
    else if line = '' then result := false { leere Zeile: enthält kein Muster }
    else begin
      { 'pattern' wird zwischen den '*' in Teilmuster zerlegt.
        Die muessen ALLE in der richtigen Reihenfolge im String vorkommen }
      found := true ;
      for i := 1 to WordCount(pattern, ['*']) do begin
        partial_pattern := ExtractWord(i, pattern, ['*']) ;
        cur_found_pos := pos(partial_pattern, line) ;
        if cur_found_pos >= 1 then { gefunden: 'line' verkürzen }
          line := Copy(line, cur_found_pos + length(partial_pattern), 999)
        else begin found := false ; Break end ;
      end ;
      result := found ;
    end { "if line = '' ... ELSE" } ;
  end { "function Old_PatternMatch" } ;


function PatternMatch(line,pattern : string) : boolean ;
  var
    l_len, p_len: integer ; // Gesamtlänge von line und pattern
    l_pos: integer; // Start in 'line' für Einzelmustervergleich
    p_pos: integer; //   "    "  'pattern' "         "
    sub_pattern_found: integer; // 0 = noch unklar, -1 = NEIN, 1 = JA
    l_idx, p_idx: integer ; // Laufindex während Einzelmustervergleich
    p_was_space, l_was_space: boolean ;
  begin
    l_len := length(line) ;
    p_len := length(pattern) ;

    // convert all white space to #$20
    for l_idx := 1 to l_len do if line[l_idx] = #9 then line[l_idx] := ' ' ;
    for p_idx := 1 to p_len do if pattern[p_idx] = #9 then pattern[p_idx] := ' ' ;
    l_pos := 1 ;
    p_pos := 1 ;

    // Sonderfälle:
    // 1. pattern nur aus '*' gilt als leeres 'pattern'
    // 2. leeres 'pattern' matcht alles: leere 'line' und gesetzte 'line'
    // 3. gesetztes 'pattern' matcht nie leere 'line'
    while (p_pos <= p_len) and (pattern[p_pos] = '*') do inc(p_pos) ; // Regel 1
    if p_pos > p_len then
      result := true // Regel 2
    else if l_pos > l_len then
      result := false // Regel 3
    else begin
      repeat
        //////////////// Einzelmustervergleich (sub_pattern) ////////////////////
        //  finde string aus pattern ab p_pos in line ab l_pos.
        // wenn gefunden: 'found'! l_pos = start in zeile, l_pos + l_idx = ende pos in zeile

        // Check: steht in 'line' ab l_pos das Pattern ab p_pos?
        // fertig, wenn "found <>0"
        // wenn found = 1:
        // Mutispaces werden wie ein Space behandelt
        // VB: line[l_pos] gültig, pattern[p_pos] gültig
        // line[l_pos] nicht auf folge-Spaces
        // pattern[p_pos] nicht auf folge-Spaces oder '*'

        // skip leading '*'
        while (p_pos <= p_len) and (pattern[p_pos] = '*') do inc(p_pos) ;
        if p_pos > p_len
          then sub_pattern_found := 1 // Pattern leer: match
          else sub_pattern_found := 0 ; // do some work
        l_idx := 0 ; p_idx := 0 ;
        while sub_pattern_found = 0 do begin
          // Compare current chars
          if UpCase(line[l_pos + l_idx]) <> UpCase(pattern[p_pos + p_idx]) then
            sub_pattern_found := -1  // Misserfolg
          else begin
            // goto next pair of chars
            p_was_space := pattern[p_pos + p_idx] = ' ';
            l_was_space := line[l_pos + l_idx] = ' ' ;
            inc(p_idx) ; inc(l_idx) ; // weder pattern noch line sind zuende

            // skip over additional spaces
            if p_was_space then
              while ((p_pos + p_idx) <= p_len) and (pattern[p_pos + p_idx] = ' ') do inc(p_idx) ;
            if l_was_space then
              while ((l_pos + l_idx) <= l_len) and (line[l_pos + l_idx] = ' ') do inc(l_idx) ;

            // Test for end of pattern or end of line
            if (p_pos + p_idx) > p_len then // pattern ends: match!
              sub_pattern_found := 1
            else if pattern[p_pos+ p_idx] = '*' then begin // pattern ends on wildcard: match!
              sub_pattern_found := 1 ; // skip *'s
              while ((p_pos + p_idx) <= p_len) and (pattern[p_pos + p_idx] = '*') do inc(p_idx) ;
            end else if (l_pos+ l_idx) > l_len then // line ends, but pattern not: mismatch!
          end { "if UpCase(line[l_pos + l_idx]) <> UpCase(pattern[p_pos + p_idx]) ... ELSE" } ;
        end { "while sub_pattern_found = 0" } ;
        //////////////// Ende Einzelmustervergleich (sub_pattern) ////////////////////
        // wenn found = -1: pattern[p_pos] steht nicht ab line[l_pos]
        if sub_pattern_found = -1 then
          inc(l_pos)  // suche das erste pattern ab dem naechsten Zeichen in line
        else begin // pattern [p_os] bei line[l_pos] gefunden:
          inc(l_pos, l_idx) ; // skip match
          inc(p_pos, p_idx) ; // skip pattern
        end ;
      until (l_pos > l_len) or (p_pos > p_len) ;
      // Totaler Erfolg, wenn alle pattern gematcht (pattern[] endet vor line[])
      // Misserfolg, wenn aktuelles pattern nirgend gefunden (line[] endet vor pattern[])
      if  p_pos > p_len then
        result := true
      else result := false ;
    end { "if l_pos > l_len ... ELSE" } ;
  end { "function PatternMatch" } ;

procedure TestPatternMatch ;
  procedure testmatch(line, pattern:string; match: boolean) ;
    begin
      if PatternMatch(line, pattern) <> match then
        raise Exception.CreateFmt('PatternMatch(''%s'', ''%s'') -> %d, soll -> %d!',
                [line, pattern, ord(not match), ord(match)]) ;
    end ;
  begin
    (**)
    // Stringkonstanten
    testmatch('Hello, pattern matching world!', 'attern',true) ;
    testmatch('Hello, PATTERN matching world!', 'attern',true) ;
    testmatch('Hello, pattern matching world!', 'aTTern',true) ;
    testmatch('Hello, pattern matching world!', 'attern',true) ;
    testmatch('Hello, pattern matching world!', 'atern',false) ;
    testmatch('Hello, pattern matching world!', ' attern',false) ;
    testmatch('Hello, pattern matching world!', 'attern ',true) ;
    // Start/End of line
    testmatch('Hello, pattern matching world!', 'Hello',true) ;
    testmatch('Hello, pattern matching world!', ' Hello',false) ;
    testmatch('Hello, pattern matching world!', 'orld!',true) ;
    testmatch('Hello, pattern matching world!', 'orld! ',false) ;
    // ignore multi spaces
    testmatch('Hello, pattern matching world!', 'pattern matching', true) ;
    testmatch('Hello, pattern matching world!', 'pattern  matching', true) ;
    testmatch('Hello, pattern  matching world!', 'pattern matching', true) ;
    testmatch('Hello, pattern matching world!', 'patternmatching', false) ;
    testmatch('Hello, pattern matching world!', '  attern',false) ;

    // Wildcards, multiple subpattern
    testmatch('Hello, pattern matching world!', 'ell*patt*orl', true) ;
    testmatch('Hello, pattern matching world!', '*ell*patt*orl*', true) ;
    testmatch('Hello, pattern matching world!', 'ell**patt*orl', true) ;
    testmatch('Hello, pattern matching world!', 'ell*orl*patt', false) ;

    testmatch('Hello, pattern matching world!', 'ell**patt*orl*too_much', false) ;
    testmatch('Hello, pattern*matching world!', 'pattern matching', false) ;
    testmatch('Hello, pattern*matching world!', 'pattern*matching', true) ;
    testmatch('Hello, pattern*matching world!', 'pat*tern', true) ;
    testmatch('Hello, pattern*matching world!', 'pat**tern', true) ;
    testmatch('Hello, pattern matching world!', '*', true) ;

    testmatch('Hello, pattern matching world!', 'pattern *matching', true) ;
    testmatch('Hello, pattern matching world!', 'pattern  *matching', true) ;
    testmatch('Hello, pattern matching world!', 'pattern * matching', false) ;
    testmatch('Hello, pattern matching world!', 'pattern* matching', true) ;
    testmatch('Hello, pattern matching world!', 'pattern*  matching', true) ;

    testmatch('Hello, pattern matching world!', '*Hello',true) ;
    testmatch('Hello, pattern matching world!', '**Hello',true) ;

    testmatch('Hello, pattern matching world!', 'orld!*',true) ;
    testmatch('Hello, pattern matching world!', 'orld!**',true) ;

    // Anomalien
    testmatch('', '', true) ;
    testmatch('', '*', true) ;
    testmatch('Any line', '', true) ;
    testmatch('', ' ', false) ;
  end { "procedure TestPatternMatch" } ;


function Replace (var s: string; s_find, s_replace : string; ignorecase: boolean = false) : boolean ;
{ ---- ersetzt in 's_org' den Substring 's_find' durch 's_replace'.
       ersetzt das erste Auftreten von 's_find', Gross/Kleinschreibung
       muss stimmen.
}
  var i : integer ;
  begin
    result := false ;
    if ignorecase then
      i := pos(AnsiUpperCase(s_find), AnsiUpperCase(s))
    else i := pos(s_find, s) ;
    if i <> 0 then begin
      s := concat(
              Copy(s, 1, i-1),
              s_replace,
              Copy (s, i + length(s_find), 999 ) ) ;
      result := true
    end ;
  end { "function Replace" } ;

{ -- Trailing Spaces abschneiden }
function RTrim(s: string): string ;
  var i, n: integer ;
  begin
    n := length(s) ;
    i := n ; // i = letztes non-space-Zeichen
    while (i > 0) and ((s[i] = ' ') or (s[i] = #9)) do
      dec(i) ;
    if i < n
      then result := Copy(s, 1, i)
      else result := s ;
  end ;

// trailing chars abschneiden
function RTrimChars(s: string; cs: charset): string ;
  var i: integer ;
  begin
    i := length(s) ;
    while (i > 0) and CharInSet(s[i], cs ) do dec(i) ;
    result := Copy(s, 1, i) ;
  end;

// leading chars abschneiden
function LTrimChars(s: string; cs: charset): string ;
  var i: integer ;
  begin
    i := 1 ;
    while (i <= length(s)) and CharInSet(s[i], cs ) do inc(i) ;
    result := Copy(s, i, maxint) ;
  end;

// chars auf beiden seiten abschneiden
function TrimChars(s: string; cs: charset): string ;
  begin
    result := LTrimChars(RTrimChars(s, cs), cs) ;
  end;

{-Given a set of word delimiters, return words into stringlist }
procedure ExtractWords(sl: TStringList; s : string; WordDelims : charset);
  var
    i0, i : integer;
    slen : integer ;
  begin
    sl.Clear ;
    slen := length(s) ;
    i := 1 ;
    while (i <= slen) do begin
      {1. skip over delimiters}
      while (i <= slen) and CharInSet(s[i], WordDelims) do
        inc(i);
      {if we're not beyond end of S, we're at the start of a word}
      if i <= slen then begin
        i0 := i ;  // mark start of word
        // 2. find end of word
        while (i <= slen) and not CharInSet(s[i], WordDelims) do
          inc(i) ;
        if i > slen then inc(i) ;
        // 3. add word to list
        sl.Add(Copy(s, i0, i-i0)) ;
      end ;
    end { while } ;
  end{ "procedure ExtractWords" } ;


constructor TJH_Registry.Create ;
  begin
    inherited Create ;
    fRegistryKey := '' ;
    fReadOnly := false ;
    fSaveTimer := TTimer.Create(nil) ;
    fSaveTimer.OnTimer := autoSaveToFile ;
    fSaveTimer.Interval := 5000 ;
    fSaveTimer.Enabled := true ;
    fFilename := '' ;
    fChanged := false ;
    Loading := false ;
  end ;

destructor TJH_Registry.Destroy ;
  begin
    fSaveTimer.Free ;
    autoSaveToFile(nil)  ;
    inherited ;
  end ;

// periodisch speichern, wenn Filename gesetzt
procedure TJH_Registry.autoSaveToFile(Sender: TObject) ;
  begin
    if fChanged and (fFilename <> '') then begin
      try
        fChanged := false ; // selbst wenn schief geht: nicht nochmal probieren
        SaveToFile ;
      except
      end ;
    end ;
  end ;


// Key sichern, durch Aufruf von "REGEDIT.EXE"
procedure TJH_Registry.SaveToFile ;

// Here are some Regedit command line options that I have found through various sources.
//
//  * filename: Import .reg files into the registry
//  * /a: Export non uni-code
//  * /C: Compressfilename (Windows 98)
//  * /e: Export a registry file -- Example: RegEdit /e HKCU-Soft.reg HKEY_CURRENT_USERSoftware
//  * /i: Import .reg files into the registry
//  * /L: system: Specify the location of the system.dat to use
//  * /R:user: Specify the location of the user.dat to use
//  * /s: Silent -- Doesn't diplay any dialog boxes for confirmation

  var buff1, buff2: array[0..1024] of char ;
  begin
    if (fFilename <> '') and (fRegistryKey <> '') then
      if OpenKey(fRegistryKey, true) then begin
        fFilename := ExpandUNCFilename(fFilename) ;
        DeleteFile(fFilename) ;
        CloseKey ;
        ShellExecute( 0, 'open', 'regedit.exe',
                strpcopy(buff1, '/a ' + ExtractFilename(fFilename) + ' "HKEY_CURRENT_USER' + fRegistryKey + '"'),
                strpcopy(buff2, ExtractFilePath(fFilename)),  SW_HIDE) ;
      end else
        raise Exception.CreateFmt('Registry key "%s" is invalid', [fRegistryKey]) ;
  end { "procedure TJH_Registry.SaveToFile" } ;


// Key laden, durch Aufruf von "REGEDIT.EXE"

procedure TJH_Registry.LoadFromFile ;
  var buff1, buff2: array[0..1024] of char ;
  begin
    if fFilename <> '' then begin
      fFilename := ExpandUNCFilename(fFilename) ;
      if not FileExists(fFilename) then
        raise Exception.CreateFmt('File "%s" does not exist', [Filename]) ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then begin
        DeleteKey(fRegistryKey) ;
        // Execute der Reg
        ShellExecute( 0, 'open', 'regedit.exe',
                strpcopy(buff1, '/s ' + ExtractFilename(fFilename)),
                strpcopy(buff2, ExtractFilePath(fFilename)),  SW_HIDE) ;
      end else raise Exception.CreateFmt('Registry key "%s" is invalid', [fRegistryKey]) ;
    end ;
  end { "procedure TJH_Registry.LoadFromFile" } ;


procedure TJH_Registry.Save(parmname:string ; parmval:string) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteString(parmname, parmval) ;
    end ;
    fChanged := true ;
  end ;

function TJH_Registry.Load(parmname:string; defaultval: string = '') : string ;
  begin
    result := '' ;
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) and ValueExists(parmname) then begin
        fWasFound := true ;
        result := ReadString(parmname);
      end else result := defaultval ;
    finally
      Loading := false ;
    end ;
  end { "function TJH_Registry.Load" } ;


procedure TJH_Registry.Save(name: string ; strstr: TStringStream) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteString(name, strstr.DataString) ;
    end ;
    fChanged := true ;
  end;

procedure TJH_Registry.Load(name: string ; strstr: TStringStream) ;
  begin
    strstr.Seek(0, soFromBeginning) ;
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) and ValueExists(name) then begin
        fWasFound := true ;
        strstr.WriteString(ReadString(name));
      end ;
    finally
      Loading := false ;
    end ;
  end{ "procedure TJH_Registry.Load" } ;



procedure TJH_Registry.Save(parmname:string; parmval: integer) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteInteger(parmname, parmval) ;
    end ;
    fChanged := true ;
  end ;

function  TJH_Registry.Load(parmname:string; defaultval: integer): integer ;
  begin
    result := defaultval ;
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) and ValueExists(parmname) then begin
        fWasFound := true ;
        result := ReadInteger(parmname) ;
      end ;
    finally
      Loading := false ;
    end ;
  end { "function TJH_Registry.Load" } ;


procedure TJH_Registry.Save(parmname:string; parmval: boolean) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteBool(parmname, parmval) ;
    end ;
    fChanged := true ;
  end ;

function TJH_Registry.Load(parmname:string; defaultval: boolean): boolean ;
  begin
    result := defaultval ;
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) and ValueExists(parmname) then begin
        fWasFound := true ;
        result := ReadBool(parmname) ;
      end ;
    finally
      Loading := false ;
    end ;
  end { "function TJH_Registry.Load" } ;


procedure TJH_Registry.Save(parmname:string ; parmval:double) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteFloat(parmname, parmval) ;
    end ;
    fChanged := true ;
  end ;


function TJH_Registry.Load(parmname:string; defaultval: double = 0) : double ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) and ValueExists(parmname) then begin
        fWasFound := true ;
        result := ReadFloat(parmname)
      end else result := defaultval ;
    finally
      Loading := false ;
    end ;
  end ;



procedure TJH_Registry.Save(cb: TComboBox) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteString(getControlNamePath(cb), cb.text) ;
    end ;
    fChanged := true ;
  end ;

procedure TJH_Registry.Load(cb: TComboBox; defaultval: string) ;
  var
    s: string ;
    i: integer ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false)
              and ValueExists(getControlNamePath(cb)) then begin
        fWasFound := true ;
        s := ReadString(getControlNamePath(cb)) ;
      end else s := defaultval ;
      i := cb.Items.IndexOf(s) ;
      if i >= 0 then cb.ItemIndex := i else cb.ItemIndex := -1 ;
      if cb.Style = csDropDown then
        cb.text := s ;
    finally
      Loading := false ;
    end { "try" } ;
  end { "procedure TJH_Registry.Load" } ;


procedure TJH_Registry.Save(lb: TListBox) ;
  var s: string ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then begin
        if lb.ItemIndex < 0 then s:= ''
        else s := lb.Items[lb.ItemIndex] ;
        WriteString(getControlNamePath(lb), s) ;
      end;
    end ;
    fChanged := true ;
  end ;

procedure TJH_Registry.Load(lb: TListBox; defaultval: string = '') ;
  var
    s: string ;
    i: integer ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false)
              and ValueExists(getControlNamePath(lb)) then begin
        fWasFound := true ;
        s := ReadString(getControlNamePath(lb)) ;
      end else s := defaultval ;
      i := lb.Items.IndexOf(s) ;
      if i >= 0 then lb.ItemIndex := i else lb.ItemIndex := -1 ;
    finally
      Loading := false ;
    end { "try" } ;
  end { "procedure TJH_Registry.Load" } ;


procedure TJH_Registry.Save(cb: TCheckBox) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteBool(getControlNamePath(cb), cb.Checked) ;
    end ;
    fChanged := true ;
  end ;


procedure TJH_Registry.Load(cb: TCheckBox; defaultval: boolean) ;
  var b: boolean ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) and ValueExists(getControlNamePath(cb)) then begin
        fWasFound := true ;
        b := ReadBool(getControlNamePath(cb));
      end else b := defaultval ;
      cb.Checked := b ;  // löst einen Event aus
    finally
      Loading := false ;
    end ;
  end { "procedure TJH_Registry.Load" } ;


procedure TJH_Registry.Save(rb: TRadioButton) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteBool(getControlNamePath(rb), rb.Checked) ;
    end ;
    fChanged := true ;
  end ;


procedure TJH_Registry.Load(rb: TRadioButton; defaultval: boolean) ;
  var b: boolean ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false)
              and ValueExists(getControlNamePath(rb)) then begin
        fWasFound := true ;
        b := ReadBool(getControlNamePath(rb));
      end else b := defaultval ;
      rb.Checked := b ;  // löst einen Event aus
    finally
      Loading := false ;
    end ;
  end { "procedure TJH_Registry.Load" } ;


procedure TJH_Registry.Save(ed: TEdit) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteString(getControlNamePath(ed), ed.text) ;
    end ;
    fChanged := true ;
  end ;


procedure TJH_Registry.Load(ed: TEdit; defaultval: string = '') ;
  var s: string ;
    namepath: string ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      namepath := getControlNamePath(ed) ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) and ValueExists(namepath) then begin
        fWasFound := true ;
        s := ReadString(namepath);
      end else
        s := defaultval ;
      ed.text := s ;  // löst einen Event aus
    finally
      Loading := false ;
    end { "try" } ;
  end { "procedure TJH_Registry.Load" } ;

procedure TJH_Registry.Save(mi: TMenuItem) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteBool(mi.name, mi.Checked) ;
    end ;
    fChanged := true ;
  end ;

procedure TJH_Registry.Load(mi: TMenuItem; defaultval: boolean = false) ;
  var b: boolean ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false)
              and ValueExists(mi.name) then begin
        fWasFound := true ;
        b := ReadBool(mi.name);
      end else b := defaultval ;
      mi.Checked := b ;  // löst einen Event aus
    finally
      Loading := false ;
    end ;
  end { "procedure TJH_Registry.Load" } ;


procedure TJH_Registry.Save(ed: TMaskEdit) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteString(getControlNamePath(ed), ed.text) ;
    end ;
    fChanged := true ;
  end ;


procedure TJH_Registry.Load(ed: TMaskEdit; defaultval: string = '') ;
  var s: string ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false)
              and ValueExists(getControlNamePath(ed)) then begin
        fWasFound := true ;
        s := ReadString(getControlNamePath(ed));
      end else s := defaultval ;
      ed.text := s ;  // löst einen Event aus
    finally
      Loading := false ;
    end ;
  end { "procedure TJH_Registry.Load" } ;

procedure TJH_Registry.Save(pagecontrol: TPageControl) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then begin
        WriteInteger(getControlNamePath(pagecontrol)+'.ActivePageIndex', pagecontrol.ActivePageIndex) ;
      end;
    end;
  end;

procedure TJH_Registry.Load(pagecontrol: TPageControl) ;
  const defaultval = 0 ;
  var result: integer ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) and
              ValueExists(getControlNamePath(pagecontrol)+'.ActivePageIndex') then begin
        fWasFound := true ;
        result := ReadInteger(getControlNamePath(pagecontrol)+'.ActivePageIndex');
      end else result := defaultval ;
      pagecontrol.ActivePageIndex := result ;  // löst einen Event aus
    finally
      Loading := false ;
    end ;
  end{ "procedure TJH_Registry.Load" } ;



procedure TJH_Registry.Save(form: TForm) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then begin
        WriteInteger(getControlNamePath(form)+'.WindowState', ord(form.WindowState)) ;
        WriteInteger(getControlNamePath(form)+'.Visible', ord(form.Visible)) ;
        if (form.WindowState <> wsMinimized) and form.Visible then begin // sonst sind die Koordinaten ungültig
          WriteInteger(getControlNamePath(form)+'.Left', form.left) ;
          WriteInteger(getControlNamePath(form)+'.Top', form.top) ;
          WriteInteger(getControlNamePath(form)+'.Width', form.width) ;
          WriteInteger(getControlNamePath(form)+'.Height', form.height) ;
        end ;
      end ;
    end ;
    fChanged := true ;
  end { "procedure TJH_Registry.Save" } ;

procedure TJH_Registry.Load(form: TForm{; left, top, width, height: integer; state: TWindowState}) ;

  function get(suffix:string; default: integer): integer ;
    var keyname: string ;
    begin
      keyname := getControlNamePath(form) + '.' + suffix ;
      fWasFound := false ;
      if ValueExists(keyname) then begin
        fWasFound := true ;
        result := ReadInteger(keyname);
      end else result := default ;
    end ;

  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) then begin
        form.WindowState := TWindowState(get('WindowState', ord(form.WindowState))) ;
        if form.formstyle <> fsMDIChild then
          form.Visible := boolean(get('Visible', ord(form.Visible))) ;
        form.left := get('Left', form.left) ;
        form.top := get('Top', form.top) ;
        // festdesignte Fenstergrössen nicht übersteuern
        if form.BorderStyle in [bsSizeable, bsSizeToolWin] then begin
          form.width := get('Width', form.width) ;
          form.height := get('Height', form.height) ;
        end;
      end ;
    finally
      Loading := false ;
    end { "try" } ;
  end { "procedure TJH_Registry.Load" } ;


procedure TJH_Registry.Save(button: TSpeedButton) ;
  begin
    if not Loading and not fReadOnly then begin
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then
        WriteBool(getControlNamePath(button), button.Down) ;
    end ;
    fChanged := true ;
  end ;


procedure TJH_Registry.Load(button: TSpeedButton; defaultval: boolean = false) ;
  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false)
              and ValueExists(getControlNamePath(button)) then begin
        fWasFound := true ;
        button.Down :=  ReadBool(getControlNamePath(button));
      end else button.Down := defaultval ;
    finally
      Loading := false ;
    end ;
  end { "procedure TJH_Registry.Load" } ;


procedure TJH_Registry.Save(panel: TPanel) ;
  begin
    begin
      if not Loading and not fReadOnly then begin
        if (fRegistryKey <> '') and OpenKey(fRegistryKey, true) then begin
          WriteInteger(getControlNamePath(panel)+'.Left', ord(panel.left)) ;
          WriteInteger(getControlNamePath(panel)+'.Top', ord(panel.top)) ;
          WriteInteger(getControlNamePath(panel)+'.Width', ord(panel.width)) ;
          WriteInteger(getControlNamePath(panel)+'.Height', ord(panel.height)) ;
        end ;
      end ;
    end ;
    fChanged := true ;
  end { "procedure TJH_Registry.Save" } ;

procedure TJH_Registry.Load(panel: TPanel) ;
  function get(suffix:string; default: integer): integer ;
    var keyname: string ;
    begin
      keyname := getControlNamePath(panel) + '.' + suffix ;
      fWasFound := false ;
      if ValueExists(keyname) then begin
        fWasFound := true ;
        result := ReadInteger(keyname);
      end else result := default ;
    end ;

  begin
    try
      Loading := true ;
      fWasFound := false ;
      if (fRegistryKey <> '') and OpenKey(fRegistryKey, false) then begin
        panel.left := get('Left', panel.left) ;
        panel.top := get('Top', panel.top) ;
        panel.width := get('Width', panel.width) ;
        panel.height := get('Height', panel.height) ;
      end ;
    finally
      Loading := false ;
    end ;

  end { "procedure TJH_Registry.Load" } ;


procedure TJH_Registry.Save(name: string ; sl: TStrings) ;
  var
    i: integer ;
    subkey: string ;
  begin
    // <name> wird ein Subkey, der wird erstmal total gelöscht
    if not Loading and not fReadOnly and (fRegistryKey <> '') then begin
      subkey := fRegistryKey+ '\' + name ;
      DeleteKey(subkey) ;
      if OpenKey(subkey, true) then
        for i := 0 to sl.Count-1 do
          WriteString(IntToStr(i), sl[i]) ; // Keyname= Zahl
    end ;
    fChanged := true ;
  end { "procedure TJH_Registry.Save" } ;

procedure TJH_Registry.Load(name: string ; sl: TStrings) ;
  var
    i: integer ;
    subkey: string ;
  begin
    try
      Loading := true ;
      subkey := fRegistryKey+ '\' + name ;
      sl.Clear ;
      if (fRegistryKey <> '') and OpenKey(subkey, false) then begin
        // die Keys sind die Zeilennummern im String: in dieser Reihenfolge einlesen
        i := 0 ;
        while ValueExists(IntToStr(i)) do begin
          sl.Add(ReadString(IntToStr(i))) ;
          inc(i) ;
        end ;
      end ;
    finally
      Loading := false ;
    end { "try" } ;

  end { "procedure TJH_Registry.Load" } ;



procedure OptimizeAnyGridColWidths(aGrid: TControl);
// kann TDBGrid und TStringGrids verarbeiten
  const
    gap = 8;  {zusätzliche Lücke zwischen Text aund Spaltengrenzen in Pixeln}
    max_cols = 100;
  var
    dbgrid: TDBGrid ;
    stgrid: TStringGrid ;

    _row, _col{, n}: integer;
    colcount: integer ;
    visiblecolcount: integer ;
    tmp: longint ;
    gewicht : array[0..max_cols] of double;  // Gewicht des sichtbaren Inhalts der Spalte
    gewicht_summe: double ; // Summe der Gewichte aller Spalten

    max_breite : array[0..max_cols] of longint;   // Obergrenze für Spaltenbreite
    breite : array[0..max_cols] of integer;  // berechnete Breite in Pixel
    short : array[0..max_cols] of boolean;  // true: Spalte braucht weniger Platz und wird nicht weiter verrechnet
    invisible : array[0..max_cols] of boolean;  // true: Spalte ist unsitbar und wird nicht mit berechnet
    total_grid_colspace: longint ; // Platz im Grid für Cell-inhalte (quer, in Pixel)
    free_grid_colspace: longint ; // noch auf "not short" Spalten zu verteilen
    ready: boolean ;
    bm: TBookmark ;
  begin { "procedure OptimizeAnyGridColWidths" }
    dbgrid := nil ; stgrid := nil ;

    if aGrid is TDBGrid then begin
      dbgrid := aGrid as TDBGrid ;
      colcount := dbgrid.Columns.Count ;
    end else if aGrid is TStringGrid then begin
      stgrid := aGrid as TStringGrid ;
      colcount := stgrid.colcount ;
    end else raise Exception.Create('OptimizeAnyGridColWidths() arbeitet nur auf TDBGrid oder TStringGrid!') ;

    visiblecolcount := colcount ;
    for _col := 0 to colcount-1 do begin
      gewicht[_col] := 10;
      short[_col] := false ;
      breite[_col] := 0 ;
      invisible[_col] := false ;
      if stgrid <> nil then begin
        visiblecolcount := colcount ;
      end;
      if dbgrid <> nil then
        if dbgrid.Columns[_col].Visible then
          inc(visiblecolcount)
        else invisible[_col] := true ;
    end { "for _col" } ;


    total_grid_colspace := 0 ; // damit keine fehlende Initalisierung angemeckert wird ...
    if (dbgrid <> nil) then begin
      if dbgrid.Datasource.state in [dsInactive] then Exit ; // tue nichts

      {damit die Anzeige beim Durchscannen des Grids nicht mitläuft}
      dbgrid.Datasource.Dataset.DisableControls;
      total_grid_colspace := dbgrid.ClientRect.Right - dbgrid.ClientRect.left
              - visiblecolcount  // 1 Pixel pro Trennlinie
              - 12 // 10 für den Indikator links
//          - 10  // 10 für den Scrollbar rechts
              -1 // für rundungsfehler
      ;
    end ;
    if stgrid <> nil then begin
      total_grid_colspace := stgrid.ClientRect.Right - stgrid.ClientRect.left - stgrid.colcount ;
    end ;

    tmp := 0 ; // damit keine fehlende Initalisierung angemeckert wird ...
    // Spaltenüberschriften gewichten
    for _col := 0 to colcount-1 do begin
      if not invisible[_col] then
        if _col > max_cols then begin MessageDlg('zu viele Spalten!',mtError, [mbOK],0); Exit end;
      if dbgrid <> nil then tmp := dbgrid.Canvas.TextWidth(dbgrid.Columns[_col].Fieldname) + gap;
      if stgrid <> nil then tmp := stgrid.Canvas.TextWidth(stgrid.Cells[_col,0]) + gap;
      // kein Faktor: Titel wiegen soviel wie Zellinhalte
      gewicht[_col] := tmp;
      max_breite[_col] := tmp ;
    end;

    {für jede Spalte die Gesamtzahl der Zeichen auf Basis des gesamten Datasets ermitteln}
    {}
//    n := 0 ;
    if dbgrid <> nil then
      with dbgrid.Datasource.Dataset do begin
        bm := GetBookmark ;
        First;
        while not Eof do begin
//          inc(n) ;
          for _col := 0 to colcount-1 do
            if not invisible[_col] then begin
              tmp := dbgrid.Canvas.TextWidth(trim(dbgrid.Columns[_col].Field.DisplayText)) + gap ;
              gewicht[_col] := gewicht[_col] + (tmp);
              if max_breite[_col] < tmp then max_breite[_col] := tmp ;
            end ;
          Next;
        end;
        GotoBookmark(bm) ; // -> sonst Exception 'Gitterindex außerhalb'
        FreeBookmark(bm) ;
      end { "with dbgrid.Datasource.Dataset" } ;
    if stgrid <> nil then begin
//      n := stgrid.rowcount ;
      for _col := 0 to colcount-1 do
        if not invisible[_col] then
          for _row := 1 to stgrid.rowcount-1 do begin
            tmp := stgrid.Canvas.TextWidth(trim(stgrid.Cells[_col, _row])) + gap ;
            gewicht[_col] := gewicht[_col] + (tmp);
            if max_breite[_col] < tmp then max_breite[_col] := tmp ;
          end ;
    end ;


    // gewicht[] enthält jetzt die Gesamtlänger aller sichtbaren Inhalte der Spalte
    // Verfahren ist iterativ: loop, bis eine Proportinalitätsvertielung
    // keine Spalten mehr mit zuviel Platz versieht.
    repeat

      // 1. Verteile die Breitenseite auf Basis der Zeichensummen nichtmarkeirter so, dass das ganze Grid gefüllt wird.
      // breite neu = Gridbreite * breitensumme/zeichensumme
      // 2. setze "short"-Marke bei Spalten, die jetzt zuviel Platz gekriegt haben,
      //    und verkürze sie

      // beim verfügbarer Platz die short[]-Spalten nicht berücksichtigen
      free_grid_colspace := total_grid_colspace ;
      for _col := 0 to colcount-1 do
        if not invisible[_col] then
          if short[_col] then free_grid_colspace := free_grid_colspace - breite[_col] ;

      gewicht_summe := 0 ;
      for _col := 0 to colcount-1 do
        if not invisible[_col] then
          if not short[_col] then gewicht_summe := gewicht_summe + gewicht[_col] ;

      { den noch verfügbaren Platz auf die not-short Spalten verteilen}
      for _col := 0 to colcount-1 do
        if not invisible[_col] then
          if not short[_col] then
            breite[_col] := round(free_grid_colspace * gewicht[_col] / gewicht_summe);

      // Check: haben Spalten mehr Platz als ihr Maximum gekriegt?
      // wenn ja: verkürze sie aufs Maximum, setze sie "short" und mache noch einen Durchgang
      ready := true ;
      for _col := 0 to colcount-1 do
        if not invisible[_col] then
          if not short[_col] and (breite[_col] > max_breite[_col]) then begin
            breite[_col] := max_breite[_col] ;
            short[_col] := true ;
            ready := false ; // loop again
          end ;

    until ready ;

    // Abschlusskorrektur: wird jetzt insgesamt weniger Platz verbraucht als vorhanden:
    // freien Platz gleichmäßig auf alle Spalten verteilen
    free_grid_colspace := total_grid_colspace ;
    for _col := 0 to colcount-1 do
      if not invisible[_col] then
        free_grid_colspace := free_grid_colspace - breite[_col] ;

    while free_grid_colspace > 0 do
      for _col := 0 to colcount-1 do
        if not invisible[_col] then
          if free_grid_colspace > 0 then begin
            inc(breite[_col]) ;
            dec(free_grid_colspace) ;
          end ;

    // Spaltenbreiten setzen
    for _col := 0 to colcount-1 do begin
      if not invisible[_col] then
        if dbgrid <> nil then dbgrid.Columns[_col].width := breite[_col] ;
      if stgrid <> nil then stgrid.ColWidths[_col]:= breite[_col] ;
    end ;

    if (dbgrid <> nil) then
      dbgrid.Datasource.Dataset.EnableControls;

  end{ "procedure OptimizeAnyGridColWidths" } ;

//einen Process starten, und siene Info zurückgeben
//false: not started
function StartProcess(var procinfo: TProcessInformation; cmd, args, dir_exec: string ; CmdShow: integer; priority_class:dword = NORMAL_PRIORITY_CLASS): boolean ;
  var
    StartInfo: TStartupInfo ;
    lpCurrentDirectory :  pchar ;
    cmdline: string ;
    fullcmdpath: array[0..1023] of char ;
  begin
    { fill with known state }
    FillChar(StartInfo, sizeof(TStartupInfo), #0) ;
    FillChar(procinfo, sizeof(TProcessInformation), #0) ;
    StartInfo.cb := sizeof(TStartupInfo) ;
//    if minimized then begin
    { ggf. alle childs minimized }
    StartInfo.dwFlags := STARTF_USESHOWWINDOW ;
    StartInfo.wShowWindow := CmdShow ; // Siehe Hilfe zu "ShowWindow()"
//    end ;

    if dir_exec = ''
      then lpCurrentDirectory := nil
      else lpCurrentDirectory := pchar(dir_exec) ;

    FindExecutable(pchar(cmd), lpCurrentDirectory, fullcmdpath) ;
    cmdline := strpas(fullcmdpath) + ' ' + args ;

    result := CreateProcess( nil, pchar(cmdline), nil, nil, false,
            CREATE_NEW_PROCESS_GROUP+priority_class,
            nil, lpCurrentDirectory, StartInfo, procinfo) ;

  end { "function StartProcess" } ;

{ Einen Unterprozess ausführen, und warten, bis er fertig ist.
CmdShow: SW_SHOW, SW_MINIMIZE ....
result:  $ffffffff = nicht gestartet, sonst exitcode
}
function RunProcess(cmd, args, dir_exec: string ;
        CmdShow: integer; priority_class:dword = NORMAL_PRIORITY_CLASS): Cardinal ;
  var
    procinfo: TProcessInformation ;
    processexitcode : Cardinal ;
    CreateOK: boolean ;
  begin
    CreateOK := StartProcess(procinfo, cmd, args, dir_exec, CmdShow, priority_class) ;
    processexitcode := 0 ; { default fuer STILL_ACTIVE }

    if CreateOK then begin
//      CurrentProcessHandle := ProcInfo.hProcess ;
      { wait for child processes, with 100ms reanimation }
      while WaitForSingleObject(procinfo.hProcess, 100) = WAIT_TIMEOUT do
        Application.ProcessMessages ;

      processexitcode := $ffffffff ;
      GetExitCodeProcess(procinfo.hProcess, processexitcode) ;
      result := processexitcode ;
    end else
      result := $ffffffff ;

  end{ "function RunProcess" } ;

// Löscht rekursiv alle Registrykeys ab aPath
procedure DeleteRegKey(aRoot : HKey; aPath : string);
  var
    sl : TStringList;
    i : integer ;
  begin
    sl := TStringList.Create;
    with TRegistry.Create do
      try
        RootKey := aRoot;
        if OpenKey(aPath,false) then begin
          GetKeyNames(sl);
          // zuerst in die Tiefe
          for i := 0 to sl.Count-1 do
            DeleteRegKey(aRoot,aPath + '\' + sl[i]) ;
          CloseKey;
          DeleteKey(aPath);
        end;
      finally
        Free;
        sl.Free;
      end{ "try" } ;
  end{ "procedure DeleteRegKey" } ;


// bewegt einen ganzen key samt subkeys
// wie TRegsitry.Movekey, aber rekursiv.
procedure MoveRegKey(aRoot : HKey; aOldPath, aNewPath:string ; delete: boolean) ;
  var
    sl : TStringList ;
    i : integer ;
  begin
    sl := TStringList.Create ;
    with TRegistry.Create do
      try
        RootKey := aRoot ;
        if OpenKey(aOldPath,false) then begin
          GetKeyNames(sl) ;
          // zuerst in die Tiefe
          for i :=0 to sl.Count-1 do
            MoveRegKey(aRoot, aOldPath + '\' + sl[i], aNewPath + '\' + sl[i], delete) ;
          CloseKey ;
          MoveKey(aOldPath, aNewPath, delete) ;
        end;
      finally
        Free ;
        sl.Free ;
      end{ "try" } ;
  end{ "procedure MoveRegKey" } ;


{ erzeugt einen nicht vorhandenen Datenamen nach dem Muster
  dir\prefix_applicationhandle_lfdnr.ext }
function GetUniqueFilename(dir: string; prefix, ext: string): string ;
  var
    Count: integer ;
  begin
    Count := 0 ;
    // versuche erst den Namen ohne Zähler
    result := CorrectPath(dir + '\'
            + prefix
            + '.' + ext) ;
    while FileExists(result) do begin
      inc(Count) ;
      result := CorrectPath(dir + '\'
              + prefix + '_' + IntToStr(Count)
              + '.' + ext) ;
    end ;
  end { "function GetUniqueFilename" } ;

// falls man Delphi 5 benutzt
function FileSetReadOnly(const Filename: string; ReadOnly: boolean): boolean;
  var
    Flags: integer;
  begin
    result := false;
    Flags := GetFileAttributes(pchar(Filename));
    if Flags = -1 then Exit;
    if ReadOnly then
      Flags := Flags or SysUtils.faReadOnly
    else
      Flags := Flags and not SysUtils.faReadOnly;
    result := SetFileAttributes(pchar(Filename), Flags);
  end;


function DirectoryExists(const Directory: string): boolean;
  var
    Code: dword;
  begin
    Code := GetFileAttributes(pchar(Directory));
    result := (Code <> $ffffffff) and ( (FILE_ATTRIBUTE_DIRECTORY and Code) <> 0);
  end;

// http://stackoverflow.com/questions/3599256/how-can-i-use-delphi-to-test-if-a-directory-is-writeable
function IsDirectoryWriteable(const AName: string): boolean;
  var
    Filename: string;
    H: THandle;
  begin
    Filename := IncludeTrailingPathDelimiter(AName) + 'chk.tmp';
    H := CreateFile(pchar(Filename), GENERIC_READ or GENERIC_WRITE, 0, nil,
            CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
    result := H <> INVALID_HANDLE_VALUE;
    if result then CloseHandle(H);
  end;


function IdenticFiles(fname1, fname2: string): boolean ;
// true, wenn beide Dateien den gleichen Inhalt haben.
  var
    f1, f2: TFileStream ;
    buffer1, buffer2: array[0..$10000] of Byte ;
    count1, count2: longint ;
    i: integer ;
  begin
    result := true ; //
    f1 := nil ; f2 := nil ;
    try
      f1 := TFileStream.Create(fname1, fmOpenRead or fmShareDenyWrite) ;
      f2 := TFileStream.Create(fname2, fmOpenRead or fmShareDenyWrite) ;
      repeat
        count1 := f1.read(buffer1, $10000) ;
        count2 := f2.read(buffer2, $10000) ;
        if count1 <> count2 then
          result := false // Datei sind verschieden gross
        else if count1 > 0 then // Inhalt vergleichen
          for i := 0 to count1-1 do
            if buffer1[i] <> buffer2[i] then result := false ;
      until (result = false) or (count1 = 0) or (count2=0) ;
    finally
      if f1 <> nil then f1.Free ;
      if f2 <> nil then f2.Free ;
    end { "try" } ;
  end { "function IdenticFiles" } ;


// macht "AbsolutePath" relativ zu "BasePath", falls möglich
// Danach gilt:   Expand(BasePath + \ + result == AbsolutePath
// Bsp: Basepath = "C:\Tools\programme\MyProg"
//      AbsolutePath = "C:\Tools\Data\MyProg"
// -> Result = "..\..\Data\MyProg"
// Denn : C:\Tools\programme\MyProg \ ..\..\Data\MyProg == C:\Tools\Data\MyProg
function GetRelativePath(BasePath, AbsolutePath: string): string ;
  begin
//    ExcludeTrailingBackslash
    result := ExtractRelativePath(BasePath, AbsolutePath) ;

  end;

{
 remove all special chars, so "s" becomes a valid devicename
}
function String2Filename(s:string):string ;
  var i: integer ;
  begin
    s := trim(s) ;
    result := '' ;
    for i := 1 to length(s) do
      case s[i] of
        '\', '%', '?', '*', ':', '|', '<', '>' :
          result := result + '~' ; // visible replacement
        '/':
          result := result + '-' ; // PDP-11/05 -> PDP-11-05
        '"':
          ; // remove quotes
        ' ':
          result := result + '_' ;
        else
          result := result + s[i] ;
      end;
  end{ "function String2Filename" } ;

// setzt "BasePath" vor "RelativePath", wenn das ein relativer PFad ist
function GetAbsolutePath(BasePath, RelativePath: string): string ;
  begin
    if (length(RelativePath) >= 2) and CharInSet(RelativePath[2], [':', '\'] ) then
      result := RelativePath
    else
      result := ExpandUNCFilename(BasePath + '\' + RelativePath) ;
  end;


// Exception mit Stackdump versehen.
// Voraussetzungen: [x] project/Insert JCL debug data
procedure ReRaiseExceptionWithStackDump(e:Exception) ;
  var
    s: string ;
    level: integer ;
    lFile: string ;
    lModule:string;
    lProc:string;
    lLine: integer ;
    res: boolean ;
  begin
    // 1. Zeile: original Message
    s := e.Message ;
    level := 0 ;
    repeat
      res := JclDebug.MapByLevel(level, lFile, lModule,lProc,lLine) ;
      inc(level) ;
      if res then
        s := s + #13 + Format('%2d)  %s.%s:%d (%s)',
                [level, lModule, lProc,lLine, lFile]) ;
    until not res ;
    raise Exception.Create(s) ;
  end { "procedure ReRaiseExceptionWithStackDump" } ;




// FindFilenameInText()
// Findet in einer Textziel die erste Zeichenkette, die einen gültigen
// Dateinamen einer vorhandenen Datei bildet. Der Filename muss relativ zu "workpath" gültig sein.
function FindFilenameInText(workpath: string ; line: string): string ;
{
    Testfälle:
1.    Filename fängt mitten in gültigem Namensausdruck an:
    workdir=e:\mydir
    "error found:\temp\compile.log does not exist"
    > Es darf nicht "d:\temp\compile.log" gefunden werden,
      sondern muss "\temp\compile.log" mit Work drive "e:"  sein.

2.  Filename endet mitten in gültigem Namensausdruck:
    "d:\peak\peakcan25\current\canapi\canapi.c(1389) : error C2143: Syntaxfehler : Fehlendes ')' vor '->'"
    Es muss "d:\peak\peakcan25\current\canapi\canapi.c" gefunden werden.
3.  Filename muss sauber terminiert sein (mit Zeichen nicht aus [A-Z0-9_]
     "__ACHTUNG__d:\temp\compile_2006_10_06.log__not_found___"
    > "d:\temp\compile_2006_10_06.log" kann nicht gefunden werden da, '_' immer zum Namen gehört.
4.  Es darf nicht ein Teil eines Filenames genommen werden, nur weil dafür ein File existiert:
    Workdir = "e:\temp", "E:\temp\cur" existiert, ...\canapi.c existiert nicht:
    "d:\peak\peakcan25\current\canapi\canapi.c(1389) : error C2143: Syntaxfehler : Fehlendes ')' vor '->'"
    > Es darf nicht "E:\temp\cur" geliefert werden.
    > Absolute Pfade dürfen nicht verstümmelt werden.
5.  UNC-PFade: "\\192.128.0.1\freigabe\temp\file.txt"

Einfaches Verfahren hier (ignoriert Fälle 1. und 4.)
Solange kein Name gefunden, scanne ab "startpos"
   Verlängere Zeichenkette ab startpos um 1 und checke, obs ein gültiger Name ist
     wenn ja: merken
   Ergebnis:
   wenn ja: längster Name ab startpos gefunden

   Zusatz: eine Kette aus den klassischen Identifier-zeichen "A-Z0-9_" wird nie
   unterbrochen, um einen Namen gültig zu machen.
   Umlaute werden dabei nicht berücksichtigt
}
  var
    startpos, endpos: integer ; // Substring[start..end] checken
    cur_filename: string ; //längster NAme ab startpos
//    best_filename : string ; // globales optimum
    line_len: integer ;
    tst_filename: string ;
  begin
    result := '' ;

    startpos := 1 ;
    line_len := length(line) ;

//    best_filename := '' ; // der erste gefundene gültige Name

    ///// outer loop: finde längstn gültigen Namen ab "Startposition"
    ///
    while (startpos < line_len) do begin // see break-condition below
      cur_filename := '' ; // der längste ab startpos gefundene gültige Name
      endpos := startpos ;
      ///// inner loop: finde maximal langen gültigen Filenamen ab inkl "startpos"
      while endpos < line_len do begin
        // bricht ab, wenn cur_filename maximal verlängert ist
        // hänge das nächstes Segment dran (a-z_\.
        // An einen Filename darf niemals ein Zeichen aus A-Z0-9_ angehängt werden
        inc(endpos) ;
        while (endpos < line_len) and CharInSet(UpCase(line[endpos+1]), ['A'..'Z','0'..'9','_']) do
          inc(endpos) ;
        // Test: verlängerter Filename immer noch gültig?
        tst_filename := Copy(line, startpos, endpos-startpos+1) ;
        if FileExists(GetAbsolutePath(workpath, tst_filename)) then
          cur_filename := tst_filename ; // gültig, kürzere Lösung überschreiben
        // wenn nicht gefunden: weiter verlängern!
      end { "while endpos < line_len" } ;
      // cur_filename ist gültig: ist es soweit der beste
      if cur_filename <> '' then begin
        // ab startpos Name gefunden: gibt erste Namen zurück.
        result := cur_filename ;
        Break ;
      end;
      inc(startpos) ;
    end { "while (startpos < line_len)" } ;
  end { "function FindFilenameInText" } ;


function FindIntegerInText(line: string): integer ;
  var
    startpos, endpos: integer ; // Substring[start..end] checken
    line_len: integer ;
  begin
    result := 0 ;

    startpos := 1 ;
    line_len := length(line) ;

    // finde Anfang eines Integer. Not found: startpos > line_len
    while (startpos <= line_len) do
      if CharInSet(line[startpos], ['0'..'9']) then
        Break
      else inc(startpos) ;

    // finde Ende eines Integer

    endpos := startpos ;
    while (endpos <= line_len)  do
      if CharInSet(line[endpos], ['0'..'9']) then
        inc(endpos)
      else Break ;
    dec(endpos) ;

    if startpos <= line_len then
      result := StrToInt(Copy(line, startpos, endpos-startpos+1)) ;
  end{ "function FindIntegerInText" } ;


const
  csfsBold      = '|Bold';
  csfsItalic    = '|Italic';
  csfsUnderline = '|Underline';
  csfsStrikeout = '|Strikeout';

//
// Expected format:
//   "Arial", 9, [Bold], [clRed]
//
procedure StringToFont(sFont : string; Font : TFont );
  var
    p      : integer;
    sStyle : string;
  begin
    with Font do begin
      // get font name
      p    := pos( ',', sFont );
      name := Copy( sFont, 2, p-3 );
      delete( sFont, 1, p );

      // get font size
      p    := pos( ',', sFont );
      Size := StrToInt( Copy( sFont, 2, p-2 ) );
      delete( sFont, 1, p );

      // get font style
      p      := pos( ',', sFont );
      sStyle := '|' + Copy( sFont, 3, p-4 );
      delete( sFont, 1, p );

      // get font color
      Color := StringToColor(Copy( sFont, 3, length( sFont ) - 3 ) );

      // convert str font style to
      // font style
      Style := [];

      if( pos( csfsBold, sStyle ) > 0 )then
        Style := Style + [ fsBold ];

      if( pos( csfsItalic, sStyle ) > 0 )then
        Style := Style + [ fsItalic ];

      if( pos( csfsUnderline, sStyle ) > 0 )then
        Style := Style + [ fsUnderline ];

      if( pos( csfsStrikeout, sStyle ) > 0 )then
        Style := Style + [ fsStrikeout ];
    end{ "with Font" } ;
  end{ "procedure StringToFont" } ;

//
// Output format:
//   "Arial", 9, [Bold|Italic], [clAqua]
//
function FontToString( Font : TFont ) : string;
  var
    sStyle : string;
  begin
    with Font do begin
      // convert font style to string
      sStyle := '';

      if( fsBold in Style )then
        sStyle := sStyle + csfsBold;

      if( fsItalic in Style )then
        sStyle := sStyle + csfsItalic;

      if( fsUnderline in Style )then
        sStyle := sStyle + csfsUnderline;

      if( fsStrikeout in Style )then
        sStyle := sStyle + csfsStrikeout;

      if( ( length( sStyle ) > 0 ) and
              ( '|' = sStyle[ 1 ] ) ) then begin
        sStyle := Copy( sStyle, 2, length( sStyle ) - 1 );
      end;

      result := Format( '"%s", %d, [%s], [%s]', [ name,
              Size,
              sStyle,
              ColorToString( Color ) ] );
    end{ "with Font" } ;
  end{ "function FontToString" } ;


procedure SyncPoint ;
  begin
    Application.ProcessMessages ;
    CheckSynchronize ;
  end ;

function SetForegroundWindowEx(hWndWindow: THandle): boolean ;
{ Dient dem Setzen des Vordergrundfensters mit der Funktion
  SetForegroundWindow, die sich unter neueren Windows-Versionen
  anders verhält als unter Windows 95 und Windows NT 4.0.
  Der Rückgabewert ist True, wenn das Fenster erfolgreich in den
  Vordergrund gebracht werden konnte.
}
  var
    lThreadForeWin: THandle ; // Thread-ID für das aktuelle Vordergrundfenster
    lThreadWindow: THandle ;  // Thread-ID für das in hWndWindow spezifizierte
    // Fenster, das in den Vordergrund des Desktops
    // gebracht werden soll.
    // Falls das Fenster dem gleichen Thread wie das aktuelle
    // Vordergrundfenster angehört, ist kein Workaround erforderlich:
  begin
    lThreadWindow := GetWindowThreadProcessId(hWndWindow, nil) ;
    lThreadForeWin := GetWindowThreadProcessId(GetForegroundWindow, nil) ;
    if lThreadWindow = lThreadForeWin then begin
      // Vordergrundfenster und zu aktivierendes Fenster gehören zum
      // gleichen Thread. SetForegroundWindow allein reicht aus:
      result := SetForegroundWindow(hWndWindow) ;
    end else begin
      // Das Vordergrundfenster gehört zu einem anderen Thread als das
      // Fenster, das neues Vordergrundfenster werden soll. Mittels
      // AttachThreadInput erhaten wir kurzzeitig Zugriff auf die
      // Eingabeverarbeitung des Threads des Vordergrundfensters,
      // so dass SetForegroundWindow wie erwartet arbeitet:
      AttachThreadInput(lThreadForeWin, lThreadWindow, true) ;
      result := SetForegroundWindow(hWndWindow) ;
      AttachThreadInput(lThreadForeWin, lThreadWindow, false) ;
    end ;
  end { "function SetForegroundWindowEx" } ;


function SHGetFolderPath(csidl: integer):string ;
  const SHGFP_TYPE_CURRENT = 0;
  var path: array [0..MAX_PATH] of char;
  begin
    {$ifdef VER240}WinApi.{$endif}SHFolder.SHGetFolderPath(0,csidl,0,SHGFP_TYPE_CURRENT,@path[0]) ;
    result := path;
  end;

function GetIPAddress(const HostName: AnsiString): AnsiString;
  var
    R: integer;
    WSAData: TWSAData;
    HostEnt: PHostEnt;
    Host: array[0..MAX_PATH] of AnsiChar ;
    SockAddr: TSockAddrIn;
  begin
    result := '';
    R := WSAStartup(MakeWord(1, 1), WSAData);
    if R = 0 then
      try
        strpcopy(Host, HostName) ;
        if Host = '' then
          GetHostName(Host, MAX_PATH);
        HostEnt := GetHostByName(Host);
        if HostEnt <> nil then begin
          SockAddr.sin_addr.S_addr := longint(PLongint(HostEnt^.h_addr_list^)^);
          result := inet_ntoa(SockAddr.sin_addr);
        end;
      finally
        WSACleanup;
      end;
  end{ "function GetIPAddress" } ;


function RGB2TColor(R, G, b: integer): integer;
  begin
    if R < 0 then R := 0 ;
    if G < 0 then G := 0 ;
    if b < 0 then b := 0 ;
    if R > 255 then R := 255 ;
    if G > 255 then G := 255 ;
    if b > 255 then b := 255 ;

    // convert hexa-decimal values to RGB
    result := R + G shl 8 + b shl 16;
  end;


procedure TColor2RGB(Color: TColor; var R, G, b: integer);
  begin
    // convert hexa-decimal values to RGB
    R := Color and $FF;
    G := (Color shr 8) and $FF;
    b := (Color shr 16) and $FF;
  end;


// calc max diffeence of color channels.
// resUlt 0..255
function GetColorDist(color1, color2: TColor): integer ;
  var
    r1,g1,b1, r2,g2,b2: integer ;
    //dr, dg, DB: integer ;
  begin
    TColor2RGB(color1, r1, g1, b1) ;
    TColor2RGB(color2, r2, g2, b2) ;
    // euclidian, very bad. should use HSV modell
    result := round(sqrt(
            sqr(r1-r2) + sqr(g1-g2) + sqr(b1-b2)
            )) ;
    {
      dr := abs(r1 - r2) ;
      dg := abs(g1 - g2) ;
      DB := abs(b1 - b2) ;
      result := dr ;
      if result < dg then
        result := dg ;
      if result < DB then
        result := DB ;
        }
  end{ "function GetColorDist" } ;

// return varcolor, or a similar color different enough from "refcolor)
function AssertDifferentColor(variablecolor, referencecolor: TColor): TColor ;
  const diffDistance = $70 ; // different enough to be visible
  var
    r1,g1,b1, r2,g2,b2: integer ;
  begin
    result := variablecolor ;

    while GetColorDist(result, referencecolor) < diffDistance do begin
      TColor2RGB(referencecolor, r1, g1, b1) ;
      TColor2RGB(result, r2, g2, b2) ;

      //  r,g,b werden gleich behandelt -> farbton bleibt (nach möglichkeit),
      // maximale distance = 128
      if (r1+g1+b1) > 3*128 then begin
        // reference heller als 50% grau:  mach farbe dunkler, bis max (0,0,0)
        result := RGB2TColor(
                (r2-1) div 2,
                (g2-1) div 2,
                (b2-1) div 2) ;
      end else begin
        // reference dunkler als 50%grau:  mach farbe heller, bis max (ff,ff,ff)
        result := RGB2TColor(
                (r2+1) * 2,
                (g2+1) * 2,
                (b2+1) * 2) ;
      end ;
    end{ "while GetColorDist(result, referencecolor) < diffDistance" } ;
  end { "function AssertDifferentColor" } ;



(*
///////////// Excel
function RefToCell(ARow, ACol: Integer): string;
begin
Result := Chr(Ord('A') + ACol - 1) + IntToStr(ARow);
end;

function SaveAsExcelFile(AGrid: TStringGrid; ASheetName, AFileName: string): Boolean;
const
xlWBATWorksheet = -4167;
var
Row, Col: Integer;
GridPrevFile: string;
XLApp, Sheet, Data: OLEVariant;
i, j: Integer;
begin
// Prepare Data
Data := VarArrayCreate([1, AGrid.RowCount, 1, AGrid.ColCount], varVariant);
for i := 0 to AGrid.ColCount - 1 do
for j := 0 to AGrid.RowCount - 1 do
  Data[j + 1, i + 1] := AGrid.Cells[i, j];
// Create Excel-OLE Object
Result := False;
XLApp := CreateOleObject('Excel.Application');
try
// Hide Excel
XLApp.Visible := False;
// Add new Workbook
XLApp.Workbooks.Add(xlWBatWorkSheet);
Sheet := XLApp.Workbooks[1].WorkSheets[1];
Sheet.Name := ASheetName;
// Fill up the sheet
Sheet.Range[RefToCell(1, 1), RefToCell(AGrid.RowCount,
  AGrid.ColCount)].Value := Data;
// Save Excel Worksheet
try
  XLApp.Workbooks[1].SaveAs(AFileName);
  Result := True;
except
  // Error ?
end;
finally
// Quit Excel
if not VarIsEmpty(XLApp) then
begin
  XLApp.DisplayAlerts := False;
  XLApp.Quit;
  XLAPP := Unassigned;
  Sheet := Unassigned;
end;
end;
end;
*)


procedure OutputDebugString(s: string) ;
  var
    buff: array[0..10240] of char ;
    s1: string ;
  begin
    DateTimeToString(s1, 'hh:nn:ss.zzz', Now) ;
    s := Format('   [%s] %s', [s1, s]) ;
    // bessere Lesbarkeit in der Delphi-Ereignisanzeige
    while length(s) < 80 do  s := s  + ' ' ;
    s := s + '     // ' ;

    strpcopy(buff, s) ;
    {$ifdef VER240}WinApi.{$endif}Windows.OutputDebugString(buff) ;
  end{ "procedure OutputDebugString" } ;

procedure OutputDebugString(fmt: string; args: array of const) ;
  begin
    OutputDebugString(Format(fmt,args)) ;
  end;


// zum markieren von Breakpoints ;
procedure break_here() ;
  begin

  end;


{ sort a collection }
procedure SortCollection(aCollection: TCollection ; comparefunc: TListSortCompare) ;
  var
    tmplist: TList ;
    i: integer ;
  begin
    tmplist := TList.Create ;
    tmplist.Capacity := aCollection.Count ;
    try
      // 1. list := collection
      for i := 0 to aCollection.Count - 1 do
        tmplist.Add(aCollection.Items[i]) ;
      // 2. sort
      tmplist.Sort(comparefunc) ;
      // 3. collection := list
      for i := 0 to aCollection.Count - 1 do
        TCollectionItem(tmplist[i]).Index := i ;
    finally
      tmplist.Free ;
    end;
  end{ "procedure SortCollection" } ;

function GetFolder(Root:integer;Caption:string):string;
  var
    bi            : TBROWSEINFO;
    lpBuffer      : PChar;
    pidlPrograms,
    pidlBrowse    : PItemIDList;
  begin
    if (not SUCCEEDED(SHGetSpecialFolderLocation(getactivewindow,Root,pidlPrograms))) then
      Exit;
    lpBuffer:=StrAlloc(max_path);
    bi.hwndOwner := getactivewindow;
    bi.pidlRoot := pidlPrograms;
    bi.pszDisplayName := lpBuffer;
    bi.lpszTitle := PChar(Caption);
    bi.ulFlags := BIF_RETURNONLYFSDIRS;
    bi.lpfn := nil;
    bi.lParam := 0;
    pidlBrowse := SHBrowseForFolder(bi);
    if (pidlBrowse<>nil) then
      if (SHGetPathFromIDList(pidlBrowse, lpBuffer)) then
        result := lpBuffer;
    StrDispose(lpBuffer);
  end{ "function GetFolder" } ;


initialization
//  TestPatternMatch ;


end { "unit JH_Utilities" } .
