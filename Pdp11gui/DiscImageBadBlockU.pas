unit DiscImageBadBlockU;
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
  Manages Bad Blocks for FormDiscImage

  Read document  "DEC_STD_144.txt"


  How to test bad block list <-> DEC bad sector file functions:

  1) write bad sector file
  1.1)
  load image file with EMPTY bad sector file
  and non-empty .meta file
  -> "badsectortst.rl02"
  1.2) Write bad block list to bad sector file
  1.3) save as "tst.rl02
  1.4) check hex dump:
     start of last track at 0fd800

 2) read bad sector file
 2.1) clear tst.rl02.meta
 2.2) load tst.rl02
   no bad sectors are in bad sector list
 2.3) Load black block list from bad sector file
 2.4) block list from  "badsectortst.rl02" now in bad sector list

 3) verify bad sector file format
 3.1)
  SimH, 2 RL-disks
  RL0: att rl0 xxdp25.rl02
  RL1: att rl1 tst.rl02

  Boot XXDP
.RUN ZRLMB1
ZRLMB1.BIN

DRSSM-G2
CZRLM-B-0
CZRLM IS A UTILITY PROGRAM FOR FORMATTING BAD SECTOR FILES
UNIT IS RL01,RL02
RSTRT ADR 145702
DR>STA

CHANGE HW (L)  ? Y

# UNITS (D)  ? 2

UNIT 0
BUS ADDRESS (O)  174400 ?
DRIVE (O)  0 ?

UNIT 1
BUS ADDRESS (O)  174400 ?
DRIVE (O)  0 ? 1

CHANGE SW (L)  ? N


RLCS: 174400 DRIVE: 0
DRIVE TYPE = RL02

PACK IS WRITE LOCKED

RLCS: 174400 DRIVE: 1
DRIVE TYPE = RL01


COMMANDS AVAILABLE ARE:
1   REPORT CONTENTS OF THE BAD SECTOR FILE
2   ADD A SECTOR TO THE 'FIELD' BAD SECTOR FILE
3   DELETE A SECTOR FROM THE 'FIELD' BAD SECTOR FILE
4   VERIFY PACK - READ ONLY
5   WRITE PACK WITH WORST CASE DATA PATTERN AND VERIFY
6   MAKE A BAD SECTOR FILE
7   PRINT HELP MESSAGE

ENTER COMMAND (1 - 7) -  (D)  ?
}

interface

uses
  Classes,
  Windows,
  SysUtils,
  StdCtrls,
  Grids,
  MediaImageDevicesU,
  MediaImageBufferU ;


type
  // Source of a bad block
  TBadBlockSource = (
      bbsUnknown,
      bbsScan, // bad block found by PDP11GUI
      bbsFileFactory, // read in from bad sector file on media, factory list
      bbsFileField // ", field list
    ) ;

  TBadBlock = class(TObject)
    public
      BlockNr: integer ; // logical number
      cylinder, head, sector: integer ; // physical address on Disk
      source: TBadBlockSource ;
      firsttimestamp:string ; // erstes Auftreten yyyy/mm/dd_hh:mm
      lasttimestamp:string ; // letztes Auftreten
      info: string ;

      class function BadBlockSource2Text(source: TBadBlockSource): string ;
      class function Text2BadBlockSource(s: string): TBadBlockSource ;
      constructor Create ;
      procedure ParseFromText(s:string) ;
      function AsText: string ;

    end{ "TYPE TBadBlock = class(TObject)" } ;


  TBadBlockList = class(TList)
    private
      Grid: TStringGrid ; // merken, worin es sich anzeigen soll
      Items: array of TBadBlock ; // index = blocknr. Gute Blocks = nil

      procedure InternClear ; // ohne Show

      class function Std144_BadSectorFileCount(discimage: TMediaImage_Buffer): integer ;
      class function Std144_GetLastTrackBadSectorFileWord(discimage: TMediaImage_Buffer; badsectorfilenr: integer ; wordnr: integer): word ;
      class procedure Std144_SetLastTrackBadSectorFileWord(discimage: TMediaImage_Buffer; badsectorfilenr: integer ; wordnr: integer ; val: word) ;
    public

      CartridgeSerialNumber: dword ; // Seriennummer des Mediums is stored in bad sector file

      constructor Create ;
      destructor Destroy ; override ;
      procedure Init(len: integer ; aGrid: TStringGrid) ;
      procedure Clear ; reintroduce ; // nicht altes Clear erben!!! override ;
      // weil mein Clear() Show() macht ... das geht nicht in der destructor-Kette

      procedure registerBadBlock(aBlockNr, aCylinder, aHead, aSector: integer;
              aSource: TBadBlockSource ; aInfo:string) ; overload ;
      procedure registerBadBlock(s: string) ; overload ;
      procedure Show ; // im Memo anzeigen
      procedure SaveToFile(filename:string) ;
      procedure LoadFromFile(filename:string) ;


      procedure Std144_LoadFromImage(discimage: TMediaImage_Buffer) ;
      procedure Std144_WriteToImage(discimage: TMediaImage_Buffer) ;

    end{ "TYPE TBadBlockList = class(TList)" } ;



implementation

uses
  JH_Utilities,
  FormMainU ;


class function TBadBlock.BadBlockSource2Text(source: TBadBlockSource): string ;
  begin
    case source of
      bbsScan: result := 'Scan' ; // see ".AsText"
      bbsFileFactory: result := 'FactoryFile' ;
      bbsFileField: result := 'FieldFile' ;
      else result := '???' ;
    end;
  end;


class function TBadBlock.Text2BadBlockSource(s: string): TBadBlockSource ;
  begin
    result := bbsUnknown ;
    if Uppercase(s)= 'SCAN' then
      result := bbsScan ;
    if Uppercase(s)= 'FACTORYFILE' then
      result := bbsFileFactory ;
    if Uppercase(s)= 'FIELDFILE' then
      result := bbsFileField ;
  end;


constructor TBadBlock.Create ;
  begin
    inherited ;
    source := bbsUnknown ;
    firsttimestamp := '' ;
    lasttimestamp := '' ;
    info := '' ;
  end;


procedure TBadBlock.ParseFromText(s:string) ;
  var sl: TStringList ;
    i: integer ;
    opcode, val: string ;
  begin
    // Zeile = Liste von  "opcode=val;...."
    sl := TStringList.Create ;
    try
      sl.Delimiter := ';' ;
      sl.QuoteChar := '"' ;
      sl.DelimitedText := s ;
      for i := 0 to sl.Count - 1 do begin
        opcode := Uppercase(sl.Names[i]) ;
        val := sl.ValueFromIndex[i] ;
        if opcode = 'BADBLOCK' then
          TryStrToInt(val, BlockNr) ;
        if opcode = 'CYLINDER.HEAD.SECTOR' then begin
          TryStrToInt(ExtractWord(1, val, ['.']), cylinder) ;
          TryStrToInt(ExtractWord(2, val, ['.']), head) ;
          TryStrToInt(ExtractWord(3, val, ['.']), sector) ;
        end ;
        if opcode = 'SOURCE' then
          source := Text2BadBlockSource(val) ;
        if opcode = 'FIRSTTIME' then
          firsttimestamp := val ;
        if opcode = 'LASTTIME' then
          lasttimestamp := val ;
        if opcode = 'INFO' then
          info := val ;
      end { "for i" } ;
    finally
      sl.Free ;
    end{ "try" } ;
  end{ "procedure TBadBlock.ParseFromText" } ;


function TBadBlock.AsText: string ;
  begin
    result := Format('badblock=%d; cylinder.head.sector=%d.%d.%d; source=%s; firsttime=%s; lasttime=%s; "info=%s"',
            [BlockNr, cylinder, head, sector, BadBlockSource2Text(source),
            firsttimestamp, lasttimestamp, info]) ;
  end;


constructor TBadBlockList.Create ;
  begin
    inherited ;
    Init(0, nil) ;
  end;


destructor TBadBlockList.Destroy ;
  begin
    InternClear ;
    inherited ;
  end;

// löschen und Länge setzen
procedure TBadBlockList.Init(len: integer ; aGrid: TStringGrid) ;
  var i: integer ;
  begin
    Grid := aGrid ; // merken, worin es sich anzeigen soll
    InternClear ;
    Count := len ;
    setLength(Items, len) ;
    for i := 0 to Length(Items) - 1 do
      Items[i] := nil ;
    Show ;
  end;


procedure TBadBlockList.InternClear ;
  var i: integer ;
  begin
    for i := 0 to Length(Items) - 1 do
      if Items[i] <> nil then begin
        (Items[i] as TBadBlock).Free ;
        Items[i] := nil ;
      end;
  end;

// alle Items löschen
procedure TBadBlockList.Clear ;
  begin
    InternClear ;
    Show ;
  end;


procedure TBadBlockList.Show ;
  var i, n: integer ;
  begin
    if Grid = nil
      then Exit ;

    Grid.colcount := 7 ;
    Grid.rowcount := 2 ;
    Grid.FixedRows := 1 ;
    Grid.FixedCols := 1 ;
    Grid.Cells[0,0] := '#' ;
    Grid.Cells[1,0] := 'Block' ;
    Grid.Cells[2,0] := 'Cyl/hd/sec' ;
    Grid.Cells[3,0] := 'Source' ;
    Grid.Cells[4,0] := 'Info' ;
    Grid.Cells[5,0] := 'First' ;
    Grid.Cells[6,0] := 'Last' ;
    // zähle blocks
    n := 0 ;
    for i := 0 to Length(Items) - 1 do
      if Items[i] <> nil then
        inc(n) ;
    if n = 0 then begin
      // leeres Grid
      for i := 0 to Grid.colcount-1 do
        Grid.Cells[i,1] := '' ;
    end else begin
      Grid.rowcount := n+1;

      n := 0 ;
      for i := 0 to Length(Items) - 1 do
        if Items[i] <> nil then
          with Items[i] as TBadBlock do begin
            inc(n) ;
            Grid.Cells[0,n] := IntToStr(n) ;
            Grid.Cells[1,n] := IntToStr(BlockNr) ;
            Grid.Cells[2,n] := Format('%d.%d.%d', [cylinder, head, sector]) ;
            Grid.Cells[3,n] := BadBlockSource2Text(source) ;
            Grid.Cells[4,n] := info ;
            Grid.Cells[5,n] := firsttimestamp ;
            Grid.Cells[6,n] := lasttimestamp ;
          end;
    end{ "if n = 0 ... ELSE" } ;
    Grid.visible := true ; // falls es aus war
    OptimizeAnyGridColWidths(Grid);
  end{ "procedure TBadBlockList.Show" } ;



// Neuen Block aus Parametern
// tapes und MSCP haben kein cyl/head/sector?
procedure TBadBlockList.registerBadBlock(aBlockNr, aCylinder, aHead, aSector: integer;
        aSource: TBadBlockSource ; aInfo:string) ;
  var
    badblock: TBadBlock ;
  begin
    // neuen block anlegen, oder bestehenden ändern
    if Items[aBlockNr] = nil then
      Items[aBlockNr] := TBadBlock.Create ;
    badblock := Items[aBlockNr] as TBadBlock ;
    badblock.BlockNr := aBlockNr ;
    badblock.cylinder := aCylinder ;
    badblock.head := aHead ;
    badblock.sector := aSector ;
    badblock.source := aSource ;

    // egal, welches locale: yyyy/mm/dd_hh:mm
    DateTimeToString(badblock.lasttimestamp, 'yyyy"/"mm"/"dd_hh":"mm', Now) ;
    if badblock.firsttimestamp = '' then
      badblock.firsttimestamp := badblock.lasttimestamp ;

    badblock.info := aInfo ;
    Show ;
  end{ "procedure TBadBlockList.registerBadBlock" } ;


// neuen Block aus Textdarstellung
procedure TBadBlockList.registerBadBlock(s: string) ;
  var
    tmpbadblock: TBadBlock ;
  begin
    tmpbadblock := TBadBlock.Create ;
    try
      tmpbadblock.ParseFromText(s);
      with tmpbadblock do
        registerBadBlock(BlockNr, cylinder, head, sector, source, info) ;
    finally
      tmpbadblock.Free ;
    end;

  end{ "procedure TBadBlockList.registerBadBlock" } ;


procedure TBadBlockList.SaveToFile(filename:string) ;
  var i: integer ;
    f: System.Text ;
  begin
    Log('TBadBlockList.SaveToFile(%s)', [filename]);
    AssignFile(f, filename) ;
    try
      Rewrite(f) ;
      writeln(f, Format('serialnumber=%u', [CartridgeSerialNumber])) ;
      for i := 0 to Length(Items) - 1 do
        if Items[i] <> nil then
          writeln(f, (Items[i] as TBadBlock).AsText) ;
    finally
      CloseFile(f) ;
    end;
  end{ "procedure TBadBlockList.SaveToFile" } ;


procedure TBadBlockList.LoadFromFile(filename:string) ;
  var
    f: System.Text ;
    s: string ;
    opcode: string ;
  begin
    Log(' TBadBlockList.LoadFromFile(%s)', [filename]) ;
    AssignFile(f, filename) ;
    try
      Reset(f) ;
      InternClear ;
      while not System.Eof(f) do begin
        readln(f, s) ;
        s := trim(s) ;
        opcode := Uppercase(ExtractWord(1, s, ['='])) ;
        if opcode = 'BADBLOCK' then
          registerBadBlock(s)
        else if opcode = 'SERIALNUMBER' then
          TryStrToDword(ExtractWord(2, s, ['=']), CartridgeSerialNumber) ;
      end ;
    finally
      CloseFile(f) ;
    end{ "try" } ;
    Show;
  end{ "procedure TBadBlockList.LoadFromFile" } ;


////////////////////////////////////////////////////////////////////////////////
///  Bad sector file ("BSF") nach DEC Std 144 lesen/schreiben
///  Anforderung:
///  Wiederholtes Lesen und Schreiben des bsf soll mit
///  Scan-vorgängen kombinierbar sein, so dass der BSF stabil bleibt.
///  Daher:
///  Scans von PDP11GUI werden wie DEC "Manufacturing" behandelt
///  - Gelesener BSF ergänzt bad sector list
///  - gescannte bad sectors werden in die Manufactoring list eingetragen
///  - Bad sectors aus der "field" list werden wieder in die field list geschreiben,
///    ausser, sie fallen bei einem Scan wieder negativ auf.
///
///  ACHTUN:
///  Nach DEC Std 144 ist ein bad sector file immer 256 words lang.
///  Beim RL02 belegt das 2 sectoren, bei anderen drives nur einen sector!
///

// Ein Word aus einem Sector aus Last Track lesen/schreiben
// bei sector size = 256 word ist ein bad sector file = 1 sector
// bei sector size = 128 word   "  "   "    "      "  = 2 sectoren
class function TBadBlockList.Std144_BadSectorFileCount(discimage: TMediaImage_Buffer): integer ;
  var discdevice: TMediaImage_DiscDevice_Std ;
  begin
    // bad block management only on standard discs
    assert(discimage.device is TMediaImage_DiscDevice_Std) ;
    discdevice := discimage.device as TMediaImage_DiscDevice_Std ;

    if discdevice.BlockSize = 256 then
      result := discdevice.SectorCount div 2
    else if discdevice.BlockSize = 512 then
      result := discdevice.SectorCount
    else
      raise Exception.Create('BadBlockList supports only 256 and 512 byte sector discs') ;
  end{ "function TBadBlockList.Std144_BadSectorFileCount" } ;


class function TBadBlockList.Std144_GetLastTrackBadSectorFileWord(discimage: TMediaImage_Buffer;
        badsectorfilenr: integer ; wordnr: integer): word ;
  var BlockNr: integer ;
    discdevice: TMediaImage_DiscDevice_Std ;
  begin
    // bad block management only on standard discs
    assert(discimage.device is TMediaImage_DiscDevice_Std) ;
    discdevice := discimage.device as TMediaImage_DiscDevice_Std ;

    assert(badsectorfilenr >= 0) ;
    assert(badsectorfilenr < Std144_BadSectorFileCount(discimage)) ;
    assert(wordnr >= 0) ;
    assert(wordnr < 256) ;
    // get starting blocknr
    BlockNr := discdevice.DiscAddr2Blocknr(
            discdevice.CylinderCount-1, // last track
            discdevice.HeadCount-1, // last head
            badsectorfilenr) ;
    if discdevice.BlockSize = 256 then begin
      if wordnr >= 128 then begin
        inc(BlockNr) ; // data is in next sector
        wordnr := wordnr - 128 ;
      end;
    end else if discdevice.BlockSize = 512 then
      // everything is fine
    else
      raise Exception.Create('BadBlockList supports only 256 and 512 byte sector discs') ;
    result := discimage.getImageBufferBlockWord(BlockNr, wordnr) ;
  end{ "function TBadBlockList.Std144_GetLastTrackBadSectorFileWord" } ;


// Sector in Last Track lesen
class procedure TBadBlockList.Std144_SetLastTrackBadSectorFileWord(discimage: TMediaImage_Buffer;
        badsectorfilenr: integer ; wordnr: integer ; val: word) ;
  var BlockNr: integer ;
    discdevice: TMediaImage_DiscDevice_Std ;
  begin
    // bad block management only on standard discs
    assert(discimage.device is TMediaImage_DiscDevice_Std) ;
    discdevice := discimage.device as TMediaImage_DiscDevice_Std ;

    assert(badsectorfilenr >= 0) ;
    assert(badsectorfilenr < Std144_BadSectorFileCount(discimage)) ;
    assert(wordnr >= 0) ;
    assert(wordnr < 256) ;
    // get starting blocknr
    BlockNr := discdevice.DiscAddr2Blocknr(
            discdevice.CylinderCount-1, // last track
            discdevice.HeadCount-1, // last head
            badsectorfilenr) ;
    if discdevice.BlockSize = 256 then begin
      if wordnr >= 128 then begin
        inc(BlockNr) ; // data is in next sector
        wordnr := wordnr - 128 ;
      end;
    end else if discdevice.BlockSize = 512 then
      // everything is fine
    else
      raise Exception.Create('BadBlockList supports only 256 and 512 byte sector discs') ;
    discimage.setImageBufferBlockWord(BlockNr, wordnr, val) ;
  end{ "procedure TBadBlockList.Std144_SetLastTrackBadSectorFileWord" } ;



// den bad sector file nach DEC Std 144 aus der letzten Track des
// images auslesen
procedure TBadBlockList.Std144_LoadFromImage(discimage: TMediaImage_Buffer) ;
// den bad sector file nach DEC Std 144 aus der letzten Track des
// images auslesen
  var discdevice: TMediaImage_DiscDevice_Std ;
    // bad block management only on standard discs

// result: gültiger sector
// source: gehört der last track sector "sectornr" zu "manufacturing" oder "field"?
  function readBadSectorFile(badsectorfilenr:integer ; source: TBadBlockSource): boolean ;
    var
      w0, w1, w2: word ;
      good_sector: boolean ;
      i: integer ;
      badsectorentry_blocknr, badsectorentry_cylinder, badsectorentry_head, badsectorentry_sector: integer ; // bad sector
    begin
      good_sector := true ;

      // Test #1: bad sector file valid? or containing read errors?
      // invalid = erroneous sectors are cleared in discimage
      w0 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 254) ;
      w1 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 255) ;
      // letzter eintrag kann nicht 0/0/0 sein, da die bad sectors aufsteigend
      // in den bad sector file kommen
      if (w0 = 0) and (w1 = 0) then
        good_sector := false ;

      // Test #2: not "All-Ones"? Check serial# and reserved-word
      // RL02 XXDp ZRLMB1 sets only bad sector file #0.
      // #2 and #3 are "all-ones", rest is unchanged.
      w0 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 0) ;
      w1 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 1) ;
      w2 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 2) ;
      if (w0 = $ffff) and (w1 = $ffff) and (w2 = $ffff) then
        good_sector := false ;

      if good_sector then begin
        w0 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 3) ;
        if w0 <> 0 then begin
          good_sector := false ;
          raise Exception.Create('This is an alignment cartridge! Where do you get it from?') ;
        end;
      end ;

      if good_sector then begin
        if source = bbsFileFactory then begin
          // get Cartridge Serial Number only from "FACTORY" bad sector file
          w0 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 0) ;
          w1 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 1) ;
          CartridgeSerialNumber := (dword(w0) shl 16) or w1 ;
        end;

        // i: Index of bad sector entry
        for i := 0 to 125 do begin
          w0 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 4+2*i) ;
          w1 := Std144_GetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 5+2*i) ;
          if (w0 = $ffff) and (w1 = $ffff) then
            Break ; // all ones = end of list
          badsectorentry_cylinder := w0 ; // DEC: "Cylinder"
          badsectorentry_head := w1 shr 8 ; // DEC: "Track"
          badsectorentry_sector := w1 and $ff ;
          badsectorentry_blocknr := discdevice.DiscAddr2Blocknr(badsectorentry_cylinder, badsectorentry_head, badsectorentry_sector) ;
          // Bad block in Liste eintragen.
          // nicht überschreiben, wenn er schon drin steht!
          if Items[badsectorentry_blocknr] = nil then
            registerBadBlock(badsectorentry_blocknr,
                    badsectorentry_cylinder, badsectorentry_head, badsectorentry_sector,
                    source, Format('bad sector file sector.entry = %d.%d', [badsectorfilenr, i])) ;
        end { "for i" } ;
      end{ "if good_sector" } ;
      result := good_sector ;
    end{ "function readBadSectorFile" } ;

  var
    badsectorfilenr: integer ;
    good_sector: boolean ;
  begin { "procedure TBadBlockList.Std144_LoadFromImage" }
    // bad block management only on standard discs
    assert(discimage.device is TMediaImage_DiscDevice_Std) ;
    discdevice := discimage.device as TMediaImage_DiscDevice_Std ;

    // Manufacturing-list: identische Kopien in den ersten 10 bad sector files

    good_sector := false ;
    for badsectorfilenr := 0 to 9 do //  // versuche files 0,2,4,8 = factory list
      // read only even bad sector files, odd are for 18-bit systems
      if not odd(badsectorfilenr) then begin
        good_sector := readBadSectorFile(badsectorfilenr, bbsFileFactory) ;
        // daten aus niedrigstem bad sector file übernehmen!
        // RL02 XXDP ZRMLB1 setzt nur file #0!
        if good_sector then
          Break ; // Fertig
      end ;

    if not good_sector then
      raise Exception.Create('No valid sector found in "Manufacturing"-section of bad sector file!') ;

    for badsectorfilenr := 10 to Std144_BadSectorFileCount(discimage) -1 do // field list
      // read only even bad sector files, odd are for 18-bit systems
      if not odd(badsectorfilenr) then begin
        good_sector := readBadSectorFile(badsectorfilenr, bbsFileField) ;
        // daten aus niedrigstem bad sector file übernehmen!
        // RL02 XXDP ZRMLB1 setzt nur file #0!
        if good_sector then
          Break ; // Fertig
      end ;

    if not good_sector then
      raise Exception.Create('No valid sector found in "Field"-section of bad sector file!') ;
    Show ;
  end { "procedure TBadBlockList.Std144_LoadFromImage" } ;


// schreibe alle bad blocks in die "factory" list.
// "Field" abd sectors kommen in die Sector list,
// alle anderen kommen in die "Manufacturing" List
procedure TBadBlockList.Std144_WriteToImage(discimage: TMediaImage_Buffer) ;
  type TBadBlockSourceSet = set of TBadBlockSource ;

  var discdevice: TMediaImage_DiscDevice_Std ;
    // bad block management only on standard discs


// aSource: sollen die "manufacturing" oder "field" entries geschreiben werden?
  procedure writeBadSectorFile(badsectorfilenr:integer ; aSourceSet: TBadBlockSourceSet) ;
    var
      w0, w1: word ;
      i, n: integer ;
      badblock: TBadBlock ;
    begin
      // See chapter 3.2.1 of DEC_STD_144

      if odd(badsectorfilenr) then begin
        // Sonderlogik: Alle ungeraden sectoren 1,3,5,7,9,...
        // sind für das 20-sector-18-bit-format reserviert und
        // werden hier mit "All-Ones" gefüllt.
        // See RL02-spec: EK-RL012-UG-005 RL02 User Guide, pages 1-8 ff
        for i := 0 to 255 do
          Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, i, $ffff) ;
      end else begin
        // gerade sector nummer: gültiger bad sector file

        // write Cartridge Serial Number
        w0 := CartridgeSerialNumber shr 16 ;
        w1 := CartridgeSerialNumber and $ffff ;
        Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 0, w0) ;
        Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 1, w1) ;

        Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 2, 0) ; // unused

        Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 3, 0) ; // data cartridge

        // write bad sector entries
        n := 0 ; // bad block count
        for i := 0 to Length(Items) - 1 do
          if Items[i] <> nil then begin
            badblock := Items[i] as TBadBlock ;
            if badblock.source in aSourceSet then begin

              // bad Blocks werden automatisch aufsteigend sortiert gespeichert
              w0 := badblock.cylinder ; // DEC: "Cylinder"
              w1 := (badblock.head shl 8) or (badblock.sector) ; // DEC: "Track"
              Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 4+2*n, w0) ;
              Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 5+2*n, w1) ;

              inc(n) ;
              if n > 126 then
                raise Exception.Create('more than 126 bad sector found, bad sector file overflow') ;
            end;
          end{ "if Items[i] <> nil" } ;
        // fill remaining entries with "All-Ones
        while n < 126 do begin
          Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 4+2*n, $ffff) ;
          Std144_SetLastTrackBadSectorFileWord(discimage, badsectorfilenr, 5+2*n, $ffff) ;
          inc(n) ;
        end;

      end{ "if odd(badsectorfilenr) ... ELSE" } ;
    end{ "procedure writeBadSectorFile" } ;


  var
    badsectorfilenr: integer ;
  begin { "procedure TBadBlockList.Std144_WriteToImage" }
    // bad block management only on standard discs
    assert(discimage.device is TMediaImage_DiscDevice_Std) ;
    discdevice := discimage.device as TMediaImage_DiscDevice_Std ;

    // Schreibe alle "Manufacturing" bad sector files
    for badsectorfilenr := 0 to 9 do
      writeBadSectorFile(badsectorfilenr, [bbsScan, bbsFileFactory]) ;
    // Schreibe alle "Field" bad sector files
    for badsectorfilenr := 10 to Std144_BadSectorFileCount(discimage) - 1 do
      writeBadSectorFile(badsectorfilenr, [bbsFileField]) ;
  end;

end{ "unit DiscImageBadBlockU" } .

