unit FormMemoryTestU;
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
Speicher als editierbare Tabelle
Es werden immer 'memcol' Spalten nebeneinander angezeigt
Kopf "+0, +2, +4, ..."
Reihen

Die Anzahl der Spalten wird im Constructor festgelegt ('MemoryColumns').

}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, ExtCtrls,
  FormChildU,
  JH_Utilities,
  MemoryCellU, Menus, FrameMemoryCellGroupGridU,
  AuxU,
  AddressU,
  FormBusyU ;


type
  TFormMemoryTest = class(TFormChild)
      PanelT: TPanel;
      Label1: TLabel;
      StartAddrEdit: TEdit;
      SetStartAddrButton: TButton;
      MemoryGrid: TFrameMemoryCellGroupGrid;
      TestLogMemo: TMemo;
      TestAddressLinesButton: TButton;
      TestDatabitsButton: TButton;
      TestRandomButton: TButton;
      EndAddrEdit: TEdit;
      Label3: TLabel;
      Label2: TLabel;
      TestDataLinesButton: TButton;
      ChipAddrSizeComboBox: TComboBox;

      procedure OctalEditKeyPress(Sender: TObject; var Key: Char);
      procedure SetStartAddrButtonClick(Sender: TObject);
      procedure PanelTClick(Sender: TObject);
      procedure MemoryBlockSizeEditKeyPress(Sender: TObject; var Key: Char);
      procedure MemoryGridVerify1Click(Sender: TObject);
      procedure TestRandomButtonClick(Sender: TObject);
      procedure TestAddressLinesButtonClick(Sender: TObject);
      procedure TestDatabitsButtonClick(Sender: TObject);
      procedure TestDataLinesButtonClick(Sender: TObject);
    private
      { Private-Deklarationen }
      startaddr: TMemoryAddress ;
      endaddr: TMemoryAddress ; // redundant
      max_memoryblock_wordcount: integer; // max test size, depends on address width

      memoryblock_wordcount: integer ; // in words
      chipaddrsize: dword ; // size of memory chips to test in bytes
      // smaller = more tests

    public
      TheState: integer ; // 0 = invalid input, 1 = OK
      { Public-Deklarationen }
      constructor Create(AOwner: TComponent) ;
      destructor Destroy ; override ;

      procedure OnConsoleChanged ;

      procedure UpdateGUI ;

      procedure UpdateDisplay(Sender: TObject);

      procedure Log(line:string); overload ;
      procedure Log(fmt: string; args: array of const) ; overload ;

      function TestDataLines(polarity: integer): boolean ;
      function TestAdressLines(phase: integer): boolean;
      function TestDatabits(phase: integer): boolean;
      function TestRandom(count: integer): boolean;
    end{ "TYPE TFormMemoryTest = class(TFormChild)" } ;


implementation

{$R *.dfm}

uses
  FormMainU ; // Pdp11COnsole

function getLowestBitNo(value: dword): integer ;
  begin
    if value = 0 then
      result := -1
    else begin
      result := 0 ;
      while (value and (1 shl result)) = 0 do
        inc(result) ;
    end;
  end;

// input: 0x001a, output: '4,3,1'
function BitnumbersAsText(value: dword): string ;
  var i: integer ;
  begin
    result := '' ;
    for i := 31 downto 0 do
      if (value and (1 shl i) ) <> 0 then begin
        if result <> '' then result := result +',' ;
        result := result + IntToStr(i) ;
      end ;
  end;


constructor TFormMemoryTest.Create(AOwner: TComponent) ;
  begin
    inherited Create(AOwner) ;
    TheState := 0 ;
    MemoryGrid.OnUpdate := UpdateDisplay ; // wenn sich das grid ändert, muss diese Form reagieren
    UpdateGUI ;
  end;

destructor TFormMemoryTest.Destroy ;
  begin
    inherited ;
  end;

//  COnnection to target machine changed. New address width?
procedure TFormMemoryTest.OnConsoleChanged ;
  begin
    // get address width from current Console
    //   MemoryGrid. phyiscal address width already changed

    TheState := 0 ; // request new "Set"

    UpdateGUI ;
    UpdateDisplay(nil) ;
  end;

procedure TFormMemoryTest.UpdateGUI ;
  begin
    case TheState of
      0: begin // Input invalid
        TestDataLinesButton.Enabled := false ;
        TestAddressLinesButton.Enabled := false ;
        TestDatabitsButton.Enabled := false ;
        TestRandomButton.Enabled := false ;
      end ;
      1: begin // Operation OK
        TestDataLinesButton.Enabled := true ;
        TestAddressLinesButton.Enabled := true ;
        TestDatabitsButton.Enabled := true ;
        TestRandomButton.Enabled := true ;
      end ;
    end{ "case TheState" } ;
//    UpdateDisplay(nil) ;
  end{ "procedure TFormMemoryTest.UpdateGUI" } ;


// neue malen
procedure TFormMemoryTest.UpdateDisplay(Sender: TObject);
  begin
    // Do not update  MemoryGrid,. es ist die gnaze Zeit invisible
//    if Sender <> MemoryGrid then // hat der Frame das Update ausgelöst?
//      MemoryGrid.UpdateDisplay  // nein: update frame, er updated wieder die Form
//    else begin

    //
//    if startaddr.mat = matUnknown then begin
    if TheState = 0 then begin
      // noch nie "Set" gedrückt
      max_memoryblock_wordcount := PhysicalIopageBaseAddr(MemoryGrid.memorycellgroup.mat) div 2 ;

      startaddr.mat := MemoryGrid.memorycellgroup.mat ;
      startaddr.val := 0 ;
      endaddr.mat := MemoryGrid.memorycellgroup.mat ;
      endaddr.val := 2 * max_memoryblock_wordcount - 2 ;
      ChipAddrSizeComboBox.ItemIndex := 2 ; // "4K"
//      MemoryBlockSizeEdit.Text := Dword2OctalStr(max_memoryblock_wordcount) ;
//      Caption := setFormCaptionInfoField(Caption, '0') ;
    end ;

    StartAddrEdit.Text := Addr2OctalStr(startaddr) ;
    EndAddrEdit.Text := Addr2OctalStr(endaddr) ;
//      ChipAddrSizeEdit.Text := Dword2OctalStr(chipaddrsize);

    //MemoryBlockSizeEdit.Text := Dword2OctalStr(memoryblock_wordcount) ;
    Caption := setFormCaptionInfoField(Caption, Addr2OctalStr(startaddr)) ;

    // Das MemoryGrid ist alClient und möchte in einer bestimmten Grösse angezeigt werden,
    // tue ihm den Gefallen.
//    ClientHeight :=  MemoryGrid.optimal_height + PanelT.Height ;
//    ClientWidth := MemoryGrid.optimal_width ;
//    end;
  end{ "procedure TFormMemoryTest.UpdateDisplay" } ;


procedure TFormMemoryTest.OctalEditKeyPress(Sender: TObject;
        var Key: Char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
    TheState := 0 ;
    UpdateGUI ;
  end;


procedure TFormMemoryTest.MemoryBlockSizeEditKeyPress(Sender: TObject;
        var Key: Char);
  begin
    // nur Octalziffern als Eingabe. auch das backspace #8 erlauben
    if not CharInSet(Key, [#8, '0'..'7']) then
      Key := #0 ;
    TheState := 0 ;
    UpdateGUI ;
  end;


procedure TFormMemoryTest.MemoryGridVerify1Click(Sender: TObject);
  begin
    MemoryGrid.Verify1Click(Sender);
  end;

// set range to test
procedure TFormMemoryTest.SetStartAddrButtonClick(Sender: TObject);
  var tmp: dword ;
  begin
    TheState := 1 ; // Operation OK

    UpdateGUI ;

    // start/end come from user interface, but
    // "end-start" is limited to  max_memoryblock_wordcount
    startaddr := OctalStr2Addr(StartAddrEdit.Text, MemoryGrid.memorycellgroup.mat) ;
    startaddr.val := startaddr.val and not 1 ; // make even
    endaddr := OctalStr2Addr(EndAddrEdit.Text, MemoryGrid.memorycellgroup.mat) ;
    endaddr.val := endaddr.val and not 1 ; // make even

    case ChipAddrSizeComboBox.ItemIndex of
      0: chipaddrsize := 1024 ; // 1K
      1: chipaddrsize := 2048 ; // 2K
      2: chipaddrsize := 4096 ; // 4K
      3: chipaddrsize := 16384 ; // 16K
      4: chipaddrsize := 65536 ; // 64K
      5: chipaddrsize := 262144 ; // 256K
    end;

    if endaddr.val < startaddr.val then begin // swap?
      tmp := startaddr.val;
      startaddr.val := endaddr.val ;
      endaddr.val := tmp ;
    end ;
    memoryblock_wordcount := ((endaddr.val - startaddr.val) div 2) + 1 ;
    if memoryblock_wordcount> max_memoryblock_wordcount then
      memoryblock_wordcount:= max_memoryblock_wordcount;
    // correct again end address
    endaddr.val := startaddr.val + 2 * memoryblock_wordcount - 2 ;

    // alocate new cell range
    MemoryGrid.memorycellgroup.Clear ;
    MemoryGrid.memorycellgroup.Add(startaddr.val, memoryblock_wordcount) ;

//    if startaddr.val <> MEMORYCELL_ILLEGALVAL then begin
//    MemoryGrid.memorycellgroup.ShiftRange(startaddr, memblocksize, {optimize=}false) ;
//      MemoryGrid.memorycellgroup.ShiftRange(startaddr, -1, {optimize=}true) ;
//      MemoryGrid.memorycellgroup.Examine({optimize=}true) ;
//    end;

    // neue mcg: neu mit grid verbinden
    MemoryGrid.ConnectToMemoryCellGroup(MemoryGrid.memorycellgroup) ;
    // examine nur die zellen mit edit_value =ILLEGAL
    // nein! auf M9312 console emualtor führt jede nicht vorhandene Adresse zum stop
//    MemoryGrid.ExamineCells({unknown_only}true) ;
    UpdateDisplay(self) ;
  end{ "procedure TFormMemoryTest.SetStartAddrButtonClick" } ;


procedure TFormMemoryTest.Log(line:string);
  var datestr: string ;
  begin
    // Für Log Form: anderer Zeitstempel
    DateTimeToString(datestr, 'ddd hh:nn:ss', Now) ;
    line := Format('[%d: %s] %s', [TestLogMemo.Lines.count+1, datestr, line]) ;
    TestLogMemo.Lines.Add(line) ;
  end;

procedure TFormMemoryTest.Log(fmt: string; args: array of const) ;
  begin
    Log(Format(fmt, args)) ;
  end;

procedure TFormMemoryTest.PanelTClick(Sender: TObject);
  begin
    UpdateDisplay(nil);
  end;

{ run test procedure }

{ test data lines
   Wichtig: Resistent gegen andere Fehler (addresses, chips)
  moving 1, moving 0,

  Verfahren:
  moving one (oder moving zero) durch die 16 datenbits des PDP-11 words
  "permanent 0"/"permanent 1"/"tied together" merken.
  Das ganze für die start adresse aller Chips wiederholen.
  Nur solche bits als "permanent 0, permant 1 / tied together anzeigen,
  die an JEDER Chipadresse auftreten.

  Beispiel (vereinfachter Fall: 4 bit word):
  drei memory chips in foogendem Zustand
  #1 = "All 0" defekt
  #2 = "All 1" defekt,
  #3 = OK

  Zu foindender Fehler: Datenleitungen 2 ist "Always 1"
  Dann Shiftmuster
    Data Input    Memory  Data output
  #1: 0001     -> 0000    -> 0100
      0010     -> 0000    -> 0100
      0100     -> 0000    -> 0100
      1000     -> 0000    -> 0100

  #2: 0001     -> 1111    -> 1111
      0010     -> 1111    -> 1111
      0100     -> 1111    -> 1111
      1000     -> 1111    -> 1111

  #3: 0001     -> 0001    -> 0101
      0010     -> 0010    -> 0110
      0100     -> 0100    -> 0100
      1000     -> 1000    -> 1100

  Also: welches Bit ist immer 1/0 ?
  -> bilde "OR" aller Data output Ergebnisse
    wenn ein Bit 0: diese Datneleitung ist permannet low
  -> bilde "AND" aller Data output Ergebnisse
    wenn ein Bit 1: diese Datenleitung ist permannet high

 polarity: test with "moving 0" or "moving 1"
}


function TFormMemoryTest.TestDataLines(polarity: integer) : boolean ;

// test a single bit of 16bit word at "mc"
// polarity: 1 for "moving one", 0 for "moving zero"

// result: update von
//  stuck_l_bit_mask: OR aller test ergebnisse, invertiert
// stuck_h_bit_mask: AND aller test ergebnisse
  procedure TestSingleBit (mc: TMemoryCell ; bitnr: integer ; polarity:integer;
          var stuck_l_bit_mask: dword ;
          var stuck_h_bit_mask: dword ;
          var total_error_count: dword) ;
    var
      testval: dword ;
    begin
      testval := 1 shl bitnr ;
      if (polarity = 0) then
        testval := not testval and $ffff ; // moving zero: alle bits 1, nur 1 bit 0

      mc.edit_value := testval ;
      mc.Deposit ;
      mc.Examine ;
// mc.pdp_value := mc.pdp_value or $100 ; // bit 8 always H
//      mc.pdp_value := mc.pdp_value and not $8000 ; // bit 15 always L
      if mc.pdp_value <> testval then
        inc(total_error_count) ;
      stuck_l_bit_mask := not (not stuck_l_bit_mask or mc.pdp_value) ;
      stuck_h_bit_mask := stuck_h_bit_mask and mc.pdp_value ;
    end{ "procedure TestSingleBit" } ;

  // teste alle 16 bit eines words bei adresse "mc"
  procedure TestWord(mc: TMemoryCell; polarity:integer ;
          var stuck_l_bit_mask: dword ;
          var stuck_h_bit_mask: dword;
          var total_error_count: dword) ;
    var bitnr: integer ;
    begin
      for bitnr := 0 to 15 do
        TestSingleBit(mc, bitnr, polarity, stuck_l_bit_mask, stuck_h_bit_mask, total_error_count) ;
    end;

  var
    s: string ;
    mcg: TMemoryCellGroup ;
    mc: TMemoryCell ;
    i: integer ;
    phase: integer ;
    test_chip_end_addr: boolean ;

    testaddr: TMemoryAddress ;

    stuck_l_bit_mask: dword ;
    stuck_h_bit_mask: dword ;
    total_error_count: dword ;

    cellidx: integer ;
    all_datalines_toggled: boolean ;
  begin { "function TFormMemoryTest.TestDataLines" }

    Log('Test data lines, addr range= %s..%s, chip size = %s',
            [Dword2OctalStr(startaddr.val),
            Dword2OctalStr(endaddr.val),
            Dword2OctalStr(chipaddrsize)
            ]) ;

    mcg := MemoryGrid.memorycellgroup ;

    stuck_l_bit_mask := $ffff ; // bits are reset while testing
    stuck_h_bit_mask := $ffff ;
    total_error_count := 0 ;

    for test_chip_end_addr := false to true do begin
      s := 'Testing moving ' ;
      if polarity = 0 then s := s + 'zeros' else s := s + 'ones' ;
      s := s + ' at ' ;
      if test_chip_end_addr then s := s + 'last' else s := s + 'first' ;
      s := s + ' addr of memory chips...' ;
      Log(s) ;

      testaddr := startaddr ;
      if test_chip_end_addr then
        testaddr.val := testaddr.val + chipaddrsize - 2 ; // last word in each block
      // sammle dataline für alle Chips, Abbruch,wenn sich
      // alle Datenleitungen mal bewegt haben
      repeat
        // finde memorycell für die adresse
        cellidx := mcg.CellIndexByAddr(testaddr) ; // Zelle über Adresse finden. -1: not found
        if cellidx < 0 then
          assert(cellidx >= 0) ; // random range must be ok
        mc := mcg.Cell(cellidx) ;
        TestWord(mc, polarity, stuck_l_bit_mask, stuck_h_bit_mask, total_error_count) ;
        testaddr.val := testaddr.val + chipaddrsize ; // next chip
        all_datalines_toggled := (stuck_l_bit_mask = 0) and (stuck_h_bit_mask = 0) ;
      until all_datalines_toggled or (testaddr.val > endaddr.val) ;
      if all_datalines_toggled then
        Break ;
    end { "for test_chip_end_addr" } ;


    // Mögliche Ergebnisse:
    //  stuck_l_bit_mask oder stuck_h_bit_mask: tote data lines
    // weder stuck_l_bit_mask noch stuck_h_bit_mask: keine
    //  tote data lines, aber möglicherweise andre Fehler
    if not all_datalines_toggled then begin
      Log('Data lines not OK.') ;
      Log('Lines detected as permanent "high": %s,  bits= %s', [
              Dword2OctalStr(stuck_h_bit_mask, 16),
              BitnumbersAsText(stuck_h_bit_mask)]);
      Log('Lines detected as permanent "low" : %s, bits = %s', [
              Dword2OctalStr(stuck_l_bit_mask, 16),
              BitnumbersAsText(stuck_l_bit_mask)]);
      result := false ;
    end else
      if total_error_count = 0 then begin
        Log('OK') ;
        result := true ;
      end else begin
        Log('Data lines OK, but %d other errors!', [total_error_count]) ;
        result := false ;
      end ;

  end{ "function TFormMemoryTest.TestDataLines" } ;


{ Adress line test
Phase: 1 : finde gespieglte Bereiche
schreibe an adresse 000, 0001, 0002, 0004, 0010, ...
je die adresse, dann Kontrolle.
Wenn zwei zellen gleich:  Adressline stuck,.

Phase 2:
Wiederhole adressline test A1 mit inveriterten Adressen
177777, 177776, 177775, 177773, ....
wenn A1 ok und A2 fail: the "high" adress bits modify the low address bits
-> address line short cut
}

function TFormMemoryTest.TestAdressLines(phase: integer): boolean ;

  var
    mcg: TMemoryCellGroup ;
    mc: TMemoryCell ;
    addr: TMemoryAddress ;
    i, tryaddrbitno, cellidx:  integer ;
    tmp, addr_varpart_bitmask: dword ; // bitmask for variable part of addresses
    addr_varpart_bitno: dword ;
    addrval, tryaddrval: dword ;
    dataval: dword ;
  begin
    Log('Test address lines, phase %d, addr range= %s..%s.',
            [phase, Dword2OctalStr(startaddr.val), Dword2OctalStr(endaddr.val)]) ;
    mcg := MemoryGrid.memorycellgroup ;

    // addrbitmask: masks bits in address which are varied
    // staraddr = 0-> mask = 0
    // start addr = $1000 -> mask = $0fff
    if startaddr.val = 0 then
      addr_varpart_bitmask := $ffffffff // all bits may be used
    else begin
      tmp := startaddr.val ;
      addr_varpart_bitmask := 0 ;
      while (tmp and 1) = 0 do begin
        tmp := tmp shr 1 ;
        addr_varpart_bitmask := (addr_varpart_bitmask shl 1) or 1 ;
      end ;
    end ;
    //addr_varpart_bitmask := addr_varpart_bitmask shr 1; // one less

    for i := 0 to mcg.getCount-1 do
      mcg.Cell(i).tag := 0 ; // tag: <> 0 = tested

    // two phases, each writing and checking
    if phase = 1 then // 2er logarithmus = anzahl addressleitungen
      BusyForm.Start('Checking address lines ...', 2 * 2 * round(ln(mcg.getCount)/ln(2)), true);

    Log('Writing  ...');
    for addr_varpart_bitno := 0 to 21 do begin
      // adr.val: 0,2,4,8,16,32,.... SKIP1: only even adresses!
      if addr_varpart_bitno = 0 then
        addrval := 0
      else
        addrval := 1 shl addr_varpart_bitno ; // moving one in address bits
      addrval := (startaddr.val and not addr_varpart_bitmask) + addrval ;

      // test only address lines which fell into selected memory window
      if (addrval >= startaddr.val) and (addrval <= endaddr.val) then begin
        if phase <> 1 then begin
          // invert as much address bits as possible, but addr must still fit into range
          tryaddrbitno := 0 ;
          tryaddrval := addrval ;
          while (tryaddrval >= startaddr.val) and (tryaddrval <= endaddr.val)  do begin
            addrval := tryaddrval ;
            inc(tryaddrbitno) ; // set next bit and range := TComponent.Create(Self);
            // only even addresses: do not flip bit!
            // this is guranted because  tryaddrval > 0
            tryaddrval := addrval xor (1 shl tryaddrbitno) ; // flip next bit
          end;
        end;

        addr := startaddr;
        addr.val := addrval ;

        //  pattern from address bit
        dataval := addrval and $ffff ; // write address into cell
        cellidx := mcg.CellIndexByAddr(addr) ; // Zelle über Adresse finden. -1: not found
        if cellidx < 0 then
          assert(cellidx >= 0) ; // addr must be in range
        mc := mcg.Cell(cellidx) ;
        mc.tag := 1 ; // mark for check
        //Log('mem[%s] := %s', [Addr2OctalStr(addr), Dword2OctalStr(value, 16)]);
        mc.edit_value := dataval ; // write address into cell
        mc.Deposit ;
        BusyForm.StepIt ; if BusyForm.Aborted then Break ;
      end { "if (addrval >= startaddr.val) and (addrval <= endaddr.val)" } ;
    end { "for addr_varpart_bitno" } ;

    result := true ;
    if not BusyForm.Aborted then begin
      Log('Checking ...') ;
      // checking in ascending order
      for i := 0 to mcg.getCount-1 do
        if  mcg.Cell(i).tag <> 0 then begin
          // tag: <> 0 = tested
          mc := mcg.Cell(i) ;
          mc.Examine ;
          BusyForm.StepIt ; if BusyForm.Aborted then Break ;

          // each cell must contain the address

          if mc.edit_value <> mc.pdp_value then begin
            Log('Error: word at %s is %s, should be %s, diff mask = %s', [
                    Addr2OctalStr(mc.addr),
                    Dword2OctalStr(mc.pdp_value, 16),
                    Dword2OctalStr(mc.edit_value, 16),
                    Dword2OctalStr(mc.pdp_value xor mc.edit_value, 16)
                    ]) ;
            Log('This maybe an error in address line %d', [
                    getLowestBitNo(mc.pdp_value xor mc.edit_value)]) ;

            result := false ; // failure
          end;
        end{ "if mcg.Cell(i).tag <> 0" } ;
    end { "if not BusyForm.Aborted" } ;
    if BusyForm.Aborted then
      Log('Abort.')
    else if result then
      Log('OK.')
    else
      Log('There were errors.') ;
  end{ "function TFormMemoryTest.TestAdressLines" } ;

{ Data static bit test
Tests dead memory chips.
It is assumed thant one chip delivers one or more databits
in an consequtive addressrange of <blocksize>.
set <blocksize> to 1K (smallest memory chip in use.

Phase 1:
  fill first 16 bytes in every <block> with "shifting one data"
  mem[$1000] := 1; mem[$1001] := 2; mem[$1002] := 4; ... mem[$100f] := $8000;
  then verify.
  Find static dead chips.

Phase 2:
  as D1, but inverted bitpattern at end of each block
  mem[$1ff0] := $fffe; mem[$1ff1] := $fffd; ... mem[$1fff] := $7fff;

  So there are 32 accesses per block.
  With 1 K block, and

}

function TFormMemoryTest.TestDatabits(phase: integer): boolean;


// get data values for address, depending on variant
// addresses are even word addresses
  function getDataval(phase: integer ; addrval:dword): dword ;
    begin
      // xx00 .. xx1e -> $0001 ... $8000
      // phase 2:
      // xx00 .. xx1e -> $fffe ... $7ffff
      addrval := (addrval div 2) and $f ;
      result := 1 shl addrval ;
      if phase <> 1 then
        result := (not result) and $ffff ;
    end;

  var
    mcg: TMemoryCellGroup ;
    mc: TMemoryCell ;
    testblocknr : integer ;
    testblock_startaddr, testblock_endaddr, dataaddr: TMemoryAddress ;
    i: integer ;
    cellidx: integer ;
  begin { "function TFormMemoryTest.TestDatabits" }
    Log('Test data bits, phase %d, addr range= %s..%s, chip size = %s',
            [phase, Dword2OctalStr(startaddr.val),
            Dword2OctalStr(endaddr.val),
            Dword2OctalStr(chipaddrsize)
            ]) ;

    mcg := MemoryGrid.memorycellgroup ;

    for i := 0 to mcg.getCount-1 do
      mcg.Cell(i).tag := 0 ; // tag: <> 0 = tested

    if phase = 1 then
      // 2 phses, je write&check, auf 16 addressen, pro chip
      BusyForm.Start('Checking chips ...', round(2 * 2 *16 * (endaddr.val-startaddr.val)  / chipaddrsize ), true);

    testblocknr := 0 ;
    testblock_startaddr := startaddr ;
    if phase <> 1 then
      testblock_startaddr.val := testblock_startaddr.val + chipaddrsize - 32 ; // last 16 words in each block
    testblock_endaddr := startaddr ; // 16 words at chip block start
    testblock_endaddr.val :=  testblock_startaddr.val+32-2 ; // last word in chip block end
    while testblock_endaddr.val <= endaddr.val do begin
      // for each chip ...
      Log('  Chip block %d: Writing address range = %s .. %s', [
              testblocknr,
              Dword2OctalStr(testblock_startaddr.val),
              Dword2OctalStr(testblock_endaddr.val) ]);
      for i := 0 to 15 do begin
        // each data bit
        dataaddr := testblock_startaddr ;
        dataaddr.val := testblock_startaddr.val + 2*i ; // addr for each data bit

        //    if (dataaddr.val >= startaddr.val) and (dataaddr.val <= endaddr.val) then begin
        // last check
        cellidx := mcg.CellIndexByAddr(dataaddr) ; // Zelle über Adresse finden. -1: not found
        if cellidx >= 0 then begin
          // current test address not in range, skip
//          assert(cellidx >= 0) ; // addr must be in range
          mc := mcg.Cell(cellidx) ;
          mc.tag := 1 ; // mark for check
          //Log('mem[%s] := %s', [Addr2OctalStr(addr), Dword2OctalStr(value, 16)]);
          mc.edit_value := getDataval(phase, dataaddr.val) ;
          mc.Deposit ;
          BusyForm.StepIt ; if BusyForm.Aborted then Break ;

        end ;
      end { "for i" } ;
      // move block
      inc(testblocknr);
      testblock_startaddr.val := testblock_startaddr.val + chipaddrsize ; // next block
      testblock_endaddr.val := testblock_endaddr.val + chipaddrsize ; // next block
    end { "while testblock_endaddr.val <= endaddr.val" } ;


    result := true ;

    // checking in ascending order
    if not BusyForm.Aborted then begin
      Log('Checking ...') ;
      for i := 0 to mcg.getCount-1 do
        if  mcg.Cell(i).tag <> 0 then begin
          // tag: <> 0 = tested
          mc := mcg.Cell(i) ;
          mc.Examine ;
          BusyForm.StepIt ; if BusyForm.Aborted then Break ;

          // each cell must contain the address

          if mc.edit_value <> mc.pdp_value then begin
            Log('Error: word at %s is %s, should be %s, diff mask = %s', [
                    Addr2OctalStr(mc.addr),
                    Dword2OctalStr(mc.pdp_value, 16),
                    Dword2OctalStr(mc.edit_value, 16),
                    Dword2OctalStr(mc.pdp_value xor mc.edit_value, 16)
                    ]) ;
            Log('This maybe an error in data chip for bit %d', [
                    getLowestBitNo(mc.pdp_value xor mc.edit_value)]) ;

            result := false ; // failure
          end;
        end { "if mcg.Cell(i).tag <> 0" } ;
    end { "if not BusyForm.Aborted" } ;
    if BusyForm.Aborted then
      Log('Abort.')
    else if result then
      Log('OK.')
    else
      Log('There were errors.') ;
  end{ "function TFormMemoryTest.TestDatabits" } ;


{ Random test R1: fill <n> random cells with random values
  cont: number of read/write cycles
}

function TFormMemoryTest.TestRandom(count: integer): boolean;
  var
    mcg: TMemoryCellGroup ;
    mc: TMemoryCell ;
    i, cellidx: integer ;
    addr: TMemoryAddress ;
    datavalue: dword ;
  begin
    Log('Random test, addr range =  %s..%s: write %d cells, then check.',
            [Dword2OctalStr(startaddr.val), Dword2OctalStr(startaddr.val+2*memoryblock_wordcount-2), count]) ;
    mcg := MemoryGrid.memorycellgroup ;

    for i := 0 to mcg.getCount-1 do
      mcg.Cell(i).tag := 0 ; // tag: <> 0 = tested

    // writing and checking
    BusyForm.Start('Testing random words...', 2 * count, true);

    Log('Writing ...') ;
    // writing in random order
    for i := 0 to count-1 do begin
      addr := startaddr;
      addr.val := startaddr.val + 2 * random(memoryblock_wordcount) ; // only even addresses
      cellidx := mcg.CellIndexByAddr(addr) ; // Zelle über Adresse finden. -1: not found
      if cellidx < 0 then
        assert(cellidx >= 0) ; // random range must be ok
      mc := mcg.Cell(cellidx) ;
      mc.tag := 1 ; // mark for check
      datavalue := random($10000) ; // 16bit random value
      //Log('mem[%s] := %s', [Addr2OctalStr(addr), Dword2OctalStr(value, 16)]);
      mc.edit_value := datavalue ;
      mc.Deposit ;
      BusyForm.StepIt ; if BusyForm.Aborted then Break ;

    end{ "for i" } ;

    if not BusyForm.Aborted then begin

      Log('Checking ...') ;

      // checking in ascending order
      result := true ;
      for i := 0 to mcg.getCount-1 do
        if  mcg.Cell(i).tag <> 0 then begin
          // tag: <> 0 = tested
          mc := mcg.Cell(i) ;
          mc.Examine ;
          BusyForm.StepIt ; if BusyForm.Aborted then Break ;

          if mc.edit_value <> mc.pdp_value then begin
            Log('Error: word at %s is %s, should be %s, diff mask = %s', [
                    Addr2OctalStr(mc.addr),
                    Dword2OctalStr(mc.pdp_value, 16),
                    Dword2OctalStr(mc.edit_value, 16),
                    Dword2OctalStr(mc.pdp_value xor mc.edit_value, 16)
                    ]) ;
            result := false ; // failure
          end;
        end { "if mcg.Cell(i).tag <> 0" } ;
    end{ "if not BusyForm.Aborted" } ;
    if BusyForm.Aborted then
      Log('Abort.')
    else if result then
      Log('OK.')
    else
      Log('There were errors.') ;
  end{ "function TFormMemoryTest.TestRandom" } ;


procedure TFormMemoryTest.TestDataLinesButtonClick(Sender: TObject);
  begin
    // versuche moving 1 an verschiedenen Chips,
    // bis eine davon eine gute Speicherzelle enthält
    // moving oder moving 1
    if not TestDataLines(1) then
      Exit ;
    TestDataLines(0) ;
  end;


procedure TFormMemoryTest.TestRandomButtonClick(Sender: TObject);
  begin
    try
      TestRandom(32);
    finally
      BusyForm.Close ;
    end;
  end;



procedure TFormMemoryTest.TestAddressLinesButtonClick(Sender: TObject);
  begin
    try
      if not TestAdressLines(1) then
        Exit ;
      if not BusyForm.Aborted then
        TestAdressLines(2);
    finally
      BusyForm.Close ;
    end;
  end;


procedure TFormMemoryTest.TestDatabitsButtonClick(Sender: TObject);
  begin
    try
      if not TestDatabits(1) then
        Exit ;
      if not BusyForm.Aborted then
        TestDatabits(2);
    finally
      BusyForm.Close ;
    end;
  end;



end{ "unit FormMemoryTestU" } .


